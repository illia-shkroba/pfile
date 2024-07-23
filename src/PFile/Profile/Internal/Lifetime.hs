{- |
Module:      PFile.Profile.Internal.Lifetime
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for managing profiles lifetime.
-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module PFile.Profile.Internal.Lifetime
  ( create
  , showCreateError
  , CreateError (..)
  , showCreateRollbackCause
  , CreateRollbackCause (..)
  , CreateOptions (..)
  ) where

import           Control.Monad.Writer                 (execWriterT)
import           PFile.Env                            (Env)
import           PFile.Error
  ( fallback
  , modifyError
  , tellError
  )
import qualified PFile.Log                            as Log
import qualified PFile.Mount                          as Mount
import qualified PFile.Path                           as Path
import           PFile.Profile.Internal.Profile
  ( Entry (..)
  , Name (..)
  , Profile (..)
  , State (..)
  , profileRoot
  )
import           PFile.Profile.Internal.Registry
  ( PopError
  , PushError
  , pop
  , pushAll
  , showPopError
  , showPushError
  )
import           PFile.Profile.Internal.Serialization
  ( DumpError
  , dump
  , load
  , showDumpError
  , showLoadError
  )
import qualified PFile.Profile.LinkHandling           as LinkHandling
import           Protolude                            hiding (state)

-- | Create a new profile called 'PFile.Profile.Internal.Profile.Name' with
-- a list of 'PFile.Path.Absolute' filesystem's objects to be
-- 'PFile.Profile.Internal.Registry.push'ed inside of
-- a 'PFile.Env.profilesHomeDirPath' directory. When an error is encountered
-- during 'PFile.Profile.Internal.Registry.pushAll', 'create' attempts to
-- rollback. If the rollback fails, the profile is considered
-- 'PFile.Profile.Internal.Profile.Dangling'. Only
-- 'PFile.Profile.Internal.Profile.Valid' profiles are returned.
--
-- @since 0.1.0.0
create ::
     forall m. (MonadReader Env m, MonadError CreateError m, MonadIO m)
  => CreateOptions
  -- ^ Options that control 'create' behaviour (currently only
  -- 'linkHandlingStrategy').
  -> Name
  -- ^ 'PFile.Profile.Internal.Profile.Name' of a profile to be created. The
  -- name will be used as a directory name for the profile in
  -- 'PFile.Env.profilesHomeDirPath'.
  -> [Path.Absolute]
  -- ^ List of 'PFile.Path.Absolute' paths of filesystem's objects to be
  -- 'PFile.Profile.Internal.Registry.push'ed into a profile.
  -> m Profile
create CreateOptions {linkHandlingStrategy} name originPaths = do
  load name & runExceptT >>= either
    (Log.info . showLoadError)
    (const . throwError $ ProfileAlreadyExistsError name)
  entries <- pushAll linkHandlingStrategy name originPaths
    & fallback rollbackMounts . modifyError PushError
  let profile = Profile {name, state = Valid, entries}
  dump profile & runExceptT >>= flip either pure
    \error -> rollbackMounts (DumpError error) entries
  pure profile
  where
    rollbackMounts :: CreateRollbackCause -> [Entry] -> m ()
    rollbackMounts cause entries = do
      root <- profileRoot name
      entries
        & traverse_ (\entry -> pop name (mountPath entry) & tellError (entry, ))
        & execWriterT >>= \case
          errors@(_:_) ->
            -- Since we were unable to rollback all the 'mount' calls, some of
            -- the paths are still in the profile. Thus, we should:
            --
            -- * allow user to retrieve the paths from the created profile,
            -- * dump the profile as "dangling", since it wasn't fully created
            --
            -- 'dump' of "dangling" profile could also fail. In this case, the
            -- user should be informed appropriately.
            dump Profile {name, state = Dangling, entries = fst <$> errors}
              &   fmap leftToMaybe . runExceptT
              <&> PushRollbackError cause errors root
              >>= throwError
          [] -> do
            -- Since we were able to rollback all the 'mount' calls, we can
            -- remove partially created profile and then interrupt.
            Path.remove root
              & void . runExceptT
            throwError $ PushCreateError cause

showCreateError :: CreateError -> Text
showCreateError = \case
  ProfileAlreadyExistsError (Name name)
    -> "Profile named \"" <> name <> "\" already exists."
  PushRollbackError rollbackCause rollbackErrors root maybeDumpError
    -> "`new` has failed"
    <> " with the following error: " <> showCreateRollbackCause rollbackCause
    <> "\nAttempt to unmount paths has failed"
    <> " with the following errors:\n" <> showRollbackErrors rollbackErrors
    <> "Dangling profile with mounted paths could be found here: "
    <> Path.showAbsolute root
    <> maybe "." showDumpDanglingError maybeDumpError
  PushCreateError cause -> showCreateRollbackCause cause
  where
    showRollbackErrors :: [(Entry, PopError)] -> Text
    showRollbackErrors = unlines . fmap
      \(Entry {mountPath = Mount.Mount mountPath, originPath}, error)
        -> Path.showAbsolute originPath
        <> " -> " <> Path.showAbsolute mountPath <> " ("
        <> showPopError error <> ")"

    showDumpDanglingError :: DumpError -> Text
    showDumpDanglingError cause
      =  "\nThe profile was not marked as dangling, so you would have to"
      <> " remove it (and recover entries stored in it) manually: "
      <> showDumpError cause

-- | Error thrown by 'create'.
--
-- @since 0.1.0.0
data CreateError
  = ProfileAlreadyExistsError !Name
  -- ^ 'PFile.Profile.Internal.Profile.Profile with
  -- 'PFile.Profile.Internal.Profile.Name' was found in
  -- 'PFile.Env.profilesHomeDirPath'.
  | PushRollbackError
  -- ^ 'create' attempted to rollback due to 'CreateRollbackCause'. The
  -- rollback has failed with a list of
  -- 'PFile.Profile.Internal.Registry.PopError's. Since the rollback has
  -- failed, the profile passed to 'create' is considered
  -- 'PFile.Profile.Internal.Profile.Dangling'.
    !CreateRollbackCause
    -- ^ Cause of rollback.
    ![(Entry, PopError)]
    -- ^ List of errors encountered during rollback.
    !Path.Absolute
    -- ^ Path to a profile's root directory.
    !(Maybe DumpError)
    -- ^ Possible error that could appear during
    -- 'PFile.Profile.Internal.Serialization.dump' attempt of the profile.
  | PushCreateError !CreateRollbackCause
  -- ^ 'create' attempted to rollback due to 'CreateRollbackCause'. The
  -- rollback has succeeded. The profile passed to 'create' was not created.

showCreateRollbackCause :: CreateRollbackCause -> Text
showCreateRollbackCause = \case
  PushError cause -> showPushError cause
  DumpError cause -> showDumpError cause

-- | 'create' rollback cause.
--
-- @since 0.1.0.0
data CreateRollbackCause
  = PushError !PushError
  -- ^ Error was encountered during 'PFile.Profile.Internal.Registry.pushAll'.
  | DumpError !DumpError
  -- ^ Error was encountered during
  -- 'PFile.Profile.Internal.Serialization.dump'.

-- | 'create' options.
--
-- @since 0.1.0.0
newtype CreateOptions
  = CreateOptions
      { linkHandlingStrategy :: LinkHandling.Strategy
      -- ^ 'PFile.Profile.LinkHandling.Strategy' to be used when
      -- 'PFile.Profile.Internal.Registry.push'ing a link into a profile.
      }
