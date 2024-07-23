{- |
Module:      PFile.Profile.Internal.List
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for listing profiles.
-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PFile.Profile.Internal.List
  ( list
  , showListError
  , ListError (..)
  , ListOptions (..)
  ) where

import           PFile.Env                            (Env (..))
import           PFile.Error                          (liftIOWithError)
import qualified PFile.Log                            as Log
import           PFile.Path
  ( doesPathExist
  , listDirectory
  , takeBaseName
  )
import qualified PFile.Path                           as Path
import           PFile.Profile.Internal.Profile
  ( Name (..)
  , Profile (..)
  , State (..)
  )
import           PFile.Profile.Internal.Serialization (load, showLoadError)
import           Protolude                            hiding (list, state)

-- | List profiles in 'PFile.Env.profilesHomeDirPath' directory.
--
-- @since 0.1.0.0
list ::
     forall m. (MonadReader Env m, MonadError ListError m, MonadIO m)
  => ListOptions
  -- ^ Options that control 'list' behaviour (currently only
  -- 'shouldFilterDangling').
  -> m [Profile]
list ListOptions {shouldFilterDangling} = do
  Env {profilesHomeDirPath} <- ask
  unlessM (doesPathExist profilesHomeDirPath) . throwError
    $ ProfilesHomeDirDoesNotExistError profilesHomeDirPath
  Log.info
    $  "List profile names in "
    <> Path.showAbsolute profilesHomeDirPath
  names <- listDirectory profilesHomeDirPath
    `liftIOWithError` ListDirectoryError profilesHomeDirPath
    <&> fmap (Name . toS . takeBaseName)
  Log.info $ "Load profiles: " <> show (unName <$> names)
  profiles <- catMaybes <$> forM names \name ->
    load name & runExceptT >>= \case
      Left error -> do
        Log.warning $ showLoadError error
        pure Nothing
      Right profile -> pure $ Just profile
  if shouldFilterDangling
    then do
      Log.info "Filter Dangling profiles"
      filterDangling profiles
    else pure profiles
  where
    filterDangling :: [Profile] -> m [Profile]
    filterDangling = filterM \Profile {name, state} ->
      case state of
        Dangling -> do
          Log.warning $ "Found Dangling profile: \"" <> unName name <> "\""
          pure False
        Valid -> pure True

showListError :: ListError -> Text
showListError = \case
  ProfilesHomeDirDoesNotExistError path
    -> "Profiles home directory " <> Path.showAbsolute path
    <> " does not exist."
  ListDirectoryError path cause
    -> "Unable to list profiles home directory " <> Path.showAbsolute path
    <> " because of: " <> show cause

-- | Error thrown by 'list'.
--
-- @since 0.1.0.0
data ListError
  = ProfilesHomeDirDoesNotExistError !Path.Absolute
  -- ^ 'PFile.Env.profilesHomeDirPath' does not exist.
  | ListDirectoryError !Path.Absolute !IOException
  -- ^ 'IOException' was encountered during directory listing.

-- | 'list' options.
--
-- @since 0.1.0.0
newtype ListOptions
  = ListOptions
      { shouldFilterDangling :: Bool
      -- ^ Whether 'list' should filter out
      -- 'PFile.Profile.Internal.Profile.Dangling' profiles.
      }
