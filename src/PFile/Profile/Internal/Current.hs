{- |
Module:      PFile.Profile.Internal.Current
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for managing current profile.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Profile.Internal.Current
  ( loadCurrent
  , showLoadCurrentError
  , LoadCurrentError (..)
  , setCurrent
  , unsetCurrent
  , showSetCurrentError
  , SetCurrentError (..)
  , showUnsetCurrentError
  , UnsetCurrentError (..)
  ) where

import           PFile.Env                            (Env (..))
import           PFile.Error
  ( liftIOWithError
  , modifyError
  )
import qualified PFile.Log                            as Log
import           PFile.Path
  ( canonicalizePath
  , createDirectoryLink
  , takeBaseName
  )
import qualified PFile.Path                           as Path
import           PFile.Profile.Internal.Profile
  ( Name (..)
  , Profile (..)
  , profileRoot
  )
import           PFile.Profile.Internal.Serialization
  ( LoadError
  , load
  , showLoadError
  )
import           Protolude

-- | 'PFile.Profile.Internal.Serialization.load' current 'Profile'. Current
-- 'Profile' is resolved via 'PFile.Env.currentLinkPath'.
--
-- @since 0.1.0.0
loadCurrent ::
     (MonadReader Env m, MonadError LoadCurrentError m, MonadIO m) => m Profile
loadCurrent = do
  Env {currentLinkPath} <- ask
  Log.info "Canonicalize link pointing at current profile"
  currentPath <- canonicalizePath currentLinkPath
    `liftIOWithError` CanonicalizeCurrentError currentLinkPath
  Log.info
    $  "Canonicalized link pointing at current profile: "
    <> Path.showAbsolute currentPath
  currentPath
    & Name . toS . takeBaseName
    & load
    & modifyError LoadCurrentError

showLoadCurrentError :: LoadCurrentError -> Text
showLoadCurrentError = \case
  CanonicalizeCurrentError path cause
    -> "Unable to resolve current link " <> Path.showAbsolute path
    <> " because of: " <> show cause
  LoadCurrentError cause -> showLoadError cause

-- | Error thrown by 'loadCurrent'.
--
-- @since 0.1.0.0
data LoadCurrentError
  = CanonicalizeCurrentError !Path.Absolute !IOException
  -- ^ Unable to canonicalize 'PFile.Env.currentLinkPath'.
  | LoadCurrentError !LoadError
  -- ^ Error was encountered during
  -- 'PFile.Profile.Internal.Serialization.load'.

-- | Set current 'Profile'. Previously set 'Profile' is unset via
-- 'unsetCurrent' and then 'PFile.Env.currentLinkPath' is set to point at a new
-- current 'Profile'.
--
-- @since 0.1.0.0
setCurrent ::
     (MonadReader Env m, MonadError SetCurrentError m, MonadIO m)
  => Profile
  -> m ()
setCurrent Profile {name} = do
  Env {currentLinkPath} <- ask
  unsetCurrent
    & modifyError UnsetCurrentError
  root <- profileRoot name
  createDirectoryLink root currentLinkPath
    & modifyError CurrentLinkError

-- | Unset current 'Profile'. 'PFile.Env.currentLinkPath' is removed.
--
-- @since 0.1.0.0
unsetCurrent ::
     (MonadReader Env m, MonadError UnsetCurrentError m, MonadIO m) => m ()
unsetCurrent = do
  Env {currentLinkPath} <- ask
  Path.remove currentLinkPath
    & modifyError CurrentLinkRemoveError

showSetCurrentError :: SetCurrentError -> Text
showSetCurrentError = \case
  UnsetCurrentError cause -> showUnsetCurrentError cause
  CurrentLinkError cause  -> Path.showCreateDirectoryLinkError cause

-- | Error thrown by 'setCurrent'.
--
-- @since 0.1.0.0
data SetCurrentError
  = UnsetCurrentError !UnsetCurrentError
  -- ^ Error was encountered during 'unsetCurrent'.
  | CurrentLinkError !Path.CreateDirectoryLinkError
  -- ^ Unable to create a directory link 'PFile.Env.currentLinkPath' pointing
  -- at a new current 'Profile'.

showUnsetCurrentError :: UnsetCurrentError -> Text
showUnsetCurrentError = \case
  CurrentLinkRemoveError cause
    -> "Unable to remove current profile link because of: "
    <> Path.showRemoveError cause

-- | Error thrown by 'unsetCurrent'.
--
-- @since 0.1.0.0
newtype UnsetCurrentError
  = CurrentLinkRemoveError Path.RemoveError
  -- ^ Unable to remove a directory link 'PFile.Env.currentLinkPath'.
