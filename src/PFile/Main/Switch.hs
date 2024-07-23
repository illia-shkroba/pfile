{- |
Module:      PFile.Main.Switch
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Main for `pfile switch`.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Main.Switch
  ( main
  ) where

import           PFile.CLI.Switch (Options (..))
import           PFile.Env        (Env)
import           PFile.Error      (modifyError)
import qualified PFile.Log        as Log
import qualified PFile.Profile    as Profile
import           Protolude

main :: (MonadReader Env m, MonadIO m) => Options -> m ()
main Options {forceRemoveOccupied, nextProfileName} =
  either Log.panic pure <=< runExceptT $ do
    nextProfile <-
      Profile.load (Profile.Name nextProfileName) & runExceptT >>= \case
        Left error -> do
          Log.info $ Profile.showLoadError error
          Log.panic $ "Unable to load profile: \"" <> nextProfileName <> "\""
        Right profile -> pure profile

    Profile.loadCurrent & runExceptT >>= \case
      Left error -> do
        Log.info $ Profile.showLoadCurrentError error
        Profile.link switchOptions nextProfile
          & modifyError Profile.showLinkError
      Right currentProfile ->
        Profile.switch switchOptions currentProfile nextProfile
          & modifyError Profile.showSwitchError

    Profile.setCurrent nextProfile
      & modifyError Profile.showSetCurrentError
  where
    switchOptions :: Profile.SwitchOptions
    switchOptions = Profile.SwitchOptions {Profile.forceRemoveOccupied}
