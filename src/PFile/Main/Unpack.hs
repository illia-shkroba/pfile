{- |
Module:      PFile.Main.Unpack
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Main for `pfile unpack`.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE NamedFieldPuns   #-}

module PFile.Main.Unpack
  ( main
  ) where

import           PFile.CLI.Unpack (Options (..))
import           PFile.Env        (Env)
import           PFile.Error      (modifyError)
import qualified PFile.Log        as Log
import qualified PFile.Profile    as Profile
import           Protolude

main :: (MonadReader Env m, MonadIO m) => Options -> m ()
main Options {forceRemoveOccupied} =
  either Log.panic pure <=< runExceptT $ do
    Profile.loadCurrent & runExceptT >>= \case
      Left error -> Log.panic $ Profile.showLoadCurrentError error
      Right currentProfile ->
        Profile.unpack switchOptions currentProfile
          & modifyError Profile.showUnpackError

    Profile.unsetCurrent
      & modifyError Profile.showUnsetCurrentError
  where
    switchOptions :: Profile.SwitchOptions
    switchOptions = Profile.SwitchOptions {Profile.forceRemoveOccupied}
