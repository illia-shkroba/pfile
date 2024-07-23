{- |
Module:      PFile.Main.Which
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Main for `pfile which`.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Main.Which
  ( main
  ) where

import           PFile.CLI.Which (Options (..))
import           PFile.Env       (Env)
import qualified PFile.Log       as Log
import qualified PFile.Profile   as Profile
import           Protolude

main :: (MonadReader Env m, MonadIO m) => Options -> m ()
main Options =
  Profile.loadCurrent & runExceptT >>= \case
    Left error -> do
      Log.info $ Profile.showLoadCurrentError error
      Log.panic
        $  "No current profile set. Use `pfile new` to create a profile and"
        <> " `pfile switch` to switch to it."
    Right Profile.Profile {Profile.name = Profile.Name name} -> putStrLn name
