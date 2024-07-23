{- |
Module:      PFile.Main.List
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Main for `pfile ls`.
-}

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NamedFieldPuns   #-}

module PFile.Main.List
  ( main
  ) where

import           PFile.CLI.List (Options (..))
import           PFile.Env      (Env)
import           PFile.Error    (modifyError)
import qualified PFile.Log      as Log
import qualified PFile.Profile  as Profile
import           Protolude

main :: (MonadReader Env m, MonadIO m) => Options -> m ()
main Options {shouldFilterDangling} =
  either Log.panic pure <=< runExceptT $
    Profile.list listOptions
      & modifyError Profile.showListError
      >>= traverse_ (putStrLn . Profile.unName . Profile.name)
  where
    listOptions :: Profile.ListOptions
    listOptions = Profile.ListOptions {Profile.shouldFilterDangling}
