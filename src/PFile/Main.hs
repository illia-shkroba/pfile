{- |
Module:      PFile.Main
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

PFile's Main.
-}

{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}

module PFile.Main
  ( main
  ) where

import           Options.Applicative (execParser)
import           PFile.CLI           (Command (..), Options (..), parserInfo)
import qualified PFile.Env           as Env
import qualified PFile.Log           as Log
import qualified PFile.Main.List     as List
import qualified PFile.Main.New      as New
import qualified PFile.Main.Switch   as Switch
import qualified PFile.Main.Unpack   as Unpack
import qualified PFile.Main.Which    as Which
import           Protolude

main :: IO ()
main = do
  Options {verbose, command} <- execParser parserInfo
  env <- Env.resolve Env.Options {Env.verbose}
  flip runReaderT env do
    Log.info $ Env.description env
    case command of
      New options    -> New.main options
      Switch options -> Switch.main options
      Unpack options -> Unpack.main options
      List options   -> List.main options
      Which options  -> Which.main options
