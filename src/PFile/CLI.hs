{- |
Module:      PFile.CLI
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

PFile's CLI definition.
-}

{-# LANGUAGE ApplicativeDo     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module PFile.CLI
  ( parserInfo
  , parser
  , Options (..)
  , Command (..)
  ) where

import           Options.Applicative
  ( Parser
  , ParserInfo
  , fullDesc
  , header
  , helper
  , info
  , long
  , progDesc
  , short
  , subparser
  , switch
  )
import qualified Options.Applicative
import qualified PFile.CLI.List      as List
import qualified PFile.CLI.New       as New
import qualified PFile.CLI.Switch    as Switch
import qualified PFile.CLI.Unpack    as Unpack
import qualified PFile.CLI.Which     as Which
import           Protolude

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper)
  $  fullDesc
  <> header "pfile - manage profiles defined for a set of filesystem's objects"
  <> progDesc description
  where
    description
      =  "Manage profiles defined for a set of filesystem's objects"
      <> " (directories, directory links, files, file links). Profiles could be"
      <> " defined with `pfile new`. Switch between multiple profiles with"
      <> " `pfile switch`. In order to revert the `pfile new`/`pfile switch`,"
      <> " use `pfile unpack`."

parser :: Parser Options
parser = do
  verbose <- switch (short 'v' <> long "verbose")
  command <- subparser . fold $
    [ Options.Applicative.command "new" (New <$> New.parserInfo)
    , Options.Applicative.command "switch" (Switch <$> Switch.parserInfo)
    , Options.Applicative.command "unpack" (Unpack <$> Unpack.parserInfo)
    , Options.Applicative.command "ls" (List <$> List.parserInfo)
    , Options.Applicative.command "which" (Which <$> Which.parserInfo)
    ]
  pure Options {..}

data Options
  = Options
      { verbose :: !Bool
      , command :: !Command
      }

data Command
  = New New.Options
  | Switch Switch.Options
  | Unpack Unpack.Options
  | List List.Options
  | Which Which.Options
