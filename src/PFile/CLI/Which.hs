{- |
Module:      PFile.CLI.Which
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Options for `pfile which`.
-}

module PFile.CLI.Which
  ( parserInfo
  , parser
  , Options (..)
  ) where

import           Options.Applicative
  ( Parser
  , ParserInfo
  , fullDesc
  , header
  , helper
  , info
  , progDesc
  )
import           Protolude

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper)
  $  fullDesc
  <> header "pfile which - show current profile name"
  <> progDesc description
  where
    description
      =  "Show current profile name."

parser :: Parser Options
parser = pure Options

data Options
  = Options
