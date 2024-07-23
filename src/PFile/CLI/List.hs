{- |
Module:      PFile.CLI.List
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Options for `pfile ls`.
-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module PFile.CLI.List
  ( parserInfo
  , parser
  , Options (..)
  ) where

import           Options.Applicative
  ( Parser
  , ParserInfo
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , progDesc
  , short
  , switch
  )
import           Protolude

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper)
  $  fullDesc
  <> header "pfile ls - list available profiles"
  <> progDesc description
  where
    description
      =  "List available profiles. Use `--all` option to list dangling profiles"
      <> " -- profiles that were \"partially\" initialized (an error was"
      <> " encountered during `pfile new` which couldn't be rollbacked)."

parser :: Parser Options
parser = do
  shouldFilterDangling <- fmap not . switch
    $  short 'a'
    <> long "all"
    <> help "Include dangling profiles"
  pure Options {..}

newtype Options
  = Options { shouldFilterDangling :: Bool }
