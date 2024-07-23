{- |
Module:      PFile.CLI.New
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Options for `pfile new`.
-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module PFile.CLI.New
  ( parserInfo
  , parser
  , Options (..)
  ) where

import           Options.Applicative
  ( Parser
  , ParserInfo
  , action
  , argument
  , flag
  , fullDesc
  , header
  , help
  , helper
  , info
  , long
  , metavar
  , progDesc
  , short
  , str
  )
import qualified PFile.Profile.LinkHandling as LinkHandling
import           Protolude

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper)
  $  fullDesc
  <> header "pfile new - create a new PROFILE for a set of OBJECTS"
  <> progDesc description
  where
    description
      =  "Create a PROFILE with a set of OBJECTS moved into it. After PROFILE's"
      <> " creation, entries origins (\"OBJECTS...\") are removed. Current"
      <> " profile (set with `pfile switch`) is always reverted afterwards. So"
      <> " if one of the OBJECTS is a link pointing at current profile's entry,"
      <> " the link will be recovered. By default when an entry is a link, the"
      <> " target of the link is copied into the PROFILE. When `--empty` flag"
      <> " is passed and an entry is a link, an empty entry is created in the"
      <> " PROFILE."

parser :: Parser Options
parser = do
  linkHandlingStrategy <- flag LinkHandling.CopyFromOrigin LinkHandling.CreateEmpty
    $  short 'e'
    <> long "empty"
    <> help "Make empty entries in PROFILE instead of copying links targets"
  profileName <- argument str (metavar "PROFILE")
  paths <- many $ argument str (metavar "OBJECTS..." <> action "file")
  pure Options {..}

data Options
  = Options
      { linkHandlingStrategy :: !LinkHandling.Strategy
      , profileName          :: !Text
      , paths                :: ![FilePath]
      }
