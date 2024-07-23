{- |
Module:      PFile.CLI.Switch
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Options for `pfile switch`.
-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module PFile.CLI.Switch
  ( parserInfo
  , parser
  , Options (..)
  ) where

import           Options.Applicative
  ( Parser
  , ParserInfo
  , argument
  , completer
  , fullDesc
  , header
  , help
  , helper
  , info
  , listIOCompleter
  , long
  , metavar
  , progDesc
  , short
  , str
  , switch
  )
import qualified PFile.Completion    as Completion
import           Protolude

parserInfo :: ParserInfo Options
parserInfo = info (parser <**> helper)
  $  fullDesc
  <> header "pfile switch - switch to PROFILE"
  <> progDesc description
  where
    description
      =  "Switch to PROFILE. The current profile is unlinked first (links"
      <> " pointing at current profile's entries are removed) and then the"
      <> " PROFILE is linked (links pointing at PROFILE profile's entries are"
      <> " made). If the current profile was not set yet, the first step is"
      <> " skipped. `pfile switch` could be reverted with `pfile unpack`."

parser :: Parser Options
parser = do
  forceRemoveOccupied <- switch
    $  short 'f'
    <> long "force-remove-occupied"
    <> help "Force remove of paths occupying current/PROFILE profile's link paths"
  nextProfileName <- argument str
    $  metavar "PROFILE"
    <> completer (listIOCompleter Completion.profileNames)
  pure Options {..}

data Options
  = Options
      { forceRemoveOccupied :: !Bool
      , nextProfileName     :: !Text
      }
