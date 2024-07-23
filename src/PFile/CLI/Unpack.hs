{- |
Module:      PFile.CLI.Unpack
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Options for `pfile unpack`.
-}

{-# LANGUAGE ApplicativeDo   #-}
{-# LANGUAGE RecordWildCards #-}

module PFile.CLI.Unpack
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
  <> header "pfile unpack - revert `pfile new` and `pfile switch`"
  <> progDesc description
  where
    description
      =  "Substitute links pointing at entries in the current profile with"
      <> " entries themselves. This substitution \"reverts\" `pfile switch`"
      <> " (removes links & unsets current profile) and `pfile new` (unpacks"
      <> " entries from profile). `pfile unpack` will not change the profile --"
      <> " entries that were added to the profile will remain unchanged. `pfile"
      <> " unpack` could be reverted with `pfile switch -f PROFILE`."

parser :: Parser Options
parser = do
  forceRemoveOccupied <- switch
    $  short 'f'
    <> long "force-remove-occupied"
    <> help "Force remove of paths occupying current profile's link paths"
  pure Options {..}

newtype Options
  = Options { forceRemoveOccupied :: Bool }
