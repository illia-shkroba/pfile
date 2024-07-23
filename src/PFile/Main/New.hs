{- |
Module:      PFile.Main.New
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Main for `pfile new`.
-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module PFile.Main.New
  ( main
  ) where

import qualified Data.List     as List
import           PFile.CLI.New (Options (..))
import           PFile.Env     (Env)
import           PFile.Error   (modifyError)
import qualified PFile.Log     as Log
import qualified PFile.Path    as Path
import qualified PFile.Profile as Profile
import           Protolude

main :: (MonadReader Env m, MonadIO m) => Options -> m ()
main Options {linkHandlingStrategy, profileName, paths} = do
  parsedPaths <- traverse Path.parseAbsolute paths >>= ensureAllPathsParsed paths
  either Log.panic pure <=< runExceptT $ do
    Profile.create createOptions (Profile.Name profileName) parsedPaths
      & modifyError Profile.showCreateError
      & void
    Profile.loadCurrent & runExceptT >>= \case
      Left error -> Log.info $ Profile.showLoadCurrentError error
      Right currentProfile ->
        Profile.link switchOptions currentProfile
          & modifyError Profile.showLinkError
  where
    createOptions :: Profile.CreateOptions
    createOptions = Profile.CreateOptions {Profile.linkHandlingStrategy}

    switchOptions :: Profile.SwitchOptions
    switchOptions = Profile.SwitchOptions {Profile.forceRemoveOccupied = True}

ensureAllPathsParsed ::
     forall m. (MonadReader Env m, MonadIO m)
  => [FilePath]
  -> [Maybe Path.Absolute]
  -> m [Path.Absolute]
ensureAllPathsParsed inputPaths maybeParsedPaths = do
  (unparsed, parsed) <- foldlM go ([], []) $ zip inputPaths maybeParsedPaths
  if List.null unparsed
    then pure parsed
    else Log.panic "Invalid input paths provided."
  where
    go ::
         ([FilePath], [Path.Absolute])
      -> (FilePath, Maybe Path.Absolute)
      -> m ([FilePath], [Path.Absolute])
    go (unparsed, parsed) (inputPath, maybeParsedPath) =
      case maybeParsedPath of
        Nothing -> do
          Log.error $ "Unable to parse path: \"" <> toS inputPath <> "\""
          pure (inputPath : unparsed, parsed)
        Just parsedPath -> do
          Log.info
            $  "Parsed input path \"" <> toS inputPath <> "\""
            <> " as " <> Path.showAbsolute parsedPath
          pure (unparsed, parsedPath : parsed)
