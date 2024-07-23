{- |
Module:      PFile.Profile.Internal.Serialization
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for profiles serialization.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Profile.Internal.Serialization
  ( load
  , showLoadError
  , LoadError (..)
  , dump
  , showDumpError
  , DumpError (..)
  ) where

import           Data.Aeson
  ( eitherDecodeFileStrict
  , encodeFile
  )
import           PFile.Aeson                    (encodePretty)
import           PFile.Env                      (Env)
import           PFile.Error                    (liftIOWithError, modifyError)
import qualified PFile.Log                      as Log
import qualified PFile.Path                     as Path
import           PFile.Profile.Internal.Profile
  ( Name (..)
  , Profile (..)
  , profileState
  )
import           Protolude

-- | Load 'Profile' named 'Name' from its
-- 'PFile.Profile.Internal.Profile.profileState'.
--
-- @since 0.1.0.0
load ::
     (MonadReader Env m, MonadError LoadError m, MonadIO m) => Name -> m Profile
load name = do
  statePath <- profileState name
  Log.info
    $  "Load profile \"" <> unName name <> "\" from state: "
    <> Path.showAbsolute statePath
  profile <- eitherDecodeFileStrict (Path.unAbsolute statePath)
    `liftIOWithError` LoadError statePath
    >>= either (throwError . DecodeError statePath) pure
  Log.info
    $  "Loaded profile \"" <> unName name <> "\":\n"
    <> encodePretty profile
  pure profile

showLoadError :: LoadError -> Text
showLoadError = \case
  LoadError path cause
    -> "Unable to load profile from " <> Path.showAbsolute path
    <> " because of: " <> show cause
  DecodeError path cause
    -> "Unable to decode profile from " <> Path.showAbsolute path
    <> " because of: \"" <> toS cause <> "\"."

-- | Error thrown by 'load'.
--
-- @since 0.1.0.0
data LoadError
  = LoadError !Path.Absolute !IOException
  -- ^ 'IOException' was encountered during 'eitherDecodeFileStrict'.
  | DecodeError !Path.Absolute ![Char]
  -- ^ Decoding error was encountered during 'eitherDecodeFileStrict'.

-- | Dump 'Profile' to its 'PFile.Profile.Internal.Profile.profileState'.
--
-- @since 0.1.0.0
dump ::
     (MonadReader Env m, MonadError DumpError m, MonadIO m) => Profile -> m ()
dump profile@Profile {name} = do
  Log.info
    $  "Dump profile \"" <> unName name <> "\":\n"
    <> encodePretty profile
  statePath <- profileState name
  Path.createParent statePath
    & modifyError CreateParentInDumpError
  encodeFile (Path.unAbsolute statePath) profile
    `liftIOWithError` DumpError statePath
  Log.info
    $  "Dumped profile \"" <> unName name <> "\" to state: "
    <> Path.showAbsolute statePath

showDumpError :: DumpError -> Text
showDumpError = \case
  CreateParentInDumpError cause -> Path.showCreateParentError cause
  DumpError path cause
    -> "Unable to dump profile to " <> Path.showAbsolute path
    <> " because of: " <> show cause

-- | Error thrown by 'dump'.
--
-- @since 0.1.0.0
data DumpError
  = CreateParentInDumpError !Path.CreateParentError
  -- ^ Unable to create a parent directory for
  -- 'PFile.Profile.Internal.Profile.profileState'.
  | DumpError !Path.Absolute !IOException
  -- ^ 'IOException' was encountered during 'encodeFile'.
