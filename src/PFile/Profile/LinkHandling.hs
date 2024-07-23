{- |
Module:      PFile.Profile.LinkHandling
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for handling links.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Profile.LinkHandling
  ( handle
  , showError
  , Error (..)
  , showStrategy
  , Strategy (..)
  ) where

import           PFile.Error (modifyError)
import           PFile.Path
  ( copyDirectory
  , copyFile
  , copyLink
  , createDirectory
  , createEmptyFile
  , doesDirectoryExist
  )
import qualified PFile.Path  as Path
import           Protolude   hiding (handle)

-- | Handle links relocation with 'Strategy':
--
-- * 'CopyFromOrigin' - copy source link's target to destination.
-- * 'CreateEmpty' - create an empty directory at destination when source
-- link's target is a directory or create an empty file at destination when
-- source link's target is a file.
-- * 'CopyLink' - copy source link to destination.
--
-- @since 0.1.0.0
handle ::
     (MonadError Error m, MonadIO m)
  => Strategy
  -> Path.Absolute
  -> Path.Absolute
  -> m ()
handle strategy src dest =
  case strategy of
    CopyFromOrigin ->
      ifM (doesDirectoryExist src)
        (copyDirectory src dest & modifyError CopyDirectoryFromOriginError)
        (copyFile src dest & modifyError CopyFileFromOriginError)
    CreateEmpty ->
      ifM (doesDirectoryExist src)
        (createDirectory dest & modifyError CreateEmptyDirectoryError)
        (createEmptyFile dest & modifyError CreateEmptyFileError)
    CopyLink -> copyLink src dest & modifyError CopyLinkError

showError :: Error -> Text
showError = \case
  CopyDirectoryFromOriginError cause -> Path.showCopyError cause
  CopyFileFromOriginError cause      -> Path.showCopyFileError cause
  CreateEmptyDirectoryError cause    -> Path.showCreateDirectoryError cause
  CreateEmptyFileError cause         -> Path.showWriteFileError cause
  CopyLinkError cause                -> Path.showCopyLinkError cause

-- | Error thrown by 'handle'.
--
-- @since 0.1.0.0
data Error
  = CopyDirectoryFromOriginError !Path.CopyError
  -- ^ Error was encountered during 'PFile.Path.copyDirectory'.
  | CopyFileFromOriginError !Path.CopyFileError
  -- ^ Error was encountered during 'PFile.Path.copyFile'.
  | CreateEmptyDirectoryError !Path.CreateDirectoryError
  -- ^ Error was encountered during 'PFile.Path.createDirectory'.
  | CreateEmptyFileError !Path.WriteFileError
  -- ^ Error was encountered during 'PFile.Path.createEmptyFile'.
  | CopyLinkError !Path.CopyLinkError
  -- ^ Error was encountered during 'PFile.Path.copyLink'.

showStrategy :: Strategy -> Text
showStrategy = \case
  CopyFromOrigin -> "copy from origin"
  CreateEmpty    -> "create empty"
  CopyLink       -> "copy link"

-- | Link handling strategy for directory/file links to be used by 'handle'.
--
-- @since 0.1.0.0
data Strategy
  = CopyFromOrigin
  -- ^ Copy source link's target to destination.
  | CreateEmpty
  -- ^ Create an empty directory at destination when source link's target is
  -- a directory or create an empty file at destination when source link's
  -- target is a file.
  | CopyLink
  -- ^ Copy source link to destination.
