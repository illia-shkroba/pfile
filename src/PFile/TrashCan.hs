{- |
Module:      PFile.TrashCan
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for trashing filesystem's objects.
-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns      #-}

module PFile.TrashCan
  ( create
  , showCreateError
  , CreateError (..)
  , trash
  , showTrashError
  , TrashError (..)
  , restoreAll
  , restore
  , showRestoreEntryError
  , RestoreEntryError (..)
  , showRestoreError
  , RestoreError (..)
  , remove
  , showRemoveError
  , RemoveError (..)
  , dumpTrashed
  , showDumpTrashedError
  , DumpTrashedError (..)
  , trashed
  , TrashCan (TrashCan, root, dataRoot)
  ) where

import           Control.Monad.Writer       (MonadWriter)
import           Data.IORef
  ( IORef
  , modifyIORef'
  , newIORef
  , readIORef
  )
import           Data.List                  (delete)
import           PFile.Error
  ( liftIOWithError
  , modifyError
  , tellError
  )
import qualified PFile.Mount                as Mount
import           PFile.Path                 (doesPathExist, (<//>))
import qualified PFile.Path                 as Path
import qualified PFile.Profile.LinkHandling as LinkHandling
import           Protolude
import           System.Directory           (getTemporaryDirectory)
import           System.IO.Temp             (createTempDirectory)

-- | Create a new trash can.
--
-- @since 0.1.0.0
create :: (MonadError CreateError m, MonadIO m) => m TrashCan
create = do
  temporaryRoot <- getTemporaryDirectory
    `liftIOWithError` TemporaryDirectoryResolveError
  createTempDirectory temporaryRoot "pfile"
    `liftIOWithError` TemporaryDirectoryCreateError (Path.Absolute temporaryRoot)
    >>= \(Path.Absolute -> root) -> do
      _trashed <- newIORef [] & liftIO
      pure TrashCan {root, dataRoot = root <//> "data", _trashed}

showCreateError :: CreateError -> Text
showCreateError = \case
  TemporaryDirectoryResolveError cause
    -> "Unable to resolve temporary directory because of: " <> show cause
  TemporaryDirectoryCreateError path cause
    -> "Unable to create temporary directory in " <> Path.showAbsolute path
    <> " because of: " <> show cause

-- | Error thrown by 'create'.
--
-- @since 0.1.0.0
data CreateError
  = TemporaryDirectoryResolveError !IOException
  -- ^ 'IOException' was encountered during temporary directory resolving.
  | TemporaryDirectoryCreateError !Path.Absolute !IOException
  -- ^ 'IOException' was encountered during temporary directory creation.

-- | 'PFile.Mount.mount' 'PFile.Path.Absolute' inside of a 'TrashCan' with
-- 'PFile.Profile.LinkHandling.CopyLink' strategy for links.
--
-- @since 0.1.0.0
trash ::
     (MonadError TrashError m, MonadIO m) => Path.Absolute -> TrashCan -> m ()
trash originPath trashCan@TrashCan {dataRoot, _trashed} = do
  let Mount.Mount trashPath = Mount.mountPath (Mount.Root dataRoot) originPath
  whenM (doesPathExist trashPath) . throwError
    $ AlreadyTrashedError originPath trashCan
  Mount.mount LinkHandling.CopyLink (Mount.Root dataRoot) originPath
    & modifyError MountError
    & void
  modifyIORef' _trashed (trashPath :) & liftIO

showTrashError :: TrashError -> Text
showTrashError = \case
  AlreadyTrashedError path TrashCan {root}
    -> "Path " <> Path.showAbsolute path
    <> " is already trashed in: " <> Path.showAbsolute root <> "."
  MountError cause -> Mount.showMountError cause

-- | Error thrown by 'trash'.
--
-- @since 0.1.0.0
data TrashError
  = AlreadyTrashedError !Path.Absolute !TrashCan
  -- ^ 'PFile.Path.Absolute' is already trashed.
  | MountError !Mount.MountError
  -- ^ Error was encountered during 'PFile.Mount.mount'.

-- | 'restore' all 'trash'ed filesystem's objects back to their original
-- locations. When an error is encountered during 'restore', 'restoreAll'
-- terminates and provides successfully 'restore'ed entries via
-- a 'MonadWriter'.
--
-- @since 0.1.0.0
restoreAll :: (MonadWriter [RestoreEntryError] m, MonadIO m) => TrashCan -> m ()
restoreAll trashCan = trashed trashCan >>= traverse_ \path ->
  restore trashCan path
    & tellError (RestoreEntryError path)

-- | 'PFile.Mount.unmount' 'PFile.Path.Absolute' (turned into
-- 'PFile.Mount.Mount') from a 'TrashCan' back to its original location (before
-- 'trash').
--
-- @since 0.1.0.0
restore ::
     (MonadError RestoreError m, MonadIO m) => TrashCan -> Path.Absolute -> m ()
restore TrashCan {dataRoot, _trashed} path = do
  Mount.unmount (Mount.Root dataRoot) (Mount.Mount path)
    & modifyError UnmountError
    & void
  modifyIORef' _trashed (delete path) & liftIO

showRestoreEntryError :: RestoreEntryError -> Text
showRestoreEntryError = \case
  RestoreEntryError path cause
    -> "Unable to restore entry " <> Path.showAbsolute path
    <> " because of: " <> showRestoreError cause

-- | Error provided via 'MonadWriter' by 'restoreAll'.
--
-- @since 0.1.0.0
data RestoreEntryError
  = RestoreEntryError !Path.Absolute !RestoreError
  -- ^ Error was encountered during 'restore'.

showRestoreError :: RestoreError -> Text
showRestoreError = \case
  UnmountError cause -> Mount.showUnmountError cause

-- | Error thrown by 'restore'.
--
-- @since 0.1.0.0
newtype RestoreError
  = UnmountError Mount.UnmountError
  -- ^ Error was encountered during 'PFile.Mount.unmount'.

-- | Remove a 'TrashCan' forcibly.
--
-- @since 0.1.0.0
remove :: (MonadError RemoveError m, MonadIO m) => TrashCan -> m ()
remove TrashCan {root} =
  Path.remove root
    & modifyError TemporaryDirectoryRemoveError

showRemoveError :: RemoveError -> Text
showRemoveError = \case
  TemporaryDirectoryRemoveError cause
    -> "Unable to remove temporary directory because of: "
    <> Path.showRemoveError cause

-- | Error thrown by 'remove'.
--
-- @since 0.1.0.0
newtype RemoveError
  = TemporaryDirectoryRemoveError Path.RemoveError
  -- ^ Error was encountered during temporary directory removal.

-- | Dump 'trashed' to a file inside of the 'root' directory.
--
-- @since 0.1.0.0
dumpTrashed ::
     (MonadError DumpTrashedError m, MonadIO m) => TrashCan -> m Path.Absolute
dumpTrashed trashCan@TrashCan {root} = do
  paths <- trashed trashCan
  let dumped = unlines $ toS . Path.unAbsolute <$> paths
  Path.writeFile trashedPath dumped
    & modifyError DumpTrashedError
  pure trashedPath
  where
    trashedPath :: Path.Absolute
    trashedPath = root <//> "trashed.txt"

showDumpTrashedError :: DumpTrashedError -> Text
showDumpTrashedError = \case
  DumpTrashedError cause -> case cause of
    Path.CreateParentInWriteFileError {} -> Path.showWriteFileError cause
    Path.WriteFileError path ioCause
      -> "Unable to dump a list of trashed paths to " <> Path.showAbsolute path
      <> " because of: " <> show ioCause

-- | Error thrown by 'dumpTrashed'.
--
-- @since 0.1.0.0
newtype DumpTrashedError
  = DumpTrashedError Path.WriteFileError
  -- ^ Error was encountered during writing to a "trashed.txt" file inside of
  -- the 'root' directory.

-- | List of 'trash'ed filesystem's objects.
--
-- @since 0.1.0.0
trashed :: MonadIO m => TrashCan -> m [Path.Absolute]
trashed TrashCan {_trashed} = readIORef _trashed & liftIO

-- | Trash can for filesystem's objects.
--
-- @since 0.1.0.0
data TrashCan
  = TrashCan
      { root     :: !Path.Absolute
      -- ^ Root directory of a 'TrashCan'.
      , dataRoot :: !Path.Absolute
      -- ^ Directory of a 'TrashCan' where 'trash'ed filesystem's objects are
      -- located.
      , _trashed :: !(IORef [Path.Absolute])
      -- ^ List of 'trash'ed filesystem's objects in 'IORef'.
      }
