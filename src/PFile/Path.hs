{- |
Module:      PFile.Path
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Wrapper of 'System.Directory' and 'System.FilePath'.
-}

{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE ViewPatterns               #-}

module PFile.Path
  ( findDirectories
  , findFiles
  , find
  , FindResult (..)
  , (<//>)
  , parseAbsolute
  , canonicalizePath
  , copy
  , copyDirectory
  , copyDirectoryLink
  , copyFile
  , copyFileLink
  , copyLink
  , showCopyError
  , CopyError (..)
  , showCopyLinkError
  , CopyLinkError (..)
  , showCopyFileError
  , CopyFileError (..)
  , createDirectory
  , showCreateDirectoryError
  , CreateDirectoryError (..)
  , createDirectoryLink
  , showCreateDirectoryLinkError
  , CreateDirectoryLinkError (..)
  , createEmptyFile
  , createFileLink
  , showCreateFileLinkError
  , CreateFileLinkError (..)
  , createLink
  , showCreateLinkError
  , CreateLinkError (..)
  , createParent
  , showCreateParentError
  , CreateParentError (..)
  , doesDirectoryExist
  , doesFileExist
  , doesPathExist
  , dropDrive
  , dropFileName
  , dropTrailingPathSeparator
  , getSymbolicLinkTarget
  , listDirectory
  , makeRelative
  , move
  , moveDirectory
  , moveDirectoryLink
  , moveFile
  , moveFileLink
  , showMoveError
  , MoveError (..)
  , showMoveDirectoryError
  , MoveDirectoryError (..)
  , showMoveDirectoryLinkError
  , MoveDirectoryLinkError (..)
  , showMoveFileError
  , MoveFileError (..)
  , showMoveFileLinkError
  , MoveFileLinkError (..)
  , pathIsSymbolicLink
  , remove
  , showRemoveError
  , RemoveError (..)
  , renameDirectory
  , renameFile
  , takeBaseName
  , typeOf
  , showType
  , Type (..)
  , PFile.Path.writeFile
  , showWriteFileError
  , WriteFileError (..)
  , showAbsolute
  , Absolute (..)
  ) where

import           Data.Aeson       (FromJSON, ToJSON)
import           Data.HashSet     (HashSet)
import qualified Data.HashSet     as HashSet
import           GHC.IO.Exception (IOErrorType (..))
import           PFile.Error      (liftIOWithError, modifyError, onIOError)
import           Protolude        hiding (Type, find, typeOf)
import qualified System.Directory as Directory
import           System.Directory (makeAbsolute)
import qualified System.FilePath  as FilePath
import           System.FilePath  ((</>))
import           System.IO.Error  (ioeGetErrorType, tryIOError)

findDirectories :: MonadIO m => Absolute -> m [Absolute]
findDirectories root = HashSet.toList . dirs <$> find root

findFiles :: MonadIO m => Absolute -> m [Absolute]
findFiles root = HashSet.toList . files <$> find root

find ::
     forall m. MonadIO m
  => Absolute
  -> m FindResult
find = go FindResult {files = HashSet.empty, dirs = HashSet.empty}
  where
    go :: FindResult -> Absolute -> m FindResult
    go acc@FindResult {files, dirs} root = do
      path <- canonicalizePath root
      ifM (doesFileExist path)
        (pure acc {files = HashSet.insert path files})
        if path `HashSet.member` dirs
          then pure acc
          else tryIOError (listDirectory path) & liftIO >>= either
            (const $ pure acc)
            (foldlM go acc {dirs = HashSet.insert path dirs})

data FindResult
  = FindResult
      { files :: !(HashSet Absolute)
      , dirs  :: !(HashSet Absolute)
      }

infixr 5 <//>

(<//>) :: Absolute -> FilePath -> Absolute
(<//>) (Absolute x) y = Absolute $ x </> y

parseAbsolute :: MonadIO m => FilePath -> m (Maybe Absolute)
parseAbsolute inputPath =
  makeAbsolute inputPath & tryIOError & liftIO
    >>= either (const $ pure Nothing) \(Absolute -> path) ->
      doesPathExist path <&> bool Nothing (Just $ dropTrailingPathSeparator path)

canonicalizePath :: MonadIO m => Absolute -> m Absolute
canonicalizePath (Absolute path) =
  Directory.canonicalizePath path <&> Absolute & liftIO

copy :: (MonadError CopyError m, MonadIO m) => Absolute -> Absolute -> m ()
copy src dest =
  typeOf src >>= maybe (throwError $ SourceTypeResolveCopyError src) \case
    Directory     -> copyDirectory src dest
    DirectoryLink -> copyDirectoryLink src dest & modifyError CopyLinkError
    File          -> copyFile src dest & modifyError CopyFileError
    FileLink      -> copyFileLink src dest & modifyError CopyLinkError

copyDirectory ::
     (MonadError CopyError m, MonadIO m) => Absolute -> Absolute -> m ()
copyDirectory src dest = do
  paths <- Directory.listDirectory (unAbsolute src)
    `liftIOWithError` ListDirectoryError src
  if null paths
    then createDirectory dest & modifyError CreateDirectoryInCopyError
    else zipWithM_ copy ((src <//>) <$> paths) ((dest <//>) <$> paths)

copyDirectoryLink ::
     (MonadError CopyLinkError m, MonadIO m) => Absolute -> Absolute -> m ()
copyDirectoryLink src dest = do
  createParent dest
    & modifyError CreateParentInCopyLinkError
  target <- getSymbolicLinkTarget src
    `liftIOWithError` LinkTargetResolveError src
  createDirectoryLink target dest
    & modifyError CreateDirectoryLinkInCopyLinkError

copyFile ::
     (MonadError CopyFileError m, MonadIO m) => Absolute -> Absolute -> m ()
copyFile src dest = do
  createParent dest
    & modifyError CreateParentInCopyFileError
  Directory.copyFileWithMetadata (unAbsolute src) (unAbsolute dest)
    `liftIOWithError` CopyFileWithMetadataError src dest

copyFileLink ::
     (MonadError CopyLinkError m, MonadIO m) => Absolute -> Absolute -> m ()
copyFileLink src dest = do
  createParent dest
    & modifyError CreateParentInCopyLinkError
  target <- getSymbolicLinkTarget src
    `liftIOWithError` LinkTargetResolveError src
  createFileLink target dest
    & modifyError CreateFileLinkInCopyLinkError

copyLink ::
     (MonadError CopyLinkError m, MonadIO m) => Absolute -> Absolute -> m ()
copyLink src dest =
  ifM (doesDirectoryExist src)
    (copyDirectoryLink src dest)
    (copyFileLink src dest)

showCopyError :: CopyError -> Text
showCopyError = \case
  SourceTypeResolveCopyError path
    -> "Unable to resolve type of path: " <> showAbsolute path <> "."
  ListDirectoryError path cause
    -> "Unable to list directory " <> showAbsolute path
    <> " because of: " <> show cause
  CreateDirectoryInCopyError cause -> showCreateDirectoryError cause
  CopyLinkError cause -> showCopyLinkError cause
  CopyFileError cause -> showCopyFileError cause

data CopyError
  = SourceTypeResolveCopyError !Absolute
  | ListDirectoryError !Absolute !IOException
  | CreateDirectoryInCopyError !CreateDirectoryError
  | CopyLinkError !CopyLinkError
  | CopyFileError !CopyFileError

showCopyLinkError :: CopyLinkError -> Text
showCopyLinkError = \case
  CreateParentInCopyLinkError cause -> showCreateParentError cause
  LinkTargetResolveError path cause
    -> "Unable to resolve link of path " <> showAbsolute path
    <> " because of: " <> show cause
  CreateDirectoryLinkInCopyLinkError cause -> showCreateDirectoryLinkError cause
  CreateFileLinkInCopyLinkError cause -> showCreateFileLinkError cause

data CopyLinkError
  = CreateParentInCopyLinkError !CreateParentError
  | LinkTargetResolveError !Absolute !IOException
  | CreateDirectoryLinkInCopyLinkError !CreateDirectoryLinkError
  | CreateFileLinkInCopyLinkError !CreateFileLinkError

showCopyFileError :: CopyFileError -> Text
showCopyFileError = \case
  CreateParentInCopyFileError cause -> showCreateParentError cause
  CopyFileWithMetadataError src dest cause
    -> "Unable to copy file " <> showAbsolute src
    <> " into " <> showAbsolute dest
    <> " because of: " <> show cause

data CopyFileError
  = CreateParentInCopyFileError !CreateParentError
  | CopyFileWithMetadataError !Absolute !Absolute !IOException

createDirectory ::
     (MonadError CreateDirectoryError m, MonadIO m) => Absolute -> m ()
createDirectory path =
  Directory.createDirectoryIfMissing True (unAbsolute path)
    `liftIOWithError` CreateDirectoryError path

showCreateDirectoryError :: CreateDirectoryError -> Text
showCreateDirectoryError = \case
  CreateDirectoryError path cause
    -> "Unable to create directory " <> showAbsolute path
    <> " because of: " <> show cause

data CreateDirectoryError
  = CreateDirectoryError !Absolute !IOException

createDirectoryLink ::
     (MonadError CreateDirectoryLinkError m, MonadIO m)
  => Absolute
  -> Absolute
  -> m ()
createDirectoryLink
    (dropTrailingPathSeparator -> dest)
    (dropTrailingPathSeparator -> src) = do
  createParent src
    & modifyError CreateParentInCreateDirectoryLinkError
  Directory.createDirectoryLink (unAbsolute dest) (unAbsolute src)
    `liftIOWithError` CreateDirectoryLinkError dest src

showCreateDirectoryLinkError :: CreateDirectoryLinkError -> Text
showCreateDirectoryLinkError = \case
  CreateParentInCreateDirectoryLinkError cause -> showCreateParentError cause
  CreateDirectoryLinkError dest src cause
    -> "Unable to link " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data CreateDirectoryLinkError
  = CreateParentInCreateDirectoryLinkError !CreateParentError
  | CreateDirectoryLinkError !Absolute !Absolute !IOException

createEmptyFile :: (MonadError WriteFileError m, MonadIO m) => Absolute -> m ()
createEmptyFile path = PFile.Path.writeFile path ""

createFileLink ::
     (MonadError CreateFileLinkError m, MonadIO m)
  => Absolute
  -> Absolute
  -> m ()
createFileLink
    (dropTrailingPathSeparator -> dest)
    (dropTrailingPathSeparator -> src) = do
  createParent src
    & modifyError CreateParentInCreateFileLinkError
  Directory.createFileLink (unAbsolute dest) (unAbsolute src)
    `liftIOWithError` CreateFileLinkError dest src

showCreateFileLinkError :: CreateFileLinkError -> Text
showCreateFileLinkError = \case
  CreateParentInCreateFileLinkError cause -> showCreateParentError cause
  CreateFileLinkError dest src cause
    -> "Unable to link " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data CreateFileLinkError
  = CreateParentInCreateFileLinkError !CreateParentError
  | CreateFileLinkError !Absolute !Absolute !IOException

createLink ::
     (MonadError CreateLinkError m, MonadIO m) => Absolute -> Absolute -> m ()
createLink dest src =
  ifM (doesDirectoryExist dest)
    (createDirectoryLink dest src & modifyError DirectoryLinkError)
    (createFileLink dest src & modifyError FileLinkError)

showCreateLinkError :: CreateLinkError -> Text
showCreateLinkError = \case
  DirectoryLinkError cause -> showCreateDirectoryLinkError cause
  FileLinkError cause      -> showCreateFileLinkError cause

data CreateLinkError
  = DirectoryLinkError !CreateDirectoryLinkError
  | FileLinkError !CreateFileLinkError

createParent :: (MonadError CreateParentError m, MonadIO m) => Absolute -> m ()
createParent (dropTrailingPathSeparator -> path) =
  createDirectory (dropFileName path)
    & modifyError (CreateParentError path)

showCreateParentError :: CreateParentError -> Text
showCreateParentError = \case
  CreateParentError path (CreateDirectoryError _ cause)
    -> "Unable to create parent directory for " <> showAbsolute path
    <> " because of: " <> show cause

data CreateParentError
  = CreateParentError !Absolute !CreateDirectoryError

doesDirectoryExist :: MonadIO m => Absolute -> m Bool
doesDirectoryExist (Absolute path) =
  Directory.doesDirectoryExist path `onIOError` pure False & liftIO

doesFileExist :: MonadIO m => Absolute -> m Bool
doesFileExist (Absolute path) =
  Directory.doesFileExist path `onIOError` pure False & liftIO

doesPathExist :: MonadIO m => Absolute -> m Bool
doesPathExist (Absolute path) =
  Directory.doesPathExist path `onIOError` pure False & liftIO

dropDrive :: Absolute -> FilePath
dropDrive (Absolute path) = FilePath.dropDrive path

dropFileName :: Absolute -> Absolute
dropFileName (Absolute path) = Absolute $ FilePath.dropFileName path

dropTrailingPathSeparator :: Absolute -> Absolute
dropTrailingPathSeparator (Absolute path) =
  Absolute $ FilePath.dropTrailingPathSeparator path

getSymbolicLinkTarget :: MonadIO m => Absolute -> m Absolute
getSymbolicLinkTarget (dropTrailingPathSeparator -> Absolute path) =
  Directory.getSymbolicLinkTarget path <&> Absolute & liftIO

listDirectory :: MonadIO m => Absolute -> m [Absolute]
listDirectory path
  =   Directory.listDirectory (unAbsolute path)
  <&> fmap (path <//>) & liftIO

makeRelative :: Absolute -> Absolute -> FilePath
makeRelative (Absolute root) (Absolute path) = FilePath.makeRelative root path

move :: (MonadError MoveError m, MonadIO m) => Absolute -> Absolute -> m ()
move src dest =
  typeOf src >>= maybe (throwError $ SourceTypeResolveMoveError src) \case
    Directory     -> moveDirectory src dest & modifyError MoveDirectoryError
    DirectoryLink -> moveDirectoryLink src dest & modifyError MoveDirectoryLinkError
    File          -> moveFile src dest & modifyError MoveFileError
    FileLink      -> moveFileLink src dest & modifyError MoveFileLinkError

moveDirectory ::
     (MonadError MoveDirectoryError m, MonadIO m)
  => Absolute
  -> Absolute
  -> m ()
moveDirectory src dest = do
  createParent dest
    & modifyError CreateParentInMoveDirectoryError
  tryIOError (renameDirectory src dest) & liftIO
    >>= flip either (const $ pure ()) \cause ->
      if isCrossDeviceLinkError cause
        then do
          copyDirectory src dest
            & modifyError FallbackCopyDirectoryError
          remove src
            & modifyError SourceDirectoryRemoveError
        else throwError $ RenameDirectoryError src dest cause

moveDirectoryLink ::
     (MonadError MoveDirectoryLinkError m, MonadIO m)
  => Absolute
  -> Absolute
  -> m ()
moveDirectoryLink src dest = do
  createParent dest
    & modifyError CreateParentInMoveDirectoryLinkError
  tryIOError (renameDirectory src dest) & liftIO
    >>= flip either (const $ pure ()) \cause ->
      if isCrossDeviceLinkError cause
        then do
          copyDirectoryLink src dest
            & modifyError FallbackCopyDirectoryLinkError
          remove src
            & modifyError SourceDirectoryLinkRemoveError
        else throwError $ RenameDirectoryLinkError src dest cause

moveFile ::
     (MonadError MoveFileError m, MonadIO m) => Absolute -> Absolute -> m ()
moveFile src dest = do
  createParent dest
    & modifyError CreateParentInMoveFileError
  tryIOError (renameFile src dest) & liftIO
    >>= flip either (const $ pure ()) \cause ->
      if isCrossDeviceLinkError cause
        then do
          copyFile src dest
            & modifyError FallbackCopyFileError
          remove src
            & modifyError SourceFileRemoveError
        else throwError $ RenameFileError src dest cause

moveFileLink ::
     (MonadError MoveFileLinkError m, MonadIO m) => Absolute -> Absolute -> m ()
moveFileLink src dest = do
  createParent dest
    & modifyError CreateParentInMoveFileLinkError
  tryIOError (renameFile src dest) & liftIO
    >>= flip either (const $ pure ()) \cause ->
      if isCrossDeviceLinkError cause
        then do
          copyFileLink src dest
            & modifyError FallbackCopyFileLinkError
          remove src
            & modifyError SourceFileLinkRemoveError
        else throwError $ RenameFileLinkError src dest cause

showMoveError :: MoveError -> Text
showMoveError = \case
  SourceTypeResolveMoveError path
    -> "Unable to resolve type of path: " <> showAbsolute path <> "."
  MoveDirectoryError cause -> showMoveDirectoryError cause
  MoveDirectoryLinkError cause -> showMoveDirectoryLinkError cause
  MoveFileError cause -> showMoveFileError cause
  MoveFileLinkError cause -> showMoveFileLinkError cause

data MoveError
  = SourceTypeResolveMoveError !Absolute
  | MoveDirectoryError !MoveDirectoryError
  | MoveDirectoryLinkError !MoveDirectoryLinkError
  | MoveFileError !MoveFileError
  | MoveFileLinkError !MoveFileLinkError

showMoveDirectoryError :: MoveDirectoryError -> Text
showMoveDirectoryError = \case
  CreateParentInMoveDirectoryError cause -> showCreateParentError cause
  FallbackCopyDirectoryError cause -> showCopyError cause
  SourceDirectoryRemoveError cause
    -> "Unable to remove source because of: " <> showRemoveError cause
  RenameDirectoryError src dest cause
    -> "Unable to rename directory " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data MoveDirectoryError
  = CreateParentInMoveDirectoryError !CreateParentError
  | FallbackCopyDirectoryError !CopyError
  | SourceDirectoryRemoveError !RemoveError
  | RenameDirectoryError !Absolute !Absolute !IOException

showMoveDirectoryLinkError :: MoveDirectoryLinkError -> Text
showMoveDirectoryLinkError = \case
  CreateParentInMoveDirectoryLinkError cause -> showCreateParentError cause
  FallbackCopyDirectoryLinkError cause -> showCopyLinkError cause
  SourceDirectoryLinkRemoveError cause
    -> "Unable to remove source because of: " <> showRemoveError cause
  RenameDirectoryLinkError src dest cause
    -> "Unable to rename directory link " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data MoveDirectoryLinkError
  = CreateParentInMoveDirectoryLinkError !CreateParentError
  | FallbackCopyDirectoryLinkError !CopyLinkError
  | SourceDirectoryLinkRemoveError !RemoveError
  | RenameDirectoryLinkError !Absolute !Absolute !IOException

showMoveFileError :: MoveFileError -> Text
showMoveFileError = \case
  CreateParentInMoveFileError cause -> showCreateParentError cause
  FallbackCopyFileError cause -> showCopyFileError cause
  SourceFileRemoveError cause
    -> "Unable to remove source because of: " <> showRemoveError cause
  RenameFileError src dest cause
    -> "Unable to rename file " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data MoveFileError
  = CreateParentInMoveFileError !CreateParentError
  | FallbackCopyFileError !CopyFileError
  | SourceFileRemoveError !RemoveError
  | RenameFileError !Absolute !Absolute !IOException

showMoveFileLinkError :: MoveFileLinkError -> Text
showMoveFileLinkError = \case
  CreateParentInMoveFileLinkError cause -> showCreateParentError cause
  FallbackCopyFileLinkError cause -> showCopyLinkError cause
  SourceFileLinkRemoveError cause
    -> "Unable to remove source because of: " <> showRemoveError cause
  RenameFileLinkError src dest cause
    -> "Unable to rename file link " <> showAbsolute src
    <> " to " <> showAbsolute dest
    <> " because of: " <> show cause

data MoveFileLinkError
  = CreateParentInMoveFileLinkError !CreateParentError
  | FallbackCopyFileLinkError !CopyLinkError
  | SourceFileLinkRemoveError !RemoveError
  | RenameFileLinkError !Absolute !Absolute !IOException

isCrossDeviceLinkError :: IOException -> Bool
isCrossDeviceLinkError e
  =  ioeGetErrorType e == UnsupportedOperation
  && (toLower <$> "Invalid cross-device link") `isInfixOf` (toLower <$> show e)

pathIsSymbolicLink :: MonadIO m => Absolute -> m Bool
pathIsSymbolicLink (dropTrailingPathSeparator -> Absolute path) =
  Directory.pathIsSymbolicLink path & liftIO

-- | 'remove' should be used instead of the 'Directory.removePathForcibly' to properly
-- remove a path without messing up permissions of a target in case of links
remove :: (MonadError RemoveError m, MonadIO m) => Absolute -> m ()
remove (dropTrailingPathSeparator -> path) =
  typeOf path >>= maybe (pure ()) \case
    Directory ->
      Directory.removeDirectoryRecursive (unAbsolute path)
        `liftIOWithError` RemoveDirectoryError path
    DirectoryLink ->
      Directory.removeDirectoryLink (unAbsolute path)
        `liftIOWithError` RemoveDirectoryLinkError path
    File ->
      Directory.removeFile (unAbsolute path)
        `liftIOWithError` RemoveFileError path
    FileLink ->
      Directory.removeFile (unAbsolute path)
        `liftIOWithError` RemoveFileLinkError path

showRemoveError :: RemoveError -> Text
showRemoveError = \case
  RemoveDirectoryError path cause
    -> "Unable to remove directory " <> showAbsolute path
    <> " because of: " <> show cause
  RemoveDirectoryLinkError path cause
    -> "Unable to remove directory link " <> showAbsolute path
    <> " because of: " <> show cause
  RemoveFileError path cause
    -> "Unable to remove file " <> showAbsolute path
    <> " because of: " <> show cause
  RemoveFileLinkError path cause
    -> "Unable to remove file link " <> showAbsolute path
    <> " because of: " <> show cause

data RemoveError
  = RemoveDirectoryError !Absolute !IOException
  | RemoveDirectoryLinkError !Absolute !IOException
  | RemoveFileError !Absolute !IOException
  | RemoveFileLinkError !Absolute !IOException

renameDirectory :: MonadIO m => Absolute -> Absolute -> m ()
renameDirectory (Absolute src) (Absolute dest) =
  Directory.renameDirectory src dest & liftIO

renameFile :: MonadIO m => Absolute -> Absolute -> m ()
renameFile (Absolute src) (Absolute dest) =
  Directory.renameFile src dest & liftIO

takeBaseName :: Absolute -> FilePath
takeBaseName (Absolute path) = FilePath.takeBaseName path

typeOf :: MonadIO m => Absolute -> m (Maybe Type)
typeOf path =
  ifM (doesFileExist path)
    do
      tryIOError (pathIsSymbolicLink path) & liftIO
        <&> either (const Nothing) (Just . bool File FileLink)
    do
      ifM (doesDirectoryExist path)
        do
          tryIOError (pathIsSymbolicLink path) & liftIO
            <&> either (const Nothing) (Just . bool Directory DirectoryLink)
        do
          pure Nothing

showType :: Type -> Text
showType = \case
  Directory     -> "dir"
  DirectoryLink -> "dir link"
  File          -> "file"
  FileLink      -> "file link"

data Type
  = Directory
  | DirectoryLink
  | File
  | FileLink

writeFile ::
     (MonadError WriteFileError m, MonadIO m) => Absolute -> Text -> m ()
writeFile path contents = do
  createParent path
    & modifyError CreateParentInWriteFileError
  Protolude.writeFile (unAbsolute path) contents
    `liftIOWithError` WriteFileError path

showWriteFileError :: WriteFileError -> Text
showWriteFileError = \case
  CreateParentInWriteFileError cause -> showCreateParentError cause
  WriteFileError path cause
    -> "Unable to write to a file " <> showAbsolute path
    <> " because of: " <> show cause

data WriteFileError
  = CreateParentInWriteFileError !CreateParentError
  | WriteFileError !Absolute !IOException

showAbsolute :: Absolute -> Text
showAbsolute (Absolute path) = "\"" <> toS path <> "\""

-- | Absolute 'FilePath' to a filesystem's object.
--
-- @since 0.1.0.0
newtype Absolute
  = Absolute { unAbsolute :: FilePath }
  deriving (Eq)
  deriving newtype (FromJSON, Hashable, ToJSON)
