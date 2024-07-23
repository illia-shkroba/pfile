{- |
Module:      PFile.Profile.Internal.Switch
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for profiles switching.
-}

{-# LANGUAGE BlockArguments      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module PFile.Profile.Internal.Switch
  ( switch
  , unlink
  , validateLinkedEntry
  , link
  , unpack
  , validateUnlinkedEntry
  , purge
  , showSwitchError
  , SwitchError (..)
  , showUnlinkError
  , UnlinkError (..)
  , showLinkedEntryValidateError
  , LinkedEntryValidateError (..)
  , showLinkError
  , LinkError (..)
  , showUnpackError
  , UnpackError (..)
  , showUnlinkedEntryValidateError
  , UnlinkedEntryValidateError (..)
  , showPurgeError
  , PurgeError (..)
  , SwitchOptions (..)
  ) where

import           Control.Monad.Writer            (execWriterT)
import           PFile.Error
  ( fallback
  , liftIOWithError
  , modifyError
  , tellError
  )
import qualified PFile.Mount                     as Mount
import           PFile.Path
  ( canonicalizePath
  , doesPathExist
  )
import qualified PFile.Path                      as Path
import           PFile.Profile.Internal.Profile
  ( Entry (..)
  , Name (..)
  , Profile (..)
  )
import           PFile.Profile.Internal.Registry (linkAll, unpackAll)
import qualified PFile.Profile.Internal.Registry as Registry
import           PFile.TrashCan                  (TrashCan (..))
import qualified PFile.TrashCan                  as TrashCan
import           Protolude                       hiding (link)

-- | Switch from the current profile to the next profile. 'switch' 'unlink's
-- the current profile and then 'link's the next profile.
--
-- @since 0.1.0.0
switch ::
     (MonadError SwitchError m, MonadIO m)
  => SwitchOptions
  -- ^ Options that control 'switch' behaviour (currently only
  -- 'forceRemoveOccupied').
  -> Profile
  -- ^ Current profile.
  -> Profile
  -- ^ Next profile.
  -> m ()
switch options current next = do
  unlink options current
    & modifyError UnlinkCurrentError
  link options next
    & modifyError LinkNextError

-- | Remove links pointing at entries inside of the 'Profile'. 'unlink' only
-- remove links that are know to PFile.
--
-- @since 0.1.0.0
unlink ::
     (MonadError UnlinkError m, MonadIO m)
  => SwitchOptions
  -> Profile
  -> m ()
unlink SwitchOptions {forceRemoveOccupied} profile@Profile {entries} = do
  unless forceRemoveOccupied
    $ forM_ entries
      (modifyError ValidateUnlinkError . validateLinkedEntry)
  purge profile
    & modifyError PurgeUnlinkError

-- | Validate that 'Entry's 'originPath' is a link pointing at 'mountPath'.
--
-- @since 0.1.0.0
validateLinkedEntry ::
     (MonadError LinkedEntryValidateError m, MonadIO m) => Entry -> m ()
validateLinkedEntry entry@Entry {mountPath = Mount.Mount mountPath, originPath} = do
  unlessM (doesPathExist originPath) . throwError
    $ OriginDoesNotExistError originPath
  canonicalizedPath <- canonicalizePath originPath
    `liftIOWithError` OriginCanonicalizeError originPath
  unless (canonicalizedPath == mountPath) . throwError
    $ OriginChangedError entry canonicalizedPath

-- | Create links pointing at 'Entry'ies inside of the 'Profile' with
-- 'PFile.Profile.Internal.Registry.linkAll'.
--
-- @since 0.1.0.0
link ::
     forall m. (MonadError LinkError m, MonadIO m)
  => SwitchOptions
  -> Profile
  -> m ()
link SwitchOptions {forceRemoveOccupied} profile@Profile {entries} = do
  if forceRemoveOccupied
    then
      purge profile
        & modifyError PurgeLinkError
    else
      forM_ entries
        (modifyError ValidateLinkError . validateUnlinkedEntry)
  linkAll entries
    & void . fallback rollbackLinks
  where
    rollbackLinks :: Registry.LinkError -> [Path.Absolute] -> m ()
    rollbackLinks cause originPaths = originPaths
      & traverse_ (\p -> Path.remove p & tellError (p, ))
      & execWriterT >>= \case
        errors@(_:_) -> throwError $ LinkRollbackError cause errors profile
        []           -> throwError $ LinkError cause

-- | Unpack 'Profile's entries back to their original locations with
-- 'PFile.Profile.Internal.Registry.unpackAll'.
--
-- @since 0.1.0.0
unpack ::
     forall m. (MonadError UnpackError m, MonadIO m)
  => SwitchOptions
  -> Profile
  -> m ()
unpack options@SwitchOptions {forceRemoveOccupied} profile@Profile {entries} = do
  if forceRemoveOccupied
    then
      purge profile
        & modifyError PurgeUnpackError
    else
      unlink options profile
        & modifyError UnlinkUnpackError
  unpackAll entries
    & void . fallback rollbackUnpacked
  where
    rollbackUnpacked :: Registry.UnpackError -> [Path.Absolute] -> m ()
    rollbackUnpacked cause originPaths = originPaths
      & traverse_ (\p -> Path.remove p & tellError (p, ))
      & execWriterT >>= \case
        errors@(_:_) -> throwError $ UnpackRollbackError cause errors profile
        []           -> throwError $ UnpackError cause

-- | Validate that 'Entry's 'originPath' does not exist.
--
-- @since 0.1.0.0
validateUnlinkedEntry ::
     (MonadError UnlinkedEntryValidateError m, MonadIO m) => Entry -> m ()
validateUnlinkedEntry entry@Entry {originPath} =
  whenM (doesPathExist originPath) . throwError
    $ OriginOccupiedError entry

-- | Forcibly remove 'originPath's of a 'Profile's 'Entry'ies.
--
-- @since 0.1.0.0
purge ::
     forall m. (MonadError PurgeError m, MonadIO m)
  => Profile
  -> m ()
purge profile@Profile {entries} = do
  trashCan <- TrashCan.create
    & modifyError TrashCanCreateError
  trashOrigins trashCan entries
    `catchError` rollbackTrashOrigins trashCan
  TrashCan.remove trashCan
    & modifyError TrashCanRemoveError
  where
    trashOrigins :: TrashCan -> [Entry] -> m ()
    trashOrigins trashCan = traverse_ \Entry {originPath} ->
      TrashCan.trash originPath trashCan
        & hushMissingError
        & modifyError TrashError

    hushMissingError :: MonadError TrashCan.TrashError m' => m' () -> m' ()
    hushMissingError = flip catchError \case
      TrashCan.MountError Mount.OriginMissingError {} -> pure ()
      e                                               -> throwError e

    rollbackTrashOrigins :: TrashCan -> PurgeError -> m ()
    rollbackTrashOrigins trashCan cause =
      TrashCan.restoreAll trashCan & execWriterT >>= \case
        errors@(_:_) -> TrashCan.dumpTrashed trashCan & runExceptT
          >>= either (const $ TrashCan.trashed trashCan <&> Left) (pure . Right)
          >>= throwError . PurgeRollbackError cause errors profile trashCan
        [] -> do
          TrashCan.remove trashCan
            & modifyError TrashCanRemoveError
          throwError cause

showSwitchError :: SwitchError -> Text
showSwitchError = \case
  UnlinkCurrentError cause -> showUnlinkError cause
  LinkNextError cause      -> showLinkError cause

-- | Error thrown by 'switch'.
--
-- @since 0.1.0.0
data SwitchError
  = UnlinkCurrentError !UnlinkError
  -- ^ Error was encountered during 'unlink'.
  | LinkNextError !LinkError
  -- ^ Error was encountered during 'link'.

showUnlinkError :: UnlinkError -> Text
showUnlinkError = \case
  ValidateUnlinkError cause -> showLinkedEntryValidateError cause
  PurgeUnlinkError cause    -> showPurgeError cause

-- | Error thrown by 'unlink'.
--
-- @since 0.1.0.0
data UnlinkError
  = ValidateUnlinkError !LinkedEntryValidateError
  -- ^ Validation error of entries was encountered.
  | PurgeUnlinkError !PurgeError
  -- ^ Error was encountered during 'purge'.

showLinkedEntryValidateError :: LinkedEntryValidateError -> Text
showLinkedEntryValidateError = \case
  OriginDoesNotExistError path
    -> "Origin " <> Path.showAbsolute path <> " does not exist."
  OriginCanonicalizeError path cause
    -> "Unable to resolve path " <> Path.showAbsolute path
    <> " because of: " <> show cause
  OriginChangedError Entry {mountPath = Mount.Mount mountPath, originPath} actual
    -> "Expected " <> Path.showAbsolute originPath
    <> " to be a link pointing at " <> Path.showAbsolute mountPath
    <> ". Instead it resolves to " <> Path.showAbsolute actual <> "."

-- | Error thrown by 'validateLinkedEntry'.
--
-- @since 0.1.0.0
data LinkedEntryValidateError
  = OriginDoesNotExistError !Path.Absolute
  -- ^ 'Entry's 'originPath' does not exist.
  | OriginCanonicalizeError !Path.Absolute !IOException
  -- ^ Unable to canonicalize 'originPath'.
  | OriginChangedError !Entry !Path.Absolute
  -- ^ 'originPath' is not a link pointing at 'mountPath'.

showLinkError :: LinkError -> Text
showLinkError = \case
  PurgeLinkError cause -> showPurgeError cause
  ValidateLinkError cause -> showUnlinkedEntryValidateError cause
  LinkRollbackError rollbackCause rollbackErrors Profile {name = Name name}
    -> "Linking of the \"" <> name <> "\" profile has failed"
    <> " with the following error: " <> Registry.showLinkError rollbackCause
    <> "\nAttempt to remove linked paths has failed"
    <> " with the following errors:\n" <> showRollbackErrors rollbackErrors
    <> "Please fix the errors above and then remove the links manually."
    <> "\n`pfile` is not tracking these links."
  LinkError cause -> Registry.showLinkError cause
  where
    showRollbackErrors :: [(Path.Absolute, Path.RemoveError)] -> Text
    showRollbackErrors = unlines . fmap \(originPath, error) ->
      Path.showAbsolute originPath <> " (" <> Path.showRemoveError error <> ")"

-- | Error thrown by 'link'.
--
-- @since 0.1.0.0
data LinkError
  = PurgeLinkError !PurgeError
  -- ^ Error was encountered during 'purge'.
  | ValidateLinkError !UnlinkedEntryValidateError
  -- ^ Validation error of entries was encountered.
  | LinkRollbackError
  -- ^ 'link' attempted to rollback due to 'Registry.LinkError'. The rollback
  -- has failed with a list of 'PFile.Path.RemoveError's. Since the rollback
  -- has failed, the profile passed to 'link' was partially linked - some links
  -- were created and should be removed manually.
    !Registry.LinkError
    -- ^ Cause of rollback.
    ![(Path.Absolute, Path.RemoveError)]
    -- ^ List of errors encountered during rollback.
    !Profile
    -- ^ 'Profile' passed to 'link'.
  | LinkError !Registry.LinkError
  -- ^ 'link' attempted to rollback due to 'Registry.LinkError'. The rollback
  -- has succeeded. The profile passed to 'link' was not linked.

showUnpackError :: UnpackError -> Text
showUnpackError = \case
  PurgeUnpackError cause -> showPurgeError cause
  UnlinkUnpackError cause -> showUnlinkError cause
  UnpackRollbackError rollbackCause rollbackErrors Profile {name = Name name}
    -> "Unpacking of the \"" <> name <> "\" profile has failed"
    <> " with the following error: " <> Registry.showUnpackError rollbackCause
    <> "\nAttempt to remove unpacked paths has failed"
    <> " with the following errors:\n" <> showRollbackErrors rollbackErrors
    <> "Please fix the errors above and then remove the unpacked paths manually."
    <> "\n`pfile` is not tracking these unpacked paths."
  UnpackError cause -> Registry.showUnpackError cause
  where
    showRollbackErrors :: [(Path.Absolute, Path.RemoveError)] -> Text
    showRollbackErrors = unlines . fmap \(originPath, error) ->
      Path.showAbsolute originPath <> " (" <> Path.showRemoveError error <> ")"

-- | Error thrown by 'unpack'.
--
-- @since 0.1.0.0
data UnpackError
  = PurgeUnpackError !PurgeError
  -- ^ Error was encountered during 'purge'.
  | UnlinkUnpackError !UnlinkError
  -- ^ Error was encountered during 'unlink'.
  | UnpackRollbackError
  -- ^ 'unpack' attempted to rollback due to 'Registry.UnpackError'. The
  -- rollback has failed with a list of 'PFile.Path.RemoveError's. Since the
  -- rollback has failed, the profile passed to 'unpack' was partially unpacked
  -- - some entries were unpacked and should be removed manually.
    !Registry.UnpackError
    -- ^ Cause of rollback.
    ![(Path.Absolute, Path.RemoveError)]
    -- ^ List of errors encountered during rollback.
    !Profile
    -- ^ 'Profile' passed to 'unpack'.
  | UnpackError !Registry.UnpackError

showUnlinkedEntryValidateError :: UnlinkedEntryValidateError -> Text
showUnlinkedEntryValidateError = \case
  OriginOccupiedError Entry {mountPath = Mount.Mount mountPath, originPath}
    -> "Unable to link origin " <> Path.showAbsolute originPath
    <> " to entry " <> Path.showAbsolute mountPath
    <> " because the origin is occupied."

-- | Error thrown by 'validateUnlinkedEntry'.
--
-- @since 0.1.0.0
newtype UnlinkedEntryValidateError
  = OriginOccupiedError Entry
  -- ^ 'Entry's 'originPath' is occupied.

showPurgeError :: PurgeError -> Text
showPurgeError = \case
  TrashCanCreateError cause -> TrashCan.showCreateError cause
  TrashError cause -> TrashCan.showTrashError cause
  PurgeRollbackError
      rollbackCause
      rollbackErrors
      Profile {name = Name name}
      TrashCan {root}
      trashed
    -> "Purge of the profile's \"" <> name <> "\" origins has failed"
    <> " with the following error: " <> showPurgeError rollbackCause
    <> "\nTrashed origins restoring has failed with the following errors:\n"
    <> (rollbackErrors <&> TrashCan.showRestoreEntryError & unlines)
    <> "Trash can with trashed origins could be found here: "
    <> Path.showAbsolute root
    <> ".\nA list of trashed origins could be found "
    <> either
        (\paths -> "below:\n" <> unlines (Path.showAbsolute <$> paths))
        (\path -> "in the file: " <> Path.showAbsolute path <> ".")
        trashed
  TrashCanRemoveError cause -> TrashCan.showRemoveError cause

-- | Error thrown by 'purge'.
--
-- @since 0.1.0.0
data PurgeError
  = TrashCanCreateError !TrashCan.CreateError
  -- ^ Error was encountered during 'TrashCan.create'.
  | TrashError !TrashCan.TrashError
  -- ^ Error was encountered during 'TrashCan.trash'.
  | PurgeRollbackError
  -- ^ 'purge' attempted to rollback due to 'PurgeError'. The rollback has
  -- failed with a list of 'TrashCan.RestoreEntryError's. Since the rollback
  -- has failed, the 'TrashCan.trash'ed entries of the 'Profile' are still kept
  -- inside of the 'TrashCan'.
    !PurgeError
    -- ^ Cause of rollback.
    ![TrashCan.RestoreEntryError]
    -- ^ List of errors encountered during rollback.
    !Profile
    -- ^ 'Profile' passed to 'purge'.
    !TrashCan
    -- ^ 'TrashCan' where 'TrashCan.trash'ed entries of the 'Profile' are kept.
    !(Either [Path.Absolute] Path.Absolute)
    -- ^ 'TrashCan's list of 'TrashCan.trash'ed entries is either dumped to
    -- a file with 'TrashCan.dumpTrashed' or provided as-is due to
    -- 'TrashCan.dumpTrashed' failure.
  | TrashCanRemoveError !TrashCan.RemoveError
  -- ^ Error was encountered during 'TrashCan.remove'.

-- | 'switch' options.
--
-- @since 0.1.0.0
newtype SwitchOptions
  = SwitchOptions
      { forceRemoveOccupied :: Bool
      -- ^ When 'forceRemoveOccupied' is set, forcibly remove a filesystem's
      -- object where a link pointing at an entry inside of the 'Profile' is
      -- expected.
      }
