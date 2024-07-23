{- |
Module:      PFile.Profile.Internal.Registry
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for managing profiles entries.
-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Profile.Internal.Registry
  ( pushAll
  , push
  , showPushError
  , PushError (..)
  , pop
  , showPopError
  , PopError (..)
  , linkAll
  , link
  , showLinkError
  , LinkError (..)
  , unpackAll
  , unpack
  , showUnpackError
  , UnpackError (..)
  ) where

import           Control.Monad.Writer           (MonadWriter (..))
import           PFile.Env                      (Env)
import           PFile.Error                    (modifyError)
import qualified PFile.Log                      as Log
import qualified PFile.Mount                    as Mount
import           PFile.Path
  ( CopyError
  , copy
  , createLink
  , showCopyError
  )
import qualified PFile.Path                     as Path
import           PFile.Profile.Internal.Profile (Entry, Name (..), absoluteRoot)
import qualified PFile.Profile.Internal.Profile as Entry (Entry (..))
import qualified PFile.Profile.LinkHandling     as LinkHandling
import           Protolude                      hiding (link)

-- | 'push' a list of 'PFile.Path.Absolute' inside of
-- a 'PFile.Profile.Internal.Profile.Profile' named 'Name' with a chosen
-- 'PFile.Profile.LinkHandling.Strategy' for links. When an error is
-- encountered during 'push', 'pushAll' terminates and provides successfully
-- 'push'ed entries via a 'MonadWriter'.
--
-- @since 0.1.0.0
pushAll ::
     ( MonadReader Env m
     , MonadError PushError m
     , MonadWriter [Entry] m
     , MonadIO m
     )
  => LinkHandling.Strategy
  -> Name
  -> [Path.Absolute]
  -> m ()
pushAll linkHandlingStrategy name = traverse_ \originPath ->
  push linkHandlingStrategy name originPath
    >>= \mountPath -> tell [Entry.Entry {Entry.mountPath, Entry.originPath}]

-- | 'PFile.Mount.mount' a 'PFile.Path.Absolute' inside of
-- a 'PFile.Profile.Internal.Profile.Profile' named 'Name' with a chosen
-- 'PFile.Profile.LinkHandling.Strategy' for links.
--
-- @since 0.1.0.0
push ::
     (MonadReader Env m, MonadError PushError m, MonadIO m)
  => LinkHandling.Strategy
  -> Name
  -> Path.Absolute
  -> m Mount.Mount
push linkHandlingStrategy name originPath = do
  type_ <- Path.typeOf originPath
  Log.info
    $  "Push "
    <> Path.showAbsolute originPath
    <> " (" <> maybe "unknown" Path.showType type_ <> ")"
    <> " into profile \"" <> unName name <> "\""
    <> " with link handling strategy "
    <> "\"" <> LinkHandling.showStrategy linkHandlingStrategy <> "\""
  root <- absoluteRoot name
  mountPath <- Mount.mount linkHandlingStrategy root originPath
    & modifyError PushError
  Log.info
    $  "Pushed "
    <> Path.showAbsolute originPath
    <> " into profile \"" <> unName name <> "\""
  pure mountPath

showPushError :: PushError -> Text
showPushError = \case
  PushError cause -> Mount.showMountError cause

-- | Error thrown by 'push'.
--
-- @since 0.1.0.0
newtype PushError
  = PushError Mount.MountError
  -- ^ Error was encountered during 'PFile.Mount.mount'.

-- | 'PFile.Mount.unmount' a 'PFile.Mount.Mount' from
-- a 'PFile.Profile.Internal.Profile.Profile' named 'Name' back to its original
-- location at 'PFile.Path.Absolute'.
--
-- @since 0.1.0.0
pop ::
     (MonadReader Env m, MonadError PopError m, MonadIO m)
  => Name
  -> Mount.Mount
  -> m Path.Absolute
pop name mountPath = do
  type_ <- Path.typeOf . Mount.absolute $ mountPath
  Log.info
    $  "Pop "
    <> Path.showAbsolute (Mount.absolute mountPath)
    <> " (" <> maybe "unknown" Path.showType type_ <> ")"
    <> " from profile \"" <> unName name <> "\""
  root <- absoluteRoot name
  originPath <- Mount.unmount root mountPath
    & modifyError PopError
  Log.info
    $  "Popped "
    <> Path.showAbsolute (Mount.absolute mountPath)
    <> " from profile \"" <> unName name <> "\""
  pure originPath

showPopError :: PopError -> Text
showPopError = \case
  PopError cause -> Mount.showUnmountError cause

-- | Error thrown by 'pop'.
--
-- @since 0.1.0.0
newtype PopError
  = PopError Mount.UnmountError
  -- ^ Error was encountered during 'PFile.Mount.unmount'.

-- | 'link' a list of 'Entry'ies. For each 'Entry' a link at
-- 'PFile.Profile.Internal.Profile.originPath' will be created pointing at
-- 'PFile.Profile.Internal.Profile.mountPath'. When an error is encountered
-- during 'link', 'linkAll' terminates and provides successfully 'link'ed
-- entries via a 'MonadWriter'.
--
-- @since 0.1.0.0
linkAll ::
     (MonadError LinkError m, MonadWriter [Path.Absolute] m, MonadIO m)
  => [Entry]
  -> m ()
linkAll = traverse_ \Entry.Entry {Entry.mountPath, Entry.originPath} ->
  [originPath] <$ link mountPath originPath >>= tell

-- | Create a link at 'PFile.Path.Absolute' pointing at 'PFile.Mount.Mount'.
--
-- @since 0.1.0.0
link ::
     (MonadError LinkError m, MonadIO m)
  => Mount.Mount
  -> Path.Absolute
  -> m ()
link mountPath originPath =
  createLink (Mount.absolute mountPath) originPath
    & modifyError LinkError

showLinkError :: LinkError -> Text
showLinkError = \case
  LinkError cause -> Path.showCreateLinkError cause

-- | Error thrown by 'link'.
--
-- @since 0.1.0.0
newtype LinkError
  = LinkError Path.CreateLinkError
  -- ^ Unable to create a link at 'PFile.Path.Absolute' pointing at
  -- 'PFile.Mount.Mount'.

-- | 'unpack' a list of 'Entry'ies. For each 'Entry'
-- a 'PFile.Profile.Internal.Profile.mountPath' will be copied to
-- 'PFile.Profile.Internal.Profile.originPath'. When an error is encountered
-- during 'unpack', 'unpackAll' terminates and provides successfully 'unpack'ed
-- entries via a 'MonadWriter'.
--
-- @since 0.1.0.0
unpackAll ::
     (MonadError UnpackError m, MonadWriter [Path.Absolute] m, MonadIO m)
  => [Entry]
  -> m ()
unpackAll = traverse_ \Entry.Entry {Entry.mountPath, Entry.originPath} ->
  [originPath] <$ unpack mountPath originPath >>= tell

-- | Copy filesystem's object at 'PFile.Mount.Mount' to 'PFile.Path.Absolute'.
--
-- @since 0.1.0.0
unpack ::
     (MonadError UnpackError m, MonadIO m)
  => Mount.Mount
  -> Path.Absolute
  -> m ()
unpack mountPath originPath =
  copy (Mount.absolute mountPath) originPath
    & modifyError (UnpackError mountPath originPath)

showUnpackError :: UnpackError -> Text
showUnpackError = \case
  UnpackError (Mount.Mount src) dest cause
    -> "Unable to unpack " <> Path.showAbsolute src
    <> " to " <> Path.showAbsolute dest
    <> " because of: " <> showCopyError cause

-- | Error thrown by 'unpack'.
--
-- @since 0.1.0.0
data UnpackError
  = UnpackError !Mount.Mount !Path.Absolute !CopyError
  -- ^ Unable to copy 'PFile.Mount.Mount' to 'PFile.Path.Absolute'.
