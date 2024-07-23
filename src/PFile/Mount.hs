{- |
Module:      PFile.Mount
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for mounting filesystem's objects under some "root"
directory.
-}

{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}

module PFile.Mount
  ( mount
  , mountPath
  , showMountError
  , MountError (..)
  , unmount
  , originPath
  , showUnmountError
  , UnmountError (..)
  , showOriginResolveError
  , OriginResolveError (..)
  , Root (..)
  , Mount (..)
  ) where

import           Data.Aeson                 (FromJSON, ToJSON)
import           PFile.Error                (liftIOWithError, modifyError)
import           PFile.Path
  ( dropDrive
  , makeRelative
  , move
  , pathIsSymbolicLink
  , (<//>)
  )
import qualified PFile.Path                 as Path
import qualified PFile.Profile.LinkHandling as LinkHandling
import           Protolude
import           System.FilePath            (joinDrive, takeDrive)

-- | Mount a 'PFile.Path.Absolute' inside of a 'Root' with a chosen
-- 'PFile.Profile.LinkHandling.Strategy' for links. 'mount' does the following:
--
-- 1. Moves (renames) 'PFile.Path.Absolute' into 'mountPath' under 'Root'. If
--    the move fails due to cross-device move attempt, the
--    'PFile.Path.Absolute' is copied instead.
-- 2. Removes 'PFile.Path.Absolute' at its original location.
-- 3. Handles links with 'PFile.Profile.LinkHandling.handle'.
--
-- @since 0.1.0.0
mount ::
     (MonadError MountError m, MonadIO m)
  => LinkHandling.Strategy
  -> Root
  -> Path.Absolute
  -> m Mount
mount linkHandlingStrategy root src = do
  ifM (pathIsSymbolicLink src `liftIOWithError` OriginMissingError src)
    do
      LinkHandling.handle linkHandlingStrategy src dest
        & modifyError LinkHandlingMountError
      Path.remove src
        & modifyError OriginLinkRemoveError
    (move src dest & modifyError OriginMoveError)
  pure $ Mount dest
  where
    Mount dest = mountPath root src

-- | Mount path of a 'PFile.Path.Absolute' inside of a 'Root'. 'mountPath' uses
-- 'PFile.Path.dropDrive' on the 'PFile.Path.Absolute' and then appends the
-- result to the 'Root'. For example:
--
-- >>> mountPath (Root $ Path.Absolute "/a/b/c/") (Path.Absolute "/d/e/f.txt") == Mount (Path.Absolute "/a/b/c/d/e/f.txt")
-- True
--
-- @since 0.1.0.0
mountPath :: Root -> Path.Absolute -> Mount
mountPath (Root root) path = Mount $ root <//> dropDrive path

showMountError :: MountError -> Text
showMountError = \case
  OriginMissingError path cause
    -> "Unable to find origin file " <> Path.showAbsolute path
    <> " because of: " <> show cause
  LinkHandlingMountError cause -> LinkHandling.showError cause
  OriginLinkRemoveError cause
    -> "Unable to remove link because of: " <> Path.showRemoveError cause
  OriginMoveError cause -> Path.showMoveError cause

-- | Error thrown by 'mount'.
--
-- @since 0.1.0.0
data MountError
  = OriginMissingError !Path.Absolute !IOException
  -- ^ 'PFile.Path.Absolute' is missing. 'IOException' is captured from
  -- 'pathIsSymbolicLink'.
  | LinkHandlingMountError !LinkHandling.Error
  -- ^ Error was encountered during 'PFile.Profile.LinkHandling.handle'.
  | OriginLinkRemoveError !Path.RemoveError
  -- ^ Unable to remove 'PFile.Path.Absolute'. This error is thrown after the
  -- 'PFile.Path.Absolute' got copied under 'Root'.
  | OriginMoveError !Path.MoveError
  -- ^ Error was encountered during 'PFile.Path.move'.

-- | Unmount a 'Mount' from a 'Root' back to its original location. 'unmount'
-- does the following:
--
-- 1. Moves (renames) 'Mount' into 'originPath' from the 'Root'. If the move
--    fails due to cross-device move attempt, the 'Mount' is copied instead.
-- 2. Removes 'Mount' at its original location.
-- 3. Handles links with 'PFile.Profile.LinkHandling.handle
--    PFile.Profile.LinkHandling.CopyLink'.
--
-- @since 0.1.0.0
unmount ::
     (MonadError UnmountError m, MonadIO m) => Root -> Mount -> m Path.Absolute
unmount root (Mount src) = do
  dest <- originPath root (Mount src)
    & modifyError OriginResolveError
  ifM (pathIsSymbolicLink src `liftIOWithError` MountMissingError src)
    do
      LinkHandling.handle LinkHandling.CopyLink src dest
        & modifyError LinkHandlingUnmountError
      Path.remove src
        & modifyError MountLinkRemoveError
    (move src dest & modifyError MountMoveError)
  pure dest

-- | Origin path of a 'Mount' outside of a 'Root'. 'originPath' is an inverse
-- of 'mountPath'. Here is an example usage:
--
-- >>> r = originPath (Root $ Path.Absolute "/a/b/c/") (Mount $ Path.Absolute "/a/b/c/d/e/f.txt") & runExcept
-- >>> r & either (const False) (== Path.Absolute "/d/e/f.txt")
-- True
--
-- 'originPath' works only for Posix paths. Windows paths are not supported
-- currently.
--
-- @since 0.1.0.0
originPath :: MonadError OriginResolveError m => Root -> Mount -> m Path.Absolute
originPath (Root root) (Mount path) = do
  let relativePath = makeRelative root path
  when (relativePath == Path.unAbsolute path) . throwError
    $ OriginOutsideOfRootError (Mount path) (Root root)
  relativePath
     -- Dirty hack that works for Posix paths
    & joinDrive (takeDrive $ Path.unAbsolute root)
    & pure . Path.Absolute

showUnmountError :: UnmountError -> Text
showUnmountError = \case
  OriginResolveError cause -> showOriginResolveError cause
  MountMissingError path cause
    -> "Unable to find mount file " <> Path.showAbsolute path
    <> " because of: " <> show cause
  LinkHandlingUnmountError cause -> LinkHandling.showError cause
  MountLinkRemoveError cause
    -> "Unable to remove link because of: " <> Path.showRemoveError cause
  MountMoveError cause -> Path.showMoveError cause

-- | Error thrown by 'unmount'.
--
-- @since 0.1.0.0
data UnmountError
  = OriginResolveError !OriginResolveError
  -- ^ Error was encountered during 'originPath'.
  | MountMissingError !Path.Absolute !IOException
  -- ^ 'Mount' is missing. 'IOException' is captured from 'pathIsSymbolicLink'.
  | LinkHandlingUnmountError !LinkHandling.Error
  -- ^ Error was encountered during 'PFile.Profile.LinkHandling.handle'.
  | MountLinkRemoveError !Path.RemoveError
  -- ^ Unable to remove 'Mount'. This error is thrown after the 'Mount' got
  -- copied back to its original location.
  | MountMoveError !Path.MoveError
  -- ^ Error was encountered during 'PFile.Path.move'.

showOriginResolveError :: OriginResolveError -> Text
showOriginResolveError = \case
  OriginOutsideOfRootError (Mount path) (Root root)
    -> "Expected path " <> Path.showAbsolute path
    <> " to be relative to: " <> Path.showAbsolute root <> "."

-- | Error thrown by 'originPath'.
--
-- @since 0.1.0.0
data OriginResolveError
  = OriginOutsideOfRootError !Mount !Root
  -- ^ 'Mount' is outside of the 'Root'.

-- | Root for 'mount'ed 'PFile.Path.Absolute's.
--
-- @since 0.1.0.0
newtype Root
  = Root Path.Absolute

-- | 'mount'ed 'PFile.Path.Absolute'.
--
-- @since 0.1.0.0
newtype Mount
  = Mount { absolute :: Path.Absolute }
  deriving (Eq)
  deriving newtype (FromJSON, ToJSON)
