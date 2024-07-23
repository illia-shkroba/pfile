{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE StandaloneDeriving #-}

module PFile.TrashCan.Tests.Env
  (
  ) where

import           PFile.Mount.Tests.Env ()
import           PFile.Path.Tests.Env  ()
import           PFile.TrashCan
  ( CreateError (..)
  , DumpTrashedError (..)
  , RemoveError (..)
  , RestoreEntryError (..)
  , RestoreError (..)
  , TrashCan (..)
  , TrashError (..)
  , trashed
  )
import           Protolude
import           System.IO.Unsafe      (unsafePerformIO)
import qualified Text.Show             (Show (show))

deriving instance Show CreateError

deriving instance Show TrashError

deriving instance Show RestoreError

deriving instance Show RestoreEntryError

deriving instance Show RemoveError

deriving instance Show DumpTrashedError

instance Show TrashCan where
  show trashCan@TrashCan {root, dataRoot}
    =  "TrashCan {root = " <> show root
    <> ", dataRoot = " <> show dataRoot
    <> ", _trashed = " <> show (unsafePerformIO $ trashed trashCan) <> "}"
