{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module PFile.Mount.Tests.Env
  (
  ) where

import           PFile.Mount
  ( Mount (..)
  , MountError (..)
  , OriginResolveError (..)
  , Root (..)
  , UnmountError (..)
  )
import           PFile.Path.Tests.Env                 ()
import           PFile.Profile.LinkHandling.Tests.Env ()
import           Protolude
import           Test.Tasty.QuickCheck                (Arbitrary)

deriving instance Arbitrary Root
deriving instance Show Root

deriving instance Arbitrary Mount
deriving instance Show Mount

deriving instance Show MountError

deriving instance Show UnmountError

deriving instance Show OriginResolveError
