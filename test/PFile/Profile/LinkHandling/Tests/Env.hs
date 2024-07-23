{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

module PFile.Profile.LinkHandling.Tests.Env
  (
  ) where

import           PFile.Path.Tests.Env       ()
import           PFile.Profile.LinkHandling (Error (..))
import           Protolude

deriving instance Show Error
