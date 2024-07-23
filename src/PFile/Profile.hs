{- |
Module:      PFile.Profile
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for managing profiles defined for a set of entries.

(A note on naming: `pfile` is an abbreviation for "profile".)
-}

module PFile.Profile
  ( module PFile.Profile.Internal.Current
  , module PFile.Profile.Internal.Lifetime
  , module PFile.Profile.Internal.List
  , module PFile.Profile.Internal.Profile
  , module PFile.Profile.Internal.Serialization
  , module PFile.Profile.Internal.Switch
  ) where

import           PFile.Profile.Internal.Current
  ( LoadCurrentError (..)
  , SetCurrentError (..)
  , UnsetCurrentError (..)
  , loadCurrent
  , setCurrent
  , showLoadCurrentError
  , showSetCurrentError
  , showUnsetCurrentError
  , unsetCurrent
  )
import           PFile.Profile.Internal.Lifetime
  ( CreateError (..)
  , CreateOptions (..)
  , create
  , showCreateError
  )
import           PFile.Profile.Internal.List
  ( ListError (..)
  , ListOptions (..)
  , list
  , showListError
  )
import           PFile.Profile.Internal.Profile
  ( Entry (..)
  , Name (..)
  , Profile (..)
  , State (..)
  , absoluteRoot
  , profileRoot
  , profileState
  )
import           PFile.Profile.Internal.Serialization
  ( DumpError (..)
  , LoadError (..)
  , dump
  , load
  , showDumpError
  , showLoadError
  )
import           PFile.Profile.Internal.Switch
  ( LinkError (..)
  , SwitchError (..)
  , SwitchOptions (..)
  , UnpackError (..)
  , link
  , showLinkError
  , showSwitchError
  , showUnpackError
  , switch
  , unpack
  )
