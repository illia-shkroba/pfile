{- |
Module:      PFile.Completion
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Completions for PFile.
-}

module PFile.Completion
  ( profileNames
  , profiles
  ) where

import qualified PFile.Env     as Env
import           PFile.Profile (Profile)
import qualified PFile.Profile as Profile
import           Protolude

profileNames :: IO [[Char]]
profileNames = profiles <&> fmap (toS . Profile.unName . Profile.name)

profiles :: IO [Profile]
profiles = do
  env <- Env.resolve Env.Options {Env.verbose = False}
  Profile.list listOptions
    & flip runReaderT env
    & runExceptT >>= either (const $ pure []) pure
  where
    listOptions :: Profile.ListOptions
    listOptions = Profile.ListOptions {Profile.shouldFilterDangling = True}
