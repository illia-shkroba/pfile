{- |
Module:      PFile.Profile.Internal.Profile
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Core types and functions related to profiles.
-}

{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}

module PFile.Profile.Internal.Profile
  ( absoluteRoot
  , profileState
  , profileRoot
  , Profile (..)
  , Name (..)
  , State (..)
  , Entry (..)
  ) where

import           Control.Monad (MonadFail (..))
import           Data.Aeson    (FromJSON (..), ToJSON (..), withText)
import           PFile.Env     (Env (..))
import qualified PFile.Mount   as Mount
import           PFile.Path    ((<//>))
import qualified PFile.Path    as Path
import           Protolude     hiding (State)

-- | Get directory path where entries of a 'Profile' named 'Name' are stored.
--
-- @since 0.1.0.0
absoluteRoot :: MonadReader Env m => Name -> m Mount.Root
absoluteRoot name = do
  root <- profileRoot name
  root <//> "absolute"
    & pure . Mount.Root

-- | Get file path where serialized 'Profile' named 'Name' is stored.
--
-- @since 0.1.0.0
profileState :: MonadReader Env m => Name -> m Path.Absolute
profileState name = profileRoot name <&> (<//> "state.json")

-- | Get directory path where 'absoluteRoot' and 'profileState' of a 'Profile'
-- named 'Name' are located.
--
-- @since 0.1.0.0
profileRoot :: MonadReader Env m => Name -> m Path.Absolute
profileRoot (Name name) = do
  Env {profilesHomeDirPath} <- ask
  pure $ profilesHomeDirPath <//> toS name

-- | 'Profile' holds a list of 'Entry'ies.
--
-- @since 0.1.0.0
data Profile
  = Profile
      { name    :: !Name
        -- ^ 'Name' of a 'Profile'.
      , state   :: !State
        -- ^ Current 'State' of a 'Profile'.
      , entries :: ![Entry]
        -- ^ List of a 'Profile' 'Entry'ies.
      }
  deriving (Generic)

instance FromJSON Profile
instance ToJSON Profile

-- | 'Name' of a 'Profile'.
--
-- @since 0.1.0.0
newtype Name
  = Name { unName :: Text }
  deriving newtype (FromJSON, ToJSON)

-- | 'Profile's state.
--
-- @since 0.1.0.0
data State
  = Dangling
  -- ^ When an error is encountered during
  -- 'PFile.Profile.Internal.Registry.pushAll',
  -- 'PFile.Profile.Internal.Lifetime.create' attempts to rollback. If the
  -- rollback fails, the profile is considered 'Dangling'.
  | Valid
  -- ^ When 'PFile.Profile.Internal.Lifetime.create' succeeds, the created
  -- profile has 'Valid' state.

instance FromJSON State where
  parseJSON = withText "State" \case
    "dangling" -> pure Dangling
    "valid"    -> pure Valid
    s          -> fail . toS $ "Unable to parse State: " <> s

instance ToJSON State where
  toJSON = \case
    Dangling -> "dangling"
    Valid    -> "valid"

-- | 'Entry' represents a filesystem's object (directory, directory link, file,
-- file link) that is 'PFile.Mount.mount'ed (or
-- 'PFile.Profile.Internal.Registry.push'ed) inside of a 'Profile'.
--
-- @since 0.1.0.0
data Entry
  = Entry
      { mountPath  :: !Mount.Mount
      -- ^ Path to a filesystem's object mounted inside of a 'Profile'.
      , originPath :: !Path.Absolute
      -- ^ Path to a filesystem's object original location before
      -- 'PFile.Profile.Internal.Registry.push'ing to a 'Profile'.
      }
  deriving (Generic)

instance FromJSON Entry
instance ToJSON Entry
