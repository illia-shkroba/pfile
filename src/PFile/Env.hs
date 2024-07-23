{- |
Module:      PFile.Env
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for managing PFile's environment.
-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Env
  ( resolve
  , description
  , Options (..)
  , Env (..)
  ) where

import           Control.Monad.Trans.Maybe (MaybeT (..))
import           PFile.Error               (onIOError)
import           PFile.Path                ((<//>))
import qualified PFile.Path                as Path
import           Protolude
import           System.Directory
  ( XdgDirectory (..)
  , doesDirectoryExist
  , getXdgDirectory
  , makeAbsolute
  )
import           System.Environment        (lookupEnv)

-- | Deduce PFile's 'dataHomeDirPath' in the following order:
--
-- * Lookup in "$PFILE_DATA_HOME" environment variable if exists.
-- * Use ".pfile" directory from the current working directory if exists.
-- * Use "$XDG_DATA_HOME/pfile" directory.
--
-- Based on the 'dataHomeDirPath' other fields of 'Env' are resolved.
--
-- @since 0.1.0.0
resolve :: MonadIO m => Options -> m Env
resolve options@Options {verbose} = liftIO do
  resolved <-
    runMaybeT . asum . map MaybeT $
    [ lookupEnv "PFILE_DATA_HOME"
    , doesDirectoryExist ".pfile" `onIOError` pure False
      <&> bool Nothing (Just ".pfile")
    ]
  dataHomeDirPath <-
    maybe (getXdgDirectory XdgData "pfile") makeAbsolute resolved
    <&> Path.Absolute
  pure
    Env
      { dataHomeDirPath
      , profilesHomeDirPath = dataHomeDirPath <//> "profiles"
      , currentLinkPath = dataHomeDirPath <//> "current"
      , PFile.Env.print = bool (const $ pure ()) putErrLn verbose
      , options
      }

-- | Describe an 'Env'.
--
-- @since 0.1.0.0
description :: Env -> Text
description Env {dataHomeDirPath}
  =  "PFile uses data home directory: "
  <> Path.showAbsolute dataHomeDirPath <> "."

-- | 'Env' options.
--
-- @since 0.1.0.0
newtype Options
  = Options
      { verbose :: Bool
      -- ^ Whether logging should be enabled.
      }

-- | PFile's environment that effects its behavior.
--
-- @since 0.1.0.0
data Env
  = Env
      { dataHomeDirPath     :: !Path.Absolute
      -- ^ PFile's home directory.
      , profilesHomeDirPath :: !Path.Absolute
      -- ^ Profiles home directory.
      , currentLinkPath     :: !Path.Absolute
      -- ^ Path to a current profile's link.
      , print               :: !(Text -> IO ())
      -- ^ Function used by logging functions.
      , options             :: !Options
      -- ^ 'Options' that were used to create the 'Env'.
      }
