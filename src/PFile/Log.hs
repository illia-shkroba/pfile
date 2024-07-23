{- |
Module:      PFile.Log
Copyright:   (c) 2024 Illia Shkroba
License:     BSD3
Maintainer:  Illia Shkroba <is@pjwstk.edu.pl>
Stability:   unstable
Portability: non-portable (Non-Unix systems are not supported)

Types and functions for logging.
-}

{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Log
  ( info
  , warning
  , error
  , panic
  ) where

import           PFile.Env (Env (..))
import           Protolude hiding (panic, print)

info :: (MonadReader Env m, MonadIO m) => Text -> m ()
info message = do
  Env {print} <- ask
  liftIO $ print $ "[INFO] " <> message

warning :: (MonadReader Env m, MonadIO m) => Text -> m ()
warning message = do
  Env {print} <- ask
  liftIO $ print $ "[WARNING] " <> message

error :: MonadIO m => Text -> m ()
error = putErrLn

panic :: MonadIO m => Text -> m a
panic message = error message >> liftIO exitFailure
