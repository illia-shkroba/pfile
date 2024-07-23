{-# LANGUAGE LambdaCase #-}

module PFile.Path.Tests
  ( ensureWriteFile
  ) where

import           PFile.Path           (Absolute, writeFile)
import           PFile.Path.Tests.Env ()
import           Protolude            hiding (writeFile)
import           Test.HUnit           (assertFailure)

ensureWriteFile :: Absolute -> Text -> IO ()
ensureWriteFile path contents = writeFile path contents & runExceptT >>= \case
  Left error -> assertFailure
    $ "Expected writeFile to succeed, instead got an error: " <> show error
  Right () -> pure ()
