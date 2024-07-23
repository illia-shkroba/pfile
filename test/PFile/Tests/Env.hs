module PFile.Tests.Env
  ( withTempDirectory
  ) where

import qualified PFile.Path       as Path
import           Protolude
import           System.Directory (getTemporaryDirectory)
import qualified System.IO.Temp   as Temp

withTempDirectory :: (Path.Absolute -> IO a) -> IO a
withTempDirectory callback = do
  temporaryRoot <- getTemporaryDirectory
  Temp.withTempDirectory temporaryRoot "pfile-test" (callback . Path.Absolute)
