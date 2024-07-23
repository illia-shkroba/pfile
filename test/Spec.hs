module Main
  ( main
  ) where

import qualified PFile.Mount.Tests    as PFile.Mount
import qualified PFile.TrashCan.Tests as PFile.TrashCan
import           Protolude
import           Test.Tasty           (defaultMain, testGroup)

main :: IO ()
main =
  [PFile.Mount.tests, PFile.TrashCan.tests]
    & sequenceA
    >>= defaultMain . testGroup "Tests"
