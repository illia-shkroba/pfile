{-# OPTIONS_GHC -Wno-orphans #-}

{-# LANGUAGE StandaloneDeriving #-}

module PFile.Path.Tests.Env
  (
  ) where

import           PFile.Path
  ( Absolute (..)
  , CopyError (..)
  , CopyFileError (..)
  , CopyLinkError (..)
  , CreateDirectoryError (..)
  , CreateDirectoryLinkError (..)
  , CreateFileLinkError (..)
  , CreateParentError (..)
  , MoveDirectoryError (..)
  , MoveDirectoryLinkError (..)
  , MoveError (..)
  , MoveFileError (..)
  , MoveFileLinkError (..)
  , RemoveError (..)
  , WriteFileError (..)
  )
import           Protolude
import           System.FilePath       (joinDrive, joinPath)
import           Test.Tasty.QuickCheck
  ( Arbitrary (..)
  , Gen
  , chooseInt
  , elements
  )

deriving instance Show CopyError
deriving instance Show CopyFileError
deriving instance Show CopyLinkError
deriving instance Show CreateDirectoryError
deriving instance Show CreateDirectoryLinkError
deriving instance Show CreateFileLinkError
deriving instance Show CreateParentError
deriving instance Show MoveDirectoryError
deriving instance Show MoveDirectoryLinkError
deriving instance Show MoveError
deriving instance Show MoveFileError
deriving instance Show MoveFileLinkError
deriving instance Show WriteFileError

deriving instance Show RemoveError

instance Arbitrary Absolute where
  arbitrary = do
    n <- chooseInt (1, 30)
    replicateM n word
      <&> Absolute . joinDrive "/" . joinPath
    where
      word :: Gen [Char]
      word = do
        n <- chooseInt (1, 1023)
        replicateM n $ elements posixFilePathAllowedCharacters

posixFilePathAllowedCharacters :: [Char]
posixFilePathAllowedCharacters = concat
  [ ['A' .. 'Z']
  , ['a' .. 'z']
  , ['0' .. '9']
  , [' ', '\t', '\n', '\r']
  , ['+', '-', '=', '|', '~', '(', ')', '<', '>', '{', '}', '\\']
  , ['?', ',', '.', '!', ';', ':', '\'', '"', '[', ']']
  , ['&', '%', '$', '#', '@', '^', '*', '_']
  ]

deriving instance Show Absolute
