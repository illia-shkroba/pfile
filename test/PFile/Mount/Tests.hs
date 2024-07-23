{-# OPTIONS_GHC -Wno-name-shadowing #-}

{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.Mount.Tests
  ( tests
  ) where

import qualified Data.HashSet               as Set
import           PFile.Mount
  ( Mount (..)
  , Root (..)
  , mount
  , mountPath
  , originPath
  , unmount
  )
import           PFile.Mount.Tests.Env      ()
import           PFile.Path
  ( doesPathExist
  , dropDrive
  , findFiles
  , (<//>)
  )
import qualified PFile.Path                 as Path
import           PFile.Path.Tests           (ensureWriteFile)
import qualified PFile.Profile.LinkHandling as LinkHandling
import           PFile.Tests.Env            (withTempDirectory)
import           Protolude
import           System.FilePath            ((</>))
import           Test.Hspec
  ( around
  , describe
  , it
  , shouldBe
  , shouldReturn
  )
import           Test.HUnit                 (assertFailure)
import           Test.Tasty                 (TestTree, testGroup)
import           Test.Tasty.Hspec           (testSpec)
import           Test.Tasty.QuickCheck      (testProperty)

tests :: IO TestTree
tests = do
  unitTests_ <- unitTests
  pure $ testGroup "PFile.Mount" [properties, unitTests_]

properties :: TestTree
properties =
  testGroup
    "Properties"
    [ testProperty
        "`mountPath` removes drive from `path` and attaches result under `root`"
        \(Root root) path ->
          mountPath (Root root) path == Mount (root <//> dropDrive path)
    , testProperty
        "`originPath` is an inverse of `mountPath`"
        \(Root root) path ->
          originPath (Root root) (mountPath (Root root) path)
          & either (const False) (== path)
    ]

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" $
  around withTempDirectory do
    describe "mount" do
      it "mounts a file properly" \temporaryRoot ->
        (,) <$> filesNames <*> filesContents & traverse_ \(name, contents) ->
          mountFileTest temporaryRoot (temporaryRoot <//> "origin" </> name) contents
      it "mounts a directory properly" \temporaryRoot ->
        mountDirectoryTest temporaryRoot
          $ zip filesNames filesContents
    describe "unmount" do
      it "unmounts a file properly" \temporaryRoot ->
        (,) <$> filesNames <*> filesContents & traverse_ \(name, contents) ->
          unmountFileTest temporaryRoot (temporaryRoot <//> "origin" </> name) contents
      it "unmounts a directory properly" \temporaryRoot ->
        unmountDirectoryTest temporaryRoot
          $ zip filesNames filesContents
  where
    filesNames :: [FilePath]
    filesNames =
      [ "9h(ILN:/b<t{mg2"
      , "'*_"
      , "*CDy/|\rHh?/!@"
      , "C~(?3|(y/8&"
      , "1t=_rL'/%JB0\"8"
      , "ej/s$\\K.5ZT/Z*6rn-j\t3/x':@bj"
      , "YJ/%X4C$KMr=/z\r"
      , "f~NO*a j@7"
      , "swSgYJUM9r/.EYR\"wADf/*DJd:("
      , " Y(7*/|/<-[6nv3"
      , "yfF~"
      , "zETbVl w7./]^/[m>+"
      , ";)A/:H'^og@GB"
      , "\\g2\n( M#/scIU/o^A/Nj>/nC,ASzz7q"
      , "SB$N/;F"
      , "?,@Zo\tz/*|1%%\tWm}/ k>oAV_7M/yGn$+c 7AS"
      , "\"\\_"
      , "4g/M;FD901"
      , "\t$sL;G9/3zZ|p[1/w\\6f]/=926UD"
      ]

    filesContents :: [Text]
    filesContents =
      [ "\54951\170056NU+\"\92881a-\142680\177134\SOXKE\1097408\63631hq\1021187\ETBd\1019100#\1028440\1041796\1000925Gat"
      , "RD\177342\1036654\1084661\189502\1096769 \1011758\96759\67362\1058870\96334\74438o\1094509>\1095213[,\37703SFb\147814\25819F\153345#b"
      , "\SUB\170178\178560yv\1011719Q/\\$>L\GS7\1048669\&1z\1093012\1086214r\199200P,\nOrG{\1002826L"
      , "et\39933\152786\71222\51388WFY\DC1'il9kH\132409E\\C\1024119\68094]\991083B\11343\DC1Ez\18164"
      , "!dB\999918\SUBo`-<QrS<9+MT\1044433\DELt\96208\17044\1090040@1PP\147330]K"
      , "/7!SH\98989\183950J\"\138051\161420\35056WE<2F8&~)c\181084&!\78704N_$\15030"
      , "#[&)xPg$nH]9mt60\94378jU=*B^F[\29380x\199030D\92604"
      , "CD\149540\&1TqCL\132951H:,P)^4\2636I&{r?CmS\179807Nf*'"
      , "\SIhFN:CH=OS(2?{A+\GS\SOA\STXG{|nL?8!L_"
      , "CpH`8a?\NAK,\GS\ETXqG\ETXN9\EMa)\\1AYEco3Fd_"
      ]

mountFileTest :: Path.Absolute -> Path.Absolute -> Text -> IO ()
mountFileTest temporaryRoot originPath contents = do
  let root = temporaryRoot <//> "root"
  ensureWriteFile originPath contents
  mount LinkHandling.CopyLink (Root root) originPath & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected mount to succeed, instead got an error: " <> show error
    Right (Mount mountPath) -> do
      mountPath `shouldBe` root <//> dropDrive originPath
      readFile (Path.unAbsolute mountPath) `shouldReturn` contents
      whenM (doesPathExist originPath) . assertFailure
        $ "Expected file \"" <> Path.unAbsolute originPath <> "\" to not exist"

mountDirectoryTest :: Path.Absolute -> [(FilePath, Text)] -> IO ()
mountDirectoryTest temporaryRoot files = do
  let root = temporaryRoot <//> "root"
      originPath = temporaryRoot <//> "origin"
  files & traverse_ \(fileName, contents) ->
    ensureWriteFile (originPath <//> fileName) contents
  mount LinkHandling.CopyLink (Root root) originPath & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected mount to succeed, instead got an error: " <> show error
    Right (Mount mountPath) -> do
      mountPath `shouldBe` root <//> dropDrive originPath
      actualPaths <- findFiles mountPath
      let expectedPaths = files <&> \(fileName, _) -> mountPath <//> fileName
      Set.fromList actualPaths `shouldBe` Set.fromList expectedPaths
      files & traverse_ \(fileName, contents) ->
        readFile (Path.unAbsolute $ mountPath <//> fileName) `shouldReturn` contents
      whenM (doesPathExist originPath) . assertFailure
        $ "Expected directory \"" <> Path.unAbsolute originPath <> "\" to not exist"

unmountFileTest :: Path.Absolute -> Path.Absolute -> Text -> IO ()
unmountFileTest temporaryRoot originPath contents = do
  let root = temporaryRoot <//> "root"
      mountPath = root <//> dropDrive originPath
  ensureWriteFile mountPath contents
  unmount (Root root) (Mount mountPath) & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected unmount to succeed, instead got an error: " <> show error
    Right newOriginPath -> do
      newOriginPath `shouldBe` originPath
      readFile (Path.unAbsolute newOriginPath) `shouldReturn` contents
      whenM (doesPathExist mountPath) . assertFailure
        $ "Expected file \"" <> Path.unAbsolute mountPath <> "\" to not exist"

unmountDirectoryTest :: Path.Absolute -> [(FilePath, Text)] -> IO ()
unmountDirectoryTest temporaryRoot files = do
  let root = temporaryRoot <//> "root"
      originPath = temporaryRoot <//> "origin"
      mountPath = root <//> dropDrive originPath
  files & traverse_ \(fileName, contents) ->
    ensureWriteFile (mountPath <//> fileName) contents
  unmount (Root root) (Mount mountPath) & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected unmount to succeed, instead got an error: " <> show error
    Right newOriginPath -> do
      newOriginPath `shouldBe` originPath
      actualPaths <- findFiles newOriginPath
      let expectedPaths = files <&> \(fileName, _) -> newOriginPath <//> fileName
      Set.fromList actualPaths `shouldBe` Set.fromList expectedPaths
      files & traverse_ \(fileName, contents) ->
        readFile (Path.unAbsolute $ newOriginPath <//> fileName) `shouldReturn` contents
      whenM (doesPathExist mountPath) . assertFailure
        $ "Expected directory \"" <> Path.unAbsolute mountPath <> "\" to not exist"
