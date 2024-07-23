{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE OverloadedStrings #-}

module PFile.TrashCan.Tests
  ( tests
  ) where

import           Control.Monad.Writer     (execWriterT)
import qualified Data.HashSet             as Set
import           PFile.Path               (doesPathExist, (<//>))
import qualified PFile.Path               as Path
import           PFile.Path.Tests         (ensureWriteFile)
import           PFile.Tests.Env          (withTempDirectory)
import           PFile.TrashCan           (TrashCan (..))
import qualified PFile.TrashCan           as TrashCan
import           PFile.TrashCan.Tests.Env ()
import           Protolude
import           System.FilePath          ((</>))
import           Test.Hspec
  ( around
  , describe
  , it
  , shouldBe
  , shouldReturn
  )
import           Test.HUnit               (assertFailure)
import           Test.Tasty               (TestTree, testGroup)
import           Test.Tasty.Hspec         (testSpec)

tests :: IO TestTree
tests = do
  unitTests_ <- unitTests
  pure $ testGroup "PFile.TrashCan" [unitTests_]

unitTests :: IO TestTree
unitTests = testSpec "Unit Tests" $
  around withTempDirectory do
    describe "trash" do
      it "trashes a file properly" \temporaryRoot ->
        (,) <$> filesNames <*> filesContents & traverse_ \(name, contents) ->
          trashFileTest (temporaryRoot <//> "origin" </> name) contents
    describe "restore" do
      it "restores a file properly" \temporaryRoot ->
        (,) <$> filesNames <*> filesContents & traverse_ \(name, contents) ->
          restoreFileTest (temporaryRoot <//> "origin" </> name) contents
    describe "restoreAll" do
      it "restores all files properly" \temporaryRoot ->
        restoreAllTest
          $ zip
            (filesNames <&> \name -> temporaryRoot <//> "origin" </> name)
            filesContents
    describe "remove" do
      it "removes empty trashCan properly" \_ ->
        removeTest []
      it "removes non-empty trashCan properly" \temporaryRoot ->
        removeTest
          $ zip
            (filesNames <&> \name -> temporaryRoot <//> "origin" </> name)
            filesContents
    describe "dumpTrashed" do
      it "dumps trashed list properly" \temporaryRoot ->
        dumpTrashedTest
          $ zip
            (filesNames <&> \name -> temporaryRoot <//> "origin" </> name)
            filesContents
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

trashFileTest :: Path.Absolute -> Text -> IO ()
trashFileTest path contents = withTrashCan \trashCan -> do
  ensureWriteFile path contents
  ensureTrash path trashCan

restoreFileTest :: Path.Absolute -> Text -> IO ()
restoreFileTest originPath contents = withTrashCan \trashCan -> do
  ensureWriteFile originPath contents
  ensureTrash originPath trashCan
  TrashCan.trashed trashCan >>= \case
    [mountPath] -> TrashCan.restore trashCan mountPath & runExceptT >>= \case
      Left error -> assertFailure
        $ "Expected restore to succeed, instead got an error: " <> show error
      Right () -> readFile (Path.unAbsolute originPath) `shouldReturn` contents
    xs -> assertFailure
      $ "Expected single path in trashed list, instead got: " <> show xs

restoreAllTest :: [(Path.Absolute, Text)] -> IO ()
restoreAllTest files = withTrashCan \trashCan -> do
  files & traverse_ (uncurry ensureWriteFile)
  files & traverse_ \(originPath, _) -> ensureTrash originPath trashCan
  length <$> TrashCan.trashed trashCan `shouldReturn` length files
  TrashCan.restoreAll trashCan & execWriterT >>= \case
    errors@(_:_) -> assertFailure
      $ "Expected restoreAll to succeed, instead got errors: " <> show errors
    [] -> files & traverse_ \(originPath, contents) ->
      readFile (Path.unAbsolute originPath) `shouldReturn` contents

removeTest :: [(Path.Absolute, Text)] -> IO ()
removeTest files = withTrashCan \trashCan -> do
  files & traverse_ (uncurry ensureWriteFile)
  files & traverse_ \(originPath, _) -> ensureTrash originPath trashCan
  ensureRemove trashCan

dumpTrashedTest :: [(Path.Absolute, Text)] -> IO ()
dumpTrashedTest files = withTrashCan \trashCan -> do
  files & traverse_ (uncurry ensureWriteFile)
  files & traverse_ \(originPath, _) -> ensureTrash originPath trashCan
  TrashCan.dumpTrashed trashCan & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected dumpTrashed to succeed, instead got an error: " <> show error
    Right trashedPath -> do
      unlessM (doesPathExist trashedPath) . assertFailure
        $ "Expected file \"" <> Path.unAbsolute trashedPath <> "\" to exist"
      actualPaths <-
        readFile (Path.unAbsolute trashedPath)
        <&> fmap (Path.Absolute . toS) . lines
      expectedPaths <- TrashCan.trashed trashCan
      Set.fromList actualPaths `shouldBe` Set.fromList expectedPaths

withTrashCan :: (TrashCan -> IO a) -> IO a
withTrashCan callback = do
  trashCan <- ensureCreate
  result <- callback trashCan
  ensureRemove trashCan
  pure result

ensureCreate :: IO TrashCan
ensureCreate = runExceptT TrashCan.create >>= \case
  Left error -> assertFailure
    $ "Expected create to succeed, instead got an error: " <> show error
  Right trashCan -> pure trashCan

ensureRemove :: TrashCan -> IO ()
ensureRemove trashCan@TrashCan {root} =
  TrashCan.remove trashCan & runExceptT >>= \case
    Left error -> assertFailure
      $ "Expected remove to succeed, instead got an error: " <> show error
    Right () -> whenM (doesPathExist root) . assertFailure
      $ "Expected directory \"" <> Path.unAbsolute root <> "\" to not exist"

ensureTrash :: Path.Absolute -> TrashCan -> IO ()
ensureTrash path trashCan = TrashCan.trash path trashCan & runExceptT >>= \case
  Left error -> assertFailure
    $ "Expected trash to succeed, instead got an error: " <> show error
  Right () -> whenM (doesPathExist path) . assertFailure
    $ "Expected file \"" <> Path.unAbsolute path <> "\" to not exist"
