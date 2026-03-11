{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.SysInfoSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.MemInfoSpec (spec) where

import Data.GenValidity (GenValid (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16)
import Fmt (blockMapF, build, fmt, (+|), (|+))
import MemInfo.Files.Root (initRoot, useTmp, writeRootedFile)
import MemInfo.Files.Smap (genBaseSmap)
import MemInfo.OrphanInstances ()
import System.Directory (createDirectoryIfMissing, createFileLink)
import System.FilePath (takeDirectory, (</>))
import System.MemInfo (readForOnePid', readMemUsage)
import System.MemInfo.Proc (MemUsage, amass)
import System.MemInfo.SysInfo (
  KernelVersion,
  mkReportBud,
 )
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, run)


spec :: Spec
spec = do
  describe "module System.MemInfo.MemProc" $ around useTmp $ do
    context "the proc fs stores mem info in <proc>/<id>/smap" $ do
      context "and has 1 non-root process" $ do
        context "readForOnePid'" $ do
          it "should generate the correct report" prop_ReadOneSmapProcPid
        context "readMemUsage'" $ do
          it "should generate the correct report" prop_ReadSmapProcMemUsage


prop_ReadSmapProcMemUsage :: FilePath -> Property
prop_ReadSmapProcMemUsage root = monadicIO $ do
  let fakeProc = "/usr/bin/true"
  (wantedUsage, smapsTxt) <- pick genBaseSmap
  let asReport = amass True [(fakeProc, wantedUsage)]
      writeFiles = writeSmapProcFiles root smapsTxt fakeProc
  verifyReadMemUsage root asReport gen3xKernel writeFiles


verifyReadMemUsage ::
  FilePath ->
  Map Text MemUsage ->
  Gen KernelVersion ->
  (Word16 -> IO ()) ->
  PropertyM IO ()
verifyReadMemUsage root expected genKernel writeFiles = do
  thePid <- pick genValidProcId
  version <- pick genKernel
  -- some matches are incomplete, that's ok, the test should fail if they fai
  Right procMap <- run $ do
    initRoot root version
    writeFiles thePid
    Just bud <- mkReportBud root (fromIntegral thePid :| [])
    readMemUsage bud
  assert (expected == procMap)


prop_ReadOneSmapProcPid :: FilePath -> Property
prop_ReadOneSmapProcPid root = monadicIO $ do
  let fakeProc = "/usr/bin/true"
  (wantedUsage, smapsTxt) <- pick genBaseSmap
  let asReport = amass True [(fakeProc, wantedUsage)]
      writeFiles = writeSmapProcFiles root smapsTxt fakeProc
  verifyReadForOnePid root asReport gen3xKernel writeFiles


verifyReadForOnePid ::
  FilePath ->
  Map Text MemUsage ->
  Gen KernelVersion ->
  (Word16 -> IO ()) ->
  PropertyM IO ()
verifyReadForOnePid root expected genKernel writeFiles = do
  thePid <- pick genValidProcId
  version <- pick genKernel
  -- this match is incomplete, that's ok, the test fails if the run produces a Left
  Right (theProc, theUsage) <- run $ do
    initRoot root version
    writeFiles thePid
    readForOnePid' root $ fromIntegral thePid
  assert (Map.lookup theProc expected == Just theUsage)


gen3xKernel :: Gen KernelVersion
gen3xKernel = do
  patch <- genValid
  minor <- genValid
  pure (3, minor, patch)


writeSmapProcFiles :: FilePath -> Text -> Text -> Word16 -> IO ()
writeSmapProcFiles root txt procName thePid = do
  let fakeParent = "/sbin/fake"
  writeRootedExeAndCmdLine root fakeParent 1
  writeRootedExeAndCmdLine root procName thePid
  writeRootedFakeStatus root procName thePid 1
  writeRootedFile root ("" +| thePid |+ "/smaps") txt


genValidProcId :: Gen Word16
genValidProcId = genValid `suchThat` (> 1)


writeRootedExeAndCmdLine :: FilePath -> Text -> Word16 -> IO ()
writeRootedExeAndCmdLine root fakeProc procId = do
  writeRootedFile root ("" +| procId |+ "/cmdline") fakeProc
  writeRootedExeLink root ("" +| procId |+ "/exe") fakeProc


writeRootedFakeStatus :: FilePath -> Text -> Word16 -> Word16 -> IO ()
writeRootedFakeStatus root fakeProc procId parentId = do
  let txt = fmt $ blockMapF $ statusInfoFields fakeProc parentId
      statusPath = "" +| procId |+ "/status"
  writeRootedFile root statusPath txt


statusInfoFields :: Text -> Word16 -> [(Text, Text)]
statusInfoFields fakeProc parentId =
  [ ("Name", fakeProc)
  , ("PPid", fmt $ build $ toInteger parentId)
  ]


writeRootedExeLink :: FilePath -> FilePath -> Text -> IO ()
writeRootedExeLink root path link = do
  let target = root </> path
  createDirectoryIfMissing True $ takeDirectory target
  createFileLink (Text.unpack link) target
