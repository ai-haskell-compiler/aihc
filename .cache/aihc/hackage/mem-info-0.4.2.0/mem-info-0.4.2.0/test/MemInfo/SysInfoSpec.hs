{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.SysInfoSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.SysInfoSpec (spec) where

import Control.Monad (when)
import Data.GenValidity (GenValid (..))
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Word (Word16, Word8)
import Fmt (build, fmt, (+|), (|+))
import MemInfo.Files.Root (initRoot, useTmp, writeRootedFile)
import MemInfo.Files.Smap (genBaseSmap, genSmapLine, genWithPss)
import MemInfo.OrphanInstances ()
import System.MemInfo.SysInfo (
  KernelVersion,
  RamFlaw (..),
  ReportBud (..),
  SwapFlaw (..),
  mkReportBud,
  parseKernelVersion,
 )
import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic (PropertyM, assert, monadicIO, pick, run)


spec :: Spec
spec = describe "module System.MemInfo.SysInfo" $ do
  describe "parseKernelVersion" $ do
    it "should parse values from Text successfully" prop_roundtripKernelVersion
  mkReportBudSpec


mkReportBudSpec :: Spec
mkReportBudSpec = do
  describe "mkReportBud" $ around useTmp $ do
    context "when the kernel is 2.4.x" $ do
      context "and there is no Inact_ ram" $ do
        it "generates the expected ReportBud" (prop_With24Kernel True)
      context "and there is Inact_ ram" $ do
        it "generates the expected ReportBud" (prop_With24Kernel False)
    context "when the kernel is earlier than 2.4" $ do
      it "generates the expected ReportBud" prop_With22Kernel
    context "when the kernel is a special 2.6 kernel" $ do
      it "generates the expected ReportBud" prop_WithSpecial26Kernel
    context "when the kernel is a later 2.6 kernel with no smap support" $ do
      it "generates the expected ReportBud" prop_WithNoSmap26Kernel
    context "when the kernel is a later 2.6 kernel with smap support" $ do
      context "and there are smap files but no Pss" $ do
        it "generates the expected ReportBud" $ prop_WithSmaps26Kernel False
      context "and there are smap files but with Pss" $ do
        it "generates the expected ReportBud" $ prop_WithSmaps26Kernel True
    context "when the kernel is later than 3.x" $ do
      context "and there are smap files" $ do
        it "generates the expected ReportBud" $ prop_WithAfter3xKernel True
      context "and there are no smap files" $ do
        it "generates the expected ReportBud" $ prop_WithAfter3xKernel False


prop_roundtripKernelVersion :: Property
prop_roundtripKernelVersion =
  within 5000000 $
    forAll genOsRelease $
      \(version, txt) -> Just version == parseKernelVersion txt


genOsRelease :: Gen (KernelVersion, Text)
genOsRelease = oneof [fromSingle, fromDouble, fromTriple]


fromTriple :: Gen (KernelVersion, Text)
fromTriple = do
  let toN = fromIntegral
      txt a b c = "" +| a |+ "." +| b |+ "." +| c |+ ""
  (x, y, z) <- genValid :: Gen (Word8, Word8, Word8)
  zSuffixed <- someWithSuffix z
  pure ((toN x, toN y, toN z), txt x y zSuffixed)


fromDouble :: Gen (KernelVersion, Text)
fromDouble = do
  (x, y) <- genValid :: Gen (Word8, Word8)
  ySuffixed <- someWithSuffix y
  let toN = fromIntegral
      txt a b = "" +| a |+ "." +| b |+ ""
  pure ((toN x, toN y, 0), txt x ySuffixed)


suffixes :: [Text]
suffixes = ["-pre", "b", "-kali", "-test"]


someWithSuffix :: Word8 -> Gen Text
someWithSuffix w = do
  addSuffix <- genValid
  s <- elements suffixes
  if addSuffix
    then pure $ "" +| w |+ "" +| s |+ ""
    else pure $ fmt $ build w


fromSingle :: Gen (KernelVersion, Text)
fromSingle = do
  x <- genValid :: Gen Word8
  pure ((fromIntegral x, 0, 0), fmt $ build x)


prop_WithAfter3xKernel :: Bool -> FilePath -> Property
prop_WithAfter3xKernel hasPss root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = if hasPss then Nothing else Just ExactForIsolatedMem
          , rbSwapFlaws = Just $ if hasPss then ExactForIsolatedSwap else NoSwap
          , rbHasSmaps = hasPss
          }
      writeSmaps txt thePid =
        when hasPss $ writeRootedFile root ("" +| thePid |+ "/smaps") txt
      genKernel = do
        patch <- genValid
        minor <- genValid
        pure (3, minor, patch)

  (_ignored, smapsTxt) <- pick genBaseSmap
  verifyMkReportBud root withFlaws genKernel $ writeSmaps smapsTxt


prop_With24Kernel :: Bool -> FilePath -> Property
prop_With24Kernel hasInactRam root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = Just $ if hasInactRam then SomeSharedMem else ExactForIsolatedMem
          , rbSwapFlaws = Just NoSwap
          }
      genKernel = do
        patch <- genValid
        pure (2, 4, patch)
      writeMemInfo txt _unused = writeRootedFile root "meminfo" txt

  memInfoTxt <- pick $ genMemInfoLines hasInactRam
  verifyMkReportBud root withFlaws genKernel $ writeMemInfo memInfoTxt


prop_WithSmaps26Kernel :: Bool -> FilePath -> Property
prop_WithSmaps26Kernel hasPss root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = if hasPss then Nothing else Just ExactForIsolatedMem
          , rbSwapFlaws = Just ExactForIsolatedSwap
          , rbHasSmaps = True
          , rbHasPss = hasPss
          }
      writeSmaps txt thePid =
        writeRootedFile root ("" +| thePid |+ "/smaps") txt
      genKernel = do
        patch <- chooseInteger (10, 100)
        pure (2, 6, fromIntegral patch)
      genSmap = if hasPss then genWithPss else genBaseSmap

  (_ignored, smapsTxt) <- pick genSmap
  verifyMkReportBud root withFlaws genKernel $ writeSmaps smapsTxt


prop_WithSpecial26Kernel :: FilePath -> Property
prop_WithSpecial26Kernel root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = Just NoSharedMem
          , rbSwapFlaws = Just NoSwap
          }
      genKernel = do
        patch <- chooseInteger (1, 9)
        pure (2, 6, fromIntegral patch)

  verifyMkReportBud root withFlaws genKernel $ const $ pure ()


prop_WithNoSmap26Kernel :: FilePath -> Property
prop_WithNoSmap26Kernel root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = Just SomeSharedMem
          , rbSwapFlaws = Just NoSwap
          }
      genKernel = do
        patch <- chooseInteger (10, 100)
        pure (2, 6, fromIntegral patch)

  verifyMkReportBud root withFlaws genKernel $ const $ pure ()


genMemInfoLines :: Bool -> Gen Text
genMemInfoLines hasInact = do
  -- reference: http://darenmatthews.com/blog/?p=2092
  (_unused1, memTotalTxt) <- genSmapLine "MemTotal"
  (_unused2, memFreeTxt) <- genSmapLine "MemFree"
  (_unused3, buffersTxt) <- genSmapLine "Buffers"
  (_unused4, inactTxt) <- genSmapLine "Inact_Target" -- or Inact_{Clean,Dirty}
  let baselines = [memTotalTxt, memFreeTxt, buffersTxt]
      ls = if hasInact then baselines <> [inactTxt] else baselines
  pure $ Text.unlines ls


prop_With22Kernel :: FilePath -> Property
prop_With22Kernel root = monadicIO $ do
  let withFlaws x =
        x
          { rbRamFlaws = Just ExactForIsolatedMem
          , rbSwapFlaws = Just NoSwap
          }
      genKernel = do
        patch <- genValid
        pure (2, 2, patch)
  verifyMkReportBud root withFlaws genKernel $ const $ pure ()


verifyMkReportBud ::
  FilePath ->
  (ReportBud -> ReportBud) ->
  Gen KernelVersion ->
  (Word16 -> IO ()) ->
  PropertyM IO ()
verifyMkReportBud root changeBud genKernel writeFiles = do
  (thePid, version, want) <- pick $ genExpectedBud changeBud genKernel root
  bud <- run $ do
    initRoot root version
    writeFiles thePid
    mkReportBud root (fromIntegral thePid :| [])
  assert (bud == Just want)


genExpectedBud ::
  (ReportBud -> ReportBud) ->
  Gen KernelVersion ->
  FilePath ->
  Gen (Word16, KernelVersion, ReportBud)
genExpectedBud changeBud genKernel root = do
  thePid <- genValidProcId
  rbKernel <- genKernel
  let theBud =
        ReportBud
          { rbPids = fromIntegral thePid :| []
          , rbKernel
          , rbHasPss = False
          , rbHasSwapPss = False
          , rbHasSmaps = False
          , rbRamFlaws = Nothing
          , rbSwapFlaws = Nothing
          , rbProcRoot = root
          }
  pure (thePid, rbKernel, changeBud theBud)


genValidProcId :: Gen Word16
genValidProcId = genValid `suchThat` (> 1)
