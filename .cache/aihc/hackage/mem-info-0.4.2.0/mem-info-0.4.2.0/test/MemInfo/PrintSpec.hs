{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : MemInfo.PrintSpec
Copyright   : (c) 2023 Tim Emiola
Maintainer  : Tim Emiola <adetokunbo@emio.la>
SPDX-License-Identifier: BSD3
-}
module MemInfo.PrintSpec (spec) where

import Data.Text (Text)
import qualified Data.Text as Text
import System.MemInfo.Choices (Style (Csv, Normal))
import System.MemInfo.Print (fmtAsHeader, fmtMemUsage, fmtOverall, styleOutput)
import System.MemInfo.Proc (MemUsage (..))
import System.Posix.Types (ProcessID)
import Test.Hspec


spec :: Spec
spec = describe "module System.MemInfo.Print" $ do
  fmtAsHeaderSpec
  fmtOverallSpec
  fmtMemUsageSpec
  styleOutputSpec


styleOutputSpec :: Spec
styleOutputSpec = describe "styleOutput" $ do
  describe "when accurate and swap shown" $ do
    let count = 6
        usage = sampleUsage' count
        styleOutput' s = styleOutput True s True
    describe "when style is csv" $ do
      let style = Csv
      describe "when discriminating by pid" $ do
        let want =
              [ "Private,Shared,RAM Used,Swap Used,Program [pid]"
              , "1,1,2,4,by-id-and-name-cmd [100]"
              ]
        it "should generate the expected output" $ do
          styleOutput' style [(biName, usage)] `shouldBe` want

      describe "not discriminating by pid" $ do
        let want =
              [ "Private,Shared,RAM Used,Swap Used,Program (# processes)"
              , "1,1,2,4,by-name-cmd (6)"
              ]
        it "should generate the expected output" $ do
          styleOutput' style [(monoName, usage)] `shouldBe` want

  describe "when accurate and no swap shown" $ do
    let count = 6
        usage = sampleUsage' count
        styleOutput' s = styleOutput False s True
    describe "when style is normal" $ do
      let style = Normal
          wantedSummary =
            Text.unlines
              [ "------------------------------------"
              , "                             2.0 KiB"
              , "===================================="
              ]
      describe "when discriminating by pid" $ do
        let want =
              [ "  Private  +   Shared   =   RAM Used\tProgram [pid]"
              , "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-id-and-name-cmd [100]"
              , wantedSummary
              ]
        it "should generate the expected output" $ do
          styleOutput' style [(biName, usage)] `shouldBe` want

      describe "not discriminating by pid" $ do
        let want =
              [ "  Private  +   Shared   =   RAM Used\tProgram (# processes)"
              , "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-name-cmd (6)"
              , wantedSummary
              ]
        it "should generate the expected output" $ do
          styleOutput' style [(monoName, usage)] `shouldBe` want


fmtAsHeaderSpec :: Spec
fmtAsHeaderSpec = describe "fmtAsHeader" $ do
  describe "when discriminating by pid" $ do
    let byPid = True
    describe "when swap is not required" $ do
      it "does not show it" $ do
        fmtAsHeader byPid False `shouldBe` headerNoSwap

    describe "when swap is required" $ do
      it "does show it" $ do
        fmtAsHeader byPid True `shouldBe` headerSwap

  describe "when not discriminating by pid" $ do
    let byPid = False
        withProcCount = Text.replace "[pid]" "(# processes)"
    describe "when swap is not required" $ do
      it "does not show it" $ do
        fmtAsHeader byPid False `shouldBe` withProcCount headerNoSwap

    describe "when swap is required" $ do
      it "does show it" $ do
        fmtAsHeader byPid True `shouldBe` withProcCount headerSwap


headerNoSwap :: Text
headerNoSwap = "  Private  +   Shared   =   RAM Used\tProgram [pid]"


headerSwap :: Text
headerSwap = "  Private  +   Shared   =   RAM Used Swap Used\tProgram [pid]"


fmtOverallSpec :: Spec
fmtOverallSpec = describe "fmtOverall" $ do
  describe "when swap is not required" $ do
    it "does not show it" $ do
      fmtOverall False (1, 1) `shouldBe` sampleNoSwapOverall
  describe "when swap is required" $ do
    it "does show it" $ do
      fmtOverall True (1, 1) `shouldBe` sampleSwapOverall


sampleNoSwapOverall :: Text
sampleNoSwapOverall =
  Text.unlines
    [ "------------------------------------"
    , "                             1.0 KiB"
    , "===================================="
    ]


sampleSwapOverall :: Text
sampleSwapOverall =
  Text.unlines
    [ "----------------------------------------------"
    , "                             1.0 KiB   1.0 KiB"
    , "=============================================="
    ]


fmtMemUsageSpec :: Spec
fmtMemUsageSpec = describe "fmtMemUsage" $ do
  describe "when displaying by-pid" $ do
    let usage = sampleUsage' 1
    describe "and swap is not required" $ do
      it "does not show it" $ do
        fmtMemUsage False biName usage `shouldBe` sampleTotalNoSwap

    describe "when swap is required" $ do
      it "shows it" $ do
        fmtMemUsage True biName usage `shouldBe` sampleTotalSwap

  describe "when displaying by-name" $ do
    let usage = sampleUsage' 3
    describe "and swap is not required" $ do
      it "does not show it" $ do
        fmtMemUsage False monoName usage `shouldBe` sampleTotalNoSwapMono


monoName :: Text
monoName = "by-name-cmd"


biName :: (ProcessID, Text)
biName = (100, "by-id-and-name-cmd")


sampleUsage' :: Int -> MemUsage
sampleUsage' muCount =
  MemUsage
    { muShared = 1
    , muPrivate = 2
    , muCount
    , muSwap = 4
    }


sampleTotalNoSwapMono :: Text
sampleTotalNoSwapMono = "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-name-cmd (3)"


sampleTotalNoSwap :: Text
sampleTotalNoSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB\tby-id-and-name-cmd [100]"


sampleTotalSwap :: Text
sampleTotalSwap = "   1.0 KiB +    1.0 KiB =    2.0 KiB   4.0 KiB\tby-id-and-name-cmd [100]"
