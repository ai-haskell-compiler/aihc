{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.FreqSumSpec (spec) where

import           SequenceFormats.FreqSum (FreqSumEntry (..), FreqSumHeader (..),
                                          printFreqSumFile, readFreqSumFile)
import           SequenceFormats.Utils   (Chrom (..))

import           Control.Foldl           (list, purely)
import           Pipes                   (each, runEffect, (>->))
import qualified Pipes.Prelude           as P
import           Pipes.Safe              (runSafeT)
import           Test.Hspec

spec :: Spec
spec = do
    testReadFreqSumFile
    testPrintFreqSumFile

testReadFreqSumFile :: Spec
testReadFreqSumFile = describe "readFreqSumFile" $ do
    (fsHeader, fsEntries) <- runIO . runSafeT $ do
        (fsHeader_, fsProd_) <- readFreqSumFile "testDat/example.freqsum"
        fsEntries_ <- purely P.fold list fsProd_
        return (fsHeader_, fsEntries_)
    it "should read the correct fs header" $
        fsHeader `shouldBe` mockDatFsHeader
    it "should read the correct fs entries" $
        fsEntries `shouldBe` mockDatFsEntries

testPrintFreqSumFile :: Spec
testPrintFreqSumFile = describe "printFreqSumFile" $ do
    let fn = "/tmp/freqSumWriteTest.txt"
        testDatFsProd = each mockDatFsEntries
    runIO . runSafeT . runEffect $ testDatFsProd >-> printFreqSumFile fn mockDatFsHeader
    (fsHeader, fsEntries) <- runIO . runSafeT $ do
        (fsHeader_, fsProd_) <- readFreqSumFile fn
        fsEntries_ <- purely P.fold list fsProd_
        return (fsHeader_, fsEntries_)
    it "should read the correct fs header after writing" $
        fsHeader `shouldBe` mockDatFsHeader
    it "should read the correct fs entries after writing" $
        fsEntries `shouldBe` mockDatFsEntries

mockDatFsHeader :: FreqSumHeader
mockDatFsHeader = FreqSumHeader names numbers
  where
    names = ["SAMPLE0", "SAMPLE1", "SAMPLE2", "SAMPLE3", "SAMPLE4"]
    numbers = [2, 2, 2, 1, 1]

mockDatFsEntries :: [FreqSumEntry]
mockDatFsEntries = [
    FreqSumEntry (Chrom "11") 0      Nothing Nothing 'A' 'C' [Just (1, 2), Just (1, 2),  Just (1, 2), Just (1, 1),  Just (1, 1)],
    FreqSumEntry (Chrom "11") 100000 Nothing Nothing 'A' 'G' [Just (2, 2), Just (1, 2),  Just (0, 2), Just (0, 1),  Just (0, 1)],
    FreqSumEntry (Chrom "11") 200000 Nothing Nothing 'A' 'T' [Just (0, 2), Just (1, 2),  Just (1, 2), Just (1, 1),  Just (1, 1)],
    FreqSumEntry (Chrom "11") 300000 Nothing Nothing 'C' 'A' [Just (2, 2), Nothing, Just (1, 2), Just (0, 1),  Just (0, 1)],
    FreqSumEntry (Chrom "11") 400000 Nothing Nothing 'G' 'A' [Just (0, 2), Just (1, 2),  Just (1, 2), Just (1, 1),  Just (1, 1)],
    FreqSumEntry (Chrom "11") 500000 Nothing Nothing 'T' 'A' [Just (2, 2), Just (2, 2),  Just (1, 2), Nothing, Just (1, 1)],
    FreqSumEntry (Chrom "11") 600000 Nothing Nothing 'G' 'T' [Just (0, 2), Just (0, 2),  Just (1, 2), Nothing, Nothing]]

