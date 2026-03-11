{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.EigenstratSpec where

import           Control.Foldl              (list, purely)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Vector                (fromList)
import           Pipes                      (each, runEffect, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine, Sex (..),
                                             readEigenstrat, writeEigenstrat)
import           SequenceFormats.Utils      (Chrom (..))
import           Test.Hspec

spec :: Spec
spec = do
    testReadEigenstrat
    testReadEigenstratCompressed
    testWriteEigenstrat
    testWriteEigenstratCompressed

mockDatEigenstratSnp :: [EigenstratSnpEntry]
mockDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']

mockDatEigenstratInd :: [EigenstratIndEntry]
mockDatEigenstratInd = [
    EigenstratIndEntry "SAMPLE0" Female "Case",
    EigenstratIndEntry "SAMPLE1" Male "Case",
    EigenstratIndEntry "SAMPLE2" Female "Control",
    EigenstratIndEntry "SAMPLE3" Male "Control",
    EigenstratIndEntry "SAMPLE4" Female "Control"]

mockDatEigenstratGeno :: [GenoLine]
mockDatEigenstratGeno = [
    fromList [Het, Het, Het, HomAlt, HomAlt],
    fromList [HomAlt, Het, HomRef, Het, HomRef],
    fromList [HomRef, Het, Het, HomAlt, Het],
    fromList [HomAlt, Missing, Het, HomRef, HomRef],
    fromList [HomRef, Het, Het, HomAlt, HomAlt],
    fromList [HomAlt, HomAlt, Het, Missing, Het],
    fromList [HomRef, HomRef, Het, Missing, Missing]]

testReadEigenstrat :: Spec
testReadEigenstrat = describe "readEigenstrat" $ do
    it "should read the correct eigenstrat file" $ do
        let esSnpFile = "testDat/example.snp"
            esIndFile = "testDat/example.ind"
            esGenoFile = "testDat/example.eigenstratgeno"
        (indEntries, esProd) <- runSafeT $ readEigenstrat esGenoFile esSnpFile esIndFile
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatEigenstratGeno

testReadEigenstratCompressed :: Spec
testReadEigenstratCompressed = describe "readEigenstrat with gzip" $ do
    it "should read the correct eigenstrat file" $ do
        let esSnpFile = "testDat/example.snp.gz"
            esIndFile = "testDat/example.ind"
            esGenoFile = "testDat/example.eigenstratgeno.gz"
        (indEntries, esProd) <- runSafeT $ readEigenstrat esGenoFile esSnpFile esIndFile
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatEigenstratGeno


testWriteEigenstrat :: Spec
testWriteEigenstrat = describe "writeEigenstrat" $ do
    it "should write and read back eigenstrat data correctly" $ do
        let tmpGeno = "/tmp/eigenstratWriteTest.geno"
            tmpSnp = "/tmp/eigenstratWriteTest.snp"
            tmpInd = "/tmp/eigenstratWriteTest.ind"
            testDatSnpProd = each mockDatEigenstratSnp
            testDatGenoProd = each mockDatEigenstratGeno
            testDatJointProd = P.zip testDatSnpProd testDatGenoProd
            cons = writeEigenstrat tmpGeno tmpSnp tmpInd mockDatEigenstratInd
        runSafeT . runEffect $ testDatJointProd >-> cons
        (indEntries, esProd) <- liftIO . runSafeT $ readEigenstrat tmpGeno tmpSnp tmpInd
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatEigenstratGeno

testWriteEigenstratCompressed :: Spec
testWriteEigenstratCompressed = describe "writeEigenstrat with gzip" $ do
    it "should write and read back eigenstrat data correctly" $ do
        let tmpGeno = "/tmp/eigenstratWriteTestGzip.geno.gz"
            tmpSnp = "/tmp/eigenstratWriteTestGzip.snp.gz"
            tmpInd = "/tmp/eigenstratWriteTestGzip.ind"
            testDatSnpProd = each mockDatEigenstratSnp
            testDatGenoProd = each mockDatEigenstratGeno
            testDatJointProd = P.zip testDatSnpProd testDatGenoProd
            cons = writeEigenstrat tmpGeno tmpSnp tmpInd mockDatEigenstratInd
        runSafeT . runEffect $ testDatJointProd >-> cons
        (indEntries, esProd) <- liftIO . runSafeT $ readEigenstrat tmpGeno tmpSnp tmpInd
        indEntries `shouldBe` mockDatEigenstratInd
        snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatEigenstratGeno
