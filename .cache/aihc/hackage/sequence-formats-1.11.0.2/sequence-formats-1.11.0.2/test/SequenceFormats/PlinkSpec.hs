{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.PlinkSpec (spec) where

import           SequenceFormats.Eigenstrat (EigenstratIndEntry (..),
                                             EigenstratSnpEntry (..),
                                             GenoEntry (..), GenoLine, Sex (..))
import           SequenceFormats.Plink      (PlinkFamEntry (..),
                                             PlinkPopNameMode (..),
                                             eigenstratInd2PlinkFam,
                                             plinkFam2EigenstratInd,
                                             readBimFile, readFamFile,
                                             readPlink, readPlinkBedFile,
                                             writePlink)
import           SequenceFormats.Utils      (Chrom (..))

import           Control.Foldl              (list, purely)
import           Control.Monad.IO.Class     (liftIO)
import           Data.Vector                (fromList)
import           Pipes                      (each, runEffect, (>->))
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           Test.Hspec

spec :: Spec
spec = do
    testReadBimFile
    testReadBimFileCompressed
    testReadFamFile
    testReadBedFile
    testReadBedFileCompressed
    testReadPlink
    testWritePlink
    testWritePlinkCompressed
    testFam2Ind
    testInd2Fam

mockDatEigenstratSnp :: [EigenstratSnpEntry]
mockDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "11") 0      0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 100000 0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 200000 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 300000 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 400000 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 500000 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "11") 600000 0.006000 "rs6666" 'G' 'T']

mockDatPlinkFam :: [PlinkFamEntry]
mockDatPlinkFam = [
    PlinkFamEntry "Family1" "SAMPLE0" "0" "0" Female "Pop1",
    PlinkFamEntry "Family2" "SAMPLE1" "0" "0" Male "Pop2",
    PlinkFamEntry "Family3" "SAMPLE2" "0" "0" Female "Pop3",
    PlinkFamEntry "Family4" "SAMPLE3" "0" "0" Male "Pop4",
    PlinkFamEntry "Family5" "SAMPLE4" "0" "0" Female "Pop5"]

mockDatPlinkBed :: [GenoLine]
mockDatPlinkBed = [
    fromList [Het,    Het,    Het,    HomAlt, HomAlt],
    fromList [HomAlt, Het,    HomRef, Het,    HomRef],
    fromList [HomRef, Het,    Het,    HomAlt, Het],
    fromList [HomAlt, HomAlt, Het,    HomRef, HomRef],
    fromList [HomRef, Het,    Het,    HomAlt, HomAlt],
    fromList [HomAlt, HomAlt, Het,    Het,    Het],
    fromList [HomRef, HomRef, Het,    Het,    HomAlt]]

testReadBimFile :: Spec
testReadBimFile = describe "readBimFile" $
    it "should read a BIM file correctly" $ do
        let esSnpProd = readBimFile "testDat/example.plink.bim"
        (runSafeT $ purely P.fold list esSnpProd) `shouldReturn` mockDatEigenstratSnp

testReadBimFileCompressed :: Spec
testReadBimFileCompressed = describe "readBimFile with gzip" $
    it "should read a BIM file correctly" $ do
        let esSnpProd = readBimFile "testDat/example.plink.bim.gz"
        (runSafeT $ purely P.fold list esSnpProd) `shouldReturn` mockDatEigenstratSnp

testReadFamFile :: Spec
testReadFamFile = describe "readFamFile" $
    it "should read a FAM file correctly" $ do
        readFamFile "testDat/example.fam" `shouldReturn` mockDatPlinkFam

testReadBedFile :: Spec
testReadBedFile = describe "readBedFile" $
    it "should read genotypes correctly" $ do
        let fn = "testDat/example.plink.bed"
        bedDat <- runSafeT $ do
            bedProd <- readPlinkBedFile fn 5
            purely P.fold list bedProd
        bedDat `shouldBe` mockDatPlinkBed

testReadBedFileCompressed :: Spec
testReadBedFileCompressed = describe "readBedFile with gzip" $
    it "should read genotypes correctly" $ do
        let fn = "testDat/example.plink.bed.gz"
        bedDat <- runSafeT $ do
            bedProd <- readPlinkBedFile fn 5
            purely P.fold list bedProd
        bedDat `shouldBe` mockDatPlinkBed

testReadPlink :: Spec
testReadPlink = describe "readPlink" $ do
    it "should read the correct Plink files" $ do
        let bimFile = "testDat/example.plink.bim"
            famFile = "testDat/example.fam"
            bedFile = "testDat/example.plink.bed"
        (indEntries, esProd) <- runSafeT $ readPlink bedFile bimFile famFile
        indEntries `shouldBe` mockDatPlinkFam
        snpGenoEntries <- runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatPlinkBed

testWritePlink :: Spec
testWritePlink = describe "writePlink" $ do
    it "should write and read back Plink data correctly" $ do
        let tmpGeno = "/tmp/plinkWriteTest.bed"
            tmpSnp = "/tmp/plinkWriteTest.bim"
            tmpInd = "/tmp/plinkWriteTest.fam"
            testDatSnpProd = each mockDatEigenstratSnp
            testDatGenoProd = each mockDatPlinkBed
            testDatJointProd = P.zip testDatSnpProd testDatGenoProd
            cons = writePlink tmpGeno tmpSnp tmpInd mockDatPlinkFam
        runSafeT . runEffect $ testDatJointProd >-> cons
        (indEntries, esProd) <- liftIO . runSafeT $ readPlink tmpGeno tmpSnp tmpInd
        indEntries `shouldBe` mockDatPlinkFam
        snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatPlinkBed

testWritePlinkCompressed :: Spec
testWritePlinkCompressed = describe "writePlink with gzip" $ do
    it "should write and read back Plink data correctly" $ do
        let tmpBed = "/tmp/plinkWriteTestGzip.bed.gz"
            tmpBim = "/tmp/plinkWriteTestGzip.bim.gz"
            tmpFam = "/tmp/plinkWriteTestGzip.fam"
            testDatSnpProd = each mockDatEigenstratSnp
            testDatGenoProd = each mockDatPlinkBed
            testDatJointProd = P.zip testDatSnpProd testDatGenoProd
            cons = writePlink tmpBed tmpBim tmpFam mockDatPlinkFam
        runSafeT . runEffect $ testDatJointProd >-> cons
        (indEntries, esProd) <- liftIO . runSafeT $ readPlink tmpBed tmpBim tmpFam
        indEntries `shouldBe` mockDatPlinkFam
        snpGenoEntries <- liftIO . runSafeT $ purely P.fold list esProd
        (map fst snpGenoEntries) `shouldBe` mockDatEigenstratSnp
        (map snd snpGenoEntries) `shouldBe` mockDatPlinkBed

testFam2Ind :: Spec
testFam2Ind = describe "plinkFam2EigenstratInd" $ do
    it "should correctly convert with family-option" $ do
        let fam = PlinkFamEntry "Family1" "SAMPLE0" "0" "0" Female "Pop1"
        plinkFam2EigenstratInd PlinkPopNameAsFamily fam `shouldBe` EigenstratIndEntry "SAMPLE0" Female "Family1"
    it "should correctly convert with phenotype-option" $ do
        let fam = PlinkFamEntry "Family1" "SAMPLE0" "0" "0" Female "Pop1"
        plinkFam2EigenstratInd PlinkPopNameAsPhenotype fam `shouldBe` EigenstratIndEntry "SAMPLE0" Female "Pop1"
    it "should correctly convert with both-option when they differ" $ do
        let fam = PlinkFamEntry "Family1" "SAMPLE0" "0" "0" Female "Pop1"
        plinkFam2EigenstratInd PlinkPopNameAsBoth fam `shouldBe` EigenstratIndEntry "SAMPLE0" Female "Family1:Pop1"
    it "should correctly convert with both-option when they are the same" $ do
        let fam = PlinkFamEntry "Pop1" "SAMPLE0" "0" "0" Female "Pop1"
        plinkFam2EigenstratInd PlinkPopNameAsBoth fam `shouldBe` EigenstratIndEntry "SAMPLE0" Female "Pop1"

testInd2Fam :: Spec
testInd2Fam = describe "eigenstratInd2PlinkFam" $ do
    it "should correctly convert with family-option" $ do
        let es = EigenstratIndEntry "SAMPLE0" Female "Pop1"
            fam = PlinkFamEntry "Pop1" "SAMPLE0" "0" "0" Female "0"
        eigenstratInd2PlinkFam PlinkPopNameAsFamily es `shouldBe` fam
    it "should correctly convert with phenotype-option" $ do
        let es = EigenstratIndEntry "SAMPLE0" Female "Pop1"
            fam = PlinkFamEntry "DummyFamily" "SAMPLE0" "0" "0" Female "Pop1"
        eigenstratInd2PlinkFam PlinkPopNameAsPhenotype es `shouldBe` fam
    it "should correctly convert with both-option" $ do
        let es = EigenstratIndEntry "SAMPLE0" Female "Pop1"
            fam = PlinkFamEntry "Pop1" "SAMPLE0" "0" "0" Female "Pop1"
        eigenstratInd2PlinkFam PlinkPopNameAsBoth es `shouldBe` fam
