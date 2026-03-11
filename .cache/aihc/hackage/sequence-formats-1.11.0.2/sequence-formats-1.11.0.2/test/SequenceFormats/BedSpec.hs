{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.BedSpec (spec) where

import           Control.Foldl              (list, purely)
import           Pipes                      (each)
import qualified Pipes.Prelude              as P
import           Pipes.Safe                 (runSafeT)
import           SequenceFormats.Bed        (BedEntry (..), readBedFile)
import           SequenceFormats.Eigenstrat (EigenstratSnpEntry (EigenstratSnpEntry))
import           SequenceFormats.Genomic    (genomicFilterThroughBed)
import           SequenceFormats.Utils      (Chrom (..))
import           Test.Hspec

spec :: Spec
spec = do
    testReadBed
    testFilterThroughBed

mockDatBed :: [BedEntry]
mockDatBed = [
    BedEntry (Chrom "11") 0 100,
    BedEntry (Chrom "11") 200 300,
    BedEntry (Chrom "11") 400 500]

testReadBed :: Spec
testReadBed = describe "readBed" $ do
    it "should read the correct eigenstrat file" $ do
        bedEntries <- runSafeT $ purely P.fold list (readBedFile "testDat/example.bed")
        bedEntries `shouldBe` mockDatBed

mockDatEigenstratSnp :: [EigenstratSnpEntry]
mockDatEigenstratSnp = [
    EigenstratSnpEntry (Chrom "1") 0   0.000000 "rs0000" 'A' 'C',
    EigenstratSnpEntry (Chrom "11") 10  0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 100 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 110 0.003000 "rs3333" 'C' 'A',
    EigenstratSnpEntry (Chrom "11") 210 0.004000 "rs4444" 'G' 'A',
    EigenstratSnpEntry (Chrom "11") 310 0.005000 "rs5555" 'T' 'A',
    EigenstratSnpEntry (Chrom "14") 400 0.006000 "rs6666" 'G' 'T']

mockFiltered :: [EigenstratSnpEntry]
mockFiltered = [
    EigenstratSnpEntry (Chrom "11") 10  0.001000 "rs1111" 'A' 'G',
    EigenstratSnpEntry (Chrom "11") 100 0.002000 "rs2222" 'A' 'T',
    EigenstratSnpEntry (Chrom "11") 210 0.004000 "rs4444" 'G' 'A']

testFilterThroughBed :: Spec
testFilterThroughBed = describe "filterThroughBed" $ do
    it "should filter correctly" $ do
        let bedProd = readBedFile "testDat/example.bed"
        let eigenstratProd = each mockDatEigenstratSnp
        filtered <- runSafeT $ purely P.fold list (genomicFilterThroughBed bedProd eigenstratProd)
        filtered `shouldBe` mockFiltered
