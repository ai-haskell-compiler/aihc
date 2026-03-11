{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.PileupSpec (spec) where

import           SequenceFormats.Pileup (PileupRow (..), Strand (..),
                                         readPileupFromFile)
import           SequenceFormats.Utils  (Chrom (..))

import           Control.Foldl          (list, purely)
import qualified Pipes.Prelude          as P
import           Pipes.Safe             (runSafeT)
import           Test.Hspec

spec :: Spec
spec = testReadPileupFromFile

testReadPileupFromFile :: Spec
testReadPileupFromFile = describe "readPileupFromFile" $
    it "should read correct data from file" $ do
        let pProd = readPileupFromFile "testDat/example.pileup"
        runSafeT (purely P.fold list pProd) `shouldReturn` mockDatPentries

mockDatPentries :: [PileupRow]
mockDatPentries = [
    PileupRow (Chrom "1") 1000 'A' ["AAACA", "AAAC", "AAAACCAACA"]
        [[f, f, r, f, f], [f, f, r, r], [r, r, f, r, r, r, f, f, r, f]],
    PileupRow (Chrom "1") 2000 'C' ["CCCA", "ACTCC", "CACACCCC"]
        [[f, f, f, r], [f, f, r, f, f], [f, r, f, r, f, f, f, f]],
    PileupRow (Chrom "2") 1000 'G' ["GGGGGGGCG", "GGG", "GGGGGG"]
        [[r, r, f, r, r, f, f, f, f], [f, r, r], [f, r, r, f, r, r]]]
  where
    f = ForwardStrand
    r = ReverseStrand
