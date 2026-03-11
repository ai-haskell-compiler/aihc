{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.UtilsSpec (spec) where

import           SequenceFormats.Utils (Chrom (..), SeqFormatException (..))

import           Control.Exception     (evaluate)
import           Test.Hspec

spec :: Spec
spec = testChrom

testChrom :: Spec
testChrom = describe "Chrom" $ do
    specify "2 should be smaller than 10" $
        Chrom "2" < Chrom "10" `shouldBe` True
    specify "chr2 should be smaller than 10" $
        Chrom "chr2" < Chrom "10" `shouldBe` True
    specify "2 should be smaller than chr10" $
        Chrom "2" < Chrom "chr10" `shouldBe` True
    specify "chr2 should be smaller than chr10" $
        Chrom "chr2" < Chrom "chr10" `shouldBe` True
    specify "chr22 should be smaller than chrX" $
        Chrom "chr22" < Chrom "chrX" `shouldBe` True
    specify "chrX should be smaller than chrY" $
        Chrom "chrX" < Chrom "chrY" `shouldBe` True
    specify "chrY should be smaller than chrMT" $
        Chrom "chrY" < Chrom "chrMT" `shouldBe` True
    specify "22 should be smaller than chrMT" $
        Chrom "22" < Chrom "chrMT" `shouldBe` True
    specify "X should be smaller than chrMT" $
        Chrom "X" < Chrom "chrMT" `shouldBe` True
    specify "chrSSS should throw" $
        evaluate (Chrom "chrSSS" < Chrom "chrMT") `shouldThrow` (==SeqFormatException "cannot parse chromosome SSS")


