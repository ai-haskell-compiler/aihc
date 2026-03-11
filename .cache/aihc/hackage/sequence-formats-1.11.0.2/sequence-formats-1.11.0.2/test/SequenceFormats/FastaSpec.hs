{-# LANGUAGE OverloadedStrings #-}
module SequenceFormats.FastaSpec (spec) where

import qualified Data.ByteString.Char8 as BS
import qualified Pipes.Prelude         as P
import           SequenceFormats.Fasta (loadFastaChrom)
import           SequenceFormats.Utils (Chrom (..))
import           System.IO             (IOMode (..), withFile)
import           Test.Hspec

spec :: Spec
spec = do
    testLoadFastaChrom

testLoadFastaChrom :: Spec
testLoadFastaChrom = around (withFile "testDat/example.fasta" ReadMode) $
    describe "loadFastaChrom" $
    do
      fastaSpec "short chrom" "chr1"
        "ACGACGACGACGGGGTTTAAAAAGGGTTTCCTCTCTCTCTGGG"

      fastaSpec "short chrom with junk after it" "chr2"
        "ACCAATTTCCCTTTAATATAAGACCCTTTCGGGGAAA"

      fastaSpec "long chrom" "chr4_random"
        "TTGAGAAAGAAATAATAATTTTATATAATTTAAAGGTTATTTAA"

      fastaSpec "long chrom with junk after it" "chr5_random"
        "AAAACTGAAAAAATGTGGTCACCTATGTTAGAACAACAAGGTTT"
  where
    fastaSpec desc chr testseq = it (fmtDesc desc) $ \h -> do
      fastaProd <- loadFastaChrom h (Chrom chr)
      fastaString <- BS.unpack . BS.concat <$> P.toListM fastaProd
      fastaString `shouldBe` testseq
    fmtDesc s = "should read the correct fasta string from file (" ++ s ++ ")"
