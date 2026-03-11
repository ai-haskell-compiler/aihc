-- |
-- Module      :  ELynx.Sequence.SequenceSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 14:25:42 2018.
module ELynx.Sequence.SequenceSpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import ELynx.Alphabet.Alphabet
import ELynx.Sequence.Import.Fasta
import ELynx.Sequence.Sequence
import ELynx.Tools.InputOutput
import Test.Hspec

fastaDifferentLengthFN :: FilePath
fastaDifferentLengthFN = "data/NucleotideDifferentLength.fasta"

fastaDifferentLengthTrimmedFN :: FilePath
fastaDifferentLengthTrimmedFN = "data/NucleotideDifferentLengthTrimmed.fasta"

longestSequenceInFileBS :: BL.ByteString
longestSequenceInFileBS =
  BL.unlines $
    map BL.pack [">SEQUENCE_3", "ATTTAAAAAAACCCAAAACCCGGGCCCCGGGTTTTTTTA"]

longestSequenceInFile :: Sequence
longestSequenceInFile = parseByteStringWith (fastaSequence DNA) longestSequenceInFileBS

spec :: Spec
spec = do
  describe "longest" $
    it "finds the longest sequence" $ do
      ss <- parseFileWith (fasta DNA) fastaDifferentLengthFN
      longest ss `shouldBe` longestSequenceInFile
  describe "filterLongerThan" $
    it "filters sequences that are longer than a specified length" $
      do
        ss <- parseFileWith (fasta DNA) fastaDifferentLengthFN
        ss' <- parseFileWith (fasta DNA) fastaDifferentLengthTrimmedFN
        filterLongerThan 10 ss `shouldBe` ss'
