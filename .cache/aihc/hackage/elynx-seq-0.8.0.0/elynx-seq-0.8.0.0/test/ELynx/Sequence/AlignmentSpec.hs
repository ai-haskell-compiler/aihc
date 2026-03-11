-- |
-- Module      :  ELynx.Sequence.AlignmentSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 14:25:42 2018.
module ELynx.Sequence.AlignmentSpec
  ( spec,
  )
where

import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Matrix.Unboxed as M
import ELynx.Alphabet.Alphabet
import ELynx.Alphabet.Character
import ELynx.Sequence.Alignment
import ELynx.Sequence.Import.Fasta
import ELynx.Tools.InputOutput
import Test.Hspec

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = "data/NucleotideIUPAC.fasta"

ssData :: M.Matrix Character
ssData = M.fromLists $ map (map fromChar) ["AAA", "GAA", "TAA"]

ssA :: Alignment
ssA =
  Alignment
    (map BL.pack ["SEQUENCE_1", "SEQUENCE_2", "SEQUENCE_3"])
    (replicate 3 BL.empty)
    DNAI
    ssData

spec :: Spec
spec = describe "subSample" $
  it "correctly sub sample an Alignment" $ do
    a <-
      either error id
        . fromSequences
        <$> parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
    let ss = subSample [0, 3, 5] a
    ss `shouldBe` ssA
