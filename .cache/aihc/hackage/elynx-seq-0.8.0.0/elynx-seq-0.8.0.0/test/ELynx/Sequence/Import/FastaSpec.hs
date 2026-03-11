-- |
-- Module      :  ELynx.Sequence.Import.FastaSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 18 09:54:38 2019.
module ELynx.Sequence.Import.FastaSpec
  ( spec,
  )
where

import Data.Either
import ELynx.Alphabet.Alphabet
import qualified ELynx.Sequence.Alignment as M
import ELynx.Sequence.Import.Fasta
import ELynx.Tools.InputOutput
import Test.Hspec

fastaNucleotideFN :: FilePath
fastaNucleotideFN = "data/Nucleotide.fasta"

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = "data/NucleotideIUPAC.fasta"

fastaErroneousFN :: FilePath
fastaErroneousFN = "data/Erroneous.fasta"

fastaAminoAcidFN :: FilePath
fastaAminoAcidFN = "data/AminoAcid.fasta"

spec :: Spec
spec = describe "fastaFileAlignment" $ do
  it "parses a fasta file with nucleotide sequences with equal length" $ do
    a <-
      either error id
        . M.fromSequences
        <$> parseFileWith (fasta DNA) fastaNucleotideFN
    M.nSequences a `shouldBe` 3
    M.length a `shouldBe` 40
  it "parses a fasta file with nucleotide IUPAC sequences with equal length" $
    do
      a <-
        either error id
          . M.fromSequences
          <$> parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
      M.nSequences a `shouldBe` 3
      M.length a `shouldBe` 40
  it "should not parse erroneous files" $ do
    ea <- runParserOnFile (fasta DNAI) fastaErroneousFN
    ea `shouldSatisfy` isLeft
  it "parses a fasta file with amino acid sequences with equal length" $ do
    a <-
      either error id
        . M.fromSequences
        <$> parseFileWith (fasta Protein) fastaAminoAcidFN
    M.nSequences a `shouldBe` 2
    M.length a `shouldBe` 237
  it "should not parse erroneous files" $ do
    a <- runParserOnFile (fasta ProteinI) fastaErroneousFN
    a `shouldSatisfy` isLeft
