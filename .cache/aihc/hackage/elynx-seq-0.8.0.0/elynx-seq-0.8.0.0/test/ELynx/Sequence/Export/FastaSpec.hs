-- |
-- Module      :  ELynx.Sequence.Export.FastaSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Jan 18 09:59:57 2019.
module ELynx.Sequence.Export.FastaSpec
  ( spec,
  )
where

import ELynx.Alphabet.Alphabet
import ELynx.Sequence.Export.Fasta
import ELynx.Sequence.Import.Fasta
import ELynx.Tools.InputOutput
import Test.Hspec

fastaNucleotideIUPACFN :: FilePath
fastaNucleotideIUPACFN = "data/NucleotideIUPAC.fasta"

spec :: Spec
spec =
  describe "sequencesToFasta"
    $ it
      "should create a fasta bytestring that, when parsed again, is the original sequence"
    $ do
      ss <- parseFileWith (fasta DNAI) fastaNucleotideIUPACFN
      let f = sequencesToFasta ss
          ss' = parseByteStringWith (fasta DNAI) f
      ss `shouldBe` ss'
