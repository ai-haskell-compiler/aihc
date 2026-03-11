-- |
-- Module      :  ELynx.Sequence.TranslateSpec
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Oct  5 14:25:42 2018.
module ELynx.Sequence.TranslateSpec
  ( spec,
  )
where

import ELynx.Alphabet.Alphabet
import ELynx.Character.Codon
import ELynx.Sequence.Import.Fasta
import ELynx.Sequence.Translate
import ELynx.Tools.InputOutput
import Test.Hspec

fastaTranslateDNAFN :: FilePath
fastaTranslateDNAFN = "data/TranslateMitochondrialVertebrateDNA.fasta"

fastaTranslateProteinFN :: FilePath
fastaTranslateProteinFN = "data/TranslateMitochondrialVertebrateProtein.fasta"

spec :: Spec
spec =
  describe "translateDNAX" $
    it "correctly translates a test sequence" $ do
      ss <- parseFileWith (fasta DNAX) fastaTranslateDNAFN
      ss' <- parseFileWith (fasta ProteinS) fastaTranslateProteinFN
      map (translateSeq VertebrateMitochondrial 0) ss `shouldBe` ss'
