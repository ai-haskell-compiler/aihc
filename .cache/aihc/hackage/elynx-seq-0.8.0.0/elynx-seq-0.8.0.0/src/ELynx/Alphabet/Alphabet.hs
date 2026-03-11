{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Alphabet.Alphabet
-- Description :  Alphabets store hereditary information
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
--
-- Portability :  portable
--
-- Creation date: Fri May 10 11:10:32 2019.
--
-- Hierarchy:
--
-- 1. 'Character' type.
--
-- 2. Sets of 'Character's form 'Alphabet's; each 'Alphabet' has a specification
-- 'AlphabetSpec'.
--
-- New alphabets have to be added manually to this module.
--
-- This way of handling characters and alphabets IS NOT TYPE SAFE, but much, much
-- faster. A second layer of modules such as 'ELynx.Character.Nucleotide'
-- depend on a 'ELynx.Character.Character.Character' type class. Hence, they
-- provide a type safe way of handling alphabets. Conversion is possible, for
-- instance, with 'ELynx.Alphabet.Character.fromCVec', and
-- 'ELynx.Alphabet.Character.toCVec'.
module ELynx.Alphabet.Alphabet
  ( Alphabet (..),
    AlphabetSpec (..),
    alphabetSpec,
    alphabetDescription,
    isStd,
    isGap,
    isUnknown,
    isIUPAC,
    isMember,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import qualified Data.Set as S
import ELynx.Alphabet.Character
import GHC.Generics (Generic)
import Prelude hiding (all)

-- | Available alphabets; for details see 'alphabetSpec'.
data Alphabet
  = DNA
  | DNAX
  | DNAI
  | Protein
  | ProteinX
  | ProteinS
  | ProteinI
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON Alphabet

instance ToJSON Alphabet

-- | Verbose alphabet name.
alphabetDescription :: Alphabet -> String
alphabetDescription DNA = "DNA (nucleotides)"
alphabetDescription DNAX = "DNAX (nucleotides; including gaps)"
alphabetDescription DNAI = "DNAI (nucleotides; including gaps, and IUPAC codes)"
alphabetDescription Protein = "Protein (amino acids)"
alphabetDescription ProteinX = "ProteinX (amino acids; including gaps)"
alphabetDescription ProteinS = "ProteinS (amino acids; including gaps, and translation stops)"
alphabetDescription ProteinI = "ProteinI (amino acids; including gaps, translation stops, and IUPAC codes)"

-- | Alphabet specification. 'S.Set' is used because it provides fast lookups.
data AlphabetSpec = AlphabetSpec
  { -- | Standard characters.
    std :: !(S.Set Character),
    -- | Gap characters.
    gap :: !(S.Set Character),
    -- | Unknown characters.
    unknown :: !(S.Set Character),
    -- | Other IUPAC codes.
    iupac :: !(S.Set Character),
    -- | All characters in the alphabet.
    all :: !(S.Set Character),
    -- | Convert from IUPAC to the corresponding standard characters.
    toStd :: Character -> [Character]
  }

-- Create alphabet spec.
fromChars ::
  String -> String -> String -> String -> (Char -> String) -> AlphabetSpec
fromChars st ga un iu to =
  AlphabetSpec
    st'
    ga'
    un'
    iu'
    al
    (fromString . to . toChar)
  where
    st' = S.fromList $ fromString st
    ga' = S.fromList $ fromString ga
    un' = S.fromList $ fromString un
    iu' = S.fromList $ fromString iu
    al = S.unions [st', ga', un', iu']

-- | Get the alphabet specification for a given alphabet.
alphabetSpec :: Alphabet -> AlphabetSpec
alphabetSpec DNA = dna
alphabetSpec DNAX = dnaX
alphabetSpec DNAI = dnaI
alphabetSpec Protein = protein
alphabetSpec ProteinX = proteinX
alphabetSpec ProteinS = proteinS
alphabetSpec ProteinI = proteinI

isWith :: (AlphabetSpec -> S.Set Character) -> Alphabet -> Character -> Bool
isWith set alph char = char `S.member` set (alphabetSpec alph)

-- | Test if standard character.
isStd :: Alphabet -> Character -> Bool
isStd = isWith std

-- | Test if gap.
isGap :: Alphabet -> Character -> Bool
isGap = isWith gap

-- | Test if unknown.
isUnknown :: Alphabet -> Character -> Bool
isUnknown = isWith unknown

-- | Test if extended IUPAC character (excluding gaps and unknowns).
isIUPAC :: Alphabet -> Character -> Bool
isIUPAC = isWith iupac

-- | Test if member of alphabet.
isMember :: Alphabet -> Character -> Bool
isMember = isWith all

dna :: AlphabetSpec
dna = fromChars "ACGT" [] [] [] toStdDNA

toStdDNA :: Char -> String
toStdDNA 'A' = "A"
toStdDNA 'C' = "C"
toStdDNA 'G' = "G"
toStdDNA 'T' = "T"
toStdDNA _ = error "tostdDNA: Cannot convert to standard nucleotide."

dnaX :: AlphabetSpec
dnaX = fromChars "ACGT" "-." [] [] toStdDNAX

toStdDNAX :: Char -> String
toStdDNAX 'A' = "A"
toStdDNAX 'C' = "C"
toStdDNAX 'G' = "G"
toStdDNAX 'T' = "T"
toStdDNAX '-' = []
toStdDNAX '.' = []
toStdDNAX _ = error "toStdDNAX: Cannot convert to standard nucleotide."

dnaI :: AlphabetSpec
dnaI = fromChars "ACGT" "-." "N?" "UWSMKRYBDHV" toStdDNAI

toStdDNAI :: Char -> String
toStdDNAI 'A' = "A"
toStdDNAI 'C' = "C"
toStdDNAI 'G' = "G"
toStdDNAI 'T' = "T"
toStdDNAI 'U' = "T"
toStdDNAI 'W' = "AT"
toStdDNAI 'S' = "GC"
toStdDNAI 'M' = "AC"
toStdDNAI 'K' = "GT"
toStdDNAI 'R' = "AG"
toStdDNAI 'Y' = "CT"
toStdDNAI 'B' = "CGT"
toStdDNAI 'D' = "AGT"
toStdDNAI 'H' = "ACT"
toStdDNAI 'V' = "ACG"
toStdDNAI 'N' = "ACGT"
toStdDNAI '?' = "ACGT"
toStdDNAI '-' = []
toStdDNAI '.' = []
toStdDNAI _ = error "toStdDNAI: Cannot convert to standard nucleotide."

protein :: AlphabetSpec
protein = fromChars "ACDEFGHIKLMNPQRSTVWY" [] [] [] toStdP

toStdP :: Char -> String
toStdP 'A' = "A"
toStdP 'C' = "C"
toStdP 'D' = "D"
toStdP 'E' = "E"
toStdP 'F' = "F"
toStdP 'G' = "G"
toStdP 'H' = "H"
toStdP 'I' = "I"
toStdP 'K' = "K"
toStdP 'L' = "L"
toStdP 'M' = "M"
toStdP 'N' = "N"
toStdP 'P' = "P"
toStdP 'Q' = "Q"
toStdP 'R' = "R"
toStdP 'S' = "S"
toStdP 'T' = "T"
toStdP 'V' = "V"
toStdP 'W' = "W"
toStdP 'Y' = "Y"
toStdP _ = error "toStdP: Cannot convert to standard amino acid."

proteinX :: AlphabetSpec
proteinX = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." [] [] toStdPX

toStdPX :: Char -> String
toStdPX 'A' = "A"
toStdPX 'C' = "C"
toStdPX 'D' = "D"
toStdPX 'E' = "E"
toStdPX 'F' = "F"
toStdPX 'G' = "G"
toStdPX 'H' = "H"
toStdPX 'I' = "I"
toStdPX 'K' = "K"
toStdPX 'L' = "L"
toStdPX 'M' = "M"
toStdPX 'N' = "N"
toStdPX 'P' = "P"
toStdPX 'Q' = "Q"
toStdPX 'R' = "R"
toStdPX 'S' = "S"
toStdPX 'T' = "T"
toStdPX 'V' = "V"
toStdPX 'W' = "W"
toStdPX 'Y' = "Y"
toStdPX '-' = ""
toStdPX '.' = ""
toStdPX _ = error "toStdPX: Cannot convert to standard amino acid."

proteinS :: AlphabetSpec
proteinS = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." [] "*" toStdPS

toStdPS :: Char -> String
toStdPS 'A' = "A"
toStdPS 'C' = "C"
toStdPS 'D' = "D"
toStdPS 'E' = "E"
toStdPS 'F' = "F"
toStdPS 'G' = "G"
toStdPS 'H' = "H"
toStdPS 'I' = "I"
toStdPS 'K' = "K"
toStdPS 'L' = "L"
toStdPS 'M' = "M"
toStdPS 'N' = "N"
toStdPS 'P' = "P"
toStdPS 'Q' = "Q"
toStdPS 'R' = "R"
toStdPS 'S' = "S"
toStdPS 'T' = "T"
toStdPS 'V' = "V"
toStdPS 'W' = "W"
toStdPS 'Y' = "Y"
toStdPS '-' = ""
toStdPS '.' = ""
toStdPS '*' = ""
toStdPS _ = error "toStdPS: Cannot convert to standard amino acid."

proteinI :: AlphabetSpec
proteinI = fromChars "ACDEFGHIKLMNPQRSTVWY" "-." "X?" "*JBZ" toStdPI

toStdPI :: Char -> String
toStdPI 'A' = "A"
toStdPI 'C' = "C"
toStdPI 'D' = "D"
toStdPI 'E' = "E"
toStdPI 'F' = "F"
toStdPI 'G' = "G"
toStdPI 'H' = "H"
toStdPI 'I' = "I"
toStdPI 'K' = "K"
toStdPI 'L' = "L"
toStdPI 'M' = "M"
toStdPI 'N' = "N"
toStdPI 'P' = "P"
toStdPI 'Q' = "Q"
toStdPI 'R' = "R"
toStdPI 'S' = "S"
toStdPI 'T' = "T"
toStdPI 'V' = "V"
toStdPI 'W' = "W"
toStdPI 'Y' = "Y"
toStdPI '-' = ""
toStdPI '.' = ""
toStdPI '*' = ""
toStdPI 'J' = "LI"
toStdPI 'B' = "DN"
toStdPI 'Z' = "EQ"
toStdPI 'X' = "ACDEFGHIKLMNPQRSTVWY"
toStdPI '?' = "ACDEFGHIKLMNPQRSTVWY"
toStdPI _ = error "toStdPI: Cannot convert to standard amino acid."
