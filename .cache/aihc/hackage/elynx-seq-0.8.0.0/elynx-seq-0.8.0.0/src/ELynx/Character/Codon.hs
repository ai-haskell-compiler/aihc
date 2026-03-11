{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  ELynx.Character.Codon
-- Description :  Codons are triplets of nucleotides
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu May 16 07:58:50 2019.
--
-- The different universal codes.
-- - https://www.ncbi.nlm.nih.gov/Taxonomy/Utils/wprintgc.cgi?mode=c
-- - http://www.bioinformatics.org/sms2/genetic_code.html
-- - https://en.wikipedia.org/wiki/Genetic_code
module ELynx.Character.Codon
  ( Codon (Codon),
    fromVecUnsafe,
    UniversalCode (..),
    translate,
    translateX,
    translateI,
  )
where

import Data.Aeson
  ( FromJSON,
    ToJSON,
  )
import Data.List
import qualified Data.Map as M
import qualified Data.Vector.Generic as V
import qualified ELynx.Character.AminoAcidI as AI
import ELynx.Character.AminoAcidS
import qualified ELynx.Character.Character as C
import qualified ELynx.Character.Nucleotide as N
import qualified ELynx.Character.NucleotideI as NI
import qualified ELynx.Character.NucleotideX as NX
import GHC.Generics (Generic)

-- | Codons are triplets of characters.
newtype Codon a = Codon (a, a, a)
  deriving (Show, Read, Eq, Ord)

convert :: (C.Character a, C.Character b) => Codon a -> Codon b
convert (Codon (x, y, z)) = Codon (C.convert x, C.convert y, C.convert z)

-- | Unsafe conversion from vector with at least three elements; only the first
-- three elements are used, the rest is discarded.
fromVecUnsafe :: (V.Vector v a) => v a -> Codon a
fromVecUnsafe xs =
  Codon (V.head xs, V.head . V.tail $ xs, V.head . V.tail . V.tail $ xs)

-- | Universal codes.
data UniversalCode = Standard | VertebrateMitochondrial
  deriving (Show, Read, Eq, Ord, Enum, Bounded, Generic)

instance FromJSON UniversalCode

instance ToJSON UniversalCode

-- It is important that the map is lazy, because some keys have errors as values.
mapFromLists :: (Ord a) => [a] -> [a] -> [a] -> [b] -> M.Map (Codon a) b
mapFromLists xs ys zs as =
  M.fromList $ zipWith4 (\f s t a -> (Codon (f, s, t), a)) xs ys zs as

nucs :: (Enum a) => [a]
nucs = map toEnum [3, 1, 0, 2] -- Order T, C, A , G.

-- Permutation of the triplets PLUS GAPS! I avoid 'Z' because I do not want to
-- translate DNAI.
base1, base2, base3 :: (Enum a) => [a]
base1 = [n | n <- nucs, _ <- [0 .. 3 :: Int], _ <- [0 .. 3 :: Int]]
-- base1 = "TTTTTTTTTTTTTTTTCCCCCCCCCCCCCCCCAAAAAAAAAAAAAAAAGGGGGGGGGGGGGGGG" ++ "-."
base2 = [n | _ <- [0 .. 3 :: Int], n <- nucs, _ <- [0 .. 3 :: Int]]
-- base2 = "TTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGGTTTTCCCCAAAAGGGG" ++ "-."
base3 = [n | _ <- [0 .. 3 :: Int], _ <- [0 .. 3 :: Int], n <- nucs]

-- base3 = "TCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAGTCAG" ++ "-."

-- The actual codes.
standard :: [AminoAcidS]
standard =
  [ F,
    F,
    L,
    L,
    S,
    S,
    S,
    S,
    Y,
    Y,
    Stop,
    Stop,
    C,
    C,
    Stop,
    W,
    L,
    L,
    L,
    L,
    P,
    P,
    P,
    P,
    H,
    H,
    Q,
    Q,
    R,
    R,
    R,
    R,
    I,
    I,
    I,
    M,
    T,
    T,
    T,
    T,
    N,
    N,
    K,
    K,
    S,
    S,
    R,
    R,
    V,
    V,
    V,
    V,
    A,
    A,
    A,
    A,
    D,
    D,
    E,
    E,
    G,
    G,
    G,
    G
  ]

-- "FFLLSSSSYY**CC*WLLLLPPPPHHQQRRRRIIIMTTTTNNKKSSRRVVVVAAAADDEEGGGG" ++ "--"

vertebrateMitochondrial :: [AminoAcidS]
vertebrateMitochondrial =
  [ F,
    F,
    L,
    L,
    S,
    S,
    S,
    S,
    Y,
    Y,
    Stop,
    Stop,
    C,
    C,
    W,
    W,
    L,
    L,
    L,
    L,
    P,
    P,
    P,
    P,
    H,
    H,
    Q,
    Q,
    R,
    R,
    R,
    R,
    I,
    I,
    M,
    M,
    T,
    T,
    T,
    T,
    N,
    N,
    K,
    K,
    S,
    S,
    Stop,
    Stop,
    V,
    V,
    V,
    V,
    A,
    A,
    A,
    A,
    D,
    D,
    E,
    E,
    G,
    G,
    G,
    G
  ]

-- "FFLLSSSSYY**CCWWLLLLPPPPHHQQRRRRIIMMTTTTNNKKSS**VVVVAAAADDEEGGGG" ++ "--"

-- | Translate a codon to amino acids including translation stops.
translate :: UniversalCode -> Codon N.Nucleotide -> AminoAcidS
translate code = (M.!) (universalCode code)

-- | Translate a codon to amino acids including translation stops. Translate
-- codons including gaps to amino acid gaps. Be careful, single or two character
-- gaps can lead to a reading frame shift and hence, the translated sequence may
-- be bogus.
translateX :: UniversalCode -> Codon NX.NucleotideX -> AminoAcidS
-- translateX _ (Codon (NX.Gap, NX.Gap, NX.Gap)) = Gap
-- translateX code codon                         = C.convert . translate code . convert $ codon
translateX code codon@(Codon (x, y, z))
  | C.isGap x || C.isGap y || C.isGap z = Gap
  | otherwise = C.convert . translate code . convert $ codon

-- | Translate a codon to amino acids including translation stops. Translate gap
-- triplets to amino acid gaps, and triplets including unknowns to amino acid
-- unknowns. Be careful, also translates other IUPAC characters to amino acid Xs!
translateI :: UniversalCode -> Codon NI.NucleotideI -> AI.AminoAcidI
translateI code codon@(Codon (x, y, z))
  | C.isIUPAC x || C.isIUPAC y || C.isIUPAC z = AI.X
  | otherwise = C.convert . translateX code . convert $ codon

-- translateI :: UniversalCode -> Codon NI.NucleotideI -> AI.AminoAcidI
-- translateI _ (Codon (NI.N, _,    _   )) = AI.X
-- translateI _ (Codon (_   , NI.N, _   )) = AI.X
-- translateI _ (Codon (_,    _,    NI.N)) = AI.X
-- translateI code codon                   = C.convert . translateX code . convert $ codon

-- Map from 'Codon' to amino acid character.
universalCode :: UniversalCode -> M.Map (Codon N.Nucleotide) AminoAcidS
universalCode Standard = mapFromLists base1 base2 base3 standard
universalCode VertebrateMitochondrial =
  mapFromLists base1 base2 base3 vertebrateMitochondrial
