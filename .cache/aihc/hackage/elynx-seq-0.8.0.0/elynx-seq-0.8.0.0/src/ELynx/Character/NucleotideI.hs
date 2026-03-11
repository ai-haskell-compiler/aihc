{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  ELynx.NucleotideI
-- Description :  Nucleotides with IUPAC characters
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Oct  4 18:26:35 2018.
--
-- See header of 'ELynx.Alphabet.Alphabet'.
--
-- Nucleotide IUPAC code. See also https://www.bioinformatics.org/sms/iupac.html or
-- https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.
--
-- Remarks:
--
-- - Question marks (@?@) are interpreted as unknowns (same as @N@). However, when
--   a sequence is printed/exported, @N@s will be used.
--
-- - Full stops (@.@) are interpreted as gaps (same as @-@). However, when a
--   sequence is printed/exported, @-@s will be used
--
-- @
-- Symbol  Description  Bases represented  Complement
-- ------  -----------  -----------------  ----------
-- A       Adenine      A                  T
-- C       Cytosine        C               G
-- G       Guanine            G            C
-- T       Thymine               T         A
-- ------  -----------  -----------------  ----------
-- U       Uracil                U         A
-- W       Weak         A        T         W
-- S       Strong          C  G            S
-- M       aMino        A  C               K
-- K       Keto               G  T         M
-- R       puRine       A     G            Y
-- Y       pYrimidine      C     T         R
-- B       not A           C  G  T         V
-- D       not C        A     G  T         H
-- H       not G        A  C     T         D
-- V       not T        A  C  G            B
-- ------  -----------  -----------------  ----------
-- N       any          A  C  G  T         N           (preferred)
-- ?       any          A  C  G  T         N
-- ------  -----------  -----------------  ----------
-- -       Gap (Zero)                      -           (preferred)
-- .       Gap (Zero)                      -
-- @
module ELynx.Character.NucleotideI
  ( NucleotideI (..),
  )
where

import Data.ByteString.Internal (c2w, w2c)
import Data.Vector.Unboxed.Deriving
import Data.Word8
import qualified ELynx.Character.Character as C

-- | NucleotideIs.
data NucleotideI
  = A
  | C
  | G
  | T
  | U
  | W
  | S
  | M
  | K
  | R
  | Y
  | B
  | D
  | H
  | V
  | N
  | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- See https://stackoverflow.com/a/31527024; apparently, pattern matching (and
-- case statements) are fast because they are compiled to lookup tables. Hence,
-- they are faster than guards (because equality has to be checked), and faster
-- than lookups with sets.
toWord :: NucleotideI -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord G = c2w 'G'
toWord T = c2w 'T'
toWord U = c2w 'U'
toWord W = c2w 'W'
toWord S = c2w 'S'
toWord M = c2w 'M'
toWord K = c2w 'K'
toWord R = c2w 'R'
toWord Y = c2w 'Y'
toWord B = c2w 'B'
toWord D = c2w 'D'
toWord H = c2w 'H'
toWord V = c2w 'V'
toWord N = c2w 'N'
toWord Gap = c2w '-'

fromWord :: Word8 -> NucleotideI
fromWord w = case w2c w of
  'A' -> A
  'C' -> C
  'G' -> G
  'T' -> T
  'U' -> U
  'W' -> W
  'S' -> S
  'M' -> M
  'K' -> K
  'R' -> R
  'Y' -> Y
  'B' -> B
  'D' -> D
  'H' -> H
  'V' -> V
  'N' -> N
  '?' -> N
  '-' -> Gap
  '.' -> Gap
  _ -> error "fromWord: Cannot convert to NucleotideI."

derivingUnbox
  "NucleotideI"
  [t|NucleotideI -> Word8|]
  [|toWord|]
  [|fromWord|]

instance C.Character NucleotideI where
  toWord = toWord
  fromWord = fromWord

toStandard :: NucleotideI -> [NucleotideI]
toStandard A = [A]
toStandard C = [C]
toStandard G = [G]
toStandard T = [T]
toStandard U = [T]
toStandard W = [A, T]
toStandard S = [G, C]
toStandard M = [A, C]
toStandard K = [G, T]
toStandard R = [A, G]
toStandard Y = [C, T]
toStandard B = [C, G, T]
toStandard D = [A, G, T]
toStandard H = [A, C, T]
toStandard V = [A, C, G]
toStandard N = [A, C, G, T]
toStandard Gap = []

instance C.CharacterX NucleotideI where
  gap = Gap

instance C.CharacterI NucleotideI where
  unknown = N
  iupac = [U, W, S, M, K, R, Y, B, D, H, V, N]
  toStandard = toStandard
