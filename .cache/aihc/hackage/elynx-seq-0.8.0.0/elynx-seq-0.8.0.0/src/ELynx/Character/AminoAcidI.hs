{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  ELynx.AminoAcid
-- Description :  Amino acid related types and functions
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
-- Amino acid IUPAC code. See also https://www.bioinformatics.org/sms/iupac.html or
-- https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.
--
-- Remarks:
--
-- - Question marks (@?@) are interpreted as unknowns (same as @X@). However, when
--   a sequence is printed/exported, @X@s will be used.
--
-- - Full stops (@.@) are interpreted as gaps (same as @-@). However, when a
--   sequence is printed/exported, @-@s will be used
--
-- @
-- Amino Acid Code:  Three letter Code:  Amino Acid:
-- ----------------  ------------------  -----------
-- A                 Ala                 Alanine
-- C                 Cys                 Cysteine
-- D                 Asp                 Aspartic Acid
-- E                 Glu                 Glutamic Acid
-- F                 Phe                 Phenylalanine
-- G                 Gly                 Glycine
-- H                 His                 Histidine
-- I                 Ile                 Isoleucine
-- K                 Lys                 Lysine
-- L                 Leu                 Leucine
-- M                 Met                 Methionine
-- N                 Asn                 Asparagine
-- P                 Pro                 Proline
-- Q                 Gln                 Glutamine
-- R                 Arg                 Arginine
-- S                 Ser                 Serine
-- T                 Thr                 Threonine
-- V                 Val                 Valine
-- W                 Trp                 Tryptophan
-- Y                 Tyr                 Tyrosine
-- ----------------  ------------------  -----------
-- J                                     Leucine or Isoleucine
-- B                 Asx                 Aspartic acid or Asparagine
-- Z                 Glx                 Glutamine or Glutamic acid
-- ----------------  ------------------  -----------
-- X                 Xaa                 Any amino acid (preferred; used for printing)
-- ?                 Xaa                 Any amino acid
-- ----------------  ------------------  -----------
-- *                 Stp                 No amino acid
-- ----------------  ------------------  -----------
-- -                 Gap                 No amino acid (preferred; used for printing)
-- .                 Gap                 No amino acid
-- @
module ELynx.Character.AminoAcidI
  ( AminoAcidI (..),
  )
where

import Data.ByteString.Internal (c2w, w2c)
import Data.Vector.Unboxed.Deriving
import Data.Word8
import qualified ELynx.Character.Character as C

-- | Amino acids.
data AminoAcidI
  = A
  | C
  | D
  | E
  | F
  | G
  | H
  | I
  | K
  | L
  | M
  | N
  | P
  | Q
  | R
  | S
  | T
  | V
  | W
  | Y
  | J
  | B
  | Z
  | X
  | Stop
  | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWord :: AminoAcidI -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord D = c2w 'D'
toWord E = c2w 'E'
toWord F = c2w 'F'
toWord G = c2w 'G'
toWord H = c2w 'H'
toWord I = c2w 'I'
toWord K = c2w 'K'
toWord L = c2w 'L'
toWord M = c2w 'M'
toWord N = c2w 'N'
toWord P = c2w 'P'
toWord Q = c2w 'Q'
toWord R = c2w 'R'
toWord S = c2w 'S'
toWord T = c2w 'T'
toWord V = c2w 'V'
toWord W = c2w 'W'
toWord Y = c2w 'Y'
toWord J = c2w 'J'
toWord B = c2w 'B'
toWord Z = c2w 'Z'
toWord X = c2w 'X'
toWord Stop = c2w '*'
toWord Gap = c2w '-'

fromWord :: Word8 -> AminoAcidI
fromWord w = case w2c w of
  'A' -> A
  'C' -> C
  'D' -> D
  'E' -> E
  'F' -> F
  'G' -> G
  'H' -> H
  'I' -> I
  'K' -> K
  'L' -> L
  'M' -> M
  'N' -> N
  'P' -> P
  'Q' -> Q
  'R' -> R
  'S' -> S
  'T' -> T
  'V' -> V
  'W' -> W
  'Y' -> Y
  'J' -> J
  'B' -> B
  'Z' -> Z
  'X' -> X
  -- Question marks code for @X@s.
  '?' -> X
  '*' -> Stop
  '-' -> Gap
  -- Full stops code for gaps (@-@s).
  '.' -> Gap
  _ -> error "fromWord: Cannot convert Word8 to AminoAcidI"

derivingUnbox
  "AminoAcidI"
  [t|AminoAcidI -> Word8|]
  [|toWord|]
  [|fromWord|]

instance C.Character AminoAcidI where
  toWord = toWord
  fromWord = fromWord

instance C.CharacterX AminoAcidI where
  gap = Gap

toStandard :: AminoAcidI -> [AminoAcidI]
toStandard A = [A]
toStandard C = [C]
toStandard D = [D]
toStandard E = [E]
toStandard F = [F]
toStandard G = [G]
toStandard H = [H]
toStandard I = [I]
toStandard K = [K]
toStandard L = [L]
toStandard M = [M]
toStandard N = [N]
toStandard P = [P]
toStandard Q = [Q]
toStandard R = [R]
toStandard S = [S]
toStandard T = [T]
toStandard V = [V]
toStandard W = [W]
toStandard Y = [Y]
toStandard J = [L, I]
toStandard B = [D, N]
toStandard Z = [E, Q]
toStandard X = [A, C, D, E, F, G, H, I, K, L, M, N, P, Q, R, S, T, V, W, Y]
toStandard Stop = []
toStandard Gap = []

instance C.CharacterI AminoAcidI where
  unknown = X
  iupac = [J, B, Z, X]
  toStandard = toStandard
