{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      :  ELynx.NucleotideX
-- Description :  Extended nucleotides including gaps and unknowns
-- Copyright   :  2021 Dominik Schrempf
--
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- See header of 'ELynx.Alphabet'.
--
-- Extended nucleotides with gaps. See also
-- https://www.bioinformatics.org/sms/iupac.html or
-- https://en.wikipedia.org/wiki/International_Union_of_Pure_and_Applied_Chemistry.
--
-- @
-- Symbol  Description  Bases represented  Complement
-- ------  -----------  -----------------  ----------
-- A       Adenine      A                  T
-- C       Cytosine        C               G
-- G       Guanine            G            C
-- T       Thymine               T         A
-- ------  -----------  -----------------  ----------
-- - or .  Gap (Zero)                      -
-- @
module ELynx.Character.NucleotideX
  ( NucleotideX (..),
  )
where

import Data.ByteString.Internal (c2w, w2c)
import Data.Vector.Unboxed.Deriving
import Data.Word8
import qualified ELynx.Character.Character as C

-- | Extended nucleotides.
data NucleotideX
  = A
  | C
  | G
  | T
  | Gap
  deriving (Show, Read, Eq, Ord, Enum, Bounded)

toWord :: NucleotideX -> Word8
toWord A = c2w 'A'
toWord C = c2w 'C'
toWord G = c2w 'G'
toWord T = c2w 'T'
toWord Gap = c2w '-'

fromWord :: Word8 -> NucleotideX
fromWord w = case w2c w of
  'A' -> A
  'C' -> C
  'G' -> G
  'T' -> T
  '-' -> Gap
  '.' -> Gap
  c -> error $ "fromWord: Cannot convert " ++ show c ++ " to NucleotideX."

derivingUnbox
  "NucleotideX"
  [t|NucleotideX -> Word8|]
  [|toWord|]
  [|fromWord|]

instance C.Character NucleotideX where
  toWord = toWord
  fromWord = fromWord

instance C.CharacterX NucleotideX where
  gap = Gap
