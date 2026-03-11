-- |
-- Module      :  ELynx.Sequence.Translate
-- Description :  Translate sequences
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri May 17 13:49:18 2019.
module ELynx.Sequence.Translate
  ( translateSeq,
  )
where

import qualified Data.Vector.Unboxed as V
import ELynx.Alphabet.Alphabet
import qualified ELynx.Alphabet.Character as C
import ELynx.Character.Codon
import ELynx.Sequence.Sequence

-- Chop list into chunks of given length. If the last chop is shorter than
-- length, it is dropped.
chopVec :: (V.Unbox a) => Int -> V.Vector a -> [V.Vector a]
chopVec n xs
  | V.length xs < n = []
  | otherwise = V.take n xs : chopVec n (V.drop n xs)

-- | Translate a sequence from 'DNA' or 'DNAX' to 'ProteinS'.
translateSeq :: UniversalCode -> Int -> Sequence -> Sequence
translateSeq uc rf (Sequence n d a cs) = case a of
  DNA -> Sequence n d ProteinS (cs' $ translate uc)
  DNAX -> Sequence n d ProteinS (cs' $ translateX uc)
  DNAI -> Sequence n d ProteinI (cs' $ translateI uc)
  _ -> error "translate: can only translate DNA, DNAX, and DNAI."
  where
    cs' f = C.fromCVec $ translateVecWith f rf (C.toCVec cs)

-- Translate from DNA to Protein with given reading frame (0, 1, 2).
translateVecWith ::
  (V.Unbox a, Ord a, V.Unbox b) =>
  (Codon a -> b) ->
  Int ->
  V.Vector a ->
  V.Vector b
translateVecWith f rf cs
  | rf > 2 = error "translateVecWith: reading frame is larger than 2."
  | rf < 0 = error "translateVecWith: reading frame is negative."
  | otherwise = aas
  where
    codons = map fromVecUnsafe $ chopVec 3 $ V.drop rf cs
    aas = V.fromList $ map f codons
