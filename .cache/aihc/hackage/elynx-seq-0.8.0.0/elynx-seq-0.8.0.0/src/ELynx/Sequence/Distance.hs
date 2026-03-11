-- |
-- Module      :  ELynx.Sequence.Distance
-- Description :  Distance functions between sequences
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Fri Aug 21 15:09:58 2020.
module ELynx.Sequence.Distance
  ( hamming,
  )
where

import qualified Data.Vector.Unboxed as V
import ELynx.Sequence.Sequence

countFalses :: (Int -> Bool -> Int)
countFalses n False = succ n
countFalses n True = n

-- | Compute hamming distance between two sequences.
hamming :: Sequence -> Sequence -> Either String Int
hamming l r
  | alphabet l /= alphabet r = Left "hamming: Alphabets of sequences differ."
  | V.length csL /= V.length csR = Left "hamming: Sequence lengths differ."
  | V.null csL || V.null csR = Left "hamming: Empty sequence encountered."
  | otherwise = Right $ V.foldl' countFalses 0 $ V.zipWith (==) (characters l) (characters r)
  where
    csL = characters l
    csR = characters r
