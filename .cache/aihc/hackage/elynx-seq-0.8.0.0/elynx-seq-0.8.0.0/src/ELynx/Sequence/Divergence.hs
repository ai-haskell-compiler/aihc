{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE KindSignatures #-}

-- |
-- Module      :  ELynx.Sequence.Divergence
-- Description :  Compute divergence matrix between sequences
-- Copyright   :  2022 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Thu Jul  7 17:55:23 2022.
module ELynx.Sequence.Divergence
  ( divergence,
  )
where

import Control.Monad.Primitive
import qualified Data.Matrix.Unboxed as MU
import qualified Data.Matrix.Unboxed.Mutable as MU
import Data.Maybe
import qualified Data.Set as S
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import ELynx.Alphabet.Alphabet
import ELynx.Sequence.Sequence
import GHC.Exts (Constraint)

type Context x = (VUM.Unbox x :: Constraint)

modify :: (Context a, PrimMonad s) => MU.MMatrix (PrimState s) a -> (Int, Int) -> (a -> a) -> s ()
modify m ij f = do
  x <- MU.read m ij
  MU.write m ij (f x)

divergence :: Sequence -> Sequence -> Either String (MU.Matrix Int)
divergence s1 s2
  | alphabet s1 /= alphabet s2 = err "sequences have different alphabets"
  | not (equalLength [s1, s2]) = err "sequences have different lengths"
  | otherwise = Right $ MU.create $ do
      m <- MU.new (n, n)
      -- Initialize matrix.
      sequence_ [MU.write m (i, j) 0 | i <- [0 .. (n - 1)], j <- [0 .. (n - 1)]]
      -- Fill matrix.
      sequence_ $
        catMaybes
          [ do
              -- Only treat sites where both characters are standard.
              i <- mi
              j <- mj
              Just $ modify m (i, j) (+ 1)
            | (x, y) <- zip (VU.toList cs1) (VU.toList cs2),
              let mi = S.lookupIndex x a,
              let mj = S.lookupIndex y a
          ]
      return m
  where
    err m = Left $ "divergence: " <> m
    a = std $ alphabetSpec $ alphabet s1
    n = S.size a
    cs1 = characters s1
    cs2 = characters s2
