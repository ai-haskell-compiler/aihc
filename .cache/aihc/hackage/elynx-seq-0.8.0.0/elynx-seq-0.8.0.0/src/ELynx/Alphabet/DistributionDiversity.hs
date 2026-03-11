{-# LANGUAGE FlexibleContexts #-}

-- |
-- Module      :  ELynx.Alphabet.DistributionDiversity
-- Description :  Summarize statistics for alphabets
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Mon Feb 25 13:32:56 2019.
module ELynx.Alphabet.DistributionDiversity
  ( -- * Entropy
    entropy,
    kEffEntropy,

    -- * Homoplasy
    homoplasy,
    kEffHomoplasy,

    -- * Count characters
    frequencyCharacters,
  )
where

import qualified Data.Set as S
import Data.Vector.Generic
  ( Vector,
    toList,
  )
import qualified Data.Vector.Generic as V
import ELynx.Alphabet.Alphabet
import ELynx.Alphabet.Character

eps :: Double
eps = 1e-12

-- Calculate x*log(x) but set to 0.0 when x is smaller than 'eps'.
xLogX :: Double -> Double
xLogX x
  | x < 0.0 = error "Argument lower than zero."
  | eps > x = 0.0
  | otherwise = x * log x

-- | Entropy of vector.
entropy :: (Vector v Double) => v Double -> Double
entropy v =
  if isNaN res
    then
      error
        ("entropy: Sesult of following vector is NaN: " ++ show (toList v) ++ ".")
    else res
  where
    res = negate $ V.sum $ V.map xLogX v

-- | Effective number of used characters measured using 'entropy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffEntropy :: (Vector v Double) => v Double -> Double
kEffEntropy v = if e < eps then 1.0 else exp e where e = entropy v

-- | Probability of homoplasy of vector. The result is the probability of
-- binomially sampling the same character twice and only makes sense when the
-- sum of the array is 1.0.
homoplasy :: (Vector v Double) => v Double -> Double
homoplasy v = V.sum $ V.map (\x -> x * x) v

-- | Effective number of used characters measured using 'homoplasy'. The result
-- only makes sense when the sum of the array is 1.0.
kEffHomoplasy :: (Vector v Double) => v Double -> Double
kEffHomoplasy v = 1.0 / homoplasy v

-- XXX: Use mutable vector; then V.// is much faster.
-- Increment element at index in vector by one.
incrementElemIndexByOne :: (Vector v Int) => [Int] -> v Int -> v Int
incrementElemIndexByOne is v = v V.// zip is es'
  where
    es' = [v V.! i + 1 | i <- is]

-- For a given code and counts vector, increment the count of the given character.
acc :: (Vector v Int) => AlphabetSpec -> v Int -> Character -> v Int
acc alph vec char = incrementElemIndexByOne is vec
  where
    is = [S.findIndex c (std alph) | c <- toStd alph char]

countCharacters ::
  (Vector v Character, Vector v Int) => AlphabetSpec -> v Character -> v Int
countCharacters alph = V.foldl' (acc alph) zeroCounts
  where
    nChars = length (std alph)
    zeroCounts = V.replicate nChars (0 :: Int)

saveDivision :: Int -> Int -> Double
saveDivision value divisor =
  if divisor == 0 then 0.0 else fromIntegral value / fromIntegral divisor

-- | For a given code vector of characters, calculate frequency of characters.
-- The input vector has arbitrary length (most often the number of sequences in
-- an alignment), the length of the output vector is the number of characters in
-- the alphabet.
frequencyCharacters ::
  (Vector v Character, Vector v Int, Vector v Double) =>
  AlphabetSpec ->
  v Character ->
  v Double
frequencyCharacters alph d = V.map (`saveDivision` s) counts
  where
    counts = countCharacters alph d
    s = V.sum counts
