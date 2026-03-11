-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Basic
-- Copyright   :  (c) Matthew Donadio 1998
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic functions for manipulating signals
--
-----------------------------------------------------------------------------

module DSP.Basic where

import DSP.Source.Basic (zeros)

import Data.Array (Array, Ix, listArray, elems)

-- * Functions

-- | 'linspace' generates a list of values linearly spaced between specified
-- start and end values (array will include both start and end values).
--
-- @linspace 0.0 1.0 5 == [ 0.0, 0.25, 0.5, 0.75 1.0 ]@

linspace :: Double -> Double -> Int -> [Double]
linspace a b n =
   map (\ i -> a + (fromIntegral i) * inc) [(0::Int) .. (n - 1)]
  where
   inc = (b - a) / (fromIntegral (n - 1))


-- | 'logspace' generates a list of values logarithmically spaced between the
-- values 10 ** start and 10 ** end (array will include both start and end values).
--
-- @logspace 0.0 1.0 4 == [ 1.0, 2.1544, 4.6416, 10.0 ]@

logspace :: Double -> Double -> Int -> [Double]
logspace a b n =
   map (\ x -> 10.0 ** x) $ linspace a b n


-- | 'delay' is the unit delay function, eg,
--
-- @delay1 [ 1, 2, 3 ] == [ 0, 1, 2, 3 ]@

delay1 :: (Num a) => [a] -> [a]
delay1 a = 0 : a

-- | 'delay' is the n sample delay function, eg,
--
-- @delay 3 [ 1, 2, 3 ] == [ 0, 0, 0, 1, 2, 3 ]@

delay :: (Num a) => Int -> [a] -> [a]
delay n a = replicate n 0 ++ a

-- | @downsample@ throws away every n'th sample, eg,
--
-- @downsample 2 [ 1, 2, 3, 4, 5, 6 ] == [ 1, 3, 5 ]@

downsample :: Int -> [a] -> [a]
downsample n =
   map head . takeWhile (not . null) . iterate (drop n)

downsampleRec :: Int -> [a] -> [a]
downsampleRec _ []     = []
downsampleRec n (x:xs) = x : downsample n (drop (n - 1) xs)

-- | @upsample@ inserts n-1 zeros between each sample, eg,
--
-- @upsample 2 [ 1, 2, 3 ] == [ 1, 0, 2, 0, 3, 0 ]@

upsample :: (Num a) => Int -> [a] -> [a]
upsample n = concatMap (: replicate (n-1) 0)

upsampleRec :: (Num a) => Int -> [a] -> [a]
upsampleRec _ []     = []
upsampleRec n (x:xs) = x : zero n xs
    where zero 1 ys = upsample n ys
	  zero i ys = 0 : zero (i-1) ys

-- | @upsampleAndHold@ replicates each sample n times, eg,
--
-- @upsampleAndHold 3 [ 1, 2, 3 ] == [ 1, 1, 1, 2, 2, 2, 3, 3, 3 ]@

upsampleAndHold :: Int -> [a] -> [a]
upsampleAndHold n = concatMap (replicate n)


-- | merges elements from two lists into one list in an alternating way
--
-- @interleave [0,1,2,3] [10,11,12,13] == [0,10,1,11,2,12,3,13]@

interleave :: [a] -> [a] -> [a]
interleave (e:es) (o:os) = e : o : interleave es os
interleave _      _      = []

-- | split a list into two lists in an alternating way
--
-- @uninterleave [1,2,3,4,5,6] == ([1,3,5],[2,4,6])@
--
-- It's a special case of 'Numeric.Random.Spectrum.Pink.split'.

uninterleave :: [a] -> ([a],[a])
uninterleave = foldr (\x ~(xs,ys) -> (x:ys,xs)) ([],[])


-- | pad a sequence with zeros to length n
--
-- @pad [ 1, 2, 3 ] 6 == [ 1, 2, 3, 0, 0, 0 ]@

pad :: (Ix a, Integral a, Num b) => Array a b -> a -> Array a b
pad x n = listArray (0,n-1) $ elems x ++ zeros


-- | generates a 'Just' if the given condition holds

toMaybe :: Bool -> a -> Maybe a
toMaybe False _ = Nothing
toMaybe True  x = Just x

-- | Computes the square of the Euclidean norm of a 2D point

norm2sqr :: Num a => (a,a) -> a
norm2sqr (x,y) = x^!2 + y^!2

-- | Power with fixed exponent type.
-- This eliminates warnings about using default types.

infixr 8 ^!

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
