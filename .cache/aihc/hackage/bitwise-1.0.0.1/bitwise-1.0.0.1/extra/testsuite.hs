{-# LANGUAGE TemplateHaskell #-}
module Main(main) where

import Prelude hiding (any, all, and, or, map, zipWith)
import qualified Prelude as P
import qualified Data.List as P

import Data.Ix (inRange, range)
import Data.Function (on)
import Data.Word (Word8, Word16)
import System.Exit (exitSuccess, exitFailure)

import Data.Array.BitArray

import Test.QuickCheck
import Test.QuickCheck.All(quickCheckAll)

fromW :: Word16 -> Int
fromW = fromIntegral

fromW8 :: Word8 -> Int
fromW8 = fromIntegral

prop_bounds1 o w = let n = fromW w in (o, o + n) == bounds (listArray (o, o + n) (take (n + 1) (cycle [False, True, True])))

prop_bounds2 o1 w1 o2 w2 = let n1 = fromW8 w1 ; n2 = fromW8 w2 ; bs = ((o1, o2), (o1 + n1, o2 + n2)) in bs == bounds (listArray bs (take ((n1 + 1) * (n2 + 1)) (cycle [False, True, True])))

prop_index1 o es = let n = length es in n > 0 ==> P.and [es !! i == listArray (o, o + n - 1) es ! (o + i) | i <- [0 .. n - 1]]

prop_index2 o1 o2 es1 = let n2 = ceiling . sqrt . fromIntegral . length $ es1 in n2 > 0 ==>
  let es = init (chunk n2 es1)
      n1 = length es
      bs = ((o1, o2), (o1 + n1 - 1,o2 + n2 - 1))
  in  n1 > 0 ==> P.and [ es !! (i - o1) !! (j - o2) == listArray bs (concat es) ! (i, j) | (i, j) <- range bs ]

prop_indices1 o w = let n = fromW w ; bs = (o, o + n) in range bs == indices (listArray bs (cycle [False, True, True]))

prop_indices2 o1 w1 o2 w2 =
  let n1 = fromW8 w1
      n2 = fromW8 w2
      bs = ((o1, o2), (o1 + n1, o2 + n2))
  in  range bs == indices (listArray bs (cycle [False, True, True]))

prop_elems1 o es = es == (elems . listArray (o, o + length es - 1)) es

prop_assocs1 o es = zip [o..] es == (assocs . listArray (o, o + length es - 1)) es

prop_map1 (Blind f) o es = P.map f es == (elems . map f . listArray (o, o + length es - 1)) es

prop_zipWith1 (Blind f) o ees = P.map (uncurry f) ees == (elems . uncurry (zipWith f `on` listArray (o, o + length ees - 1)) . unzip) ees

prop_or1 o es = P.or es == (or . listArray (o, o + length es - 1)) es

prop_and1 o es = P.and es == (and . listArray (o, o + length es - 1)) es

prop_isUniform1 o es = not (null es) ==> listUniform es == (isUniform . listArray (o, o + length es - 1)) es

prop_fill1 o w b = let n = fromW w in Just b == isUniform (fill (o, o + n) b)

prop_true1 o w = let n = fromW w in Just True == isUniform (true (o, o + n))

prop_false1 o w = let n = fromW w in Just False == isUniform (false (o, o + n))

prop_elemIndex b o es = (fmap (+ o) . P.elemIndex b) es == (elemIndex b . listArray (o, o + length es - 1)) es

prop_popCount o es = (P.length . P.filter id) es == (popCount . listArray (o, o + length es - 1)) es

listUniform l
  | null l = Nothing
  | P.and l = Just True
  | not (P.or l) = Just False
  | otherwise = Nothing

chunk _ [] = []
chunk n xs = let (ys, zs) = splitAt n xs in ys : chunk n zs

{-
  , accumArray
  , (//)
  , accum
  , ixmap
  , (!?)
-}

return []
main :: IO ()
main = do
  r <- $quickCheckAll
  if r then exitSuccess else exitFailure
