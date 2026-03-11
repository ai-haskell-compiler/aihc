-- |
-- Module      :  Length
-- Description :  Benchmark length newtype
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Nov  3 13:42:00 2020.
module Length
  ( lengthSumFoldl',
    lengthSumFoldl'Unsafe,
    lengthSumFoldl'NumInstance,
    doubleSumFoldl',
    doubleSum,
    fmapNormalFunctor,
    fmapFunctor,
    fmapBifunctor,
    totalBranchLengthFoldable,
    totalBranchLengthBifoldable,
  )
where

import Data.Bifoldable
import Data.Bifunctor
import Data.Foldable
import ELynx.Tree.Length
import ELynx.Tree.Rooted

lengthSumFoldl' :: [Length] -> Length
lengthSumFoldl' =
  foldl'
    ( \x y ->
        either (error . (<>) "lengthSumFoldl'") id $
          toLength $
            fromLength x + fromLength y
    )
    0

lengthSumFoldl'Unsafe :: [Length] -> Length
lengthSumFoldl'Unsafe = foldl' (\x y -> toLengthUnsafe $ fromLength x + fromLength y) 0

lengthSumFoldl'NumInstance :: [Length] -> Length
lengthSumFoldl'NumInstance = foldl' (+) 0

doubleSumFoldl' :: [Double] -> Double
doubleSumFoldl' = foldl' (+) 0

doubleSum :: [Double] -> Double
doubleSum = foldl' (+) 0

fmapNormalFunctor :: HasLength a => Tree e a -> Tree e a
fmapNormalFunctor = fmap (modifyLength cos)

fmapFunctor :: HasLength e => Tree e a -> Tree e a
fmapFunctor = getZipBranchTree . fmap (modifyLength cos) . ZipBranchTree

fmapBifunctor :: HasLength e => Tree e a -> Tree e a
fmapBifunctor = first (modifyLength cos)

totalBranchLengthFoldable :: HasLength e => Tree e a -> Length
totalBranchLengthFoldable = totalBranchLength

totalBranchLengthBifoldable :: HasLength e => Tree e a -> Length
totalBranchLengthBifoldable = bifoldl' (+) const 0 . first getLength
