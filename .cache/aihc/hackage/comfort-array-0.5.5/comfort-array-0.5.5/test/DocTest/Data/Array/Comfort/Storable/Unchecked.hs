-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Storable/Unchecked.hs
{-# LINE 64 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}

module DocTest.Data.Array.Comfort.Storable.Unchecked where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 65 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
import     DocTest.Data.Array.Comfort.Storable (ShapeInt, genArray)

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Storable (Array, (!))

import     qualified Test.QuickCheck as QC

import     Data.Word (Word16)

newtype     Array16 = Array16 (Array ShapeInt Word16)
       deriving (Show)

instance     QC.Arbitrary Array16 where
       arbitrary = fmap Array16 genArray

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:166: "
{-# LINE 166 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 166 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \x  ->  Array.singleton x ! () == (x::Word16)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:180: "
{-# LINE 180 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 180 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \(QC.NonNegative n) (Array16 x)  ->  x == Array.mapShape (Shape.ZeroBased . Shape.size) (Array.append (Array.take n x) (Array.drop n x))
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:195: "
{-# LINE 195 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 195 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \(Array16 x) (Array16 y) -> let xy = Array.append x y in x == Array.takeLeft xy  &&  y == Array.takeRight xy
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:215: "
{-# LINE 215 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 215 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \(Array16 x) (Array16 y) (Array16 z) -> let xyz = Array.append x $ Array.append y z in y == Array.takeCenter xyz
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:228: "
{-# LINE 228 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 228 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \(Array16 xs)  ->  Array.sum xs == sum (Array.toList xs)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable.Unchecked:234: "
{-# LINE 234 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 234 "src/Data/Array/Comfort/Storable/Unchecked.hs" #-}
      \(Array16 xs)  ->  Array.product xs == product (Array.toList xs)
  )
