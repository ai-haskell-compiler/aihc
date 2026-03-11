-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Storable.hs
{-# LINE 100 "src/Data/Array/Comfort/Storable.hs" #-}

module DocTest.Data.Array.Comfort.Storable where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 101 "src/Data/Array/Comfort/Storable.hs" #-}
import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Storable (Array, (!))

import     qualified Test.QuickCheck as QC
import     Test.ChasingBottoms.IsBottom (isBottom)

import     Control.Applicative ((<$>))

import     qualified Data.IntSet as IntSet
import     qualified Data.Set as Set
import     Data.Complex (Complex((:+)))
import     Data.Word (Word8, Word16)

type     ShapeInt = Shape.ZeroBased Int
type     X = Shape.Element

shapeInt     :: Int -> ShapeInt
shapeInt     = Shape.ZeroBased

genArray     :: QC.Gen (Array ShapeInt Word16)
genArray     = Array.vectorFromList <$> QC.arbitrary

infix     4 ==?
(==?)     :: a -> a -> (a,a)
(==?)     = (,)

forAllNonEmpty     :: (Eq b) => (Array ShapeInt Word16 -> (b,b)) -> QC.Property
forAllNonEmpty     f =
       QC.forAll genArray $ \xs ->
       case f xs of
          (resultArray,resultList) ->
             if Array.shape xs == Shape.ZeroBased 0
                then isBottom resultArray
                else resultArray == resultList

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Storable:151: "
{-# LINE 151 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 151 "src/Data/Array/Comfort/Storable.hs" #-}
    Array.fromList (shapeInt 5) ['a'..]
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (ZeroBased {zeroBasedSize = 5}) \"abcde\""]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:170: "
{-# LINE 170 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 170 "src/Data/Array/Comfort/Storable.hs" #-}
    Array.fromTuple ('a',('b','c')) :: Array (Shape.NestedTuple Shape.TupleIndex (X,(X,X))) Char
  )
  [ExpectedLine [LineChunk "StorableArray.fromList (NestedTuple {getNestedTuple = (Element 0,(Element 1,Element 2))}) \"abc\""]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:173: "
{-# LINE 173 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 173 "src/Data/Array/Comfort/Storable.hs" #-}
      
   let arr :: Array (Shape.NestedTuple Shape.TupleAccessor (X,(X,X))) Char
       arr = Array.fromTuple ('a',('b','c'))
   in (arr ! fst, arr ! (fst.snd))
  )
  [ExpectedLine [LineChunk "('a','b')"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:196: "
{-# LINE 196 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 196 "src/Data/Array/Comfort/Storable.hs" #-}
      
   let arr = Array.fromRecord ('a' :+ 'b') in
   let (real:+imag) = Shape.indexRecordFromShape $ Array.shape arr in
   (arr ! real, arr ! imag)
  )
  [ExpectedLine [LineChunk "('a','b')"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:338: "
{-# LINE 338 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 338 "src/Data/Array/Comfort/Storable.hs" #-}
    Array.takeSet (Set.fromList [0,2,4,7,13]) (Array.vectorFromList [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3::Word8])
  )
  [ExpectedLine [LineChunk "StorableArray",WildCardChunk,LineChunk " (",WildCardChunk,LineChunk " [0,2,4,7,13]) [3,4,5,6,7]"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:348: "
{-# LINE 348 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.example(
{-# LINE 348 "src/Data/Array/Comfort/Storable.hs" #-}
    Array.takeIntSet (IntSet.fromList [0,2,4,7,13]) (Array.vectorFromList [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3::Word8])
  )
  [ExpectedLine [LineChunk "StorableArray",WildCardChunk,LineChunk " (",WildCardChunk,LineChunk " [0,2,4,7,13]) [3,4,5,6,7]"]]
 DocTest.printPrefix "Data.Array.Comfort.Storable:393: "
{-# LINE 393 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.property(
{-# LINE 393 "src/Data/Array/Comfort/Storable.hs" #-}
      forAllNonEmpty $ \xs -> Array.minimum xs ==? minimum (Array.toList xs)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable:401: "
{-# LINE 401 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.property(
{-# LINE 401 "src/Data/Array/Comfort/Storable.hs" #-}
      forAllNonEmpty $ \xs -> Array.maximum xs ==? maximum (Array.toList xs)
  )
 DocTest.printPrefix "Data.Array.Comfort.Storable:413: "
{-# LINE 413 "src/Data/Array/Comfort/Storable.hs" #-}
 DocTest.property(
{-# LINE 413 "src/Data/Array/Comfort/Storable.hs" #-}
      forAllNonEmpty $ \xs -> Array.limits xs ==? (Array.minimum xs, Array.maximum xs)
  )
