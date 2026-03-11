-- Do not edit! Automatically created with doctest-extract from src/Data/Array/Comfort/Boxed/Unchecked.hs
{-# LINE 38 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}

module DocTest.Data.Array.Comfort.Boxed.Unchecked where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 39 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
import     qualified Data.Array.Comfort.Boxed as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     Data.Array.Comfort.Boxed (Array, (!))
import     Data.Tuple.HT (swap)
import     Control.Applicative ((<$>))

import     qualified Test.QuickCheck as QC

type     ShapeInt = Shape.ZeroBased Int

genArray     :: QC.Gen (Array ShapeInt Char)
genArray     = Array.vectorFromList <$> QC.arbitrary

newtype     ArrayChar = ArrayChar (Array ShapeInt Char)
       deriving (Show)

instance     QC.Arbitrary ArrayChar where
       arbitrary = fmap ArrayChar genArray


transpose     ::
       (Shape.Indexed sh0, Shape.Indexed sh1) =>
       Array (sh0,sh1) a -> Array (sh1,sh0) a
transpose     a =
       fmap (\(i,j) -> a!(j,i)) $ Array.indices $ swap $ Array.shape a

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Data.Array.Comfort.Boxed.Unchecked:173: "
{-# LINE 173 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 173 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
      \(QC.NonNegative n) (ArrayChar x)  ->  x == Array.mapShape (Shape.ZeroBased . Shape.size) (Array.append (Array.take n x) (Array.drop n x))
  )
 DocTest.printPrefix "Data.Array.Comfort.Boxed.Unchecked:188: "
{-# LINE 188 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 188 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
      \(ArrayChar x) (ArrayChar y) -> let xy = Array.append x y in x == Array.takeLeft xy  &&  y == Array.takeRight xy
  )
 DocTest.printPrefix "Data.Array.Comfort.Boxed.Unchecked:208: "
{-# LINE 208 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
 DocTest.property(
{-# LINE 208 "src/Data/Array/Comfort/Boxed/Unchecked.hs" #-}
      \(ArrayChar x) (ArrayChar y) (ArrayChar z) -> let xyz = Array.append x $ Array.append y z in y == Array.takeCenter xyz
  )
