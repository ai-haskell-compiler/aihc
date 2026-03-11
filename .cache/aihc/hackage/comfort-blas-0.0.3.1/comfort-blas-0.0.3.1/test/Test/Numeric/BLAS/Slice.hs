-- Do not edit! Automatically created with doctest-extract from src/Numeric/BLAS/Slice.hs
{-# LINE 16 "src/Numeric/BLAS/Slice.hs" #-}

{-# OPTIONS_GHC -XTypeOperators #-}

module Test.Numeric.BLAS.Slice where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 19 "src/Numeric/BLAS/Slice.hs" #-}
import     qualified Numeric.BLAS.Slice as Slice
import     Test.Slice (shapeInt)

import     qualified Data.Array.Comfort.Boxed as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.Map as Map
import     Data.Array.Comfort.Shape ((::+)((::+)))
import     Data.Array.Comfort.Boxed ((!))

import     Control.Applicative (liftA2, liftA3, pure)

import     qualified Test.QuickCheck as QC

genShape     :: QC.Gen (Shape.Range Int)
genShape     =
       liftA2
          (\m n -> Shape.Range (min m n) (max m n))
          QC.arbitrary QC.arbitrary

genAppend     :: QC.Gen (Shape.Range Int ::+ Shape.Range Int)
genAppend     = liftA2 (::+) genShape genShape

genSlice     :: sh -> QC.Gen (Slice.T sh)
genSlice     sh =
       liftA3 Slice.Cons (QC.choose (0,100)) (QC.choose (1,100)) (pure sh)

genSlice2     :: shA -> shB -> QC.Gen (Slice.T shA, Slice.T shB)
genSlice2     shA shB = do
       s <- QC.choose (0,100)
       k <- QC.choose (1,100)
       return (Slice.Cons s k shA, Slice.Cons s k shB)

type     ShapeInt = Shape.ZeroBased Int

mapShape     :: (shA -> shB) -> Slice.T shA -> Slice.T shB
mapShape     f (Slice.Cons s k sh) = Slice.Cons s k (f sh)

toShapeInt     :: (Shape.C sh) => Slice.T sh -> Slice.T ShapeInt
toShapeInt     = mapShape (shapeInt . Shape.size)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.BLAS.Slice:80: "
{-# LINE 80 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 80 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (QC.choose (1,100)) $ \numRows -> QC.forAll (QC.choose (0,100)) $ \numColumns -> QC.forAll (genSlice (shapeInt numRows, shapeInt numColumns)) $ \slice -> QC.forAll (QC.elements $ Shape.indices $ shapeInt numRows) $ \ix -> Slice.row ix slice == Slice.rowArray slice ! ix
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:92: "
{-# LINE 92 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 92 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (QC.choose (0,100)) $ \numRows -> QC.forAll (QC.choose (1,100)) $ \numColumns -> QC.forAll (genSlice (shapeInt numRows, shapeInt numColumns)) $ \slice -> QC.forAll (QC.elements $ Shape.indices $ shapeInt numColumns) $ \ix -> Slice.column ix slice == Slice.columnArray slice ! ix
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:105: "
{-# LINE 105 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 105 "src/Numeric/BLAS/Slice.hs" #-}
        
   QC.forAll (QC.choose (1,100)) $ \numTopRows ->
   QC.forAll (QC.choose (1,100)) $ \numBottomRows ->
   QC.forAll (QC.choose (0,100)) $ \numColumns ->
   QC.forAll (genSlice (shapeInt numTopRows ::+ shapeInt numBottomRows,
                        shapeInt numColumns)) $ \slice ->
   Slice.rowArray slice
   ==
   Array.append
      (Slice.rowArray (Slice.topSubmatrix slice))
      (Slice.rowArray (Slice.bottomSubmatrix slice))
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:187: "
{-# LINE 187 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 187 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (genSlice =<< genAppend) $ \slice -> Slice.take (case Slice.shape slice of (sh::+_) -> Shape.size sh) (toShapeInt slice) == toShapeInt (Slice.left slice)
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:196: "
{-# LINE 196 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 196 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (genSlice =<< genAppend) $ \slice -> Slice.drop (case Slice.shape slice of (sh::+_) -> Shape.size sh) (toShapeInt slice) == toShapeInt (Slice.right slice)
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:207: "
{-# LINE 207 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 207 "src/Numeric/BLAS/Slice.hs" #-}
      \(QC.NonNegative n) -> QC.forAll (genSlice $ shapeInt n) $ \slice (QC.NonNegative start) (QC.NonNegative size) -> Slice.sub start size slice == Slice.take size (Slice.drop start slice)
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:217: "
{-# LINE 217 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 217 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeA -> QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeB -> QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeC -> QC.forAll (genSlice2 (Map.fromList $ ('a', shapeA) : ('b', shapeB) : ('c', shapeC) : []) (shapeA ::+ shapeB ::+ shapeC)) $ \(sliceMap, sliceParted) -> Slice.map sliceMap Map.! 'b' == Slice.left (Slice.right sliceParted)
  )
 DocTest.printPrefix "Numeric.BLAS.Slice:219: "
{-# LINE 219 "src/Numeric/BLAS/Slice.hs" #-}
 DocTest.property(
{-# LINE 219 "src/Numeric/BLAS/Slice.hs" #-}
      QC.forAll (QC.choose (0,100)) $ \numRows -> QC.forAll (QC.choose (0,100)) $ \numColumns -> let rowShape = shapeInt numRows; columnShape = shapeInt numColumns; mapShape_ = Map.fromList $ map (\k -> (k, columnShape)) (Shape.indices rowShape) in QC.forAll (genSlice2 mapShape_ (rowShape, columnShape)) $ \(sliceMap, sliceMatrix) -> Map.toAscList (Slice.map sliceMap) == Array.toAssociations (Slice.rowArray sliceMatrix)
  )
