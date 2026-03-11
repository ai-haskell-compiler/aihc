{-# LANGUAGE TypeOperators #-}
module Numeric.BLAS.Slice where

import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.Array.Comfort.Boxed as BoxedArray
import Data.Array.Comfort.Shape ((::+)((::+)))

import qualified Data.Traversable as Trav
import qualified Data.List as List
import Data.Map (Map)
import Data.Bool.HT (if')

import Text.Printf (printf)


{- $setup
>>> :set -XTypeOperators
>>>
>>> import qualified Numeric.BLAS.Slice as Slice
>>> import Test.Slice (shapeInt)
>>>
>>> import qualified Data.Array.Comfort.Boxed as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.Map as Map
>>> import Data.Array.Comfort.Shape ((::+)((::+)))
>>> import Data.Array.Comfort.Boxed ((!))
>>>
>>> import Control.Applicative (liftA2, liftA3, pure)
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> genShape :: QC.Gen (Shape.Range Int)
>>> genShape =
>>>    liftA2
>>>       (\m n -> Shape.Range (min m n) (max m n))
>>>       QC.arbitrary QC.arbitrary
>>>
>>> genAppend :: QC.Gen (Shape.Range Int ::+ Shape.Range Int)
>>> genAppend = liftA2 (::+) genShape genShape
>>>
>>> genSlice :: sh -> QC.Gen (Slice.T sh)
>>> genSlice sh =
>>>    liftA3 Slice.Cons (QC.choose (0,100)) (QC.choose (1,100)) (pure sh)
>>>
>>> genSlice2 :: shA -> shB -> QC.Gen (Slice.T shA, Slice.T shB)
>>> genSlice2 shA shB = do
>>>    s <- QC.choose (0,100)
>>>    k <- QC.choose (1,100)
>>>    return (Slice.Cons s k shA, Slice.Cons s k shB)
>>>
>>> type ShapeInt = Shape.ZeroBased Int
>>>
>>> mapShape :: (shA -> shB) -> Slice.T shA -> Slice.T shB
>>> mapShape f (Slice.Cons s k sh) = Slice.Cons s k (f sh)
>>>
>>> toShapeInt :: (Shape.C sh) => Slice.T sh -> Slice.T ShapeInt
>>> toShapeInt = mapShape (shapeInt . Shape.size)
-}


data T sh = Cons {start, skip :: Int, shape :: sh}
   deriving (Eq, Show)

fromShape :: (Shape.C sh) => sh -> T sh
fromShape = Cons 0 1


row ::
   (Shape.Indexed sh0, Shape.C sh1) => Shape.Index sh0 -> T (sh0,sh1) -> T sh1
row ix0 (Cons s k (sh0,sh1)) =
   Cons (s + k * Shape.offset sh0 ix0 * Shape.size sh1) k sh1

column ::
   (Shape.C sh0, Shape.Indexed sh1) => Shape.Index sh1 -> T (sh0,sh1) -> T sh0
column ix1 (Cons s k (sh0,sh1)) =
   let (size1, offset1) = Shape.sizeOffset sh1
   in Cons (s + k * offset1 ix1) (k * size1) sh0

{- |
prop> QC.forAll (QC.choose (1,100)) $ \numRows -> QC.forAll (QC.choose (0,100)) $ \numColumns -> QC.forAll (genSlice (shapeInt numRows, shapeInt numColumns)) $ \slice -> QC.forAll (QC.elements $ Shape.indices $ shapeInt numRows) $ \ix -> Slice.row ix slice == Slice.rowArray slice ! ix
-}
rowArray ::
   (Shape.Indexed sh0, Shape.C sh1) =>
   T (sh0,sh1) -> BoxedArray.Array sh0 (T sh1)
rowArray (Cons s k (sh0,sh1)) =
   let step = Shape.size sh1 * k
   in BoxedArray.fromList sh0 $
      List.map (\si -> Cons si k sh1) $
      List.take (Shape.size sh0) $ iterate (step+) s

{- |
prop> QC.forAll (QC.choose (0,100)) $ \numRows -> QC.forAll (QC.choose (1,100)) $ \numColumns -> QC.forAll (genSlice (shapeInt numRows, shapeInt numColumns)) $ \slice -> QC.forAll (QC.elements $ Shape.indices $ shapeInt numColumns) $ \ix -> Slice.column ix slice == Slice.columnArray slice ! ix
-}
columnArray ::
   (Shape.C sh0, Shape.Indexed sh1) =>
   T (sh0,sh1) -> BoxedArray.Array sh1 (T sh0)
columnArray (Cons s k (sh0,sh1)) =
   let step = Shape.size sh1 * k
   in BoxedArray.fromList sh1 $
      List.map (\si -> Cons si step sh0) $
      List.take (Shape.size sh1) $ iterate (k+) s


{- |
prop> :{
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
:}
-}
topSubmatrix ::
   (Shape.C sh, Shape.C sh0, Shape.C sh1) =>
   T (sh0::+sh1, sh) -> T (sh0,sh)
topSubmatrix (Cons s k (sh0::+_sh1, sh)) =
   Cons s k (sh0,sh)

bottomSubmatrix ::
   (Shape.C sh, Shape.C sh0, Shape.C sh1) =>
   T (sh0::+sh1, sh) -> T (sh1,sh)
bottomSubmatrix (Cons s k (sh0::+sh1, sh)) =
   Cons (s + k * Shape.size (sh0,sh)) k (sh1,sh)


diagonal :: (Shape.C sh) => T (Shape.Square sh) -> T sh
diagonal (Cons s k (Shape.Square sh)) =
   Cons s (k * (Shape.size sh + 1)) sh

cartesianFromSquare :: T (Shape.Square sh) -> T (sh,sh)
cartesianFromSquare (Cons s k (Shape.Square sh)) = Cons s k (sh,sh)

squareRow ::
   (Shape.Indexed sh) => Shape.Index sh -> T (Shape.Square sh) -> T sh
squareRow ix0 = row ix0 . cartesianFromSquare

squareColumn ::
   (Shape.Indexed sh) => Shape.Index sh -> T (Shape.Square sh) -> T sh
squareColumn ix1 = column ix1 . cartesianFromSquare



plane12 ::
   (Shape.Indexed sh0, Shape.C sh1, Shape.C sh2) =>
   Shape.Index sh0 -> T (sh0,sh1,sh2) -> T (sh1,sh2)
plane12 ix0 (Cons s k (sh0,sh1,sh2)) =
   Cons (s + k * Shape.offset sh0 ix0 * Shape.size (sh1,sh2)) k (sh1,sh2)

plane01 ::
   (Shape.C sh0, Shape.C sh1, Shape.Indexed sh2) =>
   Shape.Index sh2 -> T (sh0,sh1,sh2) -> T (sh0,sh1)
plane01 ix2 (Cons s k (sh0,sh1,sh2)) =
   let (size2, offset2) = Shape.sizeOffset sh2
   in Cons (s + k * offset2 ix2) (k * size2) (sh0,sh1)

column2of3 ::
   (Shape.Indexed sh0, Shape.Indexed sh1, Shape.C sh2) =>
   Shape.Index sh0 -> Shape.Index sh1 -> T (sh0,sh1,sh2) -> T sh2
column2of3 ix0 ix1 = row ix1 . plane12 ix0

column1of3 ::
   (Shape.Indexed sh0, Shape.C sh1, Shape.Indexed sh2) =>
   Shape.Index sh0 -> Shape.Index sh2 -> T (sh0,sh1,sh2) -> T sh1
column1of3 ix0 ix2 = column ix2 . plane12 ix0

column0of3 ::
   (Shape.C sh0, Shape.Indexed sh1, Shape.Indexed sh2) =>
   Shape.Index sh1 -> Shape.Index sh2 -> T (sh0,sh1,sh2) -> T sh0
column0of3 ix1 ix2 = column ix1 . plane01 ix2


left :: (Shape.C sh0, Shape.C sh1) => T (sh0::+sh1) -> T sh0
left (Cons s k (sh0::+_sh1)) = Cons s k sh0

right :: (Shape.C sh0, Shape.C sh1) => T (sh0::+sh1) -> T sh1
right (Cons s k (sh0::+sh1)) = Cons (s + k * Shape.size sh0) k sh1


type ShapeInt = Shape.ZeroBased Int

{- |
prop> QC.forAll (genSlice =<< genAppend) $ \slice -> Slice.take (case Slice.shape slice of (sh::+_) -> Shape.size sh) (toShapeInt slice) == toShapeInt (Slice.left slice)
-}
take :: Int -> T ShapeInt -> T ShapeInt
take m (Cons s k (Shape.ZeroBased n)) =
   if m<0
      then error $ printf "Slice.take: negative size %d" m
      else Cons s k $ Shape.ZeroBased $ min m n

{- |
prop> QC.forAll (genSlice =<< genAppend) $ \slice -> Slice.drop (case Slice.shape slice of (sh::+_) -> Shape.size sh) (toShapeInt slice) == toShapeInt (Slice.right slice)
-}
drop :: Int -> T ShapeInt -> T ShapeInt
drop m (Cons s k (Shape.ZeroBased n)) =
   if m<0
      then error $ printf "Slice.drop: negative size %d" m
      else Cons (s + k * m) k $ Shape.ZeroBased $ max 0 $ n-m

{- |
@Slice.sub start size@

prop> \(QC.NonNegative n) -> QC.forAll (genSlice $ shapeInt n) $ \slice (QC.NonNegative start) (QC.NonNegative size) -> Slice.sub start size slice == Slice.take size (Slice.drop start slice)
-}
sub :: Int -> Int -> T ShapeInt -> T ShapeInt
sub start_ size (Cons s k (Shape.ZeroBased n)) =
   if' (start_<0) (error $ printf "Slice.sub: negative start %d" start_) $
   if' (size  <0) (error $ printf "Slice.sub: negative size %d" size) $
   Cons (s + k * start_) k $ Shape.ZeroBased $ max 0 $ min size $ n-start_


{- |
prop> QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeA -> QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeB -> QC.forAll (fmap shapeInt $ QC.choose (0,100)) $ \shapeC -> QC.forAll (genSlice2 (Map.fromList $ ('a', shapeA) : ('b', shapeB) : ('c', shapeC) : []) (shapeA ::+ shapeB ::+ shapeC)) $ \(sliceMap, sliceParted) -> Slice.map sliceMap Map.! 'b' == Slice.left (Slice.right sliceParted)

prop> QC.forAll (QC.choose (0,100)) $ \numRows -> QC.forAll (QC.choose (0,100)) $ \numColumns -> let rowShape = shapeInt numRows; columnShape = shapeInt numColumns; mapShape_ = Map.fromList $ map (\k -> (k, columnShape)) (Shape.indices rowShape) in QC.forAll (genSlice2 mapShape_ (rowShape, columnShape)) $ \(sliceMap, sliceMatrix) -> Map.toAscList (Slice.map sliceMap) == Array.toAssociations (Slice.rowArray sliceMatrix)
-}
map :: (Shape.C sh) => T (Map k sh) -> Map k (T sh)
map (Cons s k m) =
   snd $
   Trav.mapAccumL
      (\offset sh -> (offset + Shape.size sh * k, Cons offset k sh)) s m
