{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Comfort.Storable (
   Array,
   shape,
   reshape,
   mapShape,

   accessMaybe, (!),
   Array.toList,
   Array.vectorFromList,
   toAssociations,
   fromList,
   fromMap, toMap,
   fromIntMap, toIntMap,
   fromTuple, toTuple,
   fromRecord, toRecord,
   fromContainer,
   toContainer,
   sample,
   replicate,
   fromBoxed,
   toBoxed,
   Array.fromStorableVector,
   Array.toStorableVector,
   fromBlockArray1,
   fromBlockArray2,
   fromNonEmptyBlockArray2,

   Array.map,
   Array.mapWithIndex,
   zipWith,
   (//),
   accumulate,
   fromAssociations,

   pick,
   toRowArray,
   fromRowArray,
   Array.singleton,
   Array.append,
   Array.take, Array.drop,
   Array.takeLeft, Array.takeRight, Array.split,
   Array.takeCenter,
   takeSet,
   takeIntSet,

   Array.sum, Array.product,
   minimum, argMinimum,
   maximum, argMaximum,
   limits,
   Array.foldl,
   foldl1,
   foldMap,
   ) where

import qualified Data.Array.Comfort.Storable.Mutable.Unchecked as MutArrayNC
import qualified Data.Array.Comfort.Storable.Mutable.Private as MutArrayPriv
import qualified Data.Array.Comfort.Storable.Mutable as MutArray
import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Storable.Dim2 as Array2
import qualified Data.Array.Comfort.Storable.Memory as Memory
import qualified Data.Array.Comfort.Container as Container
import qualified Data.Array.Comfort.Boxed as BoxedArray
import qualified Data.Array.Comfort.Check as Check
import qualified Data.Array.Comfort.Shape.Tuple as TupleShape
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))

import System.IO.Unsafe (unsafePerformIO)
import Foreign.Marshal.Array (advancePtr)
import Foreign.Storable (Storable, poke, peekElemOff)
import Foreign.ForeignPtr (withForeignPtr)

import qualified Control.Monad.Trans.State as MS
import Control.Monad.ST (runST)

import qualified Data.StorableVector as SV
import qualified Data.IntMap as IntMap
import qualified Data.IntSet as IntSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Tuple.Strict as StrictTuple
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import Data.Map (Map)
import Data.Set (Set)
import Data.Foldable (forM_)
import Data.Either.HT (maybeRight)
import Data.Semigroup
         (Semigroup, (<>), Min(Min,getMin), Max(Max,getMax), Arg(Arg))

import Prelude2010 hiding (map, zipWith, foldl1, minimum, maximum, replicate)
import Prelude ()


{- $setup
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Storable (Array, (!))
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.ChasingBottoms.IsBottom (isBottom)
>>>
>>> import Control.Applicative ((<$>))
>>>
>>> import qualified Data.IntSet as IntSet
>>> import qualified Data.Set as Set
>>> import Data.Complex (Complex((:+)))
>>> import Data.Word (Word8, Word16)
>>>
>>> type ShapeInt = Shape.ZeroBased Int
>>> type X = Shape.Element
>>>
>>> shapeInt :: Int -> ShapeInt
>>> shapeInt = Shape.ZeroBased
>>>
>>> genArray :: QC.Gen (Array ShapeInt Word16)
>>> genArray = Array.vectorFromList <$> QC.arbitrary
>>>
>>> infix 4 ==?
>>> (==?) :: a -> a -> (a,a)
>>> (==?) = (,)
>>>
>>> forAllNonEmpty :: (Eq b) => (Array ShapeInt Word16 -> (b,b)) -> QC.Property
>>> forAllNonEmpty f =
>>>    QC.forAll genArray $ \xs ->
>>>    case f xs of
>>>       (resultArray,resultList) ->
>>>          if Array.shape xs == Shape.ZeroBased 0
>>>             then isBottom resultArray
>>>             else resultArray == resultList
-}


shape :: Array sh a -> sh
shape = Array.shape

reshape :: (Shape.C sh0, Shape.C sh1) => sh1 -> Array sh0 a -> Array sh1 a
reshape = Check.reshape "Storable" shape Array.reshape

mapShape ::
   (Shape.C sh0, Shape.C sh1) => (sh0 -> sh1) -> Array sh0 a -> Array sh1 a
mapShape f arr = reshape (f $ shape arr) arr


{- |
>>> Array.fromList (shapeInt 5) ['a'..]
StorableArray.fromList (ZeroBased {zeroBasedSize = 5}) "abcde"
-}
fromList :: (Shape.C sh, Storable a) => sh -> [a] -> Array sh a
fromList sh arr = runST (MutArrayNC.unsafeFreeze =<< MutArray.fromList sh arr)

fromMap :: (Ord k, Storable a) => Map k a -> Array (Set k) a
fromMap m = fromList (Map.keysSet m) (Map.elems m)

toMap :: (Ord k, Storable a) => Array (Set k) a -> Map k a
toMap = Map.fromAscList . toAssociations

fromIntMap :: (Storable a) => IntMap a -> Array IntSet a
fromIntMap m = fromList (IntMap.keysSet m) (IntMap.elems m)

toIntMap :: (Storable a) => Array IntSet a -> IntMap a
toIntMap = IntMap.fromAscList . toAssociations

{- |
>>> Array.fromTuple ('a',('b','c')) :: Array (Shape.NestedTuple Shape.TupleIndex (X,(X,X))) Char
StorableArray.fromList (NestedTuple {getNestedTuple = (Element 0,(Element 1,Element 2))}) "abc"

>>> :{
   let arr :: Array (Shape.NestedTuple Shape.TupleAccessor (X,(X,X))) Char
       arr = Array.fromTuple ('a',('b','c'))
   in (arr ! fst, arr ! (fst.snd))
:}
('a','b')
-}
fromTuple ::
   (TupleShape.NestedTuple tuple, Storable a) =>
   Shape.DataTuple tuple a -> Array (Shape.NestedTuple ixtype tuple) a
fromTuple tuple =
   case MS.evalState (TupleShape.decons tuple) (Shape.Element 0) of
      (sh, xs) -> fromList (Shape.NestedTuple sh) xs

toTuple ::
   (TupleShape.NestedTuple tuple, Storable a) =>
   Array (Shape.NestedTuple ixtype tuple) a -> Shape.DataTuple tuple a
toTuple arr =
   MS.evalState
      (TupleShape.cons $ Shape.getNestedTuple $ shape arr)
      (Array.toList arr)

{- |
>>> :{
   let arr = Array.fromRecord ('a' :+ 'b') in
   let (real:+imag) = Shape.indexRecordFromShape $ Array.shape arr in
   (arr ! real, arr ! imag)
:}
('a','b')
-}
fromRecord ::
   (Trav.Traversable f, Storable a) =>
   f a -> Array (Shape.Record f) a
fromRecord xs =
   fromList
      (Shape.Record $ flip MS.evalState (Shape.Element 0) $
       Trav.traverse (const TupleShape.next) xs)
      (Fold.toList xs)

toRecord ::
   (Trav.Traversable f, Storable a) =>
   Array (Shape.Record f) a -> f a
toRecord arr =
   MS.evalState
      (Trav.traverse (const TupleShape.get) $
       (\(Shape.Record record) -> record) $ shape arr)
      (Array.toList arr)

fromContainer ::
   (Container.C f, Storable a) => f a -> Array (Container.Shape f) a
fromContainer xs = fromList (Container.toShape xs) (Fold.toList xs)

toContainer ::
   (Container.C f, Storable a) => Array (Container.Shape f) a -> f a
toContainer arr = Container.fromList (Array.shape arr) (Array.toList arr)

sample ::
   (Shape.Indexed sh, Storable a) => sh -> (Shape.Index sh -> a) -> Array sh a
sample sh f = Array.fromList sh $ List.map f $ Shape.indices sh

replicate :: (Shape.C sh, Storable a) => sh -> a -> Array sh a
replicate sh a = runST (MutArrayNC.unsafeFreeze =<< MutArray.new sh a)


fromBoxed :: (Shape.C sh, Storable a) => BoxedArray.Array sh a -> Array sh a
fromBoxed arr = Array.fromList (BoxedArray.shape arr) $ BoxedArray.toList arr

toBoxed :: (Shape.C sh, Storable a) => Array sh a -> BoxedArray.Array sh a
toBoxed arr = BoxedArray.fromList (Array.shape arr) $ Array.toList arr


{-# DEPRECATED fromBlockArray1 "Use fromBlockArray instead." #-}
fromBlockArray, fromBlockArray1 ::
   (Ord k, Shape.C shape, Storable a) =>
   BoxedArray.Array (Set k) (Array shape a) -> Array (Map k shape) a
fromBlockArray1 = fromBlockArray
fromBlockArray a =
   reshape (BoxedArray.toMap $ fmap Array.shape a) $
   Array.fromStorableVector $ SV.concat $
   List.map Array.toStorableVector $ BoxedArray.toList a

{-# DEPRECATED fromNonEmptyBlockArray2
      "Use Storable.Dim2.fromNonEmptyBlockArray instead." #-}
fromNonEmptyBlockArray2 ::
   (Ord row,    Shape.C height, Eq height) =>
   (Ord column, Shape.C width,  Eq width) =>
   (Storable a) =>
   BoxedArray.Array (Set row, Set column) (Array (height, width) a) ->
   Array (Map row height, Map column width) a
fromNonEmptyBlockArray2 = Array2.fromNonEmptyBlockArray

{-# DEPRECATED fromBlockArray2 "Use Storable.Dim2.fromBlockArray instead." #-}
fromBlockArray2 ::
   (Ord row,    Shape.C height, Eq height) =>
   (Ord column, Shape.C width,  Eq width) =>
   (Storable a) =>
   Map row height -> Map column width ->
   BoxedArray.Array (Set row, Set column) (Array (height, width) a) ->
   Array (Map row height, Map column width) a
fromBlockArray2 = Array2.fromBlockArray


toAssociations ::
   (Shape.Indexed sh, Storable a) => Array sh a -> [(Shape.Index sh, a)]
toAssociations arr = zip (Shape.indices $ shape arr) (Array.toList arr)


errorArray :: String -> String -> a
errorArray name msg =
   error ("Array.Comfort.Storable." ++ name ++ ": " ++ msg)

infixl 9 !

(!) :: (Shape.Indexed sh, Storable a) => Array sh a -> Shape.Index sh -> a
(!) arr = either (errorArray "!") id . accessEither arr

accessMaybe ::
   (Shape.Indexed sh, Storable a) => Array sh a -> Shape.Index sh -> Maybe a
accessMaybe arr = maybeRight . accessEither arr

accessEither ::
   (Shape.Indexed sh, Storable a) =>
   Array sh a -> Shape.Index sh -> Either String a
accessEither arr ix = runST (do
   marr <- MutArrayNC.unsafeThaw arr
   case MutArrayPriv.readEither marr ix of
      Right access -> fmap Right access
      Left msg -> return $ Left msg)
--   for GHC>=7.8: Trav.sequenceA $ MutArrayPriv.readEither marr ix)


zipWith ::
   (Shape.C sh, Eq sh, Storable a, Storable b, Storable c) =>
   (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f a b =
   if shape a == shape b
      then Array.zipWith f a b
      else errorArray "zipWith" "shapes mismatch"

(//) ::
   (Shape.Indexed sh, Storable a) =>
   Array sh a -> [(Shape.Index sh, a)] -> Array sh a
(//) arr xs = runST (do
   marr <- MutArray.thaw arr
   forM_ xs $ uncurry $ MutArray.write marr
   MutArrayNC.unsafeFreeze marr)

accumulate ::
   (Shape.Indexed sh, Storable a) =>
   (a -> b -> a) -> Array sh a -> [(Shape.Index sh, b)] -> Array sh a
accumulate f arr xs = runST (do
   marr <- MutArray.thaw arr
   forM_ xs $ \(ix,b) -> MutArray.update marr ix $ flip f b
   MutArrayNC.unsafeFreeze marr)

fromAssociations ::
   (Shape.Indexed sh, Storable a) =>
   a -> sh -> [(Shape.Index sh, a)] -> Array sh a
fromAssociations a sh xs = runST (do
   marr <- MutArray.new sh a
   forM_ xs $ uncurry $ MutArray.write marr
   MutArrayNC.unsafeFreeze marr)


{- |
>>> Array.takeSet (Set.fromList [0,2,4,7,13]) (Array.vectorFromList [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3::Word8])
StorableArray... (... [0,2,4,7,13]) [3,4,5,6,7]
-}
{-# INLINE takeSet #-}
takeSet ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Ord ix, Storable a) =>
   Set ix -> Array sh a -> Array (Set ix) a
takeSet = takeSetGen Set.toAscList

{- |
>>> Array.takeIntSet (IntSet.fromList [0,2,4,7,13]) (Array.vectorFromList [3,1,4,1,5,9,2,6,5,3,5,8,9,7,9,3::Word8])
StorableArray... (... [0,2,4,7,13]) [3,4,5,6,7]
-}
{-# INLINE takeIntSet #-}
takeIntSet ::
   (Shape.Indexed sh, Shape.Index sh ~ Int, Storable a) =>
   IntSet -> Array sh a -> Array IntSet a
takeIntSet = takeSetGen IntSet.toAscList

{-# INLINE takeSetGen #-}
takeSetGen ::
   (Shape.Indexed sh, Shape.Index sh ~ ix, Shape.C set, Storable a) =>
   (set -> [ix]) -> set -> Array sh a -> Array set a
takeSetGen listFromSet ixs (Array sh a) =
   Array.unsafeCreate ixs $ \dstPtr ->
   withForeignPtr a $ \srcPtr ->
   sequence_ $
      List.zipWith
         (\src dst -> poke dst =<< peekElemOff srcPtr src)
         (List.map (Shape.offset sh) $ listFromSet ixs)
         (iterate (flip advancePtr 1) dstPtr)


{-# DEPRECATED pick "Use Storable.Dim2.takeRow instead." #-}
pick ::
   (Shape.Indexed sh0, Shape.C sh1, Storable a) =>
   Array (sh0,sh1) a -> Shape.Index sh0 -> Array sh1 a
pick = Array2.takeRow

{-# DEPRECATED toRowArray "Use Storable.Dim2.toRowArray instead." #-}
toRowArray ::
   (Shape.Indexed sh0, Shape.C sh1, Storable a) =>
   Array (sh0,sh1) a -> BoxedArray.Array sh0 (Array sh1 a)
toRowArray = Array2.toRowArray

{-# DEPRECATED fromRowArray "Use Storable.Dim2.fromRowArray instead." #-}
fromRowArray ::
   (Shape.C sh0, Shape.C sh1, Eq sh1, Storable a) =>
   sh1 -> BoxedArray.Array sh0 (Array sh1 a) -> Array (sh0,sh1) a
fromRowArray = Array2.fromRowArray


{- |
It is a checked error if the vector is empty.

prop> forAllNonEmpty $ \xs -> Array.minimum xs ==? minimum (Array.toList xs)
-}
minimum :: (Shape.C sh, Storable a, Ord a) => Array sh a -> a
minimum = foldl1 min

{- |
It is a checked error if the vector is empty.

prop> forAllNonEmpty $ \xs -> Array.maximum xs ==? maximum (Array.toList xs)
-}
maximum :: (Shape.C sh, Storable a, Ord a) => Array sh a -> a
maximum = foldl1 max

{-# INLINE foldl1 #-}
foldl1 :: (Shape.C sh, Storable a) => (a -> a -> a) -> Array sh a -> a
foldl1 op (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr ->
      Memory.foldl1 (const id) op (Shape.size sh) xPtr 1

{- |
prop> forAllNonEmpty $ \xs -> Array.limits xs ==? (Array.minimum xs, Array.maximum xs)
-}
limits :: (Shape.C sh, Storable a, Ord a) => Array sh a -> (a,a)
limits = StrictTuple.mapPair (getMin, getMax) . foldMap (\x -> (Min x, Max x))

{-# INLINE foldMap #-}
foldMap ::
   (Shape.C sh, Storable a, Ord a, Semigroup m) => (a -> m) -> Array sh a -> m
foldMap f (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr ->
      Memory.foldl1 (const f) (<>) (Shape.size sh) xPtr 1


argMinimum, argMaximum ::
   (Shape.InvIndexed sh, Storable a, Ord a) =>
   Array sh a -> (Shape.Index sh, a)
argMinimum xs = unArg xs $ getMin $ foldMapWithIndex (\k x -> Min (Arg x k)) xs
argMaximum xs = unArg xs $ getMax $ foldMapWithIndex (\k x -> Max (Arg x k)) xs

unArg ::
   (Shape.InvIndexed sh) => Array sh a -> Arg a Int -> (Shape.Index sh, a)
unArg xs (Arg x k) = (Shape.indexFromOffset (Array.shape xs) k, x)

{-# INLINE foldMapWithIndex #-}
foldMapWithIndex ::
   (Shape.C sh, Storable a, Semigroup m) => (Int -> a -> m) -> Array sh a -> m
foldMapWithIndex f (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr -> Memory.foldl1 f (<>) (Shape.size sh) xPtr 1
