module Data.Array.Comfort.Boxed (
   Array,
   shape,
   reshape,
   mapShape,
   accessMaybe, (!),
   Array.toList,
   Array.fromList,
   Array.vectorFromList,
   toAssociations,
   fromMap,
   toMap,
   fromTuple,
   toTuple,
   fromRecord,
   toRecord,
   fromContainer,
   toContainer,
   indices,
   Array.replicate,
   cartesian,

   Array.map,
   zipWith,
   (//),
   accumulate,
   fromAssociations,

   pick,
   Array.append,
   Array.take, Array.drop,
   Array.takeLeft, Array.takeRight, Array.split,
   Array.takeCenter,
   ) where

import qualified Data.Array.Comfort.Boxed.Unchecked as Array
import qualified Data.Array.Comfort.Container as Container
import qualified Data.Array.Comfort.Check as Check
import qualified Data.Array.Comfort.Shape.Tuple as TupleShape
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Boxed.Unchecked (Array(Array))

import qualified Data.Primitive.Array as Prim

import qualified Control.Monad.Primitive as PrimM
import qualified Control.Monad.Trans.State as MS
import Control.Monad.ST (runST)
import Control.Applicative (liftA2, (<$>))

import qualified Data.Foldable as Fold
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Map (Map)
import Data.Set (Set)
import Data.Traversable (Traversable, traverse)
import Data.Foldable (forM_)
import Data.Either.HT (maybeRight)

import Prelude hiding (zipWith, replicate)


{- $setup
>>> import qualified Data.Array.Comfort.Boxed as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import Data.Array.Comfort.Boxed (Array, (!))
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>> type ShapeInt = Shape.ZeroBased Int
>>>
>>> genArray2 :: QC.Gen (Array (ShapeInt,ShapeInt) Char)
>>> genArray2 = do
>>>    xs <- QC.arbitrary
>>>    let n = length xs
>>>    (k,m) <-
>>>       if n == 0
>>>          then QC.elements [(,) 0, flip (,) 0] <*> QC.choose (1,20)
>>>          else fmap (\m -> (div n m, m)) $ QC.choose (1,n)
>>>    return $
>>>       Array.fromList (Shape.ZeroBased k, Shape.ZeroBased m) $ take (k*m) xs
-}


shape :: Array.Array sh a -> sh
shape = Array.shape

reshape :: (Shape.C sh0, Shape.C sh1) => sh1 -> Array sh0 a -> Array sh1 a
reshape = Check.reshape "Boxed" shape Array.reshape

mapShape ::
   (Shape.C sh0, Shape.C sh1) => (sh0 -> sh1) -> Array sh0 a -> Array sh1 a
mapShape f arr = reshape (f $ shape arr) arr


indices :: (Shape.Indexed sh) => sh -> Array.Array sh (Shape.Index sh)
indices sh = Array.fromList sh $ Shape.indices sh

fromMap :: (Ord k) => Map k a -> Array (Set k) a
fromMap m = Array.fromList (Map.keysSet m) (Map.elems m)

toMap :: (Ord k) => Array (Set k) a -> Map k a
toMap arr = Map.fromAscList $ zip (Set.toAscList $ shape arr) (Array.toList arr)

fromTuple ::
   (TupleShape.NestedTuple tuple) =>
   Shape.DataTuple tuple a -> Array (Shape.NestedTuple ixtype tuple) a
fromTuple tuple =
   case MS.evalState (TupleShape.decons tuple) (Shape.Element 0) of
      (sh, xs) -> Array.fromList (Shape.NestedTuple sh) xs

toTuple ::
   (TupleShape.NestedTuple tuple) =>
   Array (Shape.NestedTuple ixtype tuple) a -> Shape.DataTuple tuple a
toTuple arr =
   MS.evalState
      (TupleShape.cons $ Shape.getNestedTuple $ shape arr)
      (Array.toList arr)

fromRecord ::
   (Traversable f) =>
   f a -> Array (Shape.Record f) a
fromRecord xs =
   Array.fromList
      (Shape.Record $ flip MS.evalState (Shape.Element 0) $
       traverse (const TupleShape.next) xs)
      (Fold.toList xs)

toRecord ::
   (Traversable f) =>
   Array (Shape.Record f) a -> f a
toRecord arr =
   MS.evalState
      (traverse (const TupleShape.get) $
       (\(Shape.Record record) -> record) $ shape arr)
      (Array.toList arr)

fromContainer :: (Container.C f) => f a -> Array (Container.Shape f) a
fromContainer xs = Array.fromList (Container.toShape xs) (Fold.toList xs)

toContainer :: (Container.C f) => Array (Container.Shape f) a -> f a
toContainer arr = Container.fromList (Array.shape arr) (Array.toList arr)


infixl 9 !

(!) :: (Shape.Indexed sh) => Array sh a -> Shape.Index sh -> a
(!) arr =
   either (error . ("Array.Comfort.Boxed.!: " ++)) id . accessEither arr

accessMaybe :: (Shape.Indexed sh) => Array sh a -> Shape.Index sh -> Maybe a
accessMaybe arr = maybeRight . accessEither arr

accessEither ::
   (Shape.Indexed sh) => Array sh a -> Shape.Index sh -> Either String a
accessEither (Array sh arr) ix =
   fmap (Prim.indexArray arr) $ Shape.getChecked $ Shape.unifiedOffset sh ix


zipWith ::
   (Shape.C sh, Eq sh) =>
   (a -> b -> c) -> Array sh a -> Array sh b -> Array sh c
zipWith f a b =
   if shape a == shape b
      then Array.zipWith f a b
      else error "zipWith: shapes mismatch"


(//) ::
   (Shape.Indexed sh) => Array sh a -> [(Shape.Index sh, a)] -> Array sh a
(//) (Array sh arr) xs = runST (do
   marr <- Prim.thawArray arr 0 (Shape.size sh)
   forM_ xs $ \(ix,a) -> Prim.writeArray marr (Shape.offset sh ix) a
   Array sh <$> Prim.unsafeFreezeArray marr)

accumulate ::
   (Shape.Indexed sh) =>
   (a -> b -> a) -> Array sh a -> [(Shape.Index sh, b)] -> Array sh a
accumulate f (Array sh arr) xs = runST (do
   marr <- Prim.thawArray arr 0 (Shape.size sh)
   forM_ xs $ \(ix,b) -> updateArray marr (Shape.offset sh ix) $ flip f b
   Array sh <$> Prim.unsafeFreezeArray marr)

updateArray ::
   PrimM.PrimMonad m =>
   Prim.MutableArray (PrimM.PrimState m) a -> Int -> (a -> a) -> m ()
updateArray marr k f = Prim.writeArray marr k . f =<< Prim.readArray marr k

toAssociations :: (Shape.Indexed sh) => Array sh a -> [(Shape.Index sh, a)]
toAssociations arr = zip (Shape.indices $ shape arr) (Array.toList arr)

fromAssociations ::
   (Shape.Indexed sh) => a -> sh -> [(Shape.Index sh, a)] -> Array sh a
fromAssociations a sh xs = runST (do
   marr <- Prim.newArray (Shape.size sh) a
   forM_ xs $ \(ix,x) -> Prim.writeArray marr (Shape.offset sh ix) x
   Array sh <$> Prim.unsafeFreezeArray marr)



{- |
prop> :{
   QC.forAll genArray2 $ \xs ->
   let shape = Array.shape xs in
   Shape.size shape > 0   QC.==>
   QC.forAll (QC.elements $ Shape.indices shape) $ \(ix0,ix1) ->
      Array.pick xs ix0 ! ix1 == xs!(ix0,ix1)
:}
-}
pick ::
   (Shape.Indexed sh0, Shape.C sh1) =>
   Array (sh0,sh1) a -> Shape.Index sh0 -> Array sh1 a
pick (Array (sh0,sh1) x) ix0 =
   Array sh1 $
   let k = Shape.size sh1
   in Prim.cloneArray x (Shape.offset sh0 ix0 * k) k


cartesian ::
   (Shape.C sh0, Shape.C sh1) =>
   Array sh0 a -> Array sh1 b -> Array (sh0,sh1) (a,b)
cartesian a b =
   Array.fromList (shape a, shape b) $
      liftA2 (,) (Array.toList a) (Array.toList b)
