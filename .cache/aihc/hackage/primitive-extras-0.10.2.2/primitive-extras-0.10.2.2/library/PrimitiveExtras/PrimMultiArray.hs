{-# OPTIONS_GHC -Wno-redundant-constraints -Wno-orphans #-}

module PrimitiveExtras.PrimMultiArray
  ( PrimMultiArray,
    create,
    replicateM,
    outerLength,
    toAssocsUnfoldl,
    toIndicesUnfoldl,
    toUnfoldlAt,
    toAssocsUnfoldlM,
    toIndicesUnfoldlM,
    toUnfoldlAtM,
    cerealGet,
    cerealGetAsInMemory,
    cerealPut,
    cerealPutAsInMemory,
    fold,
  )
where

import qualified Data.Serialize as Cereal
import qualified DeferredFolds.Unfoldl as Unfoldl
import qualified DeferredFolds.UnfoldlM as UnfoldlM
import qualified PrimitiveExtras.Folds as Folds
import PrimitiveExtras.Prelude hiding (fold, replicateM)
import qualified PrimitiveExtras.PrimArray as PrimArray
import PrimitiveExtras.Types
import qualified PrimitiveExtras.UnliftedArray as UnliftedArray

deriving instance (Eq a, Prim a) => Eq (PrimMultiArray a)

instance (Show a, Prim a) => Show (PrimMultiArray a) where
  show (PrimMultiArray outerArray) =
    unliftedArrayToList outerArray
      & map primArrayToList
      & show

-- | Given a size of the outer array and a function, which executes a fold over indexed elements in a monad,
-- constructs a prim multi-array
create :: (Monad m, Prim element) => Int -> (forall x. Fold (Int, element) x -> m x) -> m (PrimMultiArray element)
create outerArraySize runFold =
  do
    indexCounts <- runFold (lmap fst (Folds.indexCounts outerArraySize))
    runFold (Folds.primMultiArray (indexCounts :: PrimArray Word32))

replicateM :: (Monad m, Prim a) => Int -> m (PrimArray a) -> m (PrimMultiArray a)
replicateM size elementM =
  do
    !mutable <- return (unsafeDupablePerformIO (unsafeNewUnliftedArray size))
    let iterate index =
          if index < size
            then do
              element <- elementM
              let !() = unsafeDupablePerformIO (writeUnliftedArray mutable index element)
              iterate (succ index)
            else return (PrimMultiArray (unsafePerformIO (unsafeFreezeUnliftedArray mutable)))
     in iterate 0

-- | Get length of the outer dimension of a primitive multi array
outerLength :: PrimMultiArray a -> Int
outerLength (PrimMultiArray outerDimension) = sizeofUnliftedArray outerDimension

toAssocsUnfoldl :: (Prim a) => PrimMultiArray a -> Unfoldl (Int, a)
toAssocsUnfoldl = Unfoldl.unfoldlM . toAssocsUnfoldlM

toIndicesUnfoldl :: PrimMultiArray a -> Unfoldl Int
toIndicesUnfoldl (PrimMultiArray ua) = Unfoldl.intsInRange 0 (pred (sizeofUnliftedArray ua))

toUnfoldlAt :: (Prim prim) => PrimMultiArray prim -> Int -> Unfoldl prim
toUnfoldlAt (PrimMultiArray ua) index = UnliftedArray.at ua index empty PrimArray.toElementsUnfoldl

toAssocsUnfoldlM :: (Monad m, Prim a) => PrimMultiArray a -> UnfoldlM m (Int, a)
toAssocsUnfoldlM pma =
  do
    index <- toIndicesUnfoldlM pma
    element <- toUnfoldlAtM pma index
    return (index, element)

toIndicesUnfoldlM :: (Monad m) => PrimMultiArray a -> UnfoldlM m Int
toIndicesUnfoldlM (PrimMultiArray ua) = UnfoldlM.intsInRange 0 (pred (sizeofUnliftedArray ua))

toUnfoldlAtM :: (Monad m, Prim prim) => PrimMultiArray prim -> Int -> UnfoldlM m prim
toUnfoldlAtM (PrimMultiArray ua) index = UnliftedArray.at ua index empty PrimArray.toElementsUnfoldlM

cerealGet :: (Prim element) => Cereal.Get Int -> Cereal.Get element -> Cereal.Get (PrimMultiArray element)
cerealGet int element =
  do
    size <- int
    replicateM size (PrimArray.cerealGet int element)

cerealGetAsInMemory :: (Prim element) => Cereal.Get Int -> Cereal.Get (PrimMultiArray element)
cerealGetAsInMemory int =
  do
    size <- int
    replicateM size (PrimArray.cerealGetAsInMemory int)

cerealPut :: (Prim element) => Cereal.Putter Int -> Cereal.Putter element -> Cereal.Putter (PrimMultiArray element)
cerealPut int element (PrimMultiArray outerArrayValue) =
  size <> innerArrays
  where
    size = int (sizeofUnliftedArray outerArrayValue)
    innerArrays = UnliftedArray.traverse_ (PrimArray.cerealPut int element) outerArrayValue

cerealPutAsInMemory :: (Prim element) => Cereal.Putter Int -> Cereal.Putter (PrimMultiArray element)
cerealPutAsInMemory int (PrimMultiArray outerArrayValue) =
  size <> innerArrays
  where
    size = int (sizeofUnliftedArray outerArrayValue)
    innerArrays = UnliftedArray.traverse_ (PrimArray.cerealPutAsInMemory int) outerArrayValue

-- |
-- Having a priorly computed array of inner dimension sizes,
-- e.g., using the 'PrimitiveExtras.PrimArray.indexCountsFold',
-- construct a fold over indexed elements into a multi-array of elements.
--
-- Thus it allows to construct it in two passes over the indexed elements.
fold :: (Integral size, Prim size, Prim element) => PrimArray size -> Fold (Int, element) (PrimMultiArray element)
fold = Folds.primMultiArray
