{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Data.Array.Comfort.Storable.Unchecked.Monadic where

import qualified Data.Array.Comfort.Shape.SubSize as SubSize
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Private (Array(Array))

import Foreign.Storable (Storable, )
import Foreign.Ptr (Ptr, )

import qualified Foreign.Marshal.Array.Guarded as Alloc
import Control.Monad.Primitive (PrimMonad, unsafeIOToPrim)
import Control.Monad (liftM)

import Data.Tuple.HT (mapFst)


unsafeCreate ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Ptr a -> IO ()) -> m (Array sh a)
unsafeCreate sh f = unsafeCreateWithSize sh $ const f

unsafeCreateWithSize ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO ()) -> m (Array sh a)
unsafeCreateWithSize sh f = liftM fst $ unsafeCreateWithSizeAndResult sh f

unsafeCreateWithSizes ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   SubSize.T sh nsize -> sh -> (nsize -> Ptr a -> IO ()) -> m (Array sh a)
unsafeCreateWithSizes sub sh f =
   liftM fst $ unsafeCreateWithSizesAndResult sub sh f

unsafeCreateWithSizeAndResult ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO b) -> m (Array sh a, b)
unsafeCreateWithSizeAndResult sh f = unsafeIOToPrim $
   let size = Shape.size sh
   in fmap (mapFst (Array sh)) $ Alloc.create size $ f size

unsafeCreateWithSizesAndResult ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   SubSize.T sh nsize -> sh -> (nsize -> Ptr a -> IO b) -> m (Array sh a, b)
unsafeCreateWithSizesAndResult (SubSize.Cons subSize) sh f = unsafeIOToPrim $
   let (size, subSizes) = subSize sh
   in fmap (mapFst (Array sh)) $ Alloc.create size $ f subSizes

_unsafeCreateWithSizesAndResult ::
   (PrimMonad m, Storable a,
    SubSize.C nsize, SubSize.ToShape nsize ~ sh, Shape.C sh) =>
   sh -> (Int -> nsize -> Ptr a -> IO b) -> m (Array sh a, b)
_unsafeCreateWithSizesAndResult sh f = unsafeIOToPrim $
   let (size, subSizes) = SubSize.evaluate sh
   in fmap (mapFst (Array sh)) $ Alloc.create size $ f size subSizes
