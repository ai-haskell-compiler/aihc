{-# LANGUAGE Rank2Types #-}
module Data.Array.Comfort.Storable.Unchecked.Creator where

import qualified Data.Array.Comfort.Shape.SubSize as SubSize
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Private (Array(Array))

import Foreign.Storable (Storable, )
import Foreign.Ptr (Ptr, )

import qualified Foreign.Marshal.Array.Guarded as Alloc
import qualified Control.Monad.Trans.Cont as MC
import Control.Monad.Primitive (PrimMonad, unsafeIOToPrim)

import Data.Biapplicative (Biapplicative(bipure, (<<*>>)))
import Data.Bifunctor (Bifunctor(bimap))

import Data.Tuple.HT (mapFst)


newtype Creator arr ptr = Creator (forall a. (ptr -> IO a) -> IO (arr, a))

liftIO :: IO ptr -> Creator () ptr
liftIO act = Creator $ \f -> fmap ((,) ()) $ f =<< act

liftContT :: (forall a. MC.ContT a IO ptr) -> Creator () ptr
liftContT act = Creator $ \f -> fmap ((,) ()) $ MC.runContT act f

instance Functor (Creator arr) where
   fmap g (Creator act) = Creator $ \f -> act (f . g)


pair ::
   Creator arr0 ptr0 -> Creator arr1 ptr1 ->
   Creator (arr0,arr1) (ptr0,ptr1)
pair (Creator act0) (Creator act1) =
   Creator $ \f ->
      fmap (\(arr0,(arr1,a)) -> ((arr0,arr1),a)) $
      act0 $ \ptr0 ->
      act1 $ \ptr1 ->
         f (ptr0, ptr1)

instance Bifunctor Creator where
   bimap g h (Creator act) = Creator $ \f -> fmap (mapFst g) $ act (f . h)

instance Biapplicative Creator where
   bipure a b = Creator $ \f -> fmap ((,) a) $ f b
   creator0 <<*>> creator1 =
      bimap (uncurry id) (uncurry id) $ pair creator0 creator1

unsafeRun :: (PrimMonad m) => Creator arr ptr -> (ptr -> IO ()) -> m arr
unsafeRun (Creator act) f = unsafeIOToPrim $ fmap (\(arr,()) -> arr) $ act f

unsafeRunWithResult ::
   (PrimMonad m) => Creator arr ptr -> (ptr -> IO b) -> m (arr, b)
unsafeRunWithResult (Creator act) f = unsafeIOToPrim $ act f

{-# INLINE create #-}
create ::
   (Shape.C sh, Storable a) =>
   sh -> Creator (Array sh a) (Ptr a)
create sh = fmap snd $ createWithSize sh

{-# INLINE createWithSize #-}
createWithSize ::
   (Shape.C sh, Storable a) =>
   sh -> Creator (Array sh a) (Int, Ptr a)
createWithSize sh =
   let size = Shape.size sh
   in Creator $ \f ->
         fmap (mapFst (Array sh)) $ Alloc.create size $ curry f size

{-# INLINE createWithSizes #-}
createWithSizes ::
   (Shape.C sh, Storable a) =>
   SubSize.T sh nsize -> sh -> Creator (Array sh a) (nsize, Ptr a)
createWithSizes (SubSize.Cons subSize) sh =
   let (size, subSizes) = subSize sh
   in Creator $ \f ->
         fmap (mapFst (Array sh)) $ Alloc.create size $ curry f subSizes
