{-# LANGUAGE TypeFamilies #-}
module Data.Array.Comfort.Storable.Mutable.Private where

import qualified Data.Array.Comfort.Shape as Shape

import qualified Foreign.Marshal.Array.Guarded as Alloc
import Foreign.Marshal.Array (copyArray, pokeArray, peekArray)
import Foreign.Storable (Storable, pokeElemOff, peekElemOff)
import Foreign.Ptr (Ptr)

import Control.Monad.Primitive (PrimMonad, unsafeIOToPrim)
import Control.Monad.ST (ST)
import Control.Monad (liftM)
import Control.Applicative ((<$>))

import Data.Either.HT (maybeRight)
import Data.Tuple.HT (mapFst)

import qualified Prelude as P
import Prelude hiding (read, show)


data Array (m :: * -> *) sh a =
   Array {
      shape :: sh,
      buffer :: Alloc.MutablePtr a
   }

type STArray s = Array (ST s)
type IOArray = Array IO


copy ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   Array m sh a -> m (Array m sh a)
copy (Array sh srcFPtr) =
   unsafeCreateWithSize sh $ \n dstPtr ->
   Alloc.withMutablePtr srcFPtr $ \srcPtr ->
      copyArray dstPtr srcPtr n


create ::
   (Shape.C sh, Storable a) =>
   sh -> (Ptr a -> IO ()) -> IO (IOArray sh a)
create sh f = createWithSize sh $ const f

createWithSize ::
   (Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO ()) -> IO (IOArray sh a)
createWithSize sh f =
   fst <$> createWithSizeAndResult sh f

createWithSizeAndResult ::
   (Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO b) -> IO (IOArray sh a, b)
createWithSizeAndResult sh f = do
   let size = Shape.size sh
   mfptr <- Alloc.new size
   b <- Alloc.withMutablePtr mfptr $ f size
   return (Array sh mfptr, b)


unsafeCreate ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Ptr a -> IO ()) -> m (Array m sh a)
unsafeCreate sh f = unsafeCreateWithSize sh $ const f

unsafeCreateWithSize ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO ()) -> m (Array m sh a)
unsafeCreateWithSize sh f =
   liftM fst $ unsafeCreateWithSizeAndResult sh f

unsafeCreateWithSizeAndResult ::
   (PrimMonad m, Shape.C sh, Storable a) =>
   sh -> (Int -> Ptr a -> IO b) -> m (Array m sh a, b)
unsafeCreateWithSizeAndResult sh f =
   unsafeIOToPrim $
   fmap (mapFst unsafeArrayIOToPrim) $ createWithSizeAndResult sh f

unsafeArrayIOToPrim :: (PrimMonad m) => IOArray sh a -> Array m sh a
unsafeArrayIOToPrim (Array sh fptr) = Array sh fptr


show ::
   (PrimMonad m, Shape.C sh, Show sh, Storable a, Show a) =>
   Array m sh a -> m String
show arr = do
   xs <- toList arr
   return $
      "StorableArray.fromList " ++ showsPrec 11 (shape arr) (' ' : P.show xs)

withArrayPtr :: (PrimMonad m) => Alloc.MutablePtr a -> (Ptr a -> IO b) -> m b
withArrayPtr fptr = unsafeIOToPrim . Alloc.withMutablePtr fptr

withPtr :: (PrimMonad m) => Array m sh a -> (Ptr a -> IO b) -> m b
withPtr (Array _sh fptr) = withArrayPtr fptr

read ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> m a
read (Array sh fptr) ix =
   withArrayPtr fptr $ flip peekElemOff (Shape.uncheckedOffset sh ix)

readMaybe ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> Maybe (m a)
readMaybe arr = maybeRight . readEither arr

readEither ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> Either String (m a)
readEither (Array sh fptr) ix =
   fmap (withArrayPtr fptr . flip peekElemOff) $
   Shape.getChecked $ Shape.unifiedOffset sh ix

write ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> a -> m ()
write (Array sh fptr) ix a =
   withArrayPtr fptr $ \ptr -> pokeElemOff ptr (Shape.uncheckedOffset sh ix) a

update ::
   (PrimMonad m, Shape.Indexed sh, Storable a) =>
   Array m sh a -> Shape.Index sh -> (a -> a) -> m ()
update (Array sh fptr) ix f =
   let k = Shape.uncheckedOffset sh ix
   in withArrayPtr fptr $ \ptr -> pokeElemOff ptr k . f =<< peekElemOff ptr k

new :: (PrimMonad m, Shape.C sh, Storable a) => sh -> a -> m (Array m sh a)
new sh x =
   unsafeCreateWithSize sh $ \size ptr -> pokeArray ptr $ replicate size x

toList :: (PrimMonad m, Shape.C sh, Storable a) => Array m sh a -> m [a]
toList (Array sh fptr) = withArrayPtr fptr $ peekArray (Shape.size sh)

fromList ::
   (PrimMonad m, Shape.C sh, Storable a) => sh -> [a] -> m (Array m sh a)
fromList sh xs = unsafeCreate sh $ \ptr -> pokeArray ptr xs

vectorFromList ::
   (PrimMonad m, Storable a) => [a] -> m (Array m (Shape.ZeroBased Int) a)
vectorFromList xs =
   unsafeCreate (Shape.ZeroBased $ length xs) $ flip pokeArray xs
