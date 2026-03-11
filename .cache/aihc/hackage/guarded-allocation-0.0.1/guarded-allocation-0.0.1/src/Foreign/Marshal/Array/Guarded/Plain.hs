module Foreign.Marshal.Array.Guarded.Plain (
   -- * immutable arrays
   create,
   alloca,
   -- * mutable arrays
   MutablePtr,
   new,
   withMutablePtr,
   freeze,
   freezeInplace,
   thaw,
   thawInplace,
   ) where

import Foreign.Marshal.Array (allocaArray, copyArray)
import Foreign.Storable (Storable)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, mallocForeignPtrArray)
import Foreign.Ptr (Ptr)

import Control.Applicative ((<$>))


create :: (Storable a) => Int -> (Ptr a -> IO b) -> IO (ForeignPtr a, b)
create size f = do
   fptr <- mallocForeignPtrArray size
   (,) fptr <$> withForeignPtr fptr f

alloca :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
alloca = allocaArray


newtype MutablePtr a = MutablePtr (ForeignPtr a)

new :: (Storable a) => Int -> IO (MutablePtr a)
new size = MutablePtr <$> mallocForeignPtrArray size

withMutablePtr :: MutablePtr a -> (Ptr a -> IO b) -> IO b
withMutablePtr (MutablePtr fptr) = withForeignPtr fptr

{- |
The 'size' parameter must match the size passed to 'new'.
This is not checked.
-}
freeze :: (Storable a) => Int -> MutablePtr a -> IO (ForeignPtr a)
freeze size (MutablePtr src) = copyToNew size src

{- |
'freezeInplace' must be the last operation on the 'MutablePtr'
and its associated array.
This is not checked.
-}
{-# INLINE freezeInplace #-}
freezeInplace :: (Storable a) => Int -> MutablePtr a -> IO (ForeignPtr a)
freezeInplace _ (MutablePtr fptr) = return fptr


thaw :: (Storable a) => Int -> ForeignPtr a -> IO (MutablePtr a)
thaw size src = MutablePtr <$> copyToNew size src

{-# INLINE thawInplace #-}
thawInplace :: (Storable a) => Int -> ForeignPtr a -> IO (MutablePtr a)
thawInplace _size fptr = return $ MutablePtr fptr


copyToNew :: (Storable a) => Int -> ForeignPtr a -> IO (ForeignPtr a)
copyToNew size src = do
   dst <- mallocForeignPtrArray size
   withForeignPtr dst $ \dstPtr ->
      withForeignPtr src $ \srcPtr ->
      copyArray dstPtr srcPtr size
   return dst
