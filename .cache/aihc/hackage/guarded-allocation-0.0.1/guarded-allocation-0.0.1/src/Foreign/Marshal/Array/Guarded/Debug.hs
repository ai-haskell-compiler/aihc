module Foreign.Marshal.Array.Guarded.Debug (
   -- * immutable arrays
   create,
   alloca,
   -- * mutable arrays
   MutablePtr,
   new,
   withMutablePtr,
   thaw,
   thawInplace,
   freeze,
   freezeInplace,
   ) where

import qualified Foreign.Marshal.Array.Guarded.Plain as Plain

import Foreign.Marshal.Array
         (mallocArray, allocaArray, pokeArray, copyArray, advancePtr)
import Foreign.Marshal.Alloc (free)
import Foreign.Storable (Storable, peekByteOff, sizeOf)
import Foreign.Concurrent (newForeignPtr, addForeignPtrFinalizer)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr)

import Control.Monad (when)
import Control.Applicative ((<$>))

import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Foldable (for_)
import Data.Word (Word8)

import Text.Printf (printf)


{- |
Array creation with additional immutability check, electrical fence
and pollution of uncleaned memory.

The function checks that the array is not altered anymore after creation.
-}
create :: (Storable a) => Int -> (Ptr a -> IO b) -> IO (ForeignPtr a, b)
create = flip asTypeOf Plain.create $ \size f -> do
   let border = 64
   let fullSize = size + 2*border
   ptrApre <- mallocArray fullSize
   ptrsA@(_ptrApre, ptrA, _ptrApost) <- fillAll border size ptrApre
   result <- f ptrA
   checkAll border ptrsA
   ptrB <- copyToNew size ptrA
   fmap (flip (,) result) $ newForeignPtr ptrA $ do
      verify size ptrA ptrB
      trash fullSize ptrApre
      free ptrApre
      free ptrB


alloca :: (Storable a) => Int -> (Ptr a -> IO b) -> IO b
alloca = flip asTypeOf Plain.alloca $ \size f -> do
   let border = 64
   let fullSize = size + 2*border
   allocaArray fullSize $ \ptrPre -> do
      ptrs@(_ptrPre, ptr, _ptrPost) <- fillAll border size ptrPre
      result <- f ptr
      checkAll border ptrs
      trash fullSize ptrPre
      return result



data MutablePtr a =
   MutablePtr {
      _mutableSize :: !Int,
      _mutableFPtr :: !(ForeignPtr a),
      _mutableFrozenRef :: !(IORef Bool)
   }

new :: (Storable a) => Int -> IO (MutablePtr a)
new size = do
   let border = 64
   let fullSize = size + 2*border
   ptrApre <- mallocArray fullSize
   ptrsA <- fillAll border size ptrApre
   newMutablePtr size =<< newCheckedForeignPtr border fullSize ptrsA


withMutablePtr :: MutablePtr a -> (Ptr a -> IO b) -> IO b
withMutablePtr (MutablePtr _size fptr frozenRef) f = do
   checkFrozen "withMutablePtr" frozenRef
   withForeignPtr fptr f

thaw :: (Storable a) => Int -> ForeignPtr a -> IO (MutablePtr a)
thaw size fptrB = do
   let border = 64
   let fullSize = size + 2*border
   ptrApre <- mallocArray fullSize
   ptrsA@(_ptrApre, ptrA, _ptrApost) <- fillBorder border size ptrApre
   withForeignPtr fptrB $ \ptrB -> copyArray ptrA ptrB size
   newMutablePtr size =<< newCheckedForeignPtr border fullSize ptrsA

newCheckedForeignPtr ::
   (Storable a) => Int -> Int -> (Ptr a, Ptr a, Ptr a) -> IO (ForeignPtr a)
newCheckedForeignPtr border fullSize ptrsA@(ptrApre, ptrA, _ptrApost) =
   newForeignPtr ptrA $ do
      checkAll border ptrsA
      trash fullSize ptrApre
      free ptrApre

{- |
There is not much we can debug here.
We cannot add a fence since we do not copy the immutable array.
We could debug more
if we would implement 'thawInplace' as a redirection to 'thaw'.
-}
thawInplace :: (Storable a) => Int -> ForeignPtr a -> IO (MutablePtr a)
thawInplace = newMutablePtr

{- |
The 'size' parameter must match the size passed to 'new'.
This is checked.
-}
freeze :: (Storable a) => Int -> MutablePtr a -> IO (ForeignPtr a)
freeze freezeSize (MutablePtr size fptr frozenRef) = do
   checkSize freezeSize size
   checkFrozen "freeze" frozenRef
   ptrA <- withForeignPtr fptr $ copyToNew size
   ptrB <- withForeignPtr fptr $ copyToNew size
   newForeignPtr ptrA $ do
      verify size ptrA ptrB
      trash size ptrA
      free ptrA
      free ptrB

{- |
'freezeInplace' must be the last operation on the 'MutablePtr'
and its associated array.
The 'size' parameter must match the size passed to 'new'.
This is checked.

It may fail if the 'MutablePtr' was constructed using 'thawInplace'
from a 'ForeignPtr' with a C finalizer.
-}
freezeInplace :: (Storable a) => Int -> MutablePtr a -> IO (ForeignPtr a)
freezeInplace freezeSize (MutablePtr size fptr frozenRef) = do
   checkSize freezeSize size
   checkFrozen "freezeInplace" frozenRef
   writeIORef frozenRef True

   withForeignPtr fptr $ \ptrA -> do
      ptrB <- copyToNew size ptrA
      addForeignPtrFinalizer fptr $ do
         verify size ptrA ptrB
         free ptrB
   return fptr


newMutablePtr :: Int -> ForeignPtr a -> IO (MutablePtr a)
newMutablePtr size fptr = MutablePtr size fptr <$> newIORef False

checkSize :: Int -> Int -> IO ()
checkSize freezeSize size =
   when (freezeSize/=size) $ error $
      printf "allocation (%d) and freeze (%d) size differ" size freezeSize

checkFrozen :: String -> IORef Bool -> IO ()
checkFrozen name frozenRef =
   flip when (error $ name ++ ": array already frozen")
      =<< readIORef frozenRef



fillAll :: (Storable a) => Int -> Int -> Ptr a -> IO (Ptr a, Ptr a, Ptr a)
fillAll border size ptrPre = do
   ptrs@(_ptrPre, ptr, _ptrPost) <- fillBorder border size ptrPre
   fill ptr size [0xDE,0xAD,0xF0,0x0D]
   return ptrs

fillBorder :: (Storable a) => Int -> Int -> Ptr a -> IO (Ptr a, Ptr a, Ptr a)
fillBorder border size ptrPre = do
   let ptr = advancePtr ptrPre border
   let ptrPost = advancePtr ptr size
   fill ptrPre border [0xAB,0xAD,0xCA,0xFE]
   fill ptrPost border [0xAB,0xAD,0xCA,0xFE]
   return (ptrPre, ptr, ptrPost)

checkAll :: (Storable a) => Int -> (Ptr a, Ptr a, Ptr a) -> IO ()
checkAll border (ptrPre, _ptr, ptrPost) = do
   check "leading"  ptrPre  border [0xAB,0xAD,0xCA,0xFE]
   check "trailing" ptrPost border [0xAB,0xAD,0xCA,0xFE]

trash :: (Storable a) => Int -> Ptr a -> IO ()
trash fullSize ptrPre = fill ptrPre fullSize [0xDE,0xAD,0xBE,0xEF]


{-# INLINE fill #-}
fill :: (Storable a) => Ptr a -> Int -> [Word8] -> IO ()
fill ptr n bytes =
   pokeArray (castPtr ptr) $ take (arraySize ptr n) $ cycle bytes

{-# INLINE check #-}
check :: (Storable a) => String -> Ptr a -> Int -> [Word8] -> IO ()
check name ptr n bytes =
   for_ (take (arraySize ptr n) $ zip [0..] $ cycle bytes) $ \(i,b) -> do
      a <- peekByteOff ptr i
      when (a/=(b::Word8)) $
         error $ "damaged " ++ name ++ " fence at position " ++ show i

copyToNew :: (Storable a) => Int -> Ptr a -> IO (Ptr a)
copyToNew size ptrA = do
   ptrB <- mallocArray size
   copyArray ptrB ptrA size
   return ptrB

verify :: (Storable a) => Int -> Ptr a -> Ptr a -> IO ()
verify size ptrA ptrB =
   for_ (take (arraySize ptrA size) [0..]) $ \i -> do
      a <- peekByteOff ptrA i
      b <- peekByteOff ptrB i
      when (a/=(b::Word8)) $
         error $ "immutable array was altered at byte position " ++ show i

arraySize :: (Storable a) => Ptr a -> Int -> Int
arraySize ptr n = arraySizeAux ptr n $ error "arraySize: undefined element"

{- |
Correct size computation should also respect padding caused by alignment.
However, mallocArray uses this simple arithmetic.
-}
arraySizeAux :: (Storable a) => Ptr a -> Int -> a -> Int
arraySizeAux _ n a = n * sizeOf a
