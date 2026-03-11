{- |
In principle you can traverse through a storable vector
using repeated calls to @viewL@ or using @index@.
However this needs a bit of pointer arrangement and allocation.
This data structure should make loops optimally fast.
-}
module Data.StorableVector.Pointer (
   Pointer(..),
   cons,
   viewL,
   switchL,
   ) where

-- import qualified Data.StorableVector as V
import qualified Data.StorableVector.Base as VB

import qualified Foreign.ForeignPtr as FPtr
import Foreign.Marshal.Array (advancePtr, )
import Foreign.Storable (Storable, peek, )
import Foreign (Ptr, )
import qualified System.Unsafe as Unsafe


{-
The reference to the ForeignPtr asserts,
that the array is maintained and thus is not garbage collected.
The Ptr we use for traversing would not achieve this.
-}
{- |
We might have name the data type iterator.
-}
data Pointer a =
   Pointer {
      fptr :: {-# UNPACK #-} !(FPtr.ForeignPtr a),
      ptr  :: {-# UNPACK #-} !(Ptr a),
      left :: {-# UNPACK #-} !Int
   }


{-# INLINE cons #-}
cons :: Storable a => VB.Vector a -> Pointer a
cons (VB.SV fp s l) =
   Pointer fp (advancePtr (Unsafe.foreignPtrToPtr fp) s) l


{-# INLINE viewL #-}
viewL :: Storable a => Pointer a -> Maybe (a, Pointer a)
viewL = switchL Nothing (curry Just)

{-# INLINE switchL #-}
switchL :: Storable a =>
   b -> (a -> Pointer a -> b) -> Pointer a -> b
switchL n j (Pointer fp p l) =
   if l<=0
     then n
     else j (VB.inlinePerformIO (peek p)) (Pointer fp (advancePtr p 1) (l-1))
-- Unsafe.performIO at this place would make SpeedPointer test 0.5 s slower
