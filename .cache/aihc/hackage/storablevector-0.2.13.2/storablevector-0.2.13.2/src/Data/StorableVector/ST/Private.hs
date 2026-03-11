{- |
Module      : Data.StorableVector.ST.Strict
License     : BSD-style
Maintainer  : haskell@henning-thielemann.de
Stability   : experimental
Portability : portable, requires ffi
Tested with : GHC 6.4.1

-}
module Data.StorableVector.ST.Private where

import qualified Data.StorableVector.Base as V

import Data.StorableVector.Memory (mallocForeignPtrArray, )

import Control.Monad.ST.Strict (ST, )

import Foreign.Ptr        (Ptr, )
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, )
import Foreign.Storable   (Storable, )

import qualified System.Unsafe as Unsafe

-- import Prelude (Int, ($), (+), return, const, )
import Prelude hiding (read, length, )


data Vector s a =
   SV {-# UNPACK #-} !(ForeignPtr a)
      {-# UNPACK #-} !Int                -- length


-- | Wrapper of mallocForeignPtrArray.
create :: (Storable a) => Int -> (Ptr a -> IO ()) -> IO (Vector s a)
create l f = do
    fp <- mallocForeignPtrArray l
    withForeignPtr fp f
    return $! SV fp l

{-# INLINE unsafeCreate #-}
unsafeCreate :: (Storable a) => Int -> (Ptr a -> IO ()) -> ST s (Vector s a)
unsafeCreate l f = Unsafe.ioToST $ create l f

{-
This function must be in ST monad,
since it is usually called
as termination of a series of write accesses to the vector.
We must assert that no read access to the V.Vector can happen
before the end of the write accesses.
(And the caller must assert, that he actually never writes again into that vector.)
-}
{-# INLINE unsafeToVector #-}
unsafeToVector :: Vector s a -> ST s (V.Vector a)
unsafeToVector (SV x l) = return (V.SV x 0 l)
