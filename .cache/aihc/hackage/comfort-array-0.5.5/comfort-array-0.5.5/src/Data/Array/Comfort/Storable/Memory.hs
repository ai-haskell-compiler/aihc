module Data.Array.Comfort.Storable.Memory where

import Foreign.Marshal.Array (advancePtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, peekElemOff)

import Control.Monad (foldM)

import Prelude hiding (foldl, foldl1)


{-# INLINE foldl #-}
foldl ::
   (Storable a) => (Int -> b -> a -> b) -> b -> Int -> Ptr a -> Int -> IO b
foldl op b n xPtr incx =
   foldM (\x k -> do y <- peekElemOff xPtr (k*incx); return $! op k x y) b $
   take n $ iterate (+1) 0

{-# INLINE foldl1 #-}
foldl1 ::
   (Storable a) =>
   (Int -> a -> b) -> (b -> b -> b) -> Int -> Ptr a -> Int -> IO b
foldl1 f op n xPtr incx =
   if n<1
      then error "foldl1: empty vector"
      else do
         x0 <- peek xPtr
         foldl (\k b a -> op b (f (k+1) a))
            (f 0 x0) (n-1) (advancePtr xPtr incx) incx
