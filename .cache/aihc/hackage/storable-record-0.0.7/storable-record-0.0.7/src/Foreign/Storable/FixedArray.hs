module Foreign.Storable.FixedArray where

import Control.Monad.Trans.State (StateT, evalStateT, get, put, )
import Control.Monad.Trans.Class (lift, )

import Foreign.Ptr (Ptr, castPtr, )
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (advancePtr, )


{-# INLINE roundUp #-}
roundUp :: Int -> Int -> Int
roundUp m x = x + mod (-x) m

{-# INLINE sizeOfArray #-}
sizeOfArray :: Storable a => Int -> a -> Int
sizeOfArray n x =
   n * roundUp (alignment x) (sizeOf x)

{-# INLINE pokeNext #-}
pokeNext :: (Storable a) => a -> StateT (Ptr a) IO ()
pokeNext x =
   do ptr <- get
      lift $ poke ptr x
      put (ptr `advancePtr` 1)
-- ToDo: put (ptr `plusPtr` roundUp (alignment x) (sizeOf x))

{-# INLINE peekNext #-}
peekNext :: (Storable a) => StateT (Ptr a) IO a
peekNext =
   do ptr <- get
      a <- lift $ peek ptr
      put (ptr `advancePtr` 1)
      return a

run :: Ptr (t a) -> StateT (Ptr a) IO c -> IO c
run ptr act =
   evalStateT act (castPtr ptr)
