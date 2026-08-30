{-# LANGUAGE MagicHash #-}

module Foreign.ForeignPtr
  ( ForeignPtr,
    mallocForeignPtrArray,
    withForeignPtr,
    touchForeignPtr,
  )
where

import Foreign.Storable (Storable)
import GHC.IO (IO)
import GHC.Int (Int)
import GHC.Prim (raise#)
import GHC.Ptr (Ptr)

data ForeignPtr a

mallocForeignPtrArray :: (Storable a) => Int -> IO (ForeignPtr a)
mallocForeignPtrArray _ = raise# ()

withForeignPtr :: ForeignPtr a -> (Ptr a -> IO b) -> IO b
withForeignPtr _ _ = raise# ()

touchForeignPtr :: ForeignPtr a -> IO ()
touchForeignPtr _ = raise# ()
