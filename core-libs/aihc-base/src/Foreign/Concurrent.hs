module Foreign.Concurrent
  ( newForeignPtr,
    addForeignPtrFinalizer,
  )
where

import GHC.ForeignPtr (ForeignPtr, addForeignPtrConcFinalizer, newConcForeignPtr)
import GHC.Ptr (Ptr)
import Prelude (IO)

newForeignPtr :: Ptr a -> IO () -> IO (ForeignPtr a)
newForeignPtr = newConcForeignPtr

addForeignPtrFinalizer :: ForeignPtr a -> IO () -> IO ()
addForeignPtrFinalizer = addForeignPtrConcFinalizer
