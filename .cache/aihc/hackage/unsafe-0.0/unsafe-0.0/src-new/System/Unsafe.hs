module System.Unsafe where

-- taken from share/doc/ghc/html/libraries/base-4.5.1.0/doc-index-U.html
import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO, )
import Control.Monad.ST.Unsafe (unsafeInterleaveST, unsafeIOToST, unsafeSTToIO, )
import Foreign.ForeignPtr.Unsafe (unsafeForeignPtrToPtr, )
import Unsafe.Coerce (unsafeCoerce, )

import Control.Monad.ST (ST, )
import Foreign.ForeignPtr (ForeignPtr, )
import Foreign.Ptr (Ptr, )


performIO :: IO a -> a
performIO = unsafePerformIO

interleaveIO :: IO a -> IO a
interleaveIO = unsafeInterleaveIO

interleaveST :: ST s a -> ST s a
interleaveST = unsafeInterleaveST

ioToST :: IO a -> ST s a
ioToST = unsafeIOToST

stToIO :: ST s a -> IO a
stToIO = unsafeSTToIO

foreignPtrToPtr :: ForeignPtr a -> Ptr a
foreignPtrToPtr = unsafeForeignPtrToPtr

coerce :: a -> b
coerce = unsafeCoerce
