{-# LANGUAGE CPP #-}

module PtrPoker.Compat.ByteString (poke) where

import Data.ByteString.Internal
import Foreign.Marshal.Utils
import qualified PtrPoker.Compat.ForeignPtr as ForeignPtr
import PtrPoker.Prelude hiding (poke)

{-# INLINE poke #-}
poke :: ByteString -> Ptr Word8 -> IO (Ptr Word8)

#if MIN_VERSION_bytestring(0,11,0)

poke (BS fptr length) ptr =
  {-# SCC "poke" #-}
  ForeignPtr.unsafeWithForeignPtr fptr $ \ bytesPtr ->
    copyBytes ptr bytesPtr length $>
    plusPtr ptr length

#else

poke (PS fptr offset length) ptr =
  {-# SCC "poke" #-}
  ForeignPtr.unsafeWithForeignPtr fptr $ \ bytesPtr ->
    copyBytes ptr (plusPtr bytesPtr offset) length $>
    plusPtr ptr length

#endif
