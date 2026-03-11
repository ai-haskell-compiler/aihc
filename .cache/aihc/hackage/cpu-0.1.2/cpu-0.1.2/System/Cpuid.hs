{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE CPP #-}
-- |
-- Module      : System.Cpuid
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unknown
--
module System.Cpuid
    ( cpuidWithIndex
    , cpuid
    ) where

import Data.Word
import Control.Applicative
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Alloc

#if defined(ARCH_X86) || defined(ARCH_X86_64)

foreign import ccall safe "cpuid" c_cpuid :: CUInt -> CUInt -> Ptr CUInt -> IO ()

-- | complete cpuid call with eax and ecx set.
cpuidWithIndex :: Word32 -> Word32 -> IO (Word32, Word32, Word32, Word32)
cpuidWithIndex eax ecx = allocaBytes 16 $ \ptr -> do
    c_cpuid (fromIntegral eax) (fromIntegral ecx) ptr
    (,,,) <$> peekW32 ptr <*> peekW32 (ptr `plusPtr` 4) <*> peekW32 (ptr `plusPtr` 8) <*> peekW32 (ptr `plusPtr` 12)
    where peekW32 :: Ptr CUInt -> IO Word32
          peekW32 ptr = fromIntegral <$> peek ptr

#else

cpuidWithIndex :: Word32 -> Word32 -> IO (Word32, Word32, Word32, Word32)
cpuidWithIndex _ _ = error "cpuid is not supported on non-x86 architecture"

#endif

-- | simple cpuid call.
cpuid :: Word32 -> IO (Word32, Word32, Word32, Word32)
cpuid eax = cpuidWithIndex eax 0
