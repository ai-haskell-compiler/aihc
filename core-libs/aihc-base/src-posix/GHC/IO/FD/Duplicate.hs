{-# LANGUAGE CApiFFI #-}

-- | A second POSIX descriptor for the open file of a descriptor.
module GHC.IO.FD.Duplicate
  ( descriptorDup,
    descriptorDup2,
  )
where

import Foreign.C.Error (throwErrnoIfMinus1Retry, throwErrnoIfMinus1Retry_)
import Foreign.C.Types (CInt (..))
import GHC.IO (IO)
import GHC.Internal.Base (String)

-- | Give a new descriptor for the open file of the descriptor.
descriptorDup :: String -> CInt -> IO CInt
descriptorDup location descriptor = throwErrnoIfMinus1Retry location (c_dup descriptor)

-- | Make the second descriptor refer to the open file of the first one.
-- The system closes the file that the second descriptor had.
descriptorDup2 :: String -> CInt -> CInt -> IO ()
descriptorDup2 location source target = throwErrnoIfMinus1Retry_ location (c_dup2 source target)

foreign import capi unsafe "unistd.h dup"
  c_dup :: CInt -> IO CInt

foreign import capi unsafe "unistd.h dup2"
  c_dup2 :: CInt -> CInt -> IO CInt
