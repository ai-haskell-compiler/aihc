{-# LANGUAGE CApiFFI #-}

-- | The position and the size of a file through its POSIX descriptor.
module GHC.IO.FD.Position
  ( descriptorSeekable,
    descriptorSeek,
    descriptorSize,
  )
where

import Data.Bool (Bool)
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt (..))
import GHC.Base (Monad (..), String)
import GHC.IO (IO)
import GHC.Int (Int64)
import GHC.Integer (Integer)
import GHC.Internal.Classes (Eq (..))
import GHC.Internal.IO.Types (SeekMode (..))
import GHC.Num (Num (..))
import GHC.Real (fromIntegral, toInteger)

-- | Whether the descriptor can move its position. A pipe, a socket, or a
-- terminal cannot.
descriptorSeekable :: CInt -> IO Bool
descriptorSeekable descriptor = do
  position <- c_lseek descriptor 0 seekCurrent
  return (position /= -1)

-- | Move the position of the descriptor and give the new position.
descriptorSeek :: String -> CInt -> SeekMode -> Integer -> IO Integer
descriptorSeek location descriptor mode offset = do
  let whence =
        case mode of
          AbsoluteSeek -> seekSet
          RelativeSeek -> seekCurrent
          SeekFromEnd -> seekEnd
  position <- throwErrnoIfMinus1Retry location (c_lseek descriptor (fromIntegral offset) whence)
  return (toInteger position)

-- | The size of the file in bytes. The descriptor moves to the end of the
-- file and then back to its position, so the next transfer starts where it
-- would start without this call.
descriptorSize :: String -> CInt -> IO Integer
descriptorSize location descriptor = do
  position <- throwErrnoIfMinus1Retry location (c_lseek descriptor 0 seekCurrent)
  end <- throwErrnoIfMinus1Retry location (c_lseek descriptor 0 seekEnd)
  _ <- throwErrnoIfMinus1Retry location (c_lseek descriptor position seekSet)
  return (toInteger end)

foreign import capi unsafe "unistd.h lseek"
  c_lseek :: CInt -> Int64 -> CInt -> IO Int64

foreign import capi unsafe "unistd.h value SEEK_SET"
  seekSet :: CInt

foreign import capi unsafe "unistd.h value SEEK_CUR"
  seekCurrent :: CInt

foreign import capi unsafe "unistd.h value SEEK_END"
  seekEnd :: CInt
