{-# LANGUAGE CApiFFI #-}

-- | The position and the size of a file through its POSIX descriptor.
module GHC.IO.FD.Position
  ( descriptorSeekable,
    descriptorSeek,
    descriptorSize,
  )
where

import Data.Bool (Bool (..))
import Foreign.C.Error (throwErrnoIfMinus1Retry)
import Foreign.C.Types (CInt (..))
import GHC.IO (IO)
import GHC.Int (Int64)
import GHC.Integer (Integer)
import GHC.Internal.Base (Maybe (..), Monad (..), String)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (IOErrorType (..), IOException (..), SeekMode (..), ioException)
import GHC.Internal.Num (Num (..))
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

-- | The size of a regular file in bytes, from one fstat call.
-- Another descriptor, such as a pipe, has no size. Then the error type is
-- 'InappropriateType', as in GHC, so that a caller can catch it.
descriptorSize :: String -> CInt -> IO Integer
descriptorSize location descriptor = do
  size <- c_regular_file_size descriptor
  case size >= 0 of
    True -> return (toInteger size)
    False -> ioException (IOError Nothing InappropriateType location "not a regular file" Nothing Nothing)

foreign import capi unsafe "aihc_stat.h aihc_regular_file_size"
  c_regular_file_size :: CInt -> IO Int64

foreign import capi unsafe "unistd.h lseek"
  c_lseek :: CInt -> Int64 -> CInt -> IO Int64

foreign import capi unsafe "unistd.h value SEEK_SET"
  seekSet :: CInt

foreign import capi unsafe "unistd.h value SEEK_CUR"
  seekCurrent :: CInt

foreign import capi unsafe "unistd.h value SEEK_END"
  seekEnd :: CInt
