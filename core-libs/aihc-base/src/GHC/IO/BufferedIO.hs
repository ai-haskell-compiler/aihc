{-# LANGUAGE NamedFieldPuns #-}

-- | Buffered devices. The class lives in "GHC.Internal.IO.Types".
module GHC.IO.BufferedIO
  ( BufferedIO (..),
    readBuf,
    readBufNonBlocking,
    writeBuf,
    writeBufNonBlocking,
  )
where

import Data.Maybe (Maybe (..))
import GHC.Base (Monad (..), ($))
import GHC.IO (IO)
import GHC.IO.Buffer (Buffer (..), bufferAvailable, bufferElems, withBuffer)
import GHC.Int (Int)
import GHC.Internal.IO.Types (BufferedIO (..), RawIO (..))
import GHC.Num (Num (..))
import GHC.Ptr (plusPtr)
import GHC.Real (fromIntegral)
import GHC.Word (Word8)

readBuf :: (RawIO dev) => dev -> Buffer Word8 -> IO (Int, Buffer Word8)
readBuf device buffer@Buffer {bufR, bufOffset} = do
  let bytes = bufferAvailable buffer
  count <- withBuffer buffer $ \pointer -> read device (pointer `plusPtr` bufR) (bufOffset + fromIntegral bufR) bytes
  return (count, buffer {bufR = bufR + count})

readBufNonBlocking :: (RawIO dev) => dev -> Buffer Word8 -> IO (Maybe Int, Buffer Word8)
readBufNonBlocking device buffer@Buffer {bufR, bufOffset} = do
  let bytes = bufferAvailable buffer
  result <- withBuffer buffer $ \pointer -> readNonBlocking device (pointer `plusPtr` bufR) (bufOffset + fromIntegral bufR) bytes
  case result of
    Nothing -> return (Nothing, buffer)
    Just count -> return (Just count, buffer {bufR = bufR + count})

writeBuf :: (RawIO dev) => dev -> Buffer Word8 -> IO (Buffer Word8)
writeBuf device buffer@Buffer {bufL, bufOffset} = do
  let bytes = bufferElems buffer
  withBuffer buffer $ \pointer -> write device (pointer `plusPtr` bufL) bufOffset bytes
  return buffer {bufL = 0, bufR = 0, bufOffset = bufOffset + fromIntegral bytes}

writeBufNonBlocking :: (RawIO dev) => dev -> Buffer Word8 -> IO (Int, Buffer Word8)
writeBufNonBlocking device buffer@Buffer {bufL, bufOffset} = do
  let bytes = bufferElems buffer
  written <- withBuffer buffer $ \pointer -> writeNonBlocking device (pointer `plusPtr` bufL) bufOffset bytes
  return (written, buffer {bufL = bufL + written, bufOffset = bufOffset + fromIntegral written})
