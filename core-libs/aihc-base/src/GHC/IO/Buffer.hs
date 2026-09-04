{-# LANGUAGE NamedFieldPuns #-}

-- | Byte and character buffers for the handle layer. The layout follows
-- the GHC module of the same name.
module GHC.IO.Buffer
  ( Buffer (..),
    BufferState (..),
    CharBuffer,
    CharBufElem,
    newByteBuffer,
    newCharBuffer,
    newBuffer,
    emptyBuffer,
    bufferRemove,
    bufferAdd,
    slideContents,
    bufferAdjustL,
    bufferAddOffset,
    bufferAdjustOffset,
    isEmptyBuffer,
    isFullBuffer,
    isFullCharBuffer,
    isWriteBuffer,
    bufferElems,
    bufferAvailable,
    summaryBuffer,
    withBuffer,
    withRawBuffer,
    checkBuffer,
    RawBuffer,
    readWord8Buf,
    writeWord8Buf,
    RawCharBuffer,
    peekCharBuf,
    readCharBuf,
    writeCharBuf,
    readCharBufPtr,
    writeCharBufPtr,
    charSize,
  )
where

import Data.Bool (Bool (..), (&&))
import Foreign.Storable (Storable (..))
import GHC.Base (Monad (..), String, ($), (++))
import GHC.Err (errorWithoutStackTrace)
import GHC.ForeignPtr (ForeignPtr, castForeignPtr, mallocForeignPtrBytes, unsafeWithForeignPtr, withForeignPtr)
import GHC.IO (IO)
import GHC.Int (Int)
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Num (Num (..))
import GHC.Ptr (Ptr, castPtr, plusPtr)
import GHC.Real (fromIntegral)
import GHC.Show (Show (..))
import GHC.Types (Char)
import GHC.Word (Word64, Word8)

-- | A mutable array of bytes that a foreign pointer owns.
type RawBuffer e = ForeignPtr e

readWord8Buf :: RawBuffer Word8 -> Int -> IO Word8
readWord8Buf raw index = unsafeWithForeignPtr raw (`peekByteOff` index)

writeWord8Buf :: RawBuffer Word8 -> Int -> Word8 -> IO ()
writeWord8Buf raw index value = unsafeWithForeignPtr raw (\pointer -> pokeByteOff pointer index value)

type CharBufElem = Char

type RawCharBuffer = RawBuffer CharBufElem

peekCharBuf :: RawCharBuffer -> Int -> IO Char
peekCharBuf raw index =
  withForeignPtr raw $ \pointer -> do
    (character, _) <- readCharBufPtr pointer index
    return character

readCharBuf :: RawCharBuffer -> Int -> IO (Char, Int)
readCharBuf raw index = withForeignPtr raw (`readCharBufPtr` index)

writeCharBuf :: RawCharBuffer -> Int -> Char -> IO Int
writeCharBuf raw index character = withForeignPtr raw (\pointer -> writeCharBufPtr pointer index character)

readCharBufPtr :: Ptr CharBufElem -> Int -> IO (Char, Int)
readCharBufPtr pointer index = do
  character <- peekElemOff (castPtr pointer) index
  return (character, index + 1)

writeCharBufPtr :: Ptr CharBufElem -> Int -> Char -> IO Int
writeCharBufPtr pointer index character = do
  pokeElemOff (castPtr pointer) index character
  return (index + 1)

-- | The byte size of one character buffer element.
charSize :: Int
charSize = 4

data BufferState = ReadBuffer | WriteBuffer

instance Eq BufferState where
  ReadBuffer == ReadBuffer = True
  WriteBuffer == WriteBuffer = True
  _ == _ = False

-- | A region of a raw buffer. The elements between 'bufL' and 'bufR' are
-- live. 'bufOffset' is the device offset of the first element.
data Buffer e = Buffer
  { bufRaw :: !(RawBuffer e),
    bufState :: BufferState,
    bufSize :: !Int,
    bufOffset :: !Word64,
    bufL :: !Int,
    bufR :: !Int
  }

type CharBuffer = Buffer Char

withBuffer :: Buffer e -> (Ptr e -> IO a) -> IO a
withBuffer Buffer {bufRaw = raw} = withForeignPtr (castForeignPtr raw)

withRawBuffer :: RawBuffer e -> (Ptr e -> IO a) -> IO a
withRawBuffer raw = withForeignPtr (castForeignPtr raw)

isEmptyBuffer :: Buffer e -> Bool
isEmptyBuffer Buffer {bufL, bufR} = bufL == bufR

isFullBuffer :: Buffer e -> Bool
isFullBuffer Buffer {bufR, bufSize} = bufSize == bufR

isFullCharBuffer :: Buffer e -> Bool
isFullCharBuffer = isFullBuffer

isWriteBuffer :: Buffer e -> Bool
isWriteBuffer buffer =
  case bufState buffer of
    WriteBuffer -> True
    ReadBuffer -> False

bufferElems :: Buffer e -> Int
bufferElems Buffer {bufR, bufL} = bufR - bufL

bufferAvailable :: Buffer e -> Int
bufferAvailable Buffer {bufR, bufSize} = bufSize - bufR

bufferRemove :: Int -> Buffer e -> Buffer e
bufferRemove count buffer@Buffer {bufL} = bufferAdjustL (bufL + count) buffer

bufferAdjustL :: Int -> Buffer e -> Buffer e
bufferAdjustL left buffer@Buffer {bufR} =
  case left == bufR of
    True -> buffer {bufL = 0, bufR = 0}
    False -> buffer {bufL = left, bufR = bufR}

bufferAdd :: Int -> Buffer e -> Buffer e
bufferAdd count buffer@Buffer {bufR} = buffer {bufR = bufR + count}

emptyBuffer :: RawBuffer e -> Int -> BufferState -> Buffer e
emptyBuffer raw size state =
  Buffer {bufRaw = raw, bufState = state, bufOffset = 0, bufR = 0, bufL = 0, bufSize = size}

newByteBuffer :: Int -> BufferState -> IO (Buffer Word8)
newByteBuffer count = newBuffer count count

newCharBuffer :: Int -> BufferState -> IO CharBuffer
newCharBuffer count = newBuffer (count * charSize) count

newBuffer :: Int -> Int -> BufferState -> IO (Buffer e)
newBuffer bytes size state = do
  raw <- mallocForeignPtrBytes bytes
  return (emptyBuffer raw size state)

bufferAddOffset :: Int -> Buffer e -> Buffer e
bufferAddOffset offset buffer = buffer {bufOffset = bufOffset buffer + fromIntegral offset}

bufferAdjustOffset :: Word64 -> Buffer e -> Buffer e
bufferAdjustOffset offset buffer = buffer {bufOffset = offset}

-- | Move the live bytes to the start of the raw buffer.
slideContents :: Buffer Word8 -> IO (Buffer Word8)
slideContents buffer@Buffer {bufL, bufR, bufRaw} = do
  let count = bufR - bufL
  withRawBuffer bufRaw $ \pointer -> moveDown pointer bufL 0 count
  return buffer {bufL = 0, bufR = count}

-- | Copy @count@ bytes from a higher offset to a lower offset. The copy
-- goes upward, so the regions can overlap.
moveDown :: Ptr Word8 -> Int -> Int -> Int -> IO ()
moveDown pointer source target count =
  case count <= 0 of
    True -> return ()
    False -> do
      byte <- peekByteOff pointer source :: IO Word8
      pokeByteOff pointer target byte
      moveDown pointer (source + 1) (target + 1) (count - 1)

summaryBuffer :: Buffer a -> String
summaryBuffer buffer =
  "buf" ++ show (bufSize buffer) ++ "(" ++ show (bufL buffer) ++ "-" ++ show (bufR buffer) ++ ")"

-- | Stop the program when the buffer bounds are inconsistent.
checkBuffer :: Buffer a -> IO ()
checkBuffer buffer@Buffer {bufL, bufR, bufSize} =
  case 0 <= bufL && bufL <= bufR && bufR <= bufSize of
    True -> return ()
    False -> errorWithoutStackTrace ("checkBuffer: " ++ summaryBuffer buffer)
