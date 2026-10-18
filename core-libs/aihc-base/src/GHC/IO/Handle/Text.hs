{-# LANGUAGE MagicHash #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Text and block transfers on handles. Text goes through the byte
-- buffer as UTF-8.
module GHC.IO.Handle.Text
  ( hPutChar,
    hPutStr,
    hPutStrLn,
    hGetBuf,
    hGetBufSome,
    hGetBufNonBlocking,
    hPutBuf,
    hPutBufNonBlocking,
    hGetChar,
    hGetLine,
    hGetContents,
    hGetContents',
    hWaitForInput,
    commitBuffer',
  )
where

import Data.Bool (Bool (..), not, (&&))
import Data.Maybe (Maybe (..), maybe)
import Foreign.Marshal.Utils (copyBytes)
import GHC.Base (Monad (..), String, otherwise, ($), (++), (.))
import GHC.Char (chr)
import GHC.IO (IO (..))
import GHC.IO.Buffer
  ( Buffer (..),
    BufferState (..),
    CharBuffer,
    RawBuffer,
    RawCharBuffer,
    bufferAvailable,
    bufferElems,
    emptyBuffer,
    isEmptyBuffer,
    readCharBuf,
    readWord8Buf,
    withRawBuffer,
  )
import GHC.IO.Handle.Internals
  ( flushByteWriteBuffer,
    flushCharReadBuffer,
    flushWriteBuffer,
    ioe_EOF,
    wantReadableHandle,
    wantReadableHandle_,
    wantWritableHandle,
  )
import GHC.IO.Unsafe (unsafeInterleaveIO)
import GHC.IORef (readIORef, writeIORef)
import GHC.Int (Int (..))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Handle.Read (fillIfEmpty, readByte, readUtf8Char)
import GHC.Internal.IO.Types
  ( BufferMode (..),
    Handle (..),
    HandleType (..),
    Handle__ (..),
    IOErrorType (..),
    IOException (..),
    ioException,
  )
import GHC.Internal.IO.Types qualified as Buffered (fillReadBuffer, flushWriteBuffer)
import GHC.Internal.IO.Types qualified as RawIO (read, write)
import GHC.Num (Num (..))
import GHC.Prim
  ( Addr#,
    Int#,
    RealWorld,
    State#,
    andI#,
    chr#,
    int2Word#,
    orI#,
    ord#,
    readWord8OffAddr#,
    uncheckedIShiftRA#,
    word2Int#,
    word8ToWord#,
    wordToWord8#,
    writeWord8OffAddr#,
    (*#),
    (+#),
    (-#),
    (<#),
    (==#),
    (>#),
    (>=#),
  )
import GHC.Ptr (Ptr (..), castPtr, plusPtr)
import GHC.Real (Integral (..), fromIntegral)
import GHC.Show (Show (..))
import GHC.Types (Char (..), isTrue#)
import GHC.Word (Word8)

-- ---------------------------------------------------------------------
-- Text output

hPutChar :: Handle -> Char -> IO ()
hPutChar handle character =
  wantWritableHandle "hPutChar" handle $ \handle_ -> do
    writeChars handle_ [character]
    flushForMode handle_ (character == '\n')

hPutStr :: Handle -> String -> IO ()
hPutStr handle text = hPutStr' handle text False

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn handle text = hPutStr' handle text True

hPutStr' :: Handle -> String -> Bool -> IO ()
hPutStr' handle text addNewline =
  wantWritableHandle "hPutStr" handle $ \handle_ -> do
    newline <- writeChars handle_ text
    case addNewline of
      True -> do
        _ <- writeChars handle_ "\n"
        flushForMode handle_ True
      False -> flushForMode handle_ newline

-- | Flush the byte buffer when the buffer mode asks for it.
flushForMode :: Handle__ -> Bool -> IO ()
flushForMode handle_@Handle__ {haBufferMode} sawNewline =
  case haBufferMode of
    NoBuffering -> flushByteWriteBuffer handle_
    LineBuffering ->
      case sawNewline of
        True -> flushByteWriteBuffer handle_
        False -> return ()
    BlockBuffering _ -> return ()

-- | Encode the characters as UTF-8 into the byte buffer. The result says
-- whether the text had a newline. The loop writes the buffer record back
-- only before a flush and at the end.
writeChars :: Handle__ -> String -> IO Bool
writeChars handle_@Handle__ {haByteBuffer} = fill False
  where
    fill newline [] = return newline
    fill newline text = do
      buffer <- readIORef haByteBuffer
      buffer' <-
        case bufferAvailable buffer < 4 of
          True -> do
            flushByteWriteBuffer handle_
            readIORef haByteBuffer
          False -> return buffer
      Encoded rest offset newline' <-
        withRawBuffer (bufRaw buffer') $ \pointer ->
          encodeChars pointer (bufSize buffer' - 4) (bufR buffer') newline text
      writeIORef haByteBuffer buffer' {bufR = offset}
      fill newline' rest

-- | The result of 'encodeChars': the characters that did not fit, the
-- next offset, and whether the encoded characters had a newline.
data Encoded = Encoded String !Int !Bool

-- | Encode characters as UTF-8 at @offset@ until the text ends or the
-- offset is past @limit@. One character needs at most four bytes, so the
-- buffer must have four bytes after @limit@.
encodeChars :: Ptr Word8 -> Int -> Int -> Bool -> String -> IO Encoded
encodeChars (Ptr address) (I# limit) (I# offset) newline text =
  IO
    ( \state -> case encodeChars# address limit offset (newlineFlag newline) text state of
        (# state', rest, offset', newline' #) -> (# state', Encoded rest (I# offset') (isTrue# newline') #)
    )
  where
    newlineFlag True = 1#
    newlineFlag False = 0#

encodeChars# :: Addr# -> Int# -> Int# -> Int# -> String -> State# RealWorld -> (# State# RealWorld, String, Int#, Int# #)
encodeChars# address limit offset newline text state =
  case text of
    [] -> (# state, [], offset, newline #)
    C# character : rest -> case offset ># limit of
      1# -> (# state, text, offset, newline #)
      _ ->
        let code = ord# character
         in case encodeUtf8# address offset code state of
              (# state', offset' #) -> encodeChars# address limit offset' (newline `orI#` (code ==# 10#)) rest state'

-- | Write one code point as UTF-8 and give the next offset.
encodeUtf8# :: Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
encodeUtf8# address offset code state0 =
  case code <# 0x80# of
    1# -> case byte 0# code state0 of
      state1 -> (# state1, offset +# 1# #)
    _ -> case code <# 0x800# of
      1# -> case byte 0# (0xC0# +# shifted 6#) state0 of
        state1 -> case byte 1# (continuation 0#) state1 of
          state2 -> (# state2, offset +# 2# #)
      _ -> case code <# 0x10000# of
        1# -> case byte 0# (0xE0# +# shifted 12#) state0 of
          state1 -> case byte 1# (continuation 6#) state1 of
            state2 -> case byte 2# (continuation 0#) state2 of
              state3 -> (# state3, offset +# 3# #)
        _ -> case byte 0# (0xF0# +# shifted 18#) state0 of
          state1 -> case byte 1# (continuation 12#) state1 of
            state2 -> case byte 2# (continuation 6#) state2 of
              state3 -> case byte 3# (continuation 0#) state3 of
                state4 -> (# state4, offset +# 4# #)
  where
    shifted count = code `uncheckedIShiftRA#` count
    continuation count = 0x80# +# (shifted count `andI#` 63#)
    byte index value = writeWord8OffAddr# address (offset +# index) (wordToWord8# (int2Word# value))

-- | Encode the characters that a raw character buffer holds and hand the
-- buffer back for reuse. @sz@ is the size of the raw buffer and @count@
-- the number of characters in it; @flush@ asks for the byte buffer to
-- reach the device, and @release@ says the caller is done with the raw
-- buffer. The handle must be writable.
commitBuffer' :: RawCharBuffer -> Int -> Int -> Bool -> Bool -> Handle__ -> IO CharBuffer
commitBuffer' raw size count flush _release handle_ = do
  writeRawChars handle_ raw 0 count
  case flush of
    True -> flushByteWriteBuffer handle_
    False -> return ()
  return (emptyBuffer raw size WriteBuffer)

-- | Encode the first @count@ characters of a raw character buffer into
-- the byte buffer of a handle.
writeRawChars :: Handle__ -> RawCharBuffer -> Int -> Int -> IO ()
writeRawChars handle_ raw index count =
  case index >= count of
    True -> return ()
    False -> do
      (character, next) <- readCharBuf raw index
      _ <- writeChars handle_ [character]
      writeRawChars handle_ raw next count

-- ---------------------------------------------------------------------
-- Text input

-- | Read one character. The bytes are UTF-8.
hGetChar :: Handle -> IO Char
hGetChar handle =
  wantReadableHandle_ "hGetChar" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    result <- readUtf8Char handle_
    maybe ioe_EOF return result

hGetLine :: Handle -> IO String
hGetLine handle =
  wantReadableHandle_ "hGetLine" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    first <- readUtf8Char handle_
    case first of
      Nothing -> ioe_EOF
      Just character -> readLine handle_ character

readLine :: Handle__ -> Char -> IO String
readLine handle_ character =
  case character == '\n' of
    True -> return []
    False -> do
      next <- readUtf8Char handle_
      case next of
        Nothing -> return [character]
        Just following -> do
          rest <- readLine handle_ following
          return (character : rest)

-- | Read the rest of the input lazily and semi-close the handle.
hGetContents :: Handle -> IO String
hGetContents handle =
  wantReadableHandle "hGetContents" handle $ \handle_ -> do
    text <- lazyRead handle_
    return (handle_ {haType = SemiClosedHandle}, text)

-- | Each step decodes all the complete characters in the byte buffer, as
-- the GHC 'hGetContents' does. Thus a step, and its 'unsafeInterleaveIO',
-- is for one buffer and not for one character.
lazyRead :: Handle__ -> IO String
lazyRead handle_ = unsafeInterleaveIO (lazyReadBuffer handle_)

lazyReadBuffer :: Handle__ -> IO String
lazyReadBuffer handle_@Handle__ {haByteBuffer} = do
  filled <- fillIfEmpty handle_
  case filled of
    False -> return []
    True -> do
      buffer@Buffer {bufRaw, bufL, bufR} <- readIORef haByteBuffer
      rest <- lazyRead handle_
      withRawBuffer bufRaw $ \pointer -> do
        stop <- completeUtf8 pointer bufL bufR
        case stop == bufL of
          -- The bytes of the first character are not all in the buffer.
          -- 'readUtf8Char' reads them across the refill.
          True -> do
            next <- readUtf8Char handle_
            return (maybe [] (: rest) next)
          False -> do
            writeIORef haByteBuffer buffer {bufL = stop}
            decodeUtf8 pointer bufL stop rest

-- | The offset after the last complete UTF-8 character that starts at or
-- after @index@ and ends before @end@.
completeUtf8 :: Ptr Word8 -> Int -> Int -> IO Int
completeUtf8 (Ptr address) (I# index) (I# end) =
  IO
    ( \state -> case completeUtf8# address index end state of
        (# state', stop #) -> (# state', I# stop #)
    )

completeUtf8# :: Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
completeUtf8# address index end state =
  case index >=# end of
    1# -> (# state, index #)
    _ -> case readUtf8Byte# address index state of
      (# state1, leading #) -> case (index +# utf8Length# leading) ># end of
        1# -> (# state1, index #)
        _ -> case readUtf8# address index leading state1 of
          (# state2, _, next #) -> completeUtf8# address next end state2

-- | Decode the UTF-8 characters from @index@ to @stop@ in front of @rest@.
-- All the bytes of each character must be before @stop@. The decoding is
-- the one of 'readUtf8Char'.
decodeUtf8 :: Ptr Word8 -> Int -> Int -> String -> IO String
decodeUtf8 (Ptr address) (I# index) (I# stop) rest = IO (decodeUtf8# address index stop rest)

decodeUtf8# :: Addr# -> Int# -> Int# -> String -> State# RealWorld -> (# State# RealWorld, String #)
decodeUtf8# address index stop rest state =
  case index >=# stop of
    1# -> (# state, rest #)
    _ -> case readUtf8Byte# address index state of
      (# state1, leading #) -> case readUtf8# address index leading state1 of
        (# state2, code, next #) -> case decodeUtf8# address next stop rest state2 of
          (# state3, text #) -> (# state3, C# (chr# code) : text #)

readUtf8Byte# :: Addr# -> Int# -> State# RealWorld -> (# State# RealWorld, Int# #)
readUtf8Byte# address index state =
  case readWord8OffAddr# address index state of
    (# state', value #) -> (# state', word2Int# (word8ToWord# value) #)

-- | The number of bytes that a leading byte asks for.
utf8Length# :: Int# -> Int#
utf8Length# leading =
  case leading <# 0xC0# of
    1# -> 1#
    _ -> case leading <# 0xE0# of
      1# -> 2#
      _ -> case leading <# 0xF0# of
        1# -> 3#
        _ -> 4#

-- | The code point of the character at @index@ and the offset after it. A
-- bad sequence gives the replacement character, and a bad continuation
-- byte ends the character, as in 'readUtf8Char'.
readUtf8# :: Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Int#, Int# #)
readUtf8# address index leading state =
  case leading <# 0x80# of
    1# -> (# state, leading, index +# 1# #)
    _ -> case leading <# 0xC0# of
      1# -> (# state, 0xFFFD#, index +# 1# #)
      _ -> case leading <# 0xE0# of
        1# -> continuation (leading -# 0xC0#) (index +# 1#) 1# state
        _ -> case leading <# 0xF0# of
          1# -> continuation (leading -# 0xE0#) (index +# 1#) 2# state
          _ -> continuation (leading -# 0xF0#) (index +# 1#) 3# state
  where
    continuation code next count state0 =
      case count of
        0# -> (# state0, code, next #)
        _ -> case readUtf8Byte# address next state0 of
          (# state1, byte #) -> case (byte >=# 0x80#) `andI#` (byte <# 0xC0#) of
            1# -> continuation ((code *# 64#) +# (byte -# 0x80#)) (next +# 1#) (count -# 1#) state1
            _ -> (# state1, 0xFFFD#, next +# 1# #)

-- | Read the rest of the input strictly.
hGetContents' :: Handle -> IO String
hGetContents' handle =
  wantReadableHandle_ "hGetContents'" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    strictRead handle_

strictRead :: Handle__ -> IO String
strictRead handle_ = do
  next <- readUtf8Char handle_
  case next of
    Nothing -> return []
    Just character -> do
      rest <- strictRead handle_
      return (character : rest)

-- | Input is always ready or at the end.
hWaitForInput :: Handle -> Int -> IO Bool
hWaitForInput handle _ =
  wantReadableHandle_ "hWaitForInput" handle $ \handle_ -> do
    flushCharReadBuffer handle_
    fillIfEmpty handle_

-- ---------------------------------------------------------------------
-- Block input

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf handle pointer count
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hGetBuf" count
  | otherwise =
      wantReadableHandle_ "hGetBuf" handle $ \handle_@Handle__ {haByteBuffer} -> do
        flushCharReadBuffer handle_
        buffer <- readIORef haByteBuffer
        case isEmptyBuffer buffer of
          True -> bufReadEmpty handle_ buffer (castPtr pointer) 0 count
          False -> bufReadNonEmpty handle_ buffer (castPtr pointer) 0 count

bufReadNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNonEmpty handle_@Handle__ {haByteBuffer} buffer@Buffer {bufRaw, bufR, bufL} pointer soFar count = do
  let available = bufR - bufL
  case count < available of
    True -> do
      copyFromRawBuffer pointer bufRaw bufL count
      writeIORef haByteBuffer buffer {bufL = bufL + count}
      return (soFar + count)
    False -> do
      copyFromRawBuffer pointer bufRaw bufL available
      let buffer' = buffer {bufR = 0, bufL = 0}
      writeIORef haByteBuffer buffer'
      let remaining = count - available
          soFar' = soFar + available
          pointer' = pointer `plusPtr` available
      case remaining == 0 of
        True -> return soFar'
        False -> bufReadEmpty handle_ buffer' pointer' soFar' remaining

bufReadEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadEmpty handle_@Handle__ {haByteBuffer, haDevice} buffer pointer soFar count
  -- A read of at least the size of the buffer goes to the device
  -- directly, as GHC does it. The device fills as much of the request as
  -- it can in one read; the loop asks again for the rest until the
  -- request is complete or the input ends.
  | count >= bufSize buffer = do
      transferred <- RawIO.read haDevice pointer 0 count
      case transferred == 0 of
        True -> return soFar
        False ->
          case transferred == count of
            True -> return (soFar + count)
            False -> bufReadEmpty handle_ buffer (pointer `plusPtr` transferred) (soFar + transferred) (count - transferred)
  | otherwise = do
      (transferred, buffer') <- Buffered.fillReadBuffer haDevice buffer
      case transferred == 0 of
        True -> return soFar
        False -> do
          writeIORef haByteBuffer buffer'
          bufReadNonEmpty handle_ buffer' pointer soFar count

hGetBufSome :: Handle -> Ptr a -> Int -> IO Int
hGetBufSome handle pointer count
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hGetBufSome" count
  | otherwise =
      wantReadableHandle_ "hGetBufSome" handle $ \handle_@Handle__ {haByteBuffer, haDevice} -> do
        flushCharReadBuffer handle_
        buffer <- readIORef haByteBuffer
        case isEmptyBuffer buffer of
          True -> do
            (transferred, buffer') <- Buffered.fillReadBuffer haDevice buffer
            case transferred == 0 of
              True -> return 0
              False -> do
                writeIORef haByteBuffer buffer'
                bufReadNBNonEmpty handle_ buffer' (castPtr pointer) 0 count
          False -> bufReadNBNonEmpty handle_ buffer (castPtr pointer) 0 count

-- | The runtime has no non-blocking reads, so this is 'hGetBufSome'.
hGetBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hGetBufNonBlocking = hGetBufSome

bufReadNBNonEmpty :: Handle__ -> Buffer Word8 -> Ptr Word8 -> Int -> Int -> IO Int
bufReadNBNonEmpty Handle__ {haByteBuffer} buffer@Buffer {bufRaw, bufR, bufL} pointer soFar count = do
  let available = bufR - bufL
  case count < available of
    True -> do
      copyFromRawBuffer pointer bufRaw bufL count
      writeIORef haByteBuffer buffer {bufL = bufL + count}
      return (soFar + count)
    False -> do
      copyFromRawBuffer pointer bufRaw bufL available
      writeIORef haByteBuffer buffer {bufR = 0, bufL = 0}
      return (soFar + available)

-- | Copy @count@ bytes from the raw buffer at @offset@ to the pointer.
copyFromRawBuffer :: Ptr Word8 -> RawBuffer Word8 -> Int -> Int -> IO ()
copyFromRawBuffer pointer raw offset count =
  withRawBuffer raw $ \source -> copyBytes pointer (source `plusPtr` offset) count

-- | Copy @count@ bytes from the pointer into the raw buffer at @offset@.
copyToRawBuffer :: RawBuffer Word8 -> Int -> Ptr Word8 -> Int -> IO ()
copyToRawBuffer raw offset pointer count =
  withRawBuffer raw $ \target -> copyBytes (target `plusPtr` offset) pointer count

-- ---------------------------------------------------------------------
-- Block output

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf handle pointer count = do
  _ <- hPutBuf' handle pointer count True
  return ()

hPutBufNonBlocking :: Handle -> Ptr a -> Int -> IO Int
hPutBufNonBlocking handle pointer count = hPutBuf' handle pointer count False

hPutBuf' :: Handle -> Ptr a -> Int -> Bool -> IO Int
hPutBuf' handle pointer count canBlock
  | count == 0 = return 0
  | count < 0 = illegalBufferSize handle "hPutBuf" count
  | otherwise =
      wantWritableHandle "hPutBuf" handle $ \handle_@Handle__ {haBufferMode} -> do
        written <- bufWrite handle_ (castPtr pointer) count canBlock
        -- A handle without block buffering flushes after each write.
        case haBufferMode of
          BlockBuffering _ -> return ()
          _ -> flushWriteBuffer handle_
        return written

bufWrite :: Handle__ -> Ptr Word8 -> Int -> Bool -> IO Int
bufWrite handle_@Handle__ {haByteBuffer, haDevice} pointer count canBlock = do
  old@Buffer {bufRaw, bufR, bufSize} <- readIORef haByteBuffer
  case bufSize - bufR > count of
    True -> do
      -- The data fits in the buffer.
      copyToRawBuffer bufRaw bufR pointer count
      writeIORef haByteBuffer old {bufR = bufR + count}
      return count
    False -> do
      -- Flush the buffer. Data that fits in the empty buffer goes into
      -- it. Larger data goes to the device at once.
      old' <- Buffered.flushWriteBuffer haDevice old
      writeIORef haByteBuffer old'
      case count < bufSize of
        True -> bufWrite handle_ pointer count canBlock
        False -> do
          RawIO.write haDevice pointer (bufOffset old') count
          writeIORef haByteBuffer old' {bufOffset = bufOffset old' + fromIntegral count}
          return count

illegalBufferSize :: Handle -> String -> Int -> IO a
illegalBufferSize handle function size =
  ioException (IOError (Just handle) InvalidArgument function ("illegal buffer size " ++ show size) Nothing Nothing)
