{-# LANGUAGE NamedFieldPuns #-}

{- HLINT ignore "Use camelCase" -}

-- | Handle operations.
module GHC.IO.Handle
  ( Handle,
    BufferMode (..),
    mkFileHandle,
    mkDuplexHandle,
    hFileSize,
    hSetFileSize,
    hIsEOF,
    isEOF,
    hLookAhead,
    hSetBuffering,
    hSetBinaryMode,
    hSetEncoding,
    hGetEncoding,
    hFlush,
    hFlushAll,
    hDuplicate,
    hDuplicateTo,
    hClose,
    hClose_help,
    LockMode (..),
    hLock,
    hTryLock,
    HandlePosition,
    HandlePosn (..),
    hGetPosn,
    hSetPosn,
    SeekMode (..),
    hSeek,
    hTell,
    hIsOpen,
    hIsClosed,
    hIsReadable,
    hIsWritable,
    hGetBuffering,
    hIsSeekable,
    hSetEcho,
    hIsTerminalDevice,
    hGetEcho,
    hWaitForInput,
    hGetChar,
    hGetLine,
    hGetContents,
    hGetContents',
    hPutChar,
    hPutStr,
    hPutStrLn,
    hGetBuf,
    hGetBufNonBlocking,
    hPutBuf,
    hPutBufNonBlocking,
    hShow,
    hGetBufSome,
    hSetNewlineMode,
    Newline (..),
    NewlineMode (..),
    nativeNewline,
    noNewlineTranslation,
    universalNewlineMode,
    nativeNewlineMode,
  )
where

import Data.Bool (Bool (..), not)
import Data.Maybe (Maybe (..))
import Data.Typeable.Internal (Typeable, cast)
import GHC.Exception.Type (SomeException)
import GHC.IO (FilePath, IO, catchException, throwIO)
import GHC.IO.Buffer (Buffer (..), BufferState (..), isEmptyBuffer, newCharBuffer)
import GHC.IO.Encoding.Types (TextEncoding)
import GHC.IO.Handle.Internals
import GHC.IO.Handle.Text (hGetBuf, hGetBufNonBlocking, hGetBufSome, hGetChar, hGetContents, hGetContents', hGetLine, hPutBuf, hPutBufNonBlocking, hPutChar, hPutStr, hPutStrLn, hWaitForInput)
import GHC.IO.Handle.Types
import GHC.IO.StdHandles (stdin)
import GHC.IORef (readIORef, writeIORef)
import GHC.Int (Int)
import GHC.Integer (Integer)
import GHC.Internal.Base (Monad (..), String, ($), (++), (.))
import GHC.Internal.Classes (Eq (..), Ord (..))
import GHC.Internal.IO.Types (BufferedIO, IODevice (..), IOErrorType (..), IOException (..), RawIO, SeekMode (..), ioException)
import GHC.Internal.IO.Types qualified as Buffered (fillReadBuffer)
import GHC.MVar (MVar, putMVar, takeMVar)
import GHC.Num (Num (..))
import GHC.Real (fromIntegral)
import GHC.Show (Show (..), showString, shows)
import GHC.Types (Char)

-- ---------------------------------------------------------------------
-- Closing

-- | Flush and close the handle. A closed handle stays closed. A close
-- error reaches the caller with the handle in it.
hClose :: Handle -> IO ()
hClose handle@(FileHandle _ state) = do
  exception <- hClose' handle state
  maybeThrow exception
hClose handle@(DuplexHandle _ readSide writeSide) = do
  readException <- hClose' handle readSide
  writeException <- hClose' handle writeSide
  maybeThrow readException
  maybeThrow writeException

maybeThrow :: Maybe SomeException -> IO ()
maybeThrow Nothing = return ()
maybeThrow (Just exception) = throwIO exception

hClose' :: Handle -> MVar Handle__ -> IO (Maybe SomeException)
hClose' handle state = withHandle' "hClose" handle state hClose_help

-- ---------------------------------------------------------------------
-- Sizes and positions

hFileSize :: Handle -> IO Integer
hFileSize handle =
  withHandle_ "hFileSize" handle $ \handle_@Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> do
        flushWriteBuffer handle_
        getSize haDevice

hSetFileSize :: Handle -> Integer -> IO ()
hSetFileSize handle size =
  withHandle_ "hSetFileSize" handle $ \handle_@Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> do
        flushWriteBuffer handle_
        setSize haDevice size

hIsEOF :: Handle -> IO Bool
hIsEOF handle =
  wantReadableHandle_ "hIsEOF" handle $ \handle_@Handle__ {haByteBuffer, haDevice} -> do
    flushCharReadBuffer handle_
    buffer <- readIORef haByteBuffer
    case isEmptyBuffer buffer of
      False -> return False
      True -> do
        (count, buffer') <- Buffered.fillReadBuffer haDevice buffer
        writeIORef haByteBuffer buffer'
        return (count == 0)

isEOF :: IO Bool
isEOF = hIsEOF stdin

type HandlePosition = Integer

data HandlePosn = HandlePosn Handle HandlePosition

instance Eq HandlePosn where
  HandlePosn _ left == HandlePosn _ right = left == right

instance Show HandlePosn where
  showsPrec precedence (HandlePosn handle position) =
    showsPrec precedence handle . showString " at position " . shows position

hGetPosn :: Handle -> IO HandlePosn
hGetPosn handle = do
  position <- hTell handle
  return (HandlePosn handle position)

hSetPosn :: HandlePosn -> IO ()
hSetPosn (HandlePosn handle position) = hSeek handle AbsoluteSeek position

hSeek :: Handle -> SeekMode -> Integer -> IO ()
hSeek handle mode offset =
  wantSeekableHandle "hSeek" handle $ \handle_@Handle__ {haDevice} -> do
    flushBuffer handle_
    _ <- seek haDevice mode offset
    return ()

hTell :: Handle -> IO Integer
hTell handle =
  wantSeekableHandle "hTell" handle $ \handle_@Handle__ {haDevice, haByteBuffer} -> do
    position <- tell haDevice
    buffer <- readIORef haByteBuffer
    let buffered = fromIntegral (bufR buffer - bufL buffer)
    return
      ( case bufState buffer of
          WriteBuffer -> position + buffered
          ReadBuffer -> position - buffered
      )

-- ---------------------------------------------------------------------
-- Buffering

hSetBuffering :: Handle -> BufferMode -> IO ()
hSetBuffering handle mode =
  withAllHandles__ "hSetBuffering" handle $ \handle_@Handle__ {haType, haBufferMode, haCharBuffer} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ ->
        case mode == haBufferMode of
          True -> return handle_
          False -> do
            case mode of
              BlockBuffering (Just size) | size <= 0 -> ioe_bufsiz size
              _ -> return ()
            flushCharBuffer handle_
            let state = initBufferState haType
            newBuffer <- newCharBuffer dEFAULT_CHAR_BUFFER_SIZE state
            writeIORef haCharBuffer newBuffer
            return handle_ {haBufferMode = mode}

hGetBuffering :: Handle -> IO BufferMode
hGetBuffering handle =
  withHandle_ "hGetBuffering" handle $ \Handle__ {haType, haBufferMode} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return haBufferMode

-- | Handles are always binary in this runtime.
hSetBinaryMode :: Handle -> Bool -> IO ()
hSetBinaryMode handle _ =
  withAllHandles__ "hSetBinaryMode" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_

hSetEncoding :: Handle -> TextEncoding -> IO ()
hSetEncoding handle encoding =
  withAllHandles__ "hSetEncoding" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_ {haCodec = Just encoding}

hGetEncoding :: Handle -> IO (Maybe TextEncoding)
hGetEncoding handle =
  withHandle_ "hGetEncoding" handle $ \Handle__ {haType, haCodec} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return haCodec

hSetNewlineMode :: Handle -> NewlineMode -> IO ()
hSetNewlineMode handle (NewlineMode input output) =
  withAllHandles__ "hSetNewlineMode" handle $ \handle_@Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> return handle_ {haInputNL = input, haOutputNL = output}

hFlush :: Handle -> IO ()
hFlush handle = wantWritableHandle "hFlush" handle flushWriteBuffer

hFlushAll :: Handle -> IO ()
hFlushAll handle = withHandle_ "hFlushAll" handle flushBuffer

-- ---------------------------------------------------------------------
-- Duplication and locks

-- | Give a new handle for the open file of the handle. The new handle has
-- its own buffer and its own descriptor. The two handles share the file
-- position. Close the two handles independently.
hDuplicate :: Handle -> IO Handle
hDuplicate handle@(FileHandle path state) =
  withHandle_' "hDuplicate" handle state $ \handle_ ->
    dupHandle path Nothing handle_
hDuplicate handle@(DuplexHandle path readState writeState) = do
  writeVar <- withHandle_' "hDuplicate" handle writeState $ \handle_ -> do
    duplicate <- dupHandle path Nothing handle_
    return (handleState duplicate)
  readVar <- withHandle_' "hDuplicate" handle readState $ \handle_ -> do
    duplicate <- dupHandle path (Just writeVar) handle_
    return (handleState duplicate)
  return (DuplexHandle path readVar writeVar)

-- | The state of a handle with one side, or the read side of a duplex handle.
handleState :: Handle -> MVar Handle__
handleState (FileHandle _ state) = state
handleState (DuplexHandle _ state _) = state

-- | Flush the handle and make a new handle for a duplicate of its device.
-- The read side of a duplex handle uses the new device of the write side.
dupHandle :: FilePath -> Maybe (MVar Handle__) -> Handle__ -> IO Handle
dupHandle path otherSide handle_@Handle__ {haDevice, haType} =
  case haType of
    ClosedHandle -> ioe_closedHandle
    _ -> do
      flushBuffer handle_
      case otherSide of
        Nothing -> do
          device <- dup haDevice
          dupHandle_ device path otherSide handle_
        Just writeVar -> do
          writeSide <- takeMVar writeVar
          putMVar writeVar writeSide
          case writeSide of
            Handle__ {haDevice = device} -> dupHandle_ device path otherSide handle_

-- | Make a handle for the device with the type, the encoding, and the
-- newline mode of an existing handle.
dupHandle_ ::
  (RawIO dev, IODevice dev, BufferedIO dev, Typeable dev) =>
  dev ->
  FilePath ->
  Maybe (MVar Handle__) ->
  Handle__ ->
  IO Handle
dupHandle_ device path otherSide Handle__ {haType, haCodec, haInputNL, haOutputNL} =
  mkHandle device path haType True haCodec NewlineMode {inputNL = haInputNL, outputNL = haOutputNL} (Just handleFinalizer) otherSide

-- | Make the second handle refer to the open file of the first handle. The
-- second handle is flushed first, and the system closes its file.
hDuplicateTo :: Handle -> Handle -> IO ()
hDuplicateTo source@(FileHandle path sourceState) target@(FileHandle _ targetState) =
  withHandle__' "hDuplicateTo" target targetState $ \target_ -> do
    catchException (flushWriteBuffer target_) ignoreException
    withHandle_' "hDuplicateTo" source sourceState $ \source_ ->
      dupHandleTo path source Nothing target_ source_
hDuplicateTo source@(DuplexHandle path sourceRead sourceWrite) target@(DuplexHandle _ targetRead targetWrite) = do
  withHandle__' "hDuplicateTo" target targetWrite $ \target_ -> do
    catchException (flushWriteBuffer target_) ignoreException
    withHandle_' "hDuplicateTo" source sourceWrite $ \source_ ->
      dupHandleTo path source Nothing target_ source_
  withHandle__' "hDuplicateTo" target targetRead $ \target_ ->
    withHandle_' "hDuplicateTo" source sourceRead $ \source_ ->
      dupHandleTo path source (Just targetWrite) target_ source_
hDuplicateTo source _ = ioe_dupHandlesNotCompatible source

-- | A flush error of the target does not stop the duplication.
ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

ioe_dupHandlesNotCompatible :: Handle -> IO a
ioe_dupHandlesNotCompatible handle =
  ioException (IOError (Just handle) IllegalOperation "hDuplicateTo" "handles are incompatible" Nothing Nothing)

-- | Give the new state of the target handle. The device of the target gets
-- the open file of the source device.
dupHandleTo :: FilePath -> Handle -> Maybe (MVar Handle__) -> Handle__ -> Handle__ -> IO Handle__
dupHandleTo path source otherSide Handle__ {haDevice = targetDevice} source_@Handle__ {haDevice = sourceDevice, haType} =
  case haType of
    ClosedHandle -> ioe_closedHandle
    _ -> do
      flushBuffer source_
      case cast targetDevice of
        Nothing -> ioe_dupHandlesNotCompatible source
        Just compatible -> do
          device <- dup2 sourceDevice compatible
          duplicate <- dupHandle_ device path otherSide source_
          takeMVar (handleState duplicate)

data LockMode = SharedLock | ExclusiveLock

hLock :: Handle -> LockMode -> IO ()
hLock handle _ =
  withHandle_ "hLock" handle $ \_ ->
    ioException (IOError (Just handle) UnsupportedOperation "hLock" "file locks are not available" Nothing Nothing)

hTryLock :: Handle -> LockMode -> IO Bool
hTryLock handle _ =
  withHandle_ "hTryLock" handle $ \_ ->
    ioException (IOError (Just handle) UnsupportedOperation "hTryLock" "file locks are not available" Nothing Nothing)

-- ---------------------------------------------------------------------
-- Queries

hIsOpen :: Handle -> IO Bool
hIsOpen handle =
  withHandle_ "hIsOpen" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> return False
      SemiClosedHandle -> return False
      _ -> return True

hIsClosed :: Handle -> IO Bool
hIsClosed handle =
  withHandle_ "hIsClosed" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> return True
      _ -> return False

hIsReadable :: Handle -> IO Bool
hIsReadable DuplexHandle {} = return True
hIsReadable handle =
  withHandle_ "hIsReadable" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> return (isReadableHandleType haType)

hIsWritable :: Handle -> IO Bool
hIsWritable DuplexHandle {} = return True
hIsWritable handle =
  withHandle_ "hIsWritable" handle $ \Handle__ {haType} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      _ -> return (isWritableHandleType haType)

hIsSeekable :: Handle -> IO Bool
hIsSeekable handle =
  withHandle_ "hIsSeekable" handle $ \Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      SemiClosedHandle -> ioe_semiclosedHandle
      AppendHandle -> return False
      _ -> isSeekable haDevice

hSetEcho :: Handle -> Bool -> IO ()
hSetEcho handle on = do
  terminal <- hIsTerminalDevice handle
  case terminal of
    False -> return ()
    True ->
      withHandle_ "hSetEcho" handle $ \Handle__ {haType, haDevice} ->
        case haType of
          ClosedHandle -> ioe_closedHandle
          _ -> setEcho haDevice on

hGetEcho :: Handle -> IO Bool
hGetEcho handle = do
  terminal <- hIsTerminalDevice handle
  case terminal of
    False -> return False
    True ->
      withHandle_ "hGetEcho" handle $ \Handle__ {haType, haDevice} ->
        case haType of
          ClosedHandle -> ioe_closedHandle
          _ -> getEcho haDevice

hIsTerminalDevice :: Handle -> IO Bool
hIsTerminalDevice handle =
  withHandle_ "hIsTerminalDevice" handle $ \Handle__ {haType, haDevice} ->
    case haType of
      ClosedHandle -> ioe_closedHandle
      _ -> isTerminal haDevice

-- | A description of the handle state.
hShow :: Handle -> IO String
hShow handle@(FileHandle path state) = showHandle' path False handle state
hShow handle@(DuplexHandle path _ state) = showHandle' path True handle state

showHandle' :: String -> Bool -> Handle -> MVar Handle__ -> IO String
showHandle' path duplex handle state =
  withHandle_' "showHandle" handle state $ \Handle__ {haType, haBufferMode, haByteBuffer} -> do
    buffer <- readIORef haByteBuffer
    return
      ( "{loc="
          ++ path
          ++ ",type="
          ++ show haType
          ++ (case duplex of True -> ",duplex"; False -> "")
          ++ ",buffering="
          ++ show haBufferMode
          ++ ",bytes="
          ++ show (bufR buffer - bufL buffer)
          ++ "}"
      )

-- | The next character of a handle without consuming it.
hLookAhead :: Handle -> IO Char
hLookAhead handle = wantReadableHandle_ "hLookAhead" handle hLookAhead_
