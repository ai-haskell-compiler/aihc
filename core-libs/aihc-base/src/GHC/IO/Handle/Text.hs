module GHC.IO.Handle.Text
  ( hGetBuf,
    hPutBuf,
  )
where

import GHC.IO.Exception (illegalOperationError, ioError, ioErrorFromErrno)
import GHC.IO.FD (IOHandle, readIntoPtr, writeFromPtr)
import GHC.IO.Handle.Types (Handle (..), HandleState (..))
import GHC.IO.IOMode (isReadableMode, isWritableMode)
import GHC.MVar (putMVar, takeMVar)
import GHC.Ptr (Ptr)
import Prelude

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf (FileHandle name stateVariable) buffer count =
  case count == 0 of
    True -> return 0
    False ->
      case count < 0 of
        True -> ioError (illegalOperationError "hGetBuf" "negative buffer size")
        False -> do
          state <- takeMVar stateVariable
          case state of
            HandleClosed -> do
              putMVar stateVariable HandleClosed
              ioError (illegalOperationError "hGetBuf" "handle is closed")
            HandleOpen rawHandle mode ->
              case isReadableMode mode of
                False -> do
                  putMVar stateVariable state
                  ioError (illegalOperationError "hGetBuf" "handle is not readable")
                True -> do
                  result <- readLoop rawHandle buffer 0 count
                  putMVar stateVariable state
                  case result of
                    Left encodedError -> ioError (ioErrorFromErrno "hGetBuf" (Just name) (decodeBufferError encodedError))
                    Right transferred -> return transferred

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf (FileHandle name stateVariable) buffer count =
  case count == 0 of
    True -> return ()
    False ->
      case count < 0 of
        True -> ioError (illegalOperationError "hPutBuf" "negative buffer size")
        False -> do
          state <- takeMVar stateVariable
          case state of
            HandleClosed -> do
              putMVar stateVariable HandleClosed
              ioError (illegalOperationError "hPutBuf" "handle is closed")
            HandleOpen rawHandle mode ->
              case isWritableMode mode of
                False -> do
                  putMVar stateVariable state
                  ioError (illegalOperationError "hPutBuf" "handle is not writable")
                True -> do
                  result <- writeLoop rawHandle buffer 0 count
                  putMVar stateVariable state
                  case result of
                    Left encodedError -> ioError (ioErrorFromErrno "hPutBuf" (Just name) (decodeBufferError encodedError))
                    Right () -> return ()

readLoop :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO (Either Int Int)
readLoop rawHandle buffer offset remaining = do
  result <- readIntoPtr rawHandle buffer offset remaining
  case result < 0 of
    True -> return (Left result)
    False ->
      case result == 0 of
        True -> return (Right offset)
        False ->
          case result == remaining of
            True -> return (Right (offset + result))
            False -> readLoop rawHandle buffer (offset + result) (remaining - result)

writeLoop :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO (Either Int ())
writeLoop rawHandle buffer offset remaining = do
  result <- writeFromPtr rawHandle buffer offset remaining
  case result < 0 of
    True -> return (Left result)
    False ->
      case result == 0 of
        True -> return (Left (negate 6))
        False ->
          case result == remaining of
            True -> return (Right ())
            False -> writeLoop rawHandle buffer (offset + result) (remaining - result)

decodeBufferError :: Int -> Int
decodeBufferError result = negate result - 1
