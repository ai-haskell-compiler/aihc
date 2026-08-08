{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

module GHC.TopHandler
  ( flushStdHandles,
    runIO,
    runIOFastExit,
    runMainIO,
    runNonIO,
    topHandler,
    topHandlerFastExit,
    reportError,
    reportStackOverflow,
  )
where

import Control.Exception (SomeException, catch, displayException, fromException)
import Data.Maybe (Maybe (..))
import GHC.IO (IO (..))
import GHC.IO.Console (writeOutputByte)
import GHC.Int (Int (..))
import GHC.Internal.Char (Char (C#))
import GHC.Prim (MutableByteArray#, RealWorld, and#, int2Word#, mutableByteArrayContents#, newPinnedByteArray#, ord#, word2Int#, (+#), (==#))
import GHC.Ptr (Ptr (..))
import System.Exit (ExitCode (..))
import System.IO (hPutBuf, stderr)
import Prelude

runMainIO :: IO a -> IO a
runMainIO action = catch action topHandler

runIO :: IO a -> IO a
runIO action = catch action topHandler

runIOFastExit :: IO a -> IO a
runIOFastExit action = catch action topHandlerFastExit

runNonIO :: a -> IO a
runNonIO value = catch (value `seq` return value) topHandler

topHandler :: SomeException -> IO a
topHandler exception = catch (handleException safeExit exception) topHandler

topHandlerFastExit :: SomeException -> IO a
topHandlerFastExit exception = catch (handleException fastExit exception) topHandlerFastExit

handleException :: (Int -> IO a) -> SomeException -> IO a
handleException exit exception = do
  flushStdHandles
  case (fromException exception :: Maybe ExitCode) of
    Just ExitSuccess -> exit 0
    Just (ExitFailure status) -> exit status
    Nothing -> do
      reportError exception
      exit 1

reportStackOverflow :: IO ()
reportStackOverflow = writeStderrLine "stack overflow"

reportError :: SomeException -> IO ()
reportError = writeStderrLine . displayException

flushStdHandles :: IO ()
flushStdHandles = return ()

safeExit :: Int -> IO a
safeExit = exitWithMode 0

fastExit :: Int -> IO a
fastExit = exitWithMode 1

exitWithMode :: Int -> Int -> IO a
exitWithMode mode status = do
  shutdownHaskellAndExit status mode
  unreachable

unreachable :: IO a
unreachable = unreachable

foreign import ccall unsafe "shutdownHaskellAndExit"
  shutdownHaskellAndExit :: Int -> Int -> IO Int

writeStderrLine :: String -> IO ()
writeStderrLine characters = do
  buffer <- newStderrBuffer 4096#
  case buffer of
    StderrBuffer rawBuffer -> writeStderrChunks rawBuffer 0# (characters ++ "\n")

data StderrBuffer = StderrBuffer (MutableByteArray# RealWorld)

newStderrBuffer :: Int# -> IO StderrBuffer
newStderrBuffer size =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            (# allocatedState, StderrBuffer buffer #)
    )

writeStderrChunks :: MutableByteArray# RealWorld -> Int# -> String -> IO ()
writeStderrChunks buffer count characters =
  case characters of
    [] -> writeStderrBuffer buffer count
    character : remaining ->
      case (==#) count 4096# of
        1# -> do
          writeStderrBuffer buffer count
          writeStderrChunks buffer 0# characters
        _ -> do
          writeStderrByte buffer count character
          writeStderrChunks buffer ((+#) count 1#) remaining

writeStderrBuffer :: MutableByteArray# RealWorld -> Int# -> IO ()
writeStderrBuffer buffer count =
  case (==#) count 0# of
    1# -> return ()
    _ -> hPutBuf stderr (Ptr (mutableByteArrayContents# buffer) :: Ptr ()) (I# count)

writeStderrByte :: MutableByteArray# RealWorld -> Int# -> Char -> IO ()
writeStderrByte buffer offset (C# character) =
  writeOutputByte buffer offset (word2Int# (and# (int2Word# (ord# character)) (int2Word# 255#)))
