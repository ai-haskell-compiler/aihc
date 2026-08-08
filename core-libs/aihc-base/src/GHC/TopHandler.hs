{-# LANGUAGE GHCForeignImportPrim #-}
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
import GHC.Int (Int (..))
import GHC.Prim (RealWorld, State#)
import System.Exit (ExitCode (..))
import System.IO (hPutStr, stderr)
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
safeExit = exitWithStatus

fastExit :: Int -> IO a
fastExit = exitWithStatus

exitWithStatus :: Int -> IO a
exitWithStatus (I# status) = IO (aihcExit# status)

foreign import prim aihcExit# :: Int# -> State# RealWorld -> (# State# RealWorld, a #)

writeStderrLine :: String -> IO ()
writeStderrLine message = hPutStr stderr (message ++ "\n")
