{-# LANGUAGE OverloadedStrings #-}
module EasyLogger.Push
    ( pushLogStr
    , pushLogStrLn
    ) where


-- import Control.AutoUpdate (mkAutoUpdate, defaultUpdateSettings, updateAction)
-- import System.Log.FastLogger.Types (TimeFormat, FormattedTime)
import           Control.Concurrent
import           Data.Array            (bounds, (!))
import           Data.IORef
import           Foreign.Marshal.Alloc (allocaBytes)
import           GHC.IO.FD

import           EasyLogger.LoggerSet
import           EasyLogger.LogStr


-- | Writing a log message to the corresponding buffer.
--   If the buffer becomes full, the log messages in the buffer
--   are written to its corresponding file, stdout, or stderr.
pushLogStr :: LoggerSet -> LogStr -> IO ()
pushLogStr (LoggerSet _ fdref arr flush) logmsg = do
    (i, _) <- myThreadId >>= threadCapability
    -- The number of capability could be dynamically changed.
    -- So, let's check the upper boundary of the array.
    let u = snd $ bounds arr
        lim = u + 1
        j | i < lim   = i
          | otherwise = i `mod` lim
    let logger = arr ! j
    pushLog fdref logger logmsg
    flush


-- | Same as 'pushLogStr' but also appends a newline.
pushLogStrLn :: LoggerSet -> LogStr -> IO ()
pushLogStrLn loggerSet logStr = pushLogStr loggerSet (logStr <> "\n")

pushLog :: IORef FD -> Logger -> LogStr -> IO ()
pushLog fdref logger@(Logger size mbuf ref) nlogmsg@(LogStr nlen nbuilder)
  | nlen > size = do
      flushLog fdref logger
      -- Make sure we have a large enough buffer to hold the entire
      -- contents, thereby allowing for a single write system call and
      -- avoiding interleaving. This does not address the possibility
      -- of write not writing the entire buffer at once.
      allocaBytes nlen $ \buf -> withMVar mbuf $ \_ ->
        toBufIOWith buf nlen (write fdref) nbuilder
  | otherwise = do
    mmsg <- atomicModifyIORef' ref checkBuf
    case mmsg of
        Nothing  -> return ()
        Just msg -> withMVar mbuf $ \buf -> writeLogStr fdref buf size msg
  where
    checkBuf ologmsg@(LogStr olen _)
      | size < olen + nlen = (nlogmsg, Just ologmsg)
      | otherwise          = (ologmsg <> nlogmsg, Nothing)
