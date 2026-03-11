{-# LANGUAGE BangPatterns #-}
module EasyLogger.LoggerSet
  ( Logger(..)
  , LoggerSet(..)
  , BufSize
  , newFileLoggerSet
  , newFileLoggerSetSameFile
  , newStdoutLoggerSet
  , newStderrLoggerSet
  , newFDLoggerSet
  , toBufIOWith
  , write
  , writeLogStr
  , flushLog
  , rmLoggerSet
  , flushLoggerSet
  ) where

import           Control.Concurrent            (getNumCapabilities)
import           Control.Concurrent.MVar
import           Control.Debounce              (debounceAction, defaultDebounceSettings,
                                                mkDebounce)
import           Control.Monad                 (replicateM, when)
import           Data.Array                    (Array, bounds, listArray, (!))
import           Data.ByteString.Builder
import           Data.ByteString.Builder.Extra (Next (..))
import qualified Data.ByteString.Builder.Extra as BBE
import           Data.ByteString.Internal
import           Data.IORef
import           Data.Maybe                    (isJust)
import           Data.Word
import           Foreign.ForeignPtr            (withForeignPtr)
import           Foreign.Marshal.Alloc         (free, mallocBytes)
import           Foreign.Ptr                   (Ptr, plusPtr)
import           GHC.IO.Device                 (close)
import           GHC.IO.FD                     (FD, openFile, stderr, stdout,
                                                writeRawBufferPtr)
import           GHC.IO.IOMode                 (IOMode (..))

import           EasyLogger.LogStr

-- | The type for buffer size of each core.
type BufSize = Int
type Buffer = Ptr Word8

data Logger = Logger !BufSize (MVar Buffer) (IORef LogStr)

newLogger :: BufSize -> IO Logger
newLogger size = Logger size <$> (mallocBytes size >>= newMVar) <*> newIORef mempty

-- | A set of loggers.
--   The number of loggers is the capabilities of GHC RTS.
--   You can specify it with \"+RTS -N\<x\>\".
--   A buffer is prepared for each capability.
data LoggerSet = LoggerSet (Maybe FilePath) (IORef FD) (Array Int Logger) (IO ())

-- | Creating a new 'LoggerSet' using a file.
newFileLoggerSet :: BufSize -> FilePath -> IO LoggerSet
newFileLoggerSet size file = openFileFD >>= newFDLoggerSet size (Just file)
  where
    openFileFD = fst <$> openFile file AppendMode False

-- | Creating a new 'LoggerSet' using a file.
newFileLoggerSetSameFile :: BufSize -> LoggerSet -> IO LoggerSet
newFileLoggerSetSameFile size (LoggerSet mFp ioRefFD _ _) = readIORef ioRefFD >>= newFDLoggerSet size mFp


-- | Creating a new 'LoggerSet' using stdout.
newStdoutLoggerSet :: BufSize -> IO LoggerSet
newStdoutLoggerSet size = newFDLoggerSet size Nothing stdout

-- | Creating a new 'LoggerSet' using stderr.
newStderrLoggerSet :: BufSize -> IO LoggerSet
newStderrLoggerSet size = newFDLoggerSet size Nothing stderr

-- | Creating a new 'LoggerSet' using a FD.
newFDLoggerSet :: BufSize -> Maybe FilePath -> FD -> IO LoggerSet
newFDLoggerSet size mfile fd = do
    n <- getNumCapabilities
    loggers <- replicateM n $ newLogger (max 1 size)
    let arr = listArray (0,n-1) loggers
    fref <- newIORef fd
    flush <- mkDebounce defaultDebounceSettings
        { debounceAction = flushLogStrRaw fref arr
        }
    return $ LoggerSet mfile fref arr flush


flushLogStrRaw :: IORef FD -> Array Int Logger -> IO ()
flushLogStrRaw fdref arr = do
    let (l,u) = bounds arr
    mapM_ flushIt [l .. u]
  where
    flushIt i = flushLog fdref (arr ! i)


flushLog :: IORef FD -> Logger -> IO ()
flushLog fdref (Logger size mbuf lref) = do
    logmsg <- atomicModifyIORef' lref (\old -> (mempty, old))
    -- If a special buffer is prepared for flusher, this MVar could
    -- be removed. But such a code does not contribute logging speed
    -- according to experiment. And even with the special buffer,
    -- there is no grantee that this function is exclusively called
    -- for a buffer. So, we use MVar here.
    -- This is safe and speed penalty can be ignored.
    withMVar mbuf $ \buf -> writeLogStr fdref buf size logmsg


-- | Writting 'LogStr' using a buffer in blocking mode.
--   The size of 'LogStr' must be smaller or equal to
--   the size of buffer.
writeLogStr :: IORef FD
            -> Buffer
            -> BufSize
            -> LogStr
            -> IO ()
writeLogStr fdref buf size (LogStr len builder)
  | size < len = error "writeLogStr"
  | otherwise  = toBufIOWith buf size (write fdref) builder

write :: IORef FD -> Buffer -> Int -> IO ()
write fdref buf len' = loop buf (fromIntegral len')
  where
    loop bf !len = do
        written <- writeRawBufferPtr2FD fdref bf len
        when (written < len) $
            loop (bf `plusPtr` fromIntegral written) (len - written)

writeRawBufferPtr2FD :: IORef FD -> Ptr Word8 -> Int -> IO Int
writeRawBufferPtr2FD fdref bf len = do
    fd <- readIORef fdref
    fromIntegral <$> writeRawBufferPtr "write" fd bf 0 (fromIntegral len)


toBufIOWith :: Buffer -> BufSize -> (Buffer -> Int -> IO ()) -> Builder -> IO ()
toBufIOWith buf !size io builder = loop $ BBE.runBuilder builder
  where
    loop writer = do
        (len, next) <- writer buf size
        io buf len
        case next of
             Done -> return ()
             More minSize writer'
               | size < minSize -> error "toBufIOWith: More: minSize"
               | otherwise      -> loop writer'
             Chunk (PS fptr off siz) writer' ->
               withForeignPtr fptr $ \ptr -> io (ptr `plusPtr` off) siz >> loop writer'

-- | Flushing the buffers.
flushLoggerSet :: LoggerSet -> IO ()
flushLoggerSet (LoggerSet _ fdref arr _) = do
    let (l,u) = bounds arr
    let nums = [l .. u]
    mapM_ flushIt nums
  where
    flushIt i = flushLog fdref (arr ! i)


-- | Flushing the buffers, closing the internal file information
--   and freeing the buffers.
rmLoggerSet :: LoggerSet -> IO ()
rmLoggerSet (LoggerSet mfile fdref arr _) = do
    let (l,u) = bounds arr
    let nums = [l .. u]
    mapM_ flushIt nums
    mapM_ freeIt nums
    fd <- readIORef fdref
    when (isJust mfile) $ close fd
  where
    flushIt i = flushLog fdref (arr ! i)
    freeIt i = do
        let (Logger _ mbuf _) = arr ! i
        takeMVar mbuf >>= free
