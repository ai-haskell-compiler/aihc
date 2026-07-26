{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Raw runtime-owned IO resources. 'Handle' supplies locking, lifecycle,
-- direction checks, and complete-transfer semantics above this module.
module GHC.IO.FD
  ( IOHandle,
    stdinHandle,
    stdoutHandle,
    stderrHandle,
    openIOHandle,
    closeIOHandle,
    writeMemoryByte,
    withPinnedByteArray,
    copyAddrToByteArray,
    readIntoBuffer,
    writeFromBuffer,
    readIntoPtr,
    writeFromPtr,
  )
where

import GHC.Event (awaitIO)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Prim (MutableByteArray#, RealWorld, copyAddrToByteArray#, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr (..))
import Prelude hiding (Int)

-- | An opaque IO resource owned by the runtime.
data IOHandle

data IORequest

foreign import ccall unsafe "aihc_io_stdin"
  stdinHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stdout"
  stdoutHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stderr"
  stderrHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_submit_open"
  submitOpen :: Addr# -> Int -> Int -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_open_result_error"
  openResultError :: Ptr IOHandle -> IO Int

foreign import ccall unsafe "aihc_io_close"
  closeIOHandle :: Ptr IOHandle -> IO Int

foreign import ccall unsafe "aihc_memory_write_byte"
  writeMemoryByte :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe "aihc_io_submit_read"
  submitRead :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write"
  submitWrite :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO Int

foreign import ccall unsafe "aihc_io_take_open_result"
  takeOpenResult :: Ptr IORequest -> IO (Ptr IOHandle)

openIOHandle :: Addr# -> Int -> Int -> IO (Either Int (Ptr IOHandle))
openIOHandle path length mode = do
  request <- submitOpen path length mode
  awaitIO request
  result <- takeOpenResult request
  openCode <- openResultError result
  case openCode of
    0 -> return (Right result)
    _ -> return (Left openCode)

-- | Allocate zero-filled pinned storage for the duration of an action. The
-- proof-of-concept runtime does not reclaim the allocation yet.
withPinnedByteArray :: Int# -> (MutableByteArray# RealWorld -> IO a) -> IO a
withPinnedByteArray size action =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            case action buffer of
              IO run -> run allocatedState
    )

copyAddrToByteArray :: Addr# -> MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
copyAddrToByteArray source buffer offset length =
  IO
    ( \state ->
        case copyAddrToByteArray# source buffer offset length state of
          copiedState -> (# copiedState, () #)
    )

readIntoBuffer :: Ptr IOHandle -> MutableByteArray# RealWorld -> Int -> Int -> IO Int
readIntoBuffer handle buffer =
  readIntoAddress handle (mutableByteArrayContents# buffer)

writeFromBuffer :: Ptr IOHandle -> MutableByteArray# RealWorld -> Int -> Int -> IO Int
writeFromBuffer handle buffer =
  writeFromAddress handle (mutableByteArrayContents# buffer)

readIntoPtr :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO Int
readIntoPtr handle (Ptr address) = readIntoAddress handle address

writeFromPtr :: Ptr IOHandle -> Ptr a -> Int -> Int -> IO Int
writeFromPtr handle (Ptr address) = writeFromAddress handle address

readIntoAddress :: Ptr IOHandle -> Addr# -> Int -> Int -> IO Int
readIntoAddress handle address offset length =
  awaitRequest (submitRead handle address offset length)

writeFromAddress :: Ptr IOHandle -> Addr# -> Int -> Int -> IO Int
writeFromAddress handle address offset length =
  awaitRequest (submitWrite handle address offset length)

awaitRequest :: IO (Ptr IORequest) -> IO Int
awaitRequest submission = do
  request <- submission
  awaitIO request
  takeResult request
