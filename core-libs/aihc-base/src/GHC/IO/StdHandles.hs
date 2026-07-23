{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Raw asynchronous IO operations and standard-stream handles used below the
-- future buffered 'Handle' layer.
module GHC.IO.StdHandles
  ( IOHandle,
    stdinHandle,
    stdoutHandle,
    withPinnedByteArray,
    copyAddrToByteArray,
    readIntoBuffer,
    writeFromBuffer,
  )
where

import Foreign.C.Types (CInt)
import GHC.Event (awaitIO)
import GHC.IO (IO (..))
import GHC.Prim (MutableByteArray#, RealWorld, copyAddrToByteArray#, mutableByteArrayContents#, newPinnedByteArray#)
import GHC.Ptr (Ptr)

-- | An opaque IO resource owned by the runtime. Its representation is
-- platform-specific: for example, a POSIX descriptor or a Windows handle.
data IOHandle

data IORequest

foreign import ccall unsafe "aihc_io_stdin"
  stdinHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stdout"
  stdoutHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_submit_read"
  submitRead :: Ptr IOHandle -> Addr# -> CInt -> CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write"
  submitWrite :: Ptr IOHandle -> Addr# -> CInt -> CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO CInt

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

-- | Copy bytes synchronously from an address into a mutable byte array.
copyAddrToByteArray :: Addr# -> MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
copyAddrToByteArray source buffer offset length =
  IO
    ( \state ->
        case copyAddrToByteArray# source buffer offset length state of
          copiedState -> (# copiedState, () #)
    )

-- | Read at most the requested number of bytes into a buffer slice. A
-- non-negative result is the transferred byte count. Zero reports end-of-file
-- for a non-empty request. Negative values are encoded platform errors. The
-- caller must keep the pinned array alive and supply a valid slice.
readIntoBuffer :: Ptr IOHandle -> MutableByteArray# RealWorld -> CInt -> CInt -> IO CInt
readIntoBuffer handle buffer offset length =
  awaitRequest (submitRead handle (mutableByteArrayContents# buffer) offset length)

-- | Write bytes from a buffer slice. A non-negative result is the transferred
-- byte count and can be smaller than the requested length. Negative values are
-- encoded platform errors. The caller must keep the pinned array alive and
-- supply a valid slice.
writeFromBuffer :: Ptr IOHandle -> MutableByteArray# RealWorld -> CInt -> CInt -> IO CInt
writeFromBuffer handle buffer offset length =
  awaitRequest (submitWrite handle (mutableByteArrayContents# buffer) offset length)

awaitRequest :: IO (Ptr IORequest) -> IO CInt
awaitRequest submission =
  IO
    ( \state ->
        case submission of
          IO submit ->
            case submit state of
              (# submittedState, request #) ->
                case awaitIO request of
                  IO await ->
                    case await submittedState of
                      (# completedState, () #) ->
                        case takeResult request of
                          IO take -> take completedState
    )
