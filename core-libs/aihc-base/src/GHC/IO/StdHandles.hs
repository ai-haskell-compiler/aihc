{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Raw asynchronous IO operations and standard-stream handles used below the
-- future buffered 'Handle' layer.
module GHC.IO.StdHandles
  ( IOHandle,
    IOBuffer,
    stdinHandle,
    stdoutHandle,
    newIOBuffer,
    getIOBufferByte,
    setIOBufferByte,
    copyAddrToIOBuffer,
    readIntoBuffer,
    writeFromBuffer,
  )
where

import Foreign.C.Types (CInt)
import GHC.Event (awaitIO)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr)

-- | An opaque IO resource owned by the runtime. Its representation is
-- platform-specific: for example, a POSIX descriptor or a Windows handle.
data IOHandle

-- | Stable byte storage owned by the runtime. The proof-of-concept runtime
-- does not release this allocation. A submitted request retains the buffer,
-- and callers must not access its submitted slice before completion.
data IOBuffer

data IORequest

foreign import ccall unsafe "aihc_io_stdin"
  stdinHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stdout"
  stdoutHandle :: IO (Ptr IOHandle)

-- | Allocate zero-filled stable storage. The proof-of-concept runtime leaks
-- the allocation.
foreign import ccall unsafe "aihc_io_buffer_new"
  newIOBuffer :: CInt -> IO (Ptr IOBuffer)

-- | Read one byte from a valid buffer index.
foreign import ccall unsafe "aihc_io_buffer_get"
  getIOBufferByte :: Ptr IOBuffer -> CInt -> IO CInt

-- | Store the low eight bits at a valid buffer index. The result is zero.
foreign import ccall unsafe "aihc_io_buffer_set"
  setIOBufferByte :: Ptr IOBuffer -> CInt -> CInt -> IO CInt

-- | Copy an explicit byte range from an address into a valid buffer slice.
-- The source address must remain valid for the duration of this call. The
-- result is zero.
foreign import ccall unsafe "aihc_io_buffer_copy_from_addr"
  copyAddrToIOBuffer :: Addr# -> Ptr IOBuffer -> CInt -> CInt -> IO CInt

foreign import ccall unsafe "aihc_io_submit_read"
  submitRead :: Ptr IOHandle -> Ptr IOBuffer -> CInt -> CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write"
  submitWrite :: Ptr IOHandle -> Ptr IOBuffer -> CInt -> CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO CInt

-- | Read at most the requested number of bytes into a buffer slice. A
-- non-negative result is the transferred byte count. Zero reports end-of-file
-- for a non-empty request. Negative values are encoded platform errors.
readIntoBuffer :: Ptr IOHandle -> Ptr IOBuffer -> CInt -> CInt -> IO CInt
readIntoBuffer handle buffer offset length =
  awaitRequest (submitRead handle buffer offset length)

-- | Write bytes from a buffer slice. A non-negative result is the transferred
-- byte count and can be smaller than the requested length. Negative values are
-- encoded platform errors.
writeFromBuffer :: Ptr IOHandle -> Ptr IOBuffer -> CInt -> CInt -> IO CInt
writeFromBuffer handle buffer offset length =
  awaitRequest (submitWrite handle buffer offset length)

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
