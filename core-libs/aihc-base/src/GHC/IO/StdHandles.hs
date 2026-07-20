{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Raw asynchronous IO operations and standard-stream handles used below the
-- future buffered 'Handle' layer.
module GHC.IO.StdHandles
  ( IOHandle,
    stdinHandle,
    stdoutHandle,
    readByte,
    writeByte,
    readStdinByte,
    writeStdoutByte,
  )
where

import Foreign.C.Types (CInt)
import GHC.Event (awaitIO)
import GHC.IO (IO (..))
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
  submitRead :: Ptr IOHandle -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write"
  submitWrite :: Ptr IOHandle -> CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO CInt

-- | Read one byte from an IO resource. Non-negative results are byte values,
-- @-1@ is end-of-file, and lower values are encoded platform errors.
readByte :: Ptr IOHandle -> IO CInt
readByte handle = awaitRequest (submitRead handle)

-- | Write the low eight bits of a 'CInt' to an IO resource. Zero reports
-- success; negative values are encoded platform errors.
writeByte :: Ptr IOHandle -> CInt -> IO CInt
writeByte handle byte = awaitRequest (submitWrite handle byte)

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

-- | Read one byte from standard input. Non-negative results are byte values,
-- @-1@ is end-of-file, and lower values are encoded platform errors.
readStdinByte :: IO CInt
readStdinByte =
  IO
    ( \state ->
        case stdinHandle of
          IO getHandle ->
            case getHandle state of
              (# handleState, handle #) ->
                case readByte handle of
                  IO readFromHandle -> readFromHandle handleState
    )

-- | Write the low eight bits of a 'CInt' to standard output. Zero reports
-- success; negative values are encoded platform errors.
writeStdoutByte :: CInt -> IO CInt
writeStdoutByte byte =
  IO
    ( \state ->
        case stdoutHandle of
          IO getHandle ->
            case getHandle state of
              (# handleState, handle #) ->
                case writeByte handle byte of
                  IO writeToHandle -> writeToHandle handleState
    )
