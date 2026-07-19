{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE UnboxedTuples #-}

-- | The raw asynchronous standard-stream operations used below the future
-- buffered 'Handle' layer.
module GHC.IO.StdHandles
  ( readStdinByte,
    writeStdoutByte,
  )
where

import Foreign.C.Types (CInt)
import GHC.Event (awaitIO)
import GHC.IO (IO (..))
import GHC.Ptr (Ptr)

data IORequest

foreign import ccall unsafe "aihc_io_submit_read_stdin"
  submitReadStdin :: IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write_stdout"
  submitWriteStdout :: CInt -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO CInt

-- | Read one byte from standard input. Non-negative results are byte values,
-- @-1@ is end-of-file, and lower values are encoded platform errors.
readStdinByte :: IO CInt
readStdinByte =
  IO
    ( \state ->
        case submitReadStdin of
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

-- | Write the low eight bits of a 'CInt' to standard output. Zero reports
-- success; negative values are encoded platform errors.
writeStdoutByte :: CInt -> IO CInt
writeStdoutByte byte =
  IO
    ( \state ->
        case submitWriteStdout byte of
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
