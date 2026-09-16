{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Dependency-free bindings to the runtime IO ABI. Higher layers share these
-- declarations so each foreign wrapper has exactly one compiled definition.
module GHC.IO.Runtime
  ( IOHandle,
    IORequest,
    stdinHandle,
    stdoutHandle,
    stderrHandle,
    submitOpen,
    openResultError,
    descriptorMode,
    adoptIOHandle,
    closeIOHandle,
    readMemoryByte,
    writeMemoryByte,
    submitRead,
    submitWrite,
    takeResult,
    takeOpenResult,
    raiseIOErrorRaw,
    awaitIO,
    decodeError,
  )
where

import GHC.IO (IO (..))
import GHC.Int (Int)
import GHC.Num (Num (..))
import GHC.Prim (Addr#, awaitIO#)
import GHC.Ptr (Ptr (..))

data IOHandle

-- | Suspend the current green thread until an opaque runtime request is ready.
awaitIO :: Ptr request -> IO ()
awaitIO (Ptr request) =
  IO
    ( \state ->
        case awaitIO# request state of
          nextState -> (# nextState, () #)
    )

-- | The runtime reports an error number @e@ as @-(e + 1)@, so that a result
-- and an error share one signed word. This undoes that.
decodeError :: Int -> Int
decodeError result = negate result - 1

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

-- | The open mode of a descriptor the program already has, or a negative
-- error. Adopting a descriptor needs no request: neither call blocks.
foreign import ccall unsafe "aihc_io_descriptor_mode"
  descriptorMode :: Int -> IO Int

foreign import ccall unsafe "aihc_io_adopt"
  adoptIOHandle :: Int -> Int -> IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_close"
  closeIOHandle :: Ptr IOHandle -> IO Int

foreign import ccall unsafe "aihc_memory_write_byte"
  writeMemoryByte :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe "aihc_memory_read_byte"
  readMemoryByte :: Addr# -> Int -> IO Int

foreign import ccall unsafe "aihc_io_submit_read"
  submitRead :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_submit_write"
  submitWrite :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO Int

foreign import ccall unsafe "aihc_io_take_open_result"
  takeOpenResult :: Ptr IORequest -> IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_raise_error"
  raiseIOErrorRaw :: Int -> IO Int
