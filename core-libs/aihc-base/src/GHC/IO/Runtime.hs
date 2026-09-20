{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE GHCForeignImportPrim #-}
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
    ioHandleDescriptor,
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
import GHC.Int (Int (..))
import GHC.Num (Num (..))
import GHC.Prim (Addr#, Int#, RealWorld, State#, awaitIO#)
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

foreign import prim submitIORead# :: Addr# -> Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Addr# #)

foreign import prim submitIOWrite# :: Addr# -> Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Addr# #)

foreign import prim submitIOOpen# :: Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, Addr# #)

data IORequest

foreign import ccall unsafe "aihc_io_stdin"
  stdinHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stdout"
  stdoutHandle :: IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_stderr"
  stderrHandle :: IO (Ptr IOHandle)

submitOpen :: Addr# -> Int -> Int -> IO (Ptr IORequest)
submitOpen path (I# length) (I# mode) =
  IO
    ( \state -> case submitIOOpen# path length mode state of
        (# next, request #) -> (# next, Ptr request #)
    )

foreign import ccall unsafe "aihc_io_open_result_error"
  openResultError :: Ptr IOHandle -> IO Int

-- | The open mode of a descriptor the program already has, or a negative
-- error. Adopting a descriptor needs no request: neither call blocks.
foreign import ccall unsafe "aihc_io_descriptor_mode"
  descriptorMode :: Int -> IO Int

foreign import ccall unsafe "aihc_io_adopt"
  adoptIOHandle :: Int -> Int -> IO (Ptr IOHandle)

-- | The POSIX descriptor, or -1 when the host has no numeric descriptors.
foreign import ccall unsafe "aihc_io_handle_descriptor"
  ioHandleDescriptor :: Ptr IOHandle -> IO Int

foreign import ccall unsafe "aihc_io_close"
  closeIOHandle :: Ptr IOHandle -> IO Int

foreign import ccall unsafe "aihc_memory_write_byte"
  writeMemoryByte :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe "aihc_memory_read_byte"
  readMemoryByte :: Addr# -> Int -> IO Int

submitRead :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)
submitRead (Ptr handle) buffer (I# offset) (I# length) =
  IO
    ( \state -> case submitIORead# handle buffer offset length state of
        (# next, request #) -> (# next, Ptr request #)
    )

submitWrite :: Ptr IOHandle -> Addr# -> Int -> Int -> IO (Ptr IORequest)
submitWrite (Ptr handle) buffer (I# offset) (I# length) =
  IO
    ( \state -> case submitIOWrite# handle buffer offset length state of
        (# next, request #) -> (# next, Ptr request #)
    )

foreign import ccall unsafe "aihc_io_take_result"
  takeResult :: Ptr IORequest -> IO Int

foreign import ccall unsafe "aihc_io_take_open_result"
  takeOpenResult :: Ptr IORequest -> IO (Ptr IOHandle)

foreign import ccall unsafe "aihc_io_raise_error"
  raiseIOErrorRaw :: Int -> IO Int
