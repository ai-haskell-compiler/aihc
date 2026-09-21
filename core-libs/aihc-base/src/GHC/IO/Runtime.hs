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

import Aihc.Prim.IO (IOHandle#, IORequest#, awaitIO#)
import GHC.IO (IO (..))
import GHC.Int (Int (..))
import GHC.Num (Num (..))
import GHC.Prim (Addr#, Int#, RealWorld, State#)

data IOHandle = IOHandle IOHandle#

-- | Suspend the current green thread until an opaque runtime request is ready.
awaitIO :: IORequest -> IO ()
awaitIO (IORequest request) =
  IO
    ( \state ->
        case awaitIO# request state of
          nextState -> (# nextState, () #)
    )

-- | The runtime reports an error number @e@ as @-(e + 1)@, so that a result
-- and an error share one signed word. This undoes that.
decodeError :: Int -> Int
decodeError result = negate result - 1

foreign import prim submitIORead# :: IOHandle# -> Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, IORequest# #)

foreign import prim submitIOWrite# :: IOHandle# -> Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, IORequest# #)

foreign import prim submitIOOpen# :: Addr# -> Int# -> Int# -> State# RealWorld -> (# State# RealWorld, IORequest# #)

data IORequest = IORequest IORequest#

foreign import prim stdinIOHandle# :: State# RealWorld -> (# State# RealWorld, IOHandle# #)

stdinHandle :: IO IOHandle
stdinHandle =
  IO
    ( \state -> case stdinIOHandle# state of
        (# next, handle #) -> (# next, IOHandle handle #)
    )

foreign import prim stdoutIOHandle# :: State# RealWorld -> (# State# RealWorld, IOHandle# #)

stdoutHandle :: IO IOHandle
stdoutHandle =
  IO
    ( \state -> case stdoutIOHandle# state of
        (# next, handle #) -> (# next, IOHandle handle #)
    )

foreign import prim stderrIOHandle# :: State# RealWorld -> (# State# RealWorld, IOHandle# #)

stderrHandle :: IO IOHandle
stderrHandle =
  IO
    ( \state -> case stderrIOHandle# state of
        (# next, handle #) -> (# next, IOHandle handle #)
    )

submitOpen :: Addr# -> Int -> Int -> IO IORequest
submitOpen path (I# length) (I# mode) =
  IO
    ( \state -> case submitIOOpen# path length mode state of
        (# next, request #) -> (# next, IORequest request #)
    )

foreign import prim ioOpenResultError# :: IOHandle# -> State# RealWorld -> (# State# RealWorld, Int# #)

openResultError :: IOHandle -> IO Int
openResultError (IOHandle value) =
  IO
    ( \state -> case ioOpenResultError# value state of
        (# next, result #) -> (# next, I# result #)
    )

-- | The open mode of a descriptor the program already has, or a negative
-- error. Adopting a descriptor needs no request: neither call blocks.
foreign import ccall unsafe "aihc_io_descriptor_mode"
  descriptorMode :: Int -> IO Int

foreign import prim adoptIOHandle# :: Int# -> Int# -> State# RealWorld -> (# State# RealWorld, IOHandle# #)

adoptIOHandle :: Int -> Int -> IO IOHandle
adoptIOHandle (I# descriptor) (I# mode) =
  IO
    ( \state -> case adoptIOHandle# descriptor mode state of
        (# next, handle #) -> (# next, IOHandle handle #)
    )

-- | The POSIX descriptor, or -1 when the host has no numeric descriptors.
foreign import prim ioHandleDescriptor# :: IOHandle# -> State# RealWorld -> (# State# RealWorld, Int# #)

ioHandleDescriptor :: IOHandle -> IO Int
ioHandleDescriptor (IOHandle value) =
  IO
    ( \state -> case ioHandleDescriptor# value state of
        (# next, result #) -> (# next, I# result #)
    )

foreign import prim closeIOHandle# :: IOHandle# -> State# RealWorld -> (# State# RealWorld, Int# #)

closeIOHandle :: IOHandle -> IO Int
closeIOHandle (IOHandle value) =
  IO
    ( \state -> case closeIOHandle# value state of
        (# next, result #) -> (# next, I# result #)
    )

foreign import ccall unsafe "aihc_memory_write_byte"
  writeMemoryByte :: Addr# -> Int -> Int -> IO Int

foreign import ccall unsafe "aihc_memory_read_byte"
  readMemoryByte :: Addr# -> Int -> IO Int

submitRead :: IOHandle -> Addr# -> Int -> Int -> IO IORequest
submitRead (IOHandle handle) buffer (I# offset) (I# length) =
  IO
    ( \state -> case submitIORead# handle buffer offset length state of
        (# next, request #) -> (# next, IORequest request #)
    )

submitWrite :: IOHandle -> Addr# -> Int -> Int -> IO IORequest
submitWrite (IOHandle handle) buffer (I# offset) (I# length) =
  IO
    ( \state -> case submitIOWrite# handle buffer offset length state of
        (# next, request #) -> (# next, IORequest request #)
    )

foreign import prim takeIOResult# :: IORequest# -> State# RealWorld -> (# State# RealWorld, Int# #)

takeResult :: IORequest -> IO Int
takeResult (IORequest value) =
  IO
    ( \state -> case takeIOResult# value state of
        (# next, result #) -> (# next, I# result #)
    )

foreign import prim takeIOOpenResult# :: IORequest# -> State# RealWorld -> (# State# RealWorld, IOHandle# #)

takeOpenResult :: IORequest -> IO IOHandle
takeOpenResult (IORequest request) =
  IO
    ( \state -> case takeIOOpenResult# request state of
        (# next, handle #) -> (# next, IOHandle handle #)
    )

foreign import ccall unsafe "aihc_io_raise_error"
  raiseIOErrorRaw :: Int -> IO Int
