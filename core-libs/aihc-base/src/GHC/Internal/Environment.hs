{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Access to the runtime-owned complete program argument vector and to the
-- process environment the host handed the runtime. Both ABIs use one UTF-8
-- byte string per entry, terminated by a zero byte.
module GHC.Internal.Environment
  ( getFullArgs,
    getFullEnvironment,
    setFullArgs,
  )
where

import Data.Foldable (any, elem)
import GHC.Char (chr)
import GHC.IO (IO (..))
import GHC.IO.Exception (IOErrorType (..), ioError)
import GHC.IO.Runtime (readMemoryByte, writeMemoryByte)
import GHC.Int (Int (..))
import GHC.Internal.Utf8 (decodeUtf8, encodeUtf8)
import GHC.Prim (Addr#, MutableByteArray#, RealWorld, mutableByteArrayContents#, newPinnedByteArray#)
import System.IO.Error (mkIOError)
import Prelude

data ArgumentBuffer = ArgumentBuffer (MutableByteArray# RealWorld)

foreign import ccall unsafe "aihc_program_arguments_size"
  argumentSize :: IO Int

foreign import ccall unsafe "aihc_program_arguments_copy"
  copyArguments :: Addr# -> Int -> IO Int

foreign import ccall unsafe "aihc_program_arguments_replace"
  replaceArguments :: Addr# -> Int -> IO Int

foreign import ccall unsafe "aihc_program_environment_size"
  environmentSize :: IO Int

foreign import ccall unsafe "aihc_program_environment_copy"
  copyEnvironment :: Addr# -> Int -> IO Int

getFullArgs :: IO [String]
getFullArgs = do
  required <- argumentSize
  readSnapshot copyArguments required

-- | Every environment entry of the process, as the @NAME=VALUE@ strings the
-- host handed the runtime. The runtime keeps one snapshot taken before the
-- machine starts, so the list does not change while the program runs.
getFullEnvironment :: IO [String]
getFullEnvironment = do
  required <- environmentSize
  readSnapshot copyEnvironment required

-- | Read the runtime's string buffer. The store may grow between the size
-- query and the copy, so a short copy reports the size it needed and the read
-- starts over with it.
readSnapshot :: (Addr# -> Int -> IO Int) -> Int -> IO [String]
readSnapshot copy requested = do
  buffer <- newArgumentBuffer (atLeastOne requested)
  actual <- copyBuffer copy buffer requested
  case actual > requested of
    True -> readSnapshot copy actual
    False -> do
      bytes <- readBytes buffer 0 actual
      return (decodeArguments bytes)

setFullArgs :: [String] -> IO ()
setFullArgs arguments =
  case anyContainsNul arguments of
    True -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)
    False -> do
      let bytes = encodeArguments arguments
          size = byteCount bytes
      buffer <- newArgumentBuffer (atLeastOne size)
      writeBytes buffer 0 bytes
      result <- replaceArgumentBuffer buffer size
      case result == 0 of
        True -> return ()
        False -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)

newArgumentBuffer :: Int -> IO ArgumentBuffer
newArgumentBuffer (I# size) =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# nextState, buffer #) -> (# nextState, ArgumentBuffer buffer #)
    )

atLeastOne :: Int -> Int
atLeastOne size =
  case size < 1 of
    True -> 1
    False -> size

copyBuffer :: (Addr# -> Int -> IO Int) -> ArgumentBuffer -> Int -> IO Int
copyBuffer copy (ArgumentBuffer buffer) = copy (mutableByteArrayContents# buffer)

replaceArgumentBuffer :: ArgumentBuffer -> Int -> IO Int
replaceArgumentBuffer (ArgumentBuffer buffer) = replaceArguments (mutableByteArrayContents# buffer)

readArgumentByte :: ArgumentBuffer -> Int -> IO Int
readArgumentByte (ArgumentBuffer buffer) = readMemoryByte (mutableByteArrayContents# buffer)

writeArgumentByte :: ArgumentBuffer -> Int -> Int -> IO Int
writeArgumentByte (ArgumentBuffer buffer) = writeMemoryByte (mutableByteArrayContents# buffer)

readBytes :: ArgumentBuffer -> Int -> Int -> IO [Int]
readBytes buffer offset length =
  case offset == length of
    True -> return []
    False -> do
      byte <- readArgumentByte buffer offset
      rest <- readBytes buffer (offset + 1) length
      return (byte : rest)

writeBytes :: ArgumentBuffer -> Int -> [Int] -> IO ()
writeBytes _ _ [] = return ()
writeBytes buffer offset (byte : rest) = do
  result <- writeArgumentByte buffer offset byte
  case result == 0 of
    True -> writeBytes buffer (offset + 1) rest
    False -> ioError (mkIOError InvalidArgument "setArgs" Nothing Nothing)

encodeArguments :: [String] -> [Int]
encodeArguments [] = []
encodeArguments (argument : rest) = encodeUtf8 argument ++ (0 : encodeArguments rest)

decodeArguments :: [Int] -> [String]
decodeArguments [] = []
decodeArguments bytes =
  case takeArgumentBytes bytes of
    (argument, rest) -> decodeUtf8 argument : decodeArguments rest

takeArgumentBytes :: [Int] -> ([Int], [Int])
takeArgumentBytes [] = ([], [])
takeArgumentBytes (byte : rest) =
  case byte == 0 of
    True -> ([], rest)
    False ->
      case takeArgumentBytes rest of
        (argument, remaining) -> (byte : argument, remaining)

byteCount :: [a] -> Int
byteCount [] = 0
byteCount (_ : rest) = 1 + byteCount rest

anyContainsNul :: [String] -> Bool
anyContainsNul = any containsNul

containsNul :: String -> Bool
containsNul = elem (chr 0)
