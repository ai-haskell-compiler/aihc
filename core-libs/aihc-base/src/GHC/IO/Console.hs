{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}

-- | Low-level byte output used to bootstrap the text operations exported by
-- 'Prelude'. This module must not depend on 'Prelude', because 'Prelude'
-- supplies the public 'String' traversal and encoding policy.
module GHC.IO.Console
  ( withOutputBuffer,
    writeOutputByte,
    writeStdout,
  )
where

import GHC.Base (bindIO, returnIO)
import GHC.IO (IO (..))
import GHC.IO.Runtime (IOHandle, IORequest, raiseIOErrorRaw, stdoutHandle, submitWrite, takeResult, writeMemoryByte)
import GHC.Int (Int (..))
import GHC.Prim
  ( MutableByteArray#,
    RealWorld,
    awaitIO#,
    mutableByteArrayContents#,
    newPinnedByteArray#,
    (+#),
    (-#),
    (<#),
    (==#),
  )
import GHC.Ptr (Ptr (..))

withOutputBuffer :: Int# -> (MutableByteArray# RealWorld -> IO a) -> IO a
withOutputBuffer size action =
  IO
    ( \state ->
        case newPinnedByteArray# size state of
          (# allocatedState, buffer #) ->
            case action buffer of
              IO run -> run allocatedState
    )

writeOutputByte :: MutableByteArray# RealWorld -> Int# -> Int# -> IO ()
writeOutputByte buffer offset value =
  bindIO
    (writeMemoryByte (mutableByteArrayContents# buffer) (I# offset) (I# value))
    checkOutputByteResult

checkOutputByteResult :: Int -> IO ()
checkOutputByteResult (I# encodedError) =
  case (<#) encodedError 0# of
    1# -> raiseConsoleIOError ((-#) ((-#) 0# encodedError) 1#)
    _ -> returnIO ()

writeStdout :: MutableByteArray# RealWorld -> Int# -> IO ()
writeStdout buffer count =
  case (==#) count 0# of
    1# -> returnIO ()
    _ ->
      bindIO
        stdoutHandle
        ( \handle ->
            writeStdoutLoop handle (mutableByteArrayContents# buffer) 0# count
        )

writeStdoutLoop :: Ptr IOHandle -> Addr# -> Int# -> Int# -> IO ()
writeStdoutLoop handle buffer offset remaining =
  bindIO
    (submitWrite handle buffer (I# offset) (I# remaining))
    ( \request ->
        bindIO
          (awaitConsoleIO request)
          (takeWriteResult request handle buffer offset remaining)
    )

takeWriteResult :: Ptr IORequest -> Ptr IOHandle -> Addr# -> Int# -> Int# -> () -> IO ()
takeWriteResult request handle buffer offset remaining () =
  bindIO (takeResult request) (finishWriteResult handle buffer offset remaining)

finishWriteResult :: Ptr IOHandle -> Addr# -> Int# -> Int# -> Int -> IO ()
finishWriteResult handle buffer offset remaining (I# transferred) =
  case (<#) transferred 0# of
    1# -> raiseConsoleIOError ((-#) ((-#) 0# transferred) 1#)
    _ ->
      case (==#) transferred 0# of
        1# -> raiseConsoleIOError 6#
        _ ->
          case (==#) transferred remaining of
            1# -> returnIO ()
            _ -> writeStdoutLoop handle buffer ((+#) offset transferred) ((-#) remaining transferred)

awaitConsoleIO :: Ptr request -> IO ()
awaitConsoleIO (Ptr request) =
  IO
    ( \state ->
        case awaitIO# request state of
          nextState -> (# nextState, () #)
    )

raiseConsoleIOError :: Int# -> IO ()
raiseConsoleIOError exceptionCode =
  bindIO (raiseIOErrorRaw (I# exceptionCode)) retryConsoleIOError

retryConsoleIOError :: Int -> IO ()
retryConsoleIOError (I# exceptionCode) = raiseConsoleIOError exceptionCode
