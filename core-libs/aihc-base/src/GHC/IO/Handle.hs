{-# LANGUAGE MagicHash #-}

module GHC.IO.Handle
  ( Handle,
    hClose,
    hGetBuf,
    hPutBuf,
  )
where

import GHC.IO.Exception (ioError, ioErrorFromErrno)
import GHC.IO.FD (closeIOHandle)
import GHC.IO.Handle.Types (Handle (..), HandleState (..))
import GHC.MVar (putMVar, takeMVar)
import GHC.Prim (raise#)
import GHC.Ptr (Ptr)
import Prelude

hClose :: Handle -> IO ()
hClose (FileHandle name stateVariable) = do
  state <- takeMVar stateVariable
  case state of
    HandleClosed -> putMVar stateVariable HandleClosed
    HandleOpen rawHandle _ -> do
      result <- closeIOHandle rawHandle
      putMVar stateVariable HandleClosed
      case result < 0 of
        True -> ioError (ioErrorFromErrno "hClose" (Just name) (decodeHandleError result))
        False -> return ()

decodeHandleError :: Int -> Int
decodeHandleError result = negate result - 1

hGetBuf :: Handle -> Ptr a -> Int -> IO Int
hGetBuf _ _ _ = raise# ()

hPutBuf :: Handle -> Ptr a -> Int -> IO ()
hPutBuf _ _ _ = raise# ()
