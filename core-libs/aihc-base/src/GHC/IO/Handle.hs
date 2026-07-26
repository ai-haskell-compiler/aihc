module GHC.IO.Handle
  ( Handle,
    hClose,
  )
where

import GHC.IO.Exception (ioError, ioErrorFromErrno)
import GHC.IO.FD (closeIOHandle)
import GHC.IO.Handle.Types (Handle (..), HandleState (..))
import GHC.MVar (putMVar, takeMVar)
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
