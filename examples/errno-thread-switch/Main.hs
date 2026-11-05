module Main (main) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Foreign.C.Error (eBADF, getErrno)
import Foreign.C.String (CString, withCString)
import Foreign.C.Types (CInt (..))

foreign import ccall unsafe "close" c_close :: CInt -> IO CInt

foreign import ccall unsafe "chdir" c_chdir :: CString -> IO CInt

-- errno belongs to each Haskell thread. The main thread sets it with a
-- failed close, and then it waits. The other thread runs and sets errno
-- with a failed chdir. When the main thread runs again, it reads its own
-- errno.
main :: IO ()
main = do
  done <- newEmptyMVar
  _ <- forkIO $ do
    _ <- withCString "/aihc-errno-thread-switch-missing" c_chdir
    putMVar done ()
  _ <- c_close (-1)
  takeMVar done
  errno <- getErrno
  print (errno == eBADF)
