{-# LANGUAGE ForeignFunctionInterface, CPP #-}

-- | In ghc 6.12.3, this program spawns lots of thread when os = False.
-- If you set os = True, then it doesn't.
--
-- You can observe this either by seeing the virtual memory go crazy in top,
-- or by running in gdb and pressing ctrl-C.
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as B
import Text.XML.Expat.Tree
import System.Environment
import Data.IORef
import Foreign

os = False

foreign import ccall safe "callme" callme :: FunPtr (IO ()) -> IO ()
foreign import ccall safe "wrapper" mkPlain :: IO () -> IO (FunPtr (IO ()))

main = do
  args <- getArgs
  let (nthreads, nloops) = case args of
          threads : loops : _ -> (read threads, read loops)
          _                   -> (10, 10000)
  putStrLn $ show nthreads++" threads with "++show nloops++" loops each"++
             ", using '"++(if os then "forkOS" else "forkIO")++"'"
  qs <- newQSem 0
  replicateM_ nthreads $ do
    (if os then forkOS else forkIO) $ do
      cRef <- newIORef 0
      cb <- mkPlain $ modifyIORef cRef $ \x -> x `seq` (x+1)  
      replicateM_ nloops $ callme cb
      freeHaskellFunPtr cb
      c <- readIORef cRef
      -- 'callme' calls us back 10 times
      when (c /= nloops*10) $ fail $ "went really wrong: "++show (c, nloops*10)
      signalQSem qs
  replicateM_ nthreads $ waitQSem qs
  putStrLn "done"

