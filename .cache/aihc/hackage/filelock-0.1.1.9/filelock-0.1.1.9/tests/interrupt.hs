import Control.Concurrent
import Control.Exception
import Control.Monad
import System.FileLock
import System.Exit
import System.Timeout

main :: IO ()
main = withFileLock lockFilePath Exclusive $ \_ -> do
  mvar <- newMVar Nothing
  _ <- forkIO $ do
    -- The attempt to lock the file again should block, but it should be
    -- interrupted by the timeout, returning Nothing.
    --
    -- Also masking shouldn't change interruptibility.
    r <- timeout 1000000 $ mask $ \_ -> lockFile lockFilePath Exclusive
    _ <- swapMVar mvar (Just r)
    return ()
  threadDelay 2000000
  res <- readMVar mvar
  when (res /= Just Nothing) $
    die $ "unexpected result: " ++ show (fmap (const ()) <$> res)
  where
    lockFilePath = "interrupt_test.lock"
