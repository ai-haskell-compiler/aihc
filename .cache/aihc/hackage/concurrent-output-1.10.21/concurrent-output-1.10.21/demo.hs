import Control.Concurrent.Async
import Control.Concurrent
import Control.Concurrent.STM
import System.Console.Concurrent
import System.Console.Regions
import System.Process
import qualified Data.Text as T
import Data.List

main = displayConsoleRegions $ do
	mapConcurrently download [1..5]
		`concurrently` mapM_ message [1..15]
		`concurrently` ls

message :: Int -> IO ()
message n = do
	threadDelay 300000
	outputConcurrent ("Message " ++ show n ++ "\n")

download :: Int -> IO ()
download n = withConsoleRegion Linear $ \r -> do
	threadDelay (10000 * n)
	setConsoleRegion r basemsg
	go n r
  where
	basemsg = "Download " ++ show n
	go c r
		| c < 1 = finishConsoleRegion r
			(basemsg ++ " done!\n  Took xxx seconds.")
		| otherwise = do
			appendConsoleRegion r " ... "
			threadDelay 1000000
			go (c-1) r

ls :: IO ()
ls = do
	threadDelay 1000000
	(Nothing, Nothing, Nothing, p) <- createProcessConcurrent (proc "ls" ["-C"])
	outputConcurrent "started running ls >>>"
	_ <- waitForProcessConcurrent p
	outputConcurrent "<<< ls is done!\n"
	return ()
