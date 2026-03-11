import Control.Concurrent.Async
import Control.Concurrent
import System.Console.Regions
import qualified Data.Text as T
import Control.Concurrent.STM
import Control.Applicative
import Data.Time.Clock
import Control.Monad
import Data.Monoid
import System.Process

main :: IO ()
main = void $ displayConsoleRegions $ do
	void titleRegion
	ir <- infoRegion
	cr <- clockRegion
	rr <- rulerRegion
	growingDots `concurrently` runBash
	mapM_ closeConsoleRegion [ir, cr]

titleRegion :: IO ConsoleRegion
titleRegion = do
	r <- openConsoleRegion Linear
	setConsoleRegion r "STM demo!"
	return r

infoRegion :: IO ConsoleRegion
infoRegion = do
	r <- openConsoleRegion Linear
	setConsoleRegion r $ do
	 	w <- consoleWidth
	 	h <- consoleHeight
		regions <- readTMVar regionList
 		return $ T.pack $ unwords
 			[ "size:"
			, show w
 			, "x"
			, show h
			, "regions: "
			, show (length regions)
 			]
	return r

timeDisplay :: TVar UTCTime -> STM T.Text
timeDisplay tv = T.pack . show <$> readTVar tv

clockRegion :: IO ConsoleRegion
clockRegion = do
	tv <- atomically . newTVar =<< getCurrentTime
	async $ forever $ do
		threadDelay 1000000 -- 1 sec
		atomically . (writeTVar tv) =<< getCurrentTime
	atomically $ do
		r <- openConsoleRegion Linear
		setConsoleRegion r (timeDisplay tv)
		rightAlign r
		return r

rulerRegion :: IO ConsoleRegion
rulerRegion = do
	r <- openConsoleRegion Linear
	setConsoleRegion r $ do
		width <- consoleWidth
		return $ T.pack $ take width nums
	return r
  where
	nums = cycle $ concatMap show [0..9]

rightAlign :: ConsoleRegion -> STM ()
rightAlign r = tuneDisplay r $ \t -> do
        w <- consoleWidth
        return (T.replicate (w - T.length t) (T.singleton ' ') <> t)

growingDots :: IO ()
growingDots = withConsoleRegion Linear $ \r -> do
	atomically $ rightAlign r
	width <- atomically consoleWidth
	void $ replicateM width $ do
		appendConsoleRegion r "." 
		threadDelay (100000)

runBash :: IO ()
runBash = do
	-- Wait for growingDots to display some.
	threadDelay 1000000
	-- Temporarily clear whatever console regions are open.
	rs <- waitDisplayChange $ swapTMVar regionList []
	putStrLn "We interrupt this demo to run a shell prompt. exit to continue!"
	callCommand "bash"
	-- Restore the console regions.
	void $ atomically $ swapTMVar regionList rs
