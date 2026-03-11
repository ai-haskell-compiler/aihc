-- Demo similar to apt-get's download display.

import Control.Concurrent.Async
import Control.Concurrent
import System.Console.Concurrent
import System.Console.Regions
import System.Console.ANSI

main = displayConsoleRegions $ do
	mapConcurrently downline
		[ [dl "pony", growingdots, dl "mango"]
		, [dl "foo", dl "bar", dl "very large"]
		]
		`concurrently` mapM_ message [1..20]
		`concurrently` mapM_ errormessage [2,6..20]

message n = do
	threadDelay 500000
	outputConcurrent ("Updated blah blah #" ++ show n ++ "\n")

errormessage n = do
	threadDelay 2300000
	outputConcurrent ("Failed to frob " ++ show n ++ "\n")

downline cs = withConsoleRegion Linear $ \r ->
	mapConcurrently (\a -> a r) (reverse cs)

dl c parent = withConsoleRegion (InLine parent) (go 0)
  where
	go n r
		| n <= 100 = do
			setConsoleRegion r $
				"[" ++ setSGRCode [SetColor Foreground Vivid Green] ++ c ++ setSGRCode [Reset] ++ " " ++ show n ++ "%] "
			threadDelay (25000 * length c)
			go (n+1) r
		| otherwise = finishConsoleRegion r $
			"Downloaded " ++ c ++ ".deb"

growingdots parent = withConsoleRegion (InLine parent) (go 0)
  where
	go n r
		| n <= 300 = do
			setConsoleRegion r ("[" ++ setSGRCode [SetColor Foreground Vivid Blue] ++ replicate n '.' ++ setSGRCode [Reset] ++ "] ")
			threadDelay (100000)
			go (n+1) r
		| otherwise = return ()
