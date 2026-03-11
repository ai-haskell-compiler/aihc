import Control.Concurrent.Async
import Control.Concurrent
import System.Console.Regions
import Control.Monad

main :: IO ()
main = displayConsoleRegions $ void $ mapConcurrently id
	[ spinner 100 1 "Pinwheels!!" setConsoleRegion "/-\\|" (withtitle 1)
	, spinner 100 1 "Bubbles!!!!" setConsoleRegion ".oOo." (withtitle 1)
	, spinner 100 1 "Dots......!" appendConsoleRegion "."  (const (take 3))
	, spinner  30 2 "KleisiFish?" setConsoleRegion "  <=<   <=<  " (withtitle 10)
	, spinner   9 9 "Countdowns!" setConsoleRegion
		(reverse ([1..10] :: [Int]))
		(\t n -> t ++ show (head n))
	]
  where
	withtitle n t s = t ++ take n s

spinner :: Int -> Int -> String -> (ConsoleRegion -> String -> IO ()) -> [s] -> (String -> [s] -> String) -> IO ()
spinner cycles delay title updater source f =
	withConsoleRegion Linear $ \r -> do
		setConsoleRegion r title'
		mapM_ (go r) (zip [1..cycles] sourcestream)
		finishConsoleRegion r ("Enough " ++ title)
  where
	title' = title ++ "  "
	sourcestream = repeat (concat (repeat source))
	go r (n, s) = do
		updater r (f title' (drop n s))
		threadDelay (delay * 100000)
