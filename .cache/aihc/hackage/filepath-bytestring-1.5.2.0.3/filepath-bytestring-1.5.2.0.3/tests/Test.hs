
module Test(main) where

import System.Environment
import TestGen
import TestEquiv
import Control.Monad
import Data.Maybe
import Test.QuickCheck


main :: IO ()
main = do
    args <- getArgs
    let count = case args of i:_ -> read i; _ -> 10000
    putStrLn $ "Testing with " ++ show count ++ " repetitions"
    let alltests = equivtests ++ tests
    let ntotal = length alltests
    let showOutput x = show x{output=""} ++ "\n" ++ output x
    bad <- fmap catMaybes $ forM (zip [(1 :: Integer)..] alltests) $ \(i,(msg,prop)) -> do
        putStrLn $ "Test " ++ show i ++ " of " ++ show ntotal ++ ": " ++ msg
        res <- quickCheckWithResult stdArgs{chatty=False, maxSuccess=count} prop
        case res of
            Success{} -> return Nothing
            bad -> do putStrLn $ showOutput bad; putStrLn "TEST FAILURE!"; return $ Just (msg,bad)
    if null bad then
        putStrLn $ "Success, " ++ show ntotal ++ " tests passed"
     else do
        putStrLn $ show (length bad) ++ " FAILURES\n"
        forM_ (zip [(1 :: Integer)..] bad) $ \(i,(a,b)) ->
            putStrLn $ "FAILURE " ++ show i ++ ": " ++ a ++ "\n" ++ showOutput b ++ "\n"
        fail $ "FAILURE, failed " ++ show (length bad) ++ " of " ++ show ntotal ++ " tests"
