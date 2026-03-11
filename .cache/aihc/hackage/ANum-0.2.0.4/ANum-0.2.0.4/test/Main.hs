module Main where

import Data.ANum
import System.Exit (exitFailure)

testResults :: [Bool]
testResults =
  [ unANum (1 + ANum (Just 1)) == Just (2 :: Integer)
  , unANum (3 * ANum (Just 5)) == Just (15 :: Integer)
  ]

failureCount :: Int
failureCount = length $ filter (== False) testResults

totalTests :: Int
totalTests = length testResults

testsFailed :: IO ()
testsFailed = do
  putStrLn ""
  putStrLn $ show failureCount ++ " of " ++ show totalTests ++ " tests failed."
  exitFailure

testsPassed :: IO ()
testsPassed = do
  putStrLn ""
  putStrLn $ "All " ++ show totalTests ++ " tests passed."
  return ()

main :: IO ()
main = if (failureCount > 0) then testsFailed else testsPassed
