module Etalon (testEtalon) where

import System.Random
import Data.Ord
import Data.List (sortBy)
import Test.Tasty.HUnit

unsort :: (RandomGen g) => g -> [x] -> [x]
unsort g es = map snd . sortBy (comparing fst) $ zip rs es
  where rs = randoms g :: [Integer]

fetchRandomLines :: Read b => Int -> FilePath -> IO [[b]]
fetchRandomLines n filename = do
  etalon <- readFile filename
  gen <- newStdGen
  let items = (take n . unsort gen . lines) etalon
  let tests = map (map read . words) items
  return tests

testEtalon :: Int -> ([Integer] -> Bool) -> String -> Assertion
testEtalon n f filename = do
  tests <- fetchRandomLines n filename
  let results = map f tests
  let fails = filter (not . snd) (zip tests results)
  case fails of
    [] -> pure ()
    failure : _ -> assertFailure $ "failed at " ++ show (fst failure)
