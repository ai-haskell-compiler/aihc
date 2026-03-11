{-# LANGUAGE RecursiveDo #-}

module Main where

import qualified Control.Monad.RevState as Rev
import System.Exit (exitFailure)
import System.Timeout (timeout)


-- An example usage of reverse state
-- http://stackoverflow.com/questions/34030388/retrocausality-in-haskell-from-tardis-to-revstate/34287498
lastOccurrence :: Int -> Rev.State [Int] Bool
lastOccurrence x = mdo
  Rev.put (x : xs)
  xs <- Rev.get
  return (not (elem x xs))

lastOccurrences :: [Int] -> Rev.State [Int] [Bool]
lastOccurrences xs = mapM lastOccurrence xs

exampleValue :: [Bool]
exampleValue = flip Rev.evalState [] $ lastOccurrences [3,4,6,7,4,3,5,7]

expectedResult :: [Bool]
expectedResult = [False,False,True,False,True,True,True,True]

-- TODO: use a proper testing framework
main :: IO ()
main = do
  b <- timeout 1000000 $ return $! exampleValue == expectedResult
  if b == Just True
    then return ()
    else exitFailure
