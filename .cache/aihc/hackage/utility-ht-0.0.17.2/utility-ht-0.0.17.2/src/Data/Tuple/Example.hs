module Data.Tuple.Example where

import qualified Data.Tuple.Lazy as Lazy
import qualified Data.Tuple.Strict as Strict

import Data.List.HT (sieve, )


partitionLazy :: (a -> Bool) -> [a] -> ([a], [a])
partitionLazy p =
   foldr
      (\x -> (if p x then Lazy.mapFst else Lazy.mapSnd) (x:))
      ([], [])

partitionStrict :: (a -> Bool) -> [a] -> ([a], [a])
partitionStrict p =
   foldr
      (\x -> (if p x then Strict.mapFst else Strict.mapSnd) (x:))
      ([], [])


mainPartitionRuns :: IO ()
mainPartitionRuns =
   print $ partitionLazy (>=0) $ repeat (0::Int)

mainPartitionBlocks :: IO ()
mainPartitionBlocks =
   print $ partitionStrict (>=0) $ repeat (0::Int)



printSomeChars :: (Show a) => a -> IO ()
printSomeChars = putStrLn . sieve 100000 . show

mainMemoryOk :: IO ()
mainMemoryOk =
   printSomeChars $ Strict.mapSnd (1+) $ (iterate (1+) (0::Int), 0::Int)

mainMemoryLeak :: IO ()
mainMemoryLeak =
   printSomeChars $ Lazy.mapSnd (1+) $ (iterate (1+) (0::Int), 0::Int)
