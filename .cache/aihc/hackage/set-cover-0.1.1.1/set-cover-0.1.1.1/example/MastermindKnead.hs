module Main where

import qualified Mastermind.Example as Example
import qualified Mastermind.Guess as Guess
import Mastermind.Guess (assignsFromGuesses)

import qualified Math.SetCover.Exact.Knead.Saturated as ESC_KneadSat
import qualified Math.SetCover.Exact.Knead as ESC_Knead

import qualified System.IO.Lazy as LazyIO


mainKnead :: IO ()
mainKnead = do
   let example = Example.haskell
   mapM_ (putStrLn . Guess.codeFromLabels) $ ESC_Knead.partitions $
      Example.apply assignsFromGuesses example

mainKneadIO :: IO ()
mainKneadIO = do
   let example = Example.master
   partit <- ESC_Knead.partitionsIO
   mapM_ (putStrLn . Guess.codeFromLabels) =<<
      (LazyIO.run $ partit $ Example.apply assignsFromGuesses example)

mainKneadVector :: IO ()
mainKneadVector = do
   let example = Example.haskell
   mapM_ (putStrLn . Guess.codeFromLabels) $ ESC_KneadSat.partitions $
      Example.apply assignsFromGuesses example


main :: IO ()
main = mainKnead
