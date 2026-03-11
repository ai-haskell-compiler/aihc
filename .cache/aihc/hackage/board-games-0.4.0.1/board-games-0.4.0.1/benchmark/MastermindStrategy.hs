module Main where

import qualified Game.Mastermind as MM
import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet
import Game.Utility (randomSelect, histogram)

import qualified System.Random as Rnd
import qualified System.IO as IO

import qualified Control.Parallel.Strategies as Strategy

import qualified Control.Monad.Trans.State as MS
import qualified Control.Functor.HT as FuncHT
import Control.Monad (replicateM, liftM2)

import Text.Printf (printf)

import qualified Data.Traversable as Trav
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.Empty as Empty
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Zip as Zip
import Data.NonEmpty ((!:))
import Data.Maybe (fromMaybe)


justState :: MS.State g (Maybe a) -> MS.State g a
justState = fmap $ fromMaybe (error "contradicting evaluation")

play ::
   (Rnd.RandomGen g) =>
   (Int -> [([Char], MM.Eval)] ->
    CodeSetTree.T Char -> MS.State g (Maybe String)) ->
   NonEmptySet.T Char -> String -> MS.State g [String]
play doGuess alphabet code = do
   let width = length code
   let go oldGuesses codeSet = do
         guess <- justState $ doGuess width oldGuesses codeSet
         let eval = MM.evaluate guess code
         let currentGuesses = oldGuesses++[(guess,eval)]
         if eval == MM.Eval width 0
            then return $ fmap fst currentGuesses
            else go currentGuesses $
                  CodeSetTree.intersection codeSet $
                  MM.matching (NonEmptySet.flatten alphabet) guess eval
   go [] $ CodeSet.cube alphabet width


type Tuple = NonEmpty.T (NonEmpty.T (NonEmpty.T (NonEmpty.T Empty.T)))

playVariants ::
   (Rnd.RandomGen g) =>
   NonEmptySet.T Char -> String -> MS.State g (String, Tuple Int)
playVariants alphabet code = do
   let alphabetFlat = NonEmptySet.flatten alphabet
   guessesColumns <-
      Trav.mapM (\strategy -> play strategy alphabet code) $
         (\width _oldGuesses -> MM.mixedRandomizedAttempt width) !:
         (\width _oldGuesses -> MM.randomizedAttempt width) !:
         (\width oldGuesses ->
            MM.scanningRandomizedAttempt width alphabetFlat oldGuesses) !:
         (\width _oldGuesses ->
            MM.separatingRandomizedAttempt width alphabetFlat) !:
         Empty.Cons
   return (code, fmap length guessesColumns)

playMany ::
   (Rnd.RandomGen g) =>
   NonEmpty.T [] Char -> Int -> g -> [(String, Tuple Int)]
playMany symbols width =
   let alphabet = NonEmptySet.fromList symbols
   in map
         (\code ->
            MS.evalState (playVariants alphabet code) (Rnd.mkStdGen 3141)) .
      MS.evalState
         (replicateM 100 $ replicateM width $
          randomSelect $ NonEmpty.flatten symbols)


withWriteFile :: FilePath -> (IO.Handle -> IO a) -> IO a
withWriteFile path act =
   IO.withFile path IO.WriteMode $ \h ->
      IO.hSetBuffering h IO.LineBuffering >> act h

run :: IO.Handle -> (String, NonEmpty.T [] Char) -> Int -> IO ()
run averagesHandle (alphabetName,alphabet) width = do
   let pathEnding = printf "-%s-%d.csv" alphabetName width
   let games =
         Strategy.withStrategy
            (Strategy.parList $
             Strategy.parTuple2
                Strategy.rdeepseq (Strategy.parTraversable Strategy.rdeepseq)) $
         playMany alphabet width $ Rnd.mkStdGen 42
   let writeLines path txt = withWriteFile path $ \h -> IO.hPutStr h txt
   let csvLine cells = List.intercalate "," $ Fold.toList cells
   let writeCSV path = writeLines path . unlines . map csvLine
   writeCSV ("game-lengths" ++ pathEnding) $
      map (\(code,lengths) -> ('"':code++'"':"") !: fmap show lengths) games
   let gamesPerStrategy = Zip.transposeClip $ map snd games
   writeCSV ("histogram" ++ pathEnding) $ map (fmap show) $
      FuncHT.outerProduct (Map.findWithDefault 0)
         [1 .. NonEmpty.foldl1Map max maximum gamesPerStrategy]
         (fmap histogram gamesPerStrategy)
   let average :: [Int] -> Double
       average xs = fromIntegral (sum xs) / fromIntegral (length xs)
       sqr x = x^(2::Int)
       averageDeviation xs =
         let mean = average xs
         in [show mean,
             printf "%.4f" $ sqrt (average (map sqr xs) - sqr mean)]
   IO.hPutStrLn averagesHandle $ csvLine $
      alphabetName !: show width !:
         Fold.foldMap averageDeviation gamesPerStrategy

main :: IO ()
main =
   withWriteFile "averages.csv" $ \averageHandle ->
   sequence_ $
   liftM2 (run averageHandle)
      [("numbers", '0'!:['1'..'9']), ("words", 'a'!:['b'..'z'])]
      [3..6]
