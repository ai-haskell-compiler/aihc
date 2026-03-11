{- |
https://en.wikipedia.org/wiki/Mastermind_(board_game)

Given a list of guesses and according evaluations,
the solver computes a list of all possible codes
that match the obtained evaluations.

See also the @board-games@ package.
-}
module Main where

import qualified Mastermind.Example as Example
import qualified Mastermind.Guess as Guess
import Mastermind.Distinguish (distinguishingCodesCondensed)
import Mastermind.Utility (histogram)

import Mastermind.Guess (
   consistentCodes,
   evaluate,
   countEval,
   codeFromLabels,
   assignsFromGuesses,
   defaultAssignFlags,
   AssignFlags,
   Eval,
   EvalSumm(EvalSumm),
   Row(Row), Column(Column),
   )

import qualified Math.SetCover.Exact.UArray as ESC_UArray
import qualified Math.SetCover.Exact as ESC

import qualified System.IO as IO
import qualified Random as Random
import System.Random (StdGen, getStdGen, randomR, )

import Text.Printf (printf, )

import qualified Control.Monad.Trans.State as MS
import Control.Monad (replicateM, when, void, )
import Control.Applicative ((<$>), )

import qualified Data.Map as Map; import Data.Map (Map, )
import qualified Data.Set as Set; import Data.Set (Set, )
import qualified Data.Array as Array
import qualified Data.Foldable as Fold
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Foldable (foldMap, forM_, )
import Data.List (intercalate, )
import Data.Maybe (listToMaybe, isNothing, )


consistentCodesRnd ::
   (Ord a) =>
   AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> MS.State StdGen [[a]]
consistentCodesRnd flags width alphabet guesses =
   return . map codeFromLabels . ESC.partitions
      =<< Random.intSetFromSetAssigns
      =<< (Random.shuffle $ assignsFromGuesses flags width alphabet guesses)


newGuess, newGuessMatching, newGuessRandom ::
   (Ord a) =>
   AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> MS.State StdGen (Maybe [a])
newGuess = newGuessRandom

{-
Only guess codes that are consistent with all previously tried codes.
This can lead to overly long guess sequences like
@"master", "mastex", "mastey", "mastez"@ at the end of the game.
-}
newGuessMatching flags width alphabet oldGuesses =
   listToMaybe <$> consistentCodesRnd flags width alphabet oldGuesses

{-
Start with random guesses and use matching guesses only at the end of the game.
In order to make the attempts not obviously stupid
we rule out elements from attempts with empty evaluations
and stop searching for elements after an attempt with full evaluation.
When we have acquired enough information to match the number of possible codes
or when we reach a full evaluation,
we switch to guessing consistent codes.

As a consistent guess we use the first solution
generated from randomly shuffled assignments with a randomly shuffled alphabet.
This strategy prevents stupid guesses like "aaaaa",
but it does not minimize the number of guesses.
When the game approaches the end
there is often only one unknown letter left
and the algorithm makes a guess for ruling out every single candidate.
It would be more efficient to use non-coherent guesses in this situation
in order to rule out a whole bunch of candidates at once.
-}
newGuessRandom flags width alphabet oldGuesses = do
   let numPossibleEvals = div ((width+1)*(width+2)) 2
   let numMoves =
         floor
           (logBase
              (fromIntegral numPossibleEvals) (fromIntegral $ length alphabet)
            * fromIntegral width :: Double)
   let maybeCompleteEval =
         List.find
            ((\(EvalSumm correctPlaces correctSymbols) ->
               correctPlaces + correctSymbols >= width) . snd) oldGuesses
   let excluded =
         foldMap (Set.fromList . fst) $
         filter ((EvalSumm 0 0 ==) . snd) oldGuesses
   let restricted =
         case maybeCompleteEval of
            Just (code, _) -> Set.toList $ Set.fromList code
            Nothing -> filter (flip Set.notMember excluded) alphabet
   if null restricted
      then return Nothing
      else
         if length oldGuesses < numMoves && isNothing maybeCompleteEval
            then
               let arr = Array.listArray (0, length restricted - 1) restricted
               in  fmap Just $ replicateM width $ fmap (arr Array.!) $
                   MS.state $ randomR $ Array.bounds arr
            else newGuessMatching flags width restricted oldGuesses

formatEval :: EvalSumm -> String
formatEval (EvalSumm correctPlaces correctSymbols) =
   replicate correctPlaces 'x' ++ replicate correctSymbols 'o'

formatEvalGuess :: (Show code) => (code, EvalSumm) -> String
formatEvalGuess (guess, eval) = show guess ++ ' ' : formatEval eval


interaction :: Int -> [Char] -> IO ()
interaction width alphabet =
   let flags = defaultAssignFlags
       go guesses g0 =
          case MS.runState (newGuess flags width alphabet guesses) g0 of
             (Nothing, _) -> do
                putStrLn "Contradicting evaluations!"
                putStr "Please enter your secret code "
                putStrLn "and I will show you the corrected evaluations:"
                secret <- getLine
                forM_ (reverse guesses) $ \(guess,eval) ->
                   let correctEval = evaluate secret guess
                   in  when (correctEval /= eval) $ putStrLn $
                       formatEvalGuess (guess, correctEval)
             (Just attempt, g1) -> do
                putStr $ show attempt ++ " "
                IO.hFlush IO.stdout
                (eval@(EvalSumm numPlaces _numSymbols), evalRem)
                   <- MS.runState countEval <$> getLine
                when (not $ null evalRem) (putStrLn $ "ignoring: " ++ evalRem)
                if numPlaces >= width
                  then putStrLn "Code found!"
                  else go ((attempt, eval) : guesses) g1
   in  go [] =<< getStdGen

mainGame :: IO ()
mainGame = do
   let n = 5
   putStrLn $
      "Come up with a word consisting of " ++ show n ++
      " letters and evaluate my guesses."
   putStrLn "Enter 'x's for correct places and 'o's for correct symbols in any order."
   interaction n ['a'..'z']


testSolve :: IO ()
testSolve =
   mapM_ print $ consistentCodes defaultAssignFlags 6 ['a'..'z'] $
      Example.guesses_ Example.master


mainDistinguishing :: IO ()
mainDistinguishing =
   let codes =
         case 4::Int of
            0 -> ["abcd", "abce", "abcf"]
            1 -> ["abcdef", "abcdeg", "abcdeh"]
            2 -> ["master", "puzzle", "bubble", "flight", "people"]
            3 -> ["iuzamf", "gvarfe", "paqfes", "vamsej", "amgses", "majgep"]
            _ -> ["hlskoel", "hoskell", "hlskoll", "klosehl"]

   in mapM_ print $ take 10 $
      distinguishingCodesCondensed defaultAssignFlags
         (length $ head codes) ['a'..'z'] codes

{-
mainDistinguishing for ["hlskoel", "hoskell", "hlskoll", "klosehl"]:

("koellhs",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("kolelhs",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("eolslhk",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("kolslhe",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 3 4])
("kollshe",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("sklleho",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("kslleho",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 3 4])
("khlleso",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("kslleoh",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
("khlleos",[EvalSumm 0 7,EvalSumm 1 6,EvalSumm 0 6,EvalSumm 2 5])
-}


haskellCodes :: [String]
haskellCodes =
   "hoskell" : "hlskoel" : "hlskoll" : "klosehl" :
   "hpskell" : "hlskpel" : "hlskpll" : "klpsehl" :
   "haskell" : "hlskael" : "hlskall" : "klasehl" :
   "heskell" : "hlsksel" : "hlsksll" : "klesehl" :
   "hsskell" :
   []

groupSizesByEval :: (Ord a) => [[a]] -> [([a], Int)]
groupSizesByEval codes =
   decorate (Fold.maximum . histogram . flip map codes . evaluate) $
   replicateM (length $ head codes) $
   Set.toList $ foldMap Set.fromList codes

decorate :: (a -> b) -> [a] -> [(a, b)]
decorate f = map (\x -> (x, f x))

{- |
Return all elements that have minimal 'b'.
-}
allMinima :: (Ord b) => [(a,b)] -> ([a],b)
allMinima [] = error "allMinima: empty list"
allMinima ((a,b):abs_) =
   let go as0 b0 [] = (reverse as0, b0)
       go as0 b0 ((a1,b1):abs1) =
         case compare b1 b0 of
            LT -> go [a1] b1 abs1
            EQ -> go (a1:as0) b1 abs1
            GT -> go as0 b0 abs1
   in  go [a] b abs_

-- cf. board-games:Mastermind
mainBestSeparation :: IO ()
mainBestSeparation = do
   let codes = haskellCodes
   let (distCodes, groupSize) = allMinima $ groupSizesByEval codes
   mapM_ print distCodes
   let distCode = head distCodes
   void $ printf "%s, max group size %d\n" distCode groupSize
   mapM_ (putStrLn . formatEvalGuess) $ decorate (evaluate distCode) codes


data Step a b c =
     Attempt a
   | Complete b
   | Fail c

data Choice = None | Unique | Multiple

indentTree ::
   ESC.Tree label set ->
   [([Int], Step ((Choice, set), label, [label]) [label] (set, [label]))]
indentTree =
   let go numbers labels tree =
         case tree of
            ESC.Leaf -> [(numbers, Complete labels)]
            ESC.Branch set subTrees ->
               case subTrees of
                  [(label,subTree)] ->
                     (numbers, Attempt ((Unique, set), label, label:labels)) :
                     go numbers (label:labels) subTree
                  [] -> [(numbers, Fail (set, labels))]
                  _ ->
                     concatMap
                        (\(k, (label,subTree)) ->
                           (k:numbers,
                            Attempt ((Multiple, set), label, label:labels)) :
                           go (k:numbers) (label:labels) subTree) $
                     zip [1 ..] subTrees
   in  go [] []

formatElement :: Guess.X Char -> String
formatElement x =
   case x of
      Guess.EvalSymbol (Guess.Pos (Column col)) ->
         printf "symbol at position %d" col
      Guess.EvalSymbol (Guess.Eval eval (Row row) (Column col)) ->
         printf
            "choice whether a %s marker is at (%d,%d) or elsewhere"
            (Guess.nameFromEval $ Just eval) row col
      Guess.EvalSymbol (Guess.Symbol symbol) ->
         printf "way of placing symbol '%c'" symbol
      Guess.EvalRow eval (Row row) ->
         printf "way of placing %s markers in row %d"
            (Guess.nameFromEval $ Just eval) row
      Guess.EvalReserve (Row row) (Column col) ->
         printf
            ("choice between correct place, " ++
             "correct symbol or no marker at (%d,%d)")
            row col

formatReason :: (Choice, Set (Guess.X Char)) -> String
formatReason (choice, set) =
   let uniqueStr =
         case choice of
            None -> "no possible"
            Unique -> "unique"
            Multiple -> "try"
   in  case Set.toList set of
         [x] -> uniqueStr ++ " " ++ formatElement x
         _ -> error "reason set must be a singleton"

formatLabel :: Int -> Guess.Label Char -> String
formatLabel width label =
   case label of
      Left (Row row, eval, pattern) ->
         printf "pattern %s for %ss in row %d"
            (Guess.formatPattern eval pattern) (Guess.nameFromEval eval) row
      Right symbols ->
         "place symbols " ++ partialCodeFromLabels width [symbols]

evalMapFromLabel :: (Row, Maybe Eval, [Bool]) -> Map (Row,Column) (Maybe Eval)
evalMapFromLabel (row, eval, pattern) =
   Map.fromList $ map (\(col,_true) -> ((row,col), eval)) $
   filter snd $ zip [Column 0 ..] pattern

partialCodeFromLabels :: Int -> [[(Column, Char)]] -> String
partialCodeFromLabels width xss =
   Array.elems $
      Array.listArray (Column 0, Column (width - 1)) (repeat '_')
      Array.//
      concat xss

formatPatterns :: [(String, EvalSumm)] -> [Guess.Label Char] -> String
formatPatterns guesses labels =
   let (patternLabels, codeLabels) = ListHT.unzipEithers labels
       m = fmap Guess.charFromEval $ foldMap evalMapFromLabel patternLabels
       width = maximum $ map (length . fst) guesses
   in  unlines $
       zipWith
         (\row (guess,_eval) ->
            guess ++ ' ' :
            (map (\col -> Map.findWithDefault '_' (row,col) m) $
             Match.take guess [Column 0 ..]))
         [Row 0 ..] guesses
       ++
       ["", partialCodeFromLabels width codeLabels]


{-
   assignsFromMatchingCodes defaultAssignFlags 6 ['a'..'z']
      ["iuzamf", "gvarfe", "paqfes", "vamsej", "amgses"] -- , "majgep"]
-}


mainIntSet :: IO ()
mainIntSet = do
   let example = Example.haskell
   mapM_ (putStrLn . Guess.codeFromLabels) $ ESC.partitions $
      ESC.intSetFromSetAssigns $ Example.apply assignsFromGuesses example

mainUArray :: IO ()
mainUArray = do
   let example = Example.cover
   mapM_ (putStrLn . Guess.codeFromLabels) $ ESC_UArray.partitions $
      Example.apply assignsFromGuesses example

mainConsistent :: IO ()
mainConsistent = print $ Example.apply consistentCodes Example.cover

mainSolutions :: IO ()
mainSolutions = do
   let example = Example.cafe
   mapM_ (putStrLn . formatPatterns (Example.guesses_ example)) $
      ESC.partitions $ Example.apply assignsFromGuesses example

mainTree :: IO ()
mainTree = do
   let example = Example.cafe
       width = Example.width_ example
       guesses = Example.guesses_ example
       asns = Example.apply assignsFromGuesses example

   forM_ (indentTree $ ESC.decisionTree asns) $ \(numbers, msg) ->
      putStrLn $
         (intercalate "." $ map show $ reverse numbers)
         ++
         (case msg of
            Attempt (reason,label,_) ->
               ": " ++ formatLabel width label ++
               " - " ++ formatReason reason
            Complete labels -> "\n\n" ++ formatPatterns guesses labels
            Fail (reason,_) ->
               ": failed because " ++ formatReason (None,reason))

mainDetail :: IO ()
mainDetail = do
   let example = Example.cafe
       width = Example.width_ example
       guesses = Example.guesses_ example
       asns = Example.apply assignsFromGuesses example

   forM_ (indentTree $ ESC.decisionTree asns) $ \(numbers, msg) ->
      putStrLn $
         (intercalate "." $ map show $ reverse numbers)
         ++
         (case msg of
            Attempt (reason,label,labels) ->
               ": " ++ formatLabel width label ++
               " - " ++ formatReason reason ++ "\n\n" ++
               formatPatterns guesses labels
            Complete _labels -> " - completed\n"
            Fail (reason,_) ->
               ": failed because " ++ formatReason (None,reason) ++ "\n")


main :: IO ()
main = mainGame
