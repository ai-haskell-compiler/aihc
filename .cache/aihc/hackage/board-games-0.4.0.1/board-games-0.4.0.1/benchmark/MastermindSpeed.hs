module Main where

import Game.Mastermind (Eval(Eval), matching)

import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
import qualified Game.Mastermind.CodeSet.Union as CodeSetUnion
import qualified Game.Mastermind.CodeSet as CodeSet

import Criterion.Main (Benchmark, defaultMain, bgroup, bench, whnf)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.EnumSet as EnumSet
import Data.NonEmpty ((!:))
import Data.EnumSet (EnumSet)
import Data.Tuple.HT (mapSnd)


alphabet, digits :: EnumSet Char
alphabet = EnumSet.fromList ['a'..'z']
digits = EnumSet.fromList ['0'..'9']


type List1 = NonEmpty.T []
type List2 = NonEmpty.T List1

gamesWords, gamesNumbers :: [List2 (String, Eval)]
gamesWords =
   [
      ("flq", Eval 0 0) !:
      ("chx", Eval 0 0) !:
      ("sez", Eval 2 0) :
      ("aes", Eval 1 1) :
      ("bde", Eval 0 1) :
      ("gij", Eval 0 0) :
      ("kmn", Eval 0 0) :
      ("opr", Eval 0 0) :
      ("tuv", Eval 0 1) :
      ("set", Eval 3 0) :
      [],

      ("ncqy", Eval 0 1) !:
      ("yxcl", Eval 0 0) !:
      ("ikjn", Eval 0 2) :
      ("dnoj", Eval 0 2) :
      ("jbnz", Eval 1 0) :
      ("ofnk", Eval 1 0) :
      ("adni", Eval 1 2) :
      ("eghm", Eval 0 1) :
      ("gaah", Eval 0 0) :
      ("mind", Eval 4 0) :
      [],

      ("ozdtp", Eval 0 1) !:
      ("cxgkz", Eval 1 1) !:
      ("gbqvz", Eval 0 1) :
      ("ctdng", Eval 0 2) :
      ("dwghk", Eval 1 0) :
      ("sygpc", Eval 2 0) :
      ("gogfm", Eval 2 0) :
      ("ploir", Eval 1 2) :
      ("logic", Eval 5 0) :
      [],

      ("tynrsu", Eval 0 3) !:
      ("msycnl", Eval 1 1) !:
      ("uslher", Eval 2 1) :
      ("pceeyr", Eval 1 1) :
      ("psugen", Eval 1 1) :
      ("kscdur", Eval 1 1) :
      ("zwghob", Eval 0 0) :
      ("masafa", Eval 3 0) :
      ("master", Eval 6 0) :
      []
   ]

gamesNumbers =
   [
      ("092", Eval 1 0) !:
      ("009", Eval 2 0) !:
      ("130", Eval 0 1) :
      ("456", Eval 0 0) :
      ("007", Eval 3 0) :
      [],

      ("7483", Eval 0 0) !:
      ("2066", Eval 2 0) !:
      ("0106", Eval 0 2) :
      ("2501", Eval 1 2) :
      ("2012", Eval 3 0) :
      ("2019", Eval 4 0) :
      [],

      ("04575", Eval 1 1) !:
      ("43465", Eval 0 3) !:
      ("32556", Eval 2 2) :
      ("16553", Eval 1 3) :
      ("58536", Eval 3 1) :
      ("65536", Eval 5 0) :
      [],

      ("899820", Eval 1 1) !:
      ("872456", Eval 2 3) !:
      ("256296", Eval 0 2) :
      ("142857", Eval 5 0) :
      []
   ]

addWidth :: NonEmpty.T f ([a], b) -> (Int, NonEmpty.T f ([a], b))
addWidth xs = (length $ fst $ NonEmpty.head xs, xs)


singleBench ::
   (CodeSet.C set) =>
   (set Char -> Integer) -> EnumSet Char ->
   (Int, List1 (String, Eval)) -> Benchmark
singleBench setSize symbols (width, xs) =
   bench (show width) $
      whnf (setSize . CodeSet.intersections .
            fmap (uncurry (matching symbols))) xs

benchWordsAndNumbers ::
   (CodeSet.C set) =>
   (set Char -> Integer) ->
   (List2 (String, Eval) -> List1 (String, Eval)) -> [Benchmark]
benchWordsAndNumbers setSize cut =
   bgroup "words"
      (map (singleBench setSize alphabet . mapSnd cut . addWidth) gamesWords) :
   bgroup "numbers"
      (map (singleBench setSize digits . mapSnd cut . addWidth) gamesNumbers) :
   []

benchCodeSets :: (CodeSet.C set) => (set Char -> Integer) -> [Benchmark]
benchCodeSets setSize =
   bgroup "3 evaluations"
      (benchWordsAndNumbers setSize
         (NonEmpty.mapTail (take 2) . NonEmpty.flatten)) :
   bgroup "all but one evaluation"
      (benchWordsAndNumbers setSize NonEmpty.init) :
   []

main :: IO ()
main = defaultMain $
   bgroup "tree" (benchCodeSets CodeSetTree.size) :
   bgroup "union" (benchCodeSets CodeSetUnion.size) :
   []
