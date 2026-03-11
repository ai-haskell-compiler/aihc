module Mastermind.Distinguish where

import Mastermind.Guess (
   assignsFromCodeSymbols,
   Eval(CorrectPlace, CorrectSymbol),
   Row(Row), Column(Column),
   AssignFlags,
   EvalSumm(EvalSumm),
   EvalSymbol(Eval),
   )

import qualified Math.SetCover.Exact as ESC

import Control.Monad (replicateM, )

import qualified Data.Set as Set; import Data.Set (Set, )
import qualified Data.Array as Array
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapPair, )
import Data.Foldable (foldMap, )
import Data.Ord.HT (comparing, )



data X a = EvalSymbol (EvalSymbol a) | EvalRow Row | EvalSummary EvalSumm
   deriving (Eq, Ord, Show)

type Assign a = ESC.Assign (Either [(Column, a)] (Row, EvalSumm)) (Set (X a))

{-
The solver is pretty slow with these assignments.
E.g. for the codes @["abw", "abx", "aby", "abz"]@
there cannot be a distinguishing code,
so there cannot be one for @["abcw", "abcx", "abcy", "abcz"]@.
However, the solver cannot draw this conclusion by itself.
Another example:
The evaluation summary @EvalSumm (width-1) 1@ is impossible.
The solver should detect this automatically.

Maybe it is possible to assist the solver with redundant information
that the evaluations must be pairwise distinct
and even more must be pairwise and pointwise distinct.
However, the example above shows that searching for a distinguishing code
might be of limited utility after all.
-}
assignsFromMatchingCodes ::
   (Ord a) => AssignFlags -> Int -> [a] -> [[a]] -> [Assign a]
assignsFromMatchingCodes flags width set codes =
   map
      (\(ESC.Assign label sym) ->
         ESC.Assign (Left label) (Set.map EvalSymbol sym))
      (assignsFromCodeSymbols flags width set codes)
   ++
   concat
      (map
         (\row ->
            map
               (\xs ->
                  let fill eval =
                         mapPair
                            (length,
                             map (EvalSymbol . Eval eval row . fst)) $
                         ListHT.partition ((Just eval ==) . snd) $
                         zip [Column 0 ..] xs
                      (correctPlaces, remPlaces) = fill CorrectPlace
                      (correctSymbols, remSymbols) = fill CorrectSymbol
                      evalSumm = EvalSumm correctPlaces correctSymbols
                  in  ESC.assign (Right (row, evalSumm)) . Set.fromList $
                      EvalRow row : EvalSummary evalSumm :
                      remPlaces ++ remSymbols) $
            replicateM width [Nothing, Just CorrectPlace, Just CorrectSymbol]) $
       Match.take codes [Row 0 ..])
   ++
   (do correctPlaces <- [0..width]
       correctSymbols <- [0..width-correctPlaces]
       return . ESC.assign (Left []) . Set.singleton $
         EvalSummary $ EvalSumm correctPlaces correctSymbols)


{- |
For a given list of codes,
find a guess that has different evaluations with respect to all of these codes.
If we know at a certain point in the game
that there is only a small number of possible codes left,
we can use this procedure to find a guess that solves the puzzle immediately.
E.g. @distinguishingCodes 6 ['a'..'z'] ["master", "puzzle", "bubble"] == [..., "jalbay", ...]@
because @map (eval "jalbay") ["master", "puzzle", "bubble"] == [Eval 0 1, Eval 1 0, Eval 1 1]@

Problem:
For 4 6-letter codes the solution takes several seconds,
for more letters it does not finish within an hour.
Thus this approach is not practical.

Example:
There is no distinguishing code for @["abcdew", "abcdex", "abcdey", "abcdez"]@,
but the solver does not detect that in reasonable time.

If we would not only have a list of codes
but also a corresponding list of evaluations
the problem would boil down to the one addressed by 'assignsFromGuesses'.
-}
distinguishingCodes ::
   (Ord a) => AssignFlags -> Int -> [a] -> [[a]] -> [([a], [EvalSumm])]
distinguishingCodes flags width set codes =
   map (mapPair (codeFromLabels, map snd . List.sortBy (comparing fst)) .
        ListHT.unzipEithers) $
   ESC.partitions $ ESC.intSetFromSetAssigns $
   assignsFromMatchingCodes flags width set codes

{- |
Replace all unused symbols by a single one,
because unused symbols behave the same way with respect to the @codes@.
This returns a shorter list of codes but is also much faster.
-}
distinguishingCodesCondensed ::
   (Ord a) => AssignFlags -> Int -> [a] -> [[a]] -> [([a], [EvalSumm])]
distinguishingCodesCondensed flags width set codes =
   let unused =
         Set.deleteMin $ Set.difference (Set.fromList set) $
         foldMap Set.fromList codes
   in  distinguishingCodes flags width
         (filter (flip Set.notMember unused) set) codes


codeFromLabels :: [[(Column, a)]] -> [a]
codeFromLabels mxs =
   case concat mxs of
      xs -> Array.elems $ Array.array (Column 0, Column (length xs - 1)) xs
