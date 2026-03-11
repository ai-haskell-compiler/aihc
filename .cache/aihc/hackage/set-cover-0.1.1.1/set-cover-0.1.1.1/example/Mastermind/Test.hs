module Mastermind.Test where

import Mastermind.Guess (
   autoPlay,
   consistentCodes,
   evaluate,
   AssignFlag(UseSymbol, UniqueSymbol),
   AssignFlags, allFlagSets,
   EvalSumm(EvalSumm),
   )

import qualified Test.QuickCheck as QC

import Control.Monad (liftM2, )
import Control.Applicative ((<$>), )

import qualified Data.EnumSet as EnumSet
import qualified Data.List.HT as ListHT
import Data.Maybe (listToMaybe, )


-- cf. board-games:Test.Mastermind
genEvalSumm :: Int -> QC.Gen EvalSumm
genEvalSumm width = do
   total <- QC.frequency $ map (\k -> (k+1, return k)) [1 .. width]
   rightPlaces <- QC.choose (0,total)
   return $ EvalSumm rightPlaces (total - rightPlaces)

genGuess :: Int -> [a] -> QC.Gen [a]
genGuess width set = QC.vectorOf width $ QC.elements set

genGuessUnique :: Int -> [a] -> QC.Gen [a]
genGuessUnique 0 _ = return []
genGuessUnique width set = do
   (x,xs) <- QC.elements $ ListHT.removeEach set
   (x:) <$> genGuessUnique (width-1) xs

genGuesses :: Int -> [a] -> QC.Gen [([a], EvalSumm)]
genGuesses width set =
   fmap (take 2) $ QC.listOf1 $
   liftM2 (,) (genGuessUnique width set) (genEvalSumm width)

genConsistentCode :: (Ord a) => AssignFlags -> Int -> [a] -> QC.Gen (Maybe [a])
genConsistentCode flags width set =
   listToMaybe . consistentCodes flags width set <$> genGuesses width set


propConsistency ::
   (Ord a) => AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> Bool
propConsistency flags width set guesses =
   and $
   liftM2
      (\(guess,eval) candidate -> eval == evaluate guess candidate)
      guesses (take 10 $ consistentCodes flags width set guesses)

propAutoPlay :: (Ord a) => AssignFlags -> [a] -> [a] -> Bool
propAutoPlay flags set secret =
   fst (last (autoPlay flags set secret)) == secret


tests :: [(String, (Int, QC.Property))]
tests =
   let n = 4
       set = ['a'..'k']
       forAll count gen = (,) count . QC.forAll gen
       formatFlags flags =
         if EnumSet.member UseSymbol flags
           then "UseSymbol"
           else "OmitSymbol"

   in (allFlagSets >>= \flags ->
         ("Duplicate.Consistency." ++ formatFlags flags,
          forAll 500 (genGuesses n set) (propConsistency flags n set)) :
         ("Duplicate.AutoPlay." ++ formatFlags flags,
          forAll 100 (genGuess n set) (propAutoPlay flags set)) :
         [])
      ++
      (map (EnumSet.insert UniqueSymbol) allFlagSets >>= \flags ->
         ("Unique.Consistency." ++ formatFlags flags,
          forAll 1000 (genGuesses n set) (propConsistency flags n set)) :
         ("Unique.AutoPlay." ++ formatFlags flags,
          forAll 200 (genGuessUnique n set) (propAutoPlay flags set)) :
         [])
