module Game.Mastermind (
   Eval(Eval),
   evaluate,
   matching,
   matchingSimple,

   randomizedAttempt,
   mixedRandomizedAttempt,
   scanningRandomizedAttempt,
   separatingRandomizedAttempt,
   partitionSizes,

   mainSimple,
   mainRandom,
   main,

   propBestSeparatingCode,
   ) where

import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
-- import qualified Game.Mastermind.CodeSet.Union as CodeSetUnion
import qualified Game.Mastermind.CodeSet as CodeSet
import qualified Game.Mastermind.NonEmptyEnumSet as NonEmptySet
import Game.Mastermind.CodeSet
   (intersection, (*&), (#*&), unit, empty, union, unions, cube, )
import Game.Utility
   (Choice(Choice), mergeChoice, noChoice, randomSelect, histogram)

import qualified Data.EnumMap as EnumMap
import qualified Data.EnumSet as EnumSet
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.EnumMap (EnumMap)
import Data.EnumSet (EnumSet)

import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.NonEmpty ((!:))
import Data.List.HT (partition, )
import Data.Tuple.HT (mapPair, )
import Data.Maybe.HT (toMaybe, )
import Data.Maybe (listToMaybe, fromMaybe)
import Data.Ord.HT (comparing)
import Data.Eq.HT (equating)

import qualified Control.Monad.Trans.State as MS
import Control.Monad.IO.Class (liftIO)
import Control.Monad (guard, when, replicateM, liftM2, )

import qualified Combinatorics as Combi

import qualified System.Random as Rnd
import qualified System.IO as IO


{- $setup
>>> import qualified Test.Mastermind as TestMM
>>> import Test.Mastermind (CodeSetInt, alphabet, Code(Code), CodePair(CodePair), forAllEval)
>>> import qualified Game.Mastermind.CodeSet.Tree as CodeSetTree
>>> import qualified Game.Mastermind.CodeSet as CodeSet
>>> import qualified Game.Mastermind as MM
>>> import qualified Data.EnumSet as EnumSet
>>> import Game.Mastermind (Eval(Eval))
>>> import Control.Monad (replicateM)
>>> import Data.Function.HT (compose2)
-}

data Eval = Eval Int Int
   deriving (Eq, Ord, Show)

{- |
Given the code and a guess, compute the evaluation.

prop> \(CodePair secret attempt) -> MM.evaluate secret attempt == MM.evaluate attempt secret
-}
evaluate :: (Enum a) => [a] -> [a] -> Eval
evaluate code attempt =
   uncurry Eval $
   mapPair
      (length,
       sum . EnumMap.elems .
       uncurry (EnumMap.intersectionWith min) .
       mapPair (bagFromList,bagFromList) . unzip) $
   partition (uncurry $ equating fromEnum) $
   zip code attempt


bagFromList :: (Enum a) => [a] -> EnumMap a Int
bagFromList = EnumMap.fromListWith (+) . map (\a -> (a,1))

selectFromBag, _selectFromBag ::
   (Enum a) => EnumMap a Int -> [(a, EnumMap a Int)]
selectFromBag hist =
   map (\a -> (a, EnumMap.update (\n -> toMaybe (n>1) (pred n)) a hist)) $
   EnumMap.keys hist

_selectFromBag hist =
   EnumMap.toList $
   EnumMap.mapWithKey
      (\a _ -> EnumMap.update (\n -> toMaybe (n>1) (pred n)) a hist) hist


{- |
A variant of the game:
It is only possible to specify number of symbols at right places.

The results of 'matching' and 'matchingSimple' cannot be compared.
-}
matchingSimple :: Enum a => EnumSet a -> [a] -> Int -> [[EnumSet a]]
matchingSimple alphabet code rightPlaces =
   map
      (zipWith
         (\symbol right ->
            if right
              then EnumSet.singleton symbol
              else EnumSet.delete symbol alphabet)
         code) $
   Combi.choose (length code) rightPlaces

{- |
Given a code and an according evaluation,
compute the set of possible codes.

The Game.Mastermind game consists of collecting pairs
of codes and their evaluations.
The searched code is in the intersection of all corresponding code sets.

>>> filter ((MM.Eval 2 0 ==) . MM.evaluate "aabbb") $ replicateM 5 ['a'..'c']
["aaaaa","aaaac","aaaca","aaacc","aacaa","aacac","aacca","aaccc","acbcc","accbc","acccb","cabcc","cacbc","caccb","ccbbc","ccbcb","cccbb"]
>>> CodeSet.flatten (MM.matching (EnumSet.fromList ['a'..'c']) "aabbb" (Eval 2 0) :: CodeSetTree.T Char)
["aaaaa","aaaac","aaaca","aaacc","aacaa","aacac","aacca","aaccc","acbcc","accbc","acccb","cabcc","cacbc","caccb","ccbbc","ccbcb","cccbb"]

prop> \(CodePair secret attempt) -> CodeSetTree.member secret $ MM.matching alphabet attempt (MM.evaluate secret attempt)
prop> \(CodePair secret attempt) -> forAllEval secret $ \eval -> (eval == MM.evaluate secret attempt) == CodeSetTree.member secret (MM.matching alphabet attempt eval)
prop> \(Code attempt) -> forAllEval attempt $ \eval0 -> forAllEval attempt $ \eval1 -> eval0 == eval1 || CodeSetTree.null (compose2 CodeSetTree.intersection (MM.matching alphabet attempt) eval0 eval1)
prop> \(Code attempt) -> forAllEval attempt $ \eval -> all ((eval ==) . MM.evaluate attempt) $ take 100 $ CodeSet.flatten $ (MM.matching alphabet attempt eval :: CodeSetInt)
prop> \(Code attempt) -> forAllEval attempt $ \eval -> let set :: CodeSetInt; set = MM.matching alphabet attempt eval in map (CodeSet.select set) [0 .. min 100 (CodeSet.size set) - 1] == take 100 (CodeSet.flatten set)
prop> TestMM.intersections
prop> TestMM.solve
-}
matching :: (CodeSet.C set, Enum a) => EnumSet a -> [a] -> Eval -> set a
matching alphabet =
   let findCodes =
          foldr
             (\(fixed,c) go rightSymbols floating0 ->
                if fixed
                  then c #*& go rightSymbols floating0
                  else
                    (unions $ do
                        guard (rightSymbols > 0)
                        (src, floating1) <- selectFromBag floating0
                        guard (not $ equating fromEnum c src)
                        return $ src #*& go (rightSymbols-1) floating1)
                    `union`
                    (EnumSet.difference
                        (EnumSet.delete c alphabet)
                        (EnumMap.keysSet floating0) *&
                     go rightSymbols floating0))
             (\rightSymbols _floating ->
                if rightSymbols>0
                  then empty
                  else unit)
   in \code (Eval rightPlaces rightSymbols) ->
       unions $
       map
          (\pattern ->
             let patternCode = zip pattern code
             in  findCodes patternCode rightSymbols $
                 bagFromList $ map snd $ filter (not . fst) patternCode) $
       Combi.choose (length code) rightPlaces


{- |
A more precise test would be to check
that for different numbers of rightPlace and rightSymbol
the codesets are disjoint
and their union is the set of all possible codes.
To this end we need a union with simplification or a subset test.

prop> \(Code attempt) -> fromIntegral (EnumSet.size alphabet) ^ length attempt == sum (map snd (MM.partitionSizes alphabet attempt))
-}
partitionSizes :: (Enum a) => EnumSet a -> [a] -> [(Eval, Integer)]
partitionSizes alphabet code =
   map (\eval -> (eval, CodeSetTree.size $ matching alphabet code eval)) $
   possibleEvaluations (length code)

possibleEvaluations :: Int -> [Eval]
possibleEvaluations n = do
   rightPlaces <- [0..n]
   rightSymbols <- [0..n-rightPlaces]
   return $ Eval rightPlaces rightSymbols


interaction ::
   (CodeSetTree.T Char -> MS.State state (Maybe [Char])) ->
   state -> NonEmptySet.T Char -> Int -> IO ()
interaction select initial alphabet n =
   let go set = do
          newGuess <- MS.state $ MS.runState $ select set
          case newGuess of
             Nothing -> liftIO $ putStrLn "contradicting evaluations"
             Just attempt -> do
                liftIO $ do
                   putStr $
                      show attempt ++ " " ++
                      show (CodeSet.size set, CodeSet.representationSize set,
                            EnumSet.size (CodeSet.symbols set)) ++ " "
                   IO.hFlush IO.stdout
                eval <- liftIO getLine
                let getEval =
                      fmap (fromMaybe 0) . MS.state .
                      EnumMap.updateLookupWithKey (\_ _ -> Nothing)
                let ((rightPlaces,rightSymbols), ignored) =
                      MS.runState (liftM2 (,) (getEval 'x') (getEval 'o')) $
                      bagFromList eval
                when (not $ EnumMap.null ignored) $
                   liftIO $ putStrLn $ "ignoring: " ++ EnumMap.keys ignored
                if rightPlaces >= n
                  then liftIO $ putStrLn "I won!"
                  else go $ intersection set $
                       matching (NonEmptySet.flatten alphabet) attempt $
                       Eval rightPlaces rightSymbols
   in MS.evalStateT (go (cube alphabet n)) initial

mainSimple :: NonEmptySet.T Char -> Int -> IO ()
mainSimple = interaction (return . listToMaybe . CodeSet.flatten) ()

{- |
minimum of maximums using alpha-beta-pruning
-}
minimax :: (Ord b) => (a -> [b]) -> NonEmpty.T [] a -> a
minimax f (NonEmpty.Cons a0 rest) =
   fst $
   foldl
      (\old@(_minA, minB) a ->
         let (ltMinB, geMinB) = partition (<minB) $ f a
         in if null geMinB then (a, maximum ltMinB) else old)
      (a0, maximum $ f a0) rest

{- |
Remove all but one unused symbols from the alphabet.
-}
reduceAlphabet :: (CodeSet.C set, Enum a) => set a -> EnumSet a -> EnumSet a
reduceAlphabet set alphabet =
   let symbols = CodeSet.symbols set
   in  EnumSet.union symbols $ EnumSet.fromList $ take 1 $ EnumSet.toList $
       EnumSet.difference alphabet symbols

{- |
prop> TestMM.bestSeparatingCode
-}
bestSeparatingCode ::
   (CodeSet.C set, Enum a) => Int -> set a -> NonEmpty.T [] [a] -> [a]
bestSeparatingCode n set =
   let alphabet = CodeSet.symbols set
   in minimax $ \attempt ->
         map (CodeSet.size . intersection set . matching alphabet attempt) $
         possibleEvaluations n

{-
For small sets of codes it is faster to evaluate
all matching codes and build a histogram.
-}
bestSeparatingCodeHistogram ::
   (CodeSet.C set, Enum a) => set a -> NonEmpty.T [] [a] -> [a]
bestSeparatingCodeHistogram set =
   minimax $ \attempt ->
      Map.elems $ histogram $ map (evaluate attempt) $ CodeSet.flatten set

propBestSeparatingCode ::
   (CodeSet.C set, Enum a) => Int -> set a -> NonEmpty.T [] [a] -> Bool
propBestSeparatingCode n set attempts =
   equating (map fromEnum)
      (bestSeparatingCode n set attempts)
      (bestSeparatingCodeHistogram set attempts)


{-
Here we optimize for small set sizes.
For performance we could optimize for small set representation sizes.
However the resulting strategy looks much like the strategy
from mainSimple and needs more attempts.
-}
randomizedAttempt ::
   (CodeSet.C set, Rnd.RandomGen g, Enum a) =>
   Int -> set a -> MS.State g (Maybe [a])
randomizedAttempt n set = do
   let symbolSet = CodeSet.symbols set
   let randomCode = replicateM n $ randomSelect $ EnumSet.toList symbolSet
   randomAttempts <- liftM2 (!:) randomCode $ replicateM 9 randomCode
   let somePossible =
          -- take 10 possible codes
          let size = CodeSet.size set
              num = 10
          in  map (CodeSet.select set) $
              Set.toList $ Set.fromList $
              take num $
              map (flip div (fromIntegral num)) $
              iterate (size+) 0
   return $
      toMaybe (not $ CodeSet.null set) $
      bestSeparatingCode n set $
      NonEmpty.appendLeft somePossible randomAttempts


withNonEmptyCodeSet ::
   (Monad m, CodeSet.C set, Enum a) =>
   set a ->
   (NonEmpty.T [] [a] -> m (Maybe [a])) ->
   m (Maybe [a])
withNonEmptyCodeSet set f =
   case CodeSet.flatten set of
      [] -> return Nothing
      x:[] -> return $ Just x
      x:_:[] -> return $ Just x
      x:xs -> f $ x!:xs

{- |
In the beginning we choose codes that separate reasonably well,
based on heuristics.
At the end, when the set becomes small,
we do a brute-force search for an optimally separating code.
-}
{-
The reduced alphabet contains one symbol more than @CodeSet.symbols set@.
Is that necessary or is there always an equally good separating code
without the extra symbol?
-}
separatingRandomizedAttempt ::
   (CodeSet.C set, Rnd.RandomGen g, Enum a) =>
   Int -> EnumSet a -> set a -> MS.State g (Maybe [a])
separatingRandomizedAttempt n alphabet0 set =
   withNonEmptyCodeSet set $ \flattenedSet ->
      let size = CodeSet.size set
          alphabet = reduceAlphabet set alphabet0
          alphabetSize = EnumSet.size alphabet
      in if size * (size + toInteger alphabetSize ^ n) <= 1000000
            then return $ Just $ bestSeparatingCodeHistogram set $
                 NonEmpty.appendRight flattenedSet $
                 replicateM n (EnumSet.toList alphabet)
            else randomizedAttempt n set

{- |
In the beginning we simply choose a random code
from the set of possible codes.
In the end, when the set becomes small,
then we compare different alternatives.
-}
mixedRandomizedAttempt ::
   (CodeSet.C set, Rnd.RandomGen g, Enum a) =>
   Int -> set a -> MS.State g (Maybe [a])
mixedRandomizedAttempt n set =
   withNonEmptyCodeSet set $ \ _flattenedSet ->
      let size = CodeSet.size set
      in if size <= 100
           then randomizedAttempt n set
           else fmap (Just . CodeSet.select set) $
                MS.state $ Rnd.randomR (0, size-1)

{- |
This strategy starts with scanning the alphabet.
That is, we test sets of different symbols we did not try so far.
The idea is to sort out unused symbols early.
This is especially useful when the alphabet is large,
i.e. its size is some multiples of the code width.

We stop scanning when we are sure to have seen
all characters of the secret code.
E.g.:

> vicx
> alsn   o
> mfgt   o
> hjqw
> edpz   oo
> bkru   - we already know, that these cannot be in the secret code

We use the 'Choice' data type
for tracking the number of symbols that we can minimally use
from the ones we have already tried.
The order of applying 'mergeChoice' matters,
but I see no easy way to find a good order
or to make it robust against re-ordering.

If the user tells us that all symbols in a code are used,
then the scanning phase ends immediately.
This happens automatically according to our way of processing 'Choice's.
-}
scanningRandomizedAttempt ::
   (CodeSet.C set, Rnd.RandomGen g, Enum a) =>
   Int -> EnumSet a -> [([a], Eval)] -> set a -> MS.State g (Maybe [a])
scanningRandomizedAttempt n alphabet oldGuesses set = do
   let sumEval (Eval correctPlaces correctSymbols) =
         correctPlaces + correctSymbols
   let (Choice totalBag count) =
         foldl mergeChoice noChoice $
         map (uncurry Choice . mapPair (bagFromList, sumEval)) oldGuesses
   let unusedSymbols = EnumSet.difference alphabet $ EnumMap.keysSet totalBag
   if count>=n
      then randomizedAttempt n set
      else
         if EnumSet.size unusedSymbols <= n
            then mixedRandomizedAttempt n set
            else do
               let nextSymbols = EnumSet.toList unusedSymbols
               keys <-
                  mapM
                     (const $ MS.state $ Rnd.randomR (0,1::Double))
                     nextSymbols
               return $ Just $ map snd $ take n $
                  List.sortBy (comparing fst) $ zip keys nextSymbols


mainRandom :: NonEmptySet.T Char -> Int -> IO ()
mainRandom alphabet n = do
   g <- Rnd.getStdGen
   interaction
      (separatingRandomizedAttempt n (NonEmptySet.flatten alphabet))
      g alphabet n

main :: IO ()
main =
   let alphabet = NonEmptySet.fromList ('a'!:['b'..'z'])
   in  if True
         then mainRandom alphabet 5
         else mainSimple alphabet 7

{-
Bug: (fixed)
*Game.Mastermind> main
"uvqcm" (11881376,130) o
"wukjv" (3889620,440)
"lmoci" (1259712,372) xo
"caoab" (94275,1765) oo
"mbadi" (6856,2091) ooo
"ombed" (327,447) x
"lqbia" (2,10) xo
contradicting evaluations
*Game.Mastermind> map (evaluate "amiga") ["uvqcm","wukjv","lmoci","caoab","mbadi","ombed","lqbia"]
[Eval 0 1,Eval 0 0,Eval 1 1,Eval 0 2,Eval 0 3,Eval 1 0,Eval 1 1]
*Game.Mastermind> map (\attempt -> member "amiga" $ matching (EnumSet.fromList $ ['a'..'z']) attempt (evaluate "amiga" attempt)) ["uvqcm","wukjv","lmoci","caoab","mbadi","ombed","lqbia"]
[True,True,True,True,False,True,False]
-}
