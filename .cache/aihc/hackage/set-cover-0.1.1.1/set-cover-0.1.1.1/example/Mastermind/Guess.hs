module Mastermind.Guess where

import Mastermind.Utility (histogram)

import qualified Math.SetCover.Exact as ESC

import qualified Control.Monad.Trans.State as MS
import Control.Monad (liftM2, replicateM, guard, )

import qualified Data.EnumSet as EnumSet; import Data.EnumSet (EnumSet, )
import qualified Data.Map as Map
import qualified Data.Set as Set; import Data.Set (Set, )
import qualified Data.Foldable as Fold
import qualified Data.Array as Array
import qualified Data.Monoid.HT as Mn
import qualified Data.List.Match as Match
import qualified Data.List.HT as ListHT
import qualified Data.List as List
import Data.Tuple.HT (mapPair, )
import Data.Maybe.HT (toMaybe)
import Data.Traversable (forM, )


data EvalSymbol a = Pos Column | Eval Eval Row Column | Symbol a
        deriving (Eq, Ord, Show)

data Eval = CorrectPlace | CorrectSymbol
        deriving (Eq, Ord, Show)


newtype Row = Row Int deriving (Eq, Ord, Show)

instance Enum Row where
   toEnum = Row
   fromEnum (Row k) = k


newtype Column = Column Int deriving (Eq, Ord, Show, Array.Ix)

instance Enum Column where
   toEnum = Column
   fromEnum (Column k) = k


{- |
* 'UniqueSymbol': Only consider codes where every symbol is unique.

* 'UseSymbol': Avoid duplicates in 'consistentCodes'. See below.

> *Main> consistentCodes EnumSet.empty 2 ['a'..'b'] []
> ["aa","ab","ba","bb","aa","bb"]
> *Main> consistentCodes (EnumSet.singleton UseSymbol) 2 ['a'..'b'] []
> ["ab","ba","aa","bb"]
-}
data AssignFlag = UniqueSymbol | UseSymbol
   deriving (Eq, Ord, Show, Enum)

type AssignFlags = EnumSet AssignFlag

defaultAssignFlags :: AssignFlags
defaultAssignFlags = EnumSet.fromList [UseSymbol]

allFlagSets :: [AssignFlags]
allFlagSets = [EnumSet.empty, EnumSet.singleton UseSymbol]

assignsFromCodeSymbols ::
   (Ord a) =>
   AssignFlags ->
   Int -> [a] -> [[a]] -> [ESC.Assign [(Column, a)] (Set (EvalSymbol a))]
assignsFromCodeSymbols flags width set codes =
  let uniqueSymbol = EnumSet.member UniqueSymbol flags
      useSymbol    = EnumSet.member UseSymbol    flags
  in
   liftM2
      (\pat a ->
         let ks = map fst $ filter snd $ zip [Column 0 ..] pat
         in  ESC.assign (map (flip (,) a) ks) $ Set.unions $
             Mn.when useSymbol (Set.singleton (Symbol a)) :
             Set.fromList (map Pos ks) :
             zipWith
                (\row code ->
                   Set.fromList $
                   let (correctlyPlaced, remCode) =
                          ListHT.partition (\(_k, (used,equ)) -> used && equ) $
                          zip [Column 0 ..] $ zip pat $ map (a==) code
                   in  map (Eval CorrectPlace row . fst) correctlyPlaced
                       ++
                       map (Eval CorrectSymbol row . fst)
                          (Match.take
                             (filter (fst . snd) remCode)
                             (filter (snd . snd) remCode)))
                [Row 0 ..] codes)
      ((if useSymbol then id else tail) $
       if uniqueSymbol
         then take (width+1) $ map (take width) $
              scanl (flip (:)) (repeat False) (True : repeat False)
         else replicateM width [False, True])
      set


{-
For correctness,
'X' would not need the 'Symbol' constructor and the type parameter @a@.
I.e. it would not need any reference to symbols.
For every symbol, @Set X@ describes its distribution pattern across all guesses.
-}
data X a = EvalSymbol (EvalSymbol a) | EvalReserve Row Column | EvalRow Eval Row
        deriving (Eq, Ord, Show)

type Label a = Either (Row, Maybe Eval, [Bool]) [(Column, a)]
type Assign a = ESC.Assign (Label a) (Set (X a))

data EvalSumm = EvalSumm Int Int deriving (Eq, Ord, Show)

{-
For every symbol and every distribution pattern within a code
@assignsFromCodeSymbols@ computes the resulting evaluation pins
bound to according positions.
The second part (@concat@) marks the possible positions
by filling out the remaining positions with @EvalSymbol (Eval eval row colum)@.
@EvalReserve@ is used to avoid use of a position
for both correctly placed and wrongly placed symbols.
Without this mechanism we get this bug:

> Main> take 10 $ consistentCodes 2 ['a'..'c'] [("ab", EvalSumm 1 1)]
> ["aa","bb"]
-}
assignsFromGuesses ::
   (Ord a) => AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> [Assign a]
assignsFromGuesses flags width set guesses =
   map
      (\(ESC.Assign label sym) ->
         ESC.Assign (Right label) (Set.map EvalSymbol sym))
      (assignsFromCodeSymbols flags width set $ map fst guesses)
   ++
   concat
      (zipWith
         (\row (_, EvalSumm correctPlaces correctSymbols) ->
            let fill eval k =
                   map
                      (\pattern ->
                         ESC.assign (Left (row, Just eval, pattern)) .
                         Set.fromList . (EvalRow eval row :) .
                         uncurry (++) .
                         mapPair
                            (map (EvalReserve row . fst),
                             map (EvalSymbol . Eval eval row . fst)) .
                         ListHT.partition snd . zip [Column 0 ..] $ pattern) $
                   choose width k
            in  fill CorrectPlace correctPlaces
                ++
                fill CorrectSymbol correctSymbols
                ++
                map
                   (\pattern ->
                      ESC.assign (Left (row, Nothing, pattern)) .
                      Set.fromList .
                      map (EvalReserve row . fst) .
                      filter snd . zip [Column 0 ..] $ pattern)
                   (choose width (width-correctPlaces-correctSymbols)))
         [Row 0 ..] guesses)

nameFromEval :: Maybe Eval -> String
nameFromEval eval =
   case eval of
      Nothing -> "wrong symbol"
      Just CorrectSymbol -> "correct symbol"
      Just CorrectPlace -> "correct place"

countEval :: MS.State String EvalSumm
countEval =
   let count c = fmap length $ MS.state $ ListHT.partition (c==)
   in  liftM2 EvalSumm (count 'x') (count 'o')

charFromEval :: Maybe Eval -> Char
charFromEval eval =
   case eval of
      Nothing -> '.'
      Just CorrectSymbol -> 'o'
      Just CorrectPlace -> 'x'

formatPattern :: Maybe Eval -> [Bool] -> String
formatPattern eval =
   let char = charFromEval eval
   in  map (\b -> if b then char else '_')

-- cf. combinatorial:Combinatorics.choose
choose :: Int -> Int -> [[Bool]]
choose n0 k0 =
   flip MS.evalStateT k0 $ do
   bits <-
      forM [n0,n0-1..1] $ \n ->
      MS.StateT $ \k ->
      guard (0<=k && k<=n) >> [(False, k), (True, pred k)]
   MS.gets (0==) >>= guard
   return bits


codeFromLabels :: [Label a] -> [a]
codeFromLabels mxs =
   case concatMap (either (const []) id) mxs of
      xs -> Array.elems $ Array.array (Column 0, Column (length xs - 1)) xs


consistentCodes ::
   (Ord a) => AssignFlags -> Int -> [a] -> [([a], EvalSumm)] -> [[a]]
consistentCodes flags width alphabet guesses =
   map codeFromLabels $ ESC.partitions $ ESC.intSetFromSetAssigns $
   assignsFromGuesses flags width alphabet guesses


-- cf. board-games:Mastermind
evaluate :: (Ord a) => [a] -> [a] -> EvalSumm
evaluate code attempt =
   uncurry EvalSumm $
   mapPair
      (length,
       Fold.sum . uncurry (Map.intersectionWith min) .
       mapPair (histogram,histogram) . unzip) $
   ListHT.partition (uncurry (==)) $
   zip code attempt


{-
These ones need exceptionally much time:

> mapM_ (putStrLn . formatEvalGuess) $ autoPlay ['a'..'z'] "maple"
> mapM_ (putStrLn . formatEvalGuess) $ autoPlay ['a'..'z'] "wheat"

With the UniqueSymbol flag it becomes fast, again:

> autoPlay (EnumSet.insert UniqueSymbol defaultAssignFlags)
-}
autoPlay :: (Ord a) => AssignFlags -> [a] -> [a] -> [([a], EvalSumm)]
autoPlay flags set secret =
   List.unfoldr
      (\guesses ->
         toMaybe (all ((secret/=) . fst) guesses) $
         case consistentCodes flags (length secret) set guesses of
            [] -> error "autoPlay: algorithm went wrong"
            guess:_ ->
               let evaluatedGuess = (guess, evaluate secret guess)
               in  (evaluatedGuess, evaluatedGuess:guesses))
      []
