{- |
Given a row of n (~50) dice,
two players start with a random dice within the first m (~5) dice.
Every player moves along the row, according to the pips on the dice.
They stop if a move would exceed the row.
What is the probability that they stop at the same die?
(It is close to one.)

Kruskal's trick:
<http://faculty.uml.edu/rmontenegro/research/kruskal_count/kruskal.html>

Wuerfelschlange (german):
<http://www.math.de/exponate/wuerfelschlange.html/>
-}
module Numeric.Probability.Example.Kruskal where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Transition as Trans
import qualified Numeric.Probability.Random as Random
import qualified Numeric.Probability.Object as Obj
import qualified System.Random as Rnd
import qualified Text.Printf as P
import qualified Data.List as List
import Control.Monad (replicateM, )
import Data.Function.HT (nest, compose2, )
import Data.Tuple.HT (mapSnd, )
import Data.Bool.HT (if', )


type Die = Int

type Probability = Rational
type Dist = Dist.T Probability

die ::
   (Obj.C prob experiment, Fractional prob) =>
   Score -> experiment Die
die maxPips = Obj.uniform [1..maxPips]



type Score = Int

{- |
We reformulate the problem to the following game:
There are two players, both of them collect a number of points.
In every round the player with the smaller score throws a die
and adds the pips to his score.
If the two players somewhen get the same score, then the game ends
and the score is the result of the game (@Just score@).
If one of the players exceeds the maximum score n,
then the game stops and players lose (@Nothing@).
-}
game ::
   (Obj.C prob experiment, Fractional prob) =>
   Score -> Score -> (Score,Score) -> experiment (Maybe Score)
game maxPips maxScore =
   let go (x,y) =
          if maxScore < max x y
            then return Nothing
            else case compare x y of
                    EQ -> return (Just x)
                    LT -> do
                       d <- die maxPips
                       go (x+d, y)
                    GT -> do
                       d <- die maxPips
                       go (x, y+d)
   in  go

gameRound ::
   Score -> Score ->
   Dist (Either (Maybe Score) (Score,Score)) ->
   Dist (Either (Maybe Score) (Score,Score))
gameRound maxPips maxScore current = Dist.norm $ do
   e <- current
   case e of
      Left end -> return $ Left end
      Right (x,y) ->
         if maxScore < max x y
           then return $ Left Nothing
           else case compare x y of
                   EQ -> return $ Left (Just x)
                   LT -> do
                      d <- die maxPips
                      return $ Right (x+d, y)
                   GT -> do
                      d <- die maxPips
                      return $ Right (x, y+d)

gameFast :: Score -> Score -> Dist (Score,Score) -> Dist (Maybe Score)
gameFast maxPips maxScore start =
   Dist.mapMaybe (either Just (error "the game must be finished after maxScore moves")) $
   nest (maxScore+1) (gameRound maxPips maxScore) (fmap Right start)

gameFastEither :: Score -> Score -> Dist (Score,Score) -> Dist (Maybe Score)
gameFastEither maxPips maxScore = Trans.untilLeft $ \(x,y) ->
   if maxScore < max x y
     then return $ Left Nothing
     else case compare x y of
             EQ -> return $ Left (Just x)
             LT -> do
                d <- die maxPips
                return $ Right (x+d, y)
             GT -> do
                d <- die maxPips
                return $ Right (x, y+d)

{- |
This version could be generalized
to both Random and Distribution monad
while remaining efficient.
-}
gameFastFix :: Score -> Score -> Dist (Score,Score) -> Dist (Maybe Score)
gameFastFix maxPips maxScore =
   Trans.fix $ \go (x,y) ->
      if maxScore < max x y
        then return Nothing
        else case compare x y of
                EQ -> return (Just x)
                LT -> do
                   d <- die maxPips
                   go (x+d, y)
                GT -> do
                   d <- die maxPips
                   go (x, y+d)

{- |
In 'gameFastFix' we group the scores by rounds.
This leads to a growing probability distribution,
but we do not need the round number.
We could process the game in a different way:
We only consider the game states
where the lower score matches the round number.
-}
gameLeastScore :: Score -> Score -> Dist (Score,Score) -> Dist (Maybe Score)
gameLeastScore maxPips maxScore =
   (Trans.fix $ \go (n,(x,y)) ->
      if n > maxScore
        then return Nothing
        else
           let next = go . (,) (succ n)
           in  case (x==n, y==n) of
                  (False, False) -> next (x,y)
                  (True, False) -> do
                     d <- die maxPips
                     next (x+d, y)
                  (False, True) -> do
                     d <- die maxPips
                     next (x, y+d)
                  (True, True) -> return (Just x))
   .
   fmap ((,) 0)

{- |
'gameLeastScore' can be written in terms of a matrix power.
For n pips we need a n² × n² matrix.
Using symmetries, we reduce it to a square matrix with size n·(n+1)/2.

/ p[n+1,(n+1,n+1)] \          / p[n,(n+0,n+0)] \
| p[n+1,(n+1,n+2)] |          | p[n,(n+0,n+1)] |
| p[n+1,(n+1,n+3)] |          | p[n,(n+0,n+2)] |
|        ...       |          |       ...      |
| p[n+1,(n+1,n+6)] |  = M/6 · | p[n,(n+0,n+5)] |
| p[n+1,(n+2,n+2)] |          | p[n,(n+1,n+1)] |
|        ...       |          |       ...      |
| p[n+1,(n+2,n+6)] |          | p[n,(n+1,n+5)] |
|        ...       |          |       ...      |
\ p[n+1,(n+6,n+6)] /          \ p[n,(n+5,n+5)] /

M[(n+1,(x,y)),(n,(x,y))] = 6

M[(n+1,(min y (n+d), max y (n+d))), (n,(n,y))] = 1

M[(n+1,(x1,y1)),(n,(x0,y0))] = 0
-}
flattenedMatrix :: Score -> [Int]
flattenedMatrix maxPips = do
   x1 <- [1..maxPips]
   y1 <- [x1..maxPips]
   x0 <- [0..maxPips-1]
   y0 <- [x0..maxPips-1]
   return $
      if' ((x0,y0) == (x1,y1)) maxPips $
      if' (x0==0 && (y0==x1 || y0==y1)) 1 0

{-
let e0 = [1,0,0,...,0]

The cumulated probability is
e0 * (I + M + M^2 + ... + M^(n-1)) * startVector
and with M = V*D*V^-1 we get
e0 * V * (I + D + D^2 + ... + D^(n-1)) * V^-1 * startVector

e0 * (I - M^n) * (I-M)^(-1) * startVector
-}
startVector :: Score -> [Int]
startVector maxPips = do
   x <- [0..maxPips-1]
   y <- [x..maxPips-1]
   return $ if' (x==y) 1 2


compareMaybe :: (Ord a) => Maybe a -> Maybe a -> Ordering
compareMaybe Nothing _ = GT
compareMaybe _ Nothing = LT
compareMaybe (Just a) (Just b) = compare a b

cumulate :: (Ord a) => Dist (Maybe a) -> [(Maybe a, Probability)]
cumulate =
   uncurry zip . mapSnd (scanl1 (+)) . unzip .
   List.sortBy (compose2 compareMaybe fst) . Dist.decons

runExact :: Score -> IO ()
runExact maxPips =
   mapM_ (\(m,p) ->
      case m of
         Just n ->
            P.printf "%4d %7.2f   %s\n"
               n (fromRational (100*p) :: Double) (show p)
         Nothing -> putStrLn $ "total: " ++ show p) $
   cumulate $
   gameFastFix maxPips 120 $ fmap ((,) 0) $ die maxPips


trace :: Score -> [Score] -> [Score]
trace s xs@(x:_) = s : trace (s+x) (drop x xs)
trace s [] = [s]

chop :: [Score] -> [[Score]]
chop [] = []
chop xs@(x:_) = uncurry (:) $ mapSnd chop $ splitAt x xs

meeting :: [Score] -> [Score] -> Maybe Score
meeting xt@(x:xs) yt@(y:ys) =
   case compare x y of
      LT -> meeting xs yt
      GT -> meeting xt ys
      EQ -> Just x
meeting _ _ = Nothing

{- |
This is a bruteforce implementation of the original game:
We just roll the die @maxScore@ times
and then jump from die to die according to the number of pips.
-}
bruteforce :: Score -> Score -> (Score,Score) -> Random.T (Maybe Score)
bruteforce maxPips maxScore (x,y) = do
   points <- replicateM maxScore $ die maxPips
   let run s = trace s (drop s points)
   return (meeting (run x) (run y))

runSimulation :: Score -> IO ()
runSimulation maxPips =
   mapM_ (\(m,p) ->
      case m of
         Just n ->
            P.printf "%4d %7.2f\n"
               n (fromRational (100*p) :: Double)
         Nothing -> putStrLn $ "total: " ++ show p) $
   cumulate $
   Random.runSeed (Rnd.mkStdGen 42) $ Random.dist $
   replicate 100000 $ bruteforce maxPips 120 . (,) 0 =<< die maxPips


latexDie :: Score -> String
latexDie pips =
   "\\epsdice{" ++ show pips ++ "}"

latexMarkedDie :: Score -> String
latexMarkedDie pips =
   "\\epsdice[black]{" ++ show pips ++ "}"

latexFromChain :: [Score] -> String
latexFromChain =
   unlines . map latexDie

latexChoppedFromChain :: [Score] -> String
latexChoppedFromChain =
   unlines .
   concatMap (\(p:ps) -> latexMarkedDie p : map latexDie ps) .
   chop

makeChains :: IO ()
makeChains = do
   let chains =
          map
             (\seed ->
                Random.runSeed (Rnd.mkStdGen seed) $
                replicateM 42 $ die 6)
             [30..42]
   writeFile "KruskalDice.tex" $
      "\\noindent\n"
      ++
      (List.intercalate "\\\\[4ex]\n" $
       map (("$\\rightarrow$" ++) . latexFromChain) chains)
      ++
      "\\newpage\n" ++
      "\\noindent\n"
      ++
      (List.intercalate "\\\\[4ex]\n" $
       map (("$\\rightarrow$" ++) . latexChoppedFromChain) chains)
