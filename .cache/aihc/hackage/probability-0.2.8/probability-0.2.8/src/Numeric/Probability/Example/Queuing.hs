{- |

Model:

  one server serving customers from one queue

-}

module Numeric.Probability.Example.Queuing where

import qualified Numeric.Probability.Distribution as Dist
import qualified Numeric.Probability.Random as Rnd

import Numeric.Probability.Percentage
    (Dist, RDist, Trans, )

import Data.List (nub,sort)


{- no Random instance for Rational
type Probability = Rational
type Dist a  = Dist.T  Probability a
type RDist a = Rnd.Distribution Probability a
type Trans a = Transition    Probability a
-}


type Time = Int

-- | (servingTime, nextArrival)
type Profile = (Time, Time)

type Event a = (a,Profile)

-- | customers and their individual serving times
type Queue a = [(a,Time)]

-- | (customers waiting,validity period of that queue)
type State a = (Queue a,Time)

type System a = [([a],Time)]

type Events a = [Event a]


event :: Time -> Events a -> Queue a -> [State a]
event = mEvent 1

--event _ [] []                    = []
--event 0 ((c,(s,a)):es) q         =        event a     es (q++[(c,s)])
--event a es []                    = ([],a):event 0     es []
--event a [] (q@((c,s):q'))        =  (q,s):event a     [] q'
--event a es (q@((c,s):q')) | a<s  =  (q,a):event 0     es ((c,s-a):q')
--                          | True =  (q,s):event (a-s) es q'

system :: Events a -> System a
--system es = map (\(q,t)->(map fst q,t)) $ event 0 es []
system = mSystem 1


-- | multiple servers

mEvent :: Int -> Time -> Events a -> Queue a -> [State a]
mEvent _ _ [] []             =        []
mEvent n 0 ((c,(s,a)):es) q  = 	      mEvent n a     es (q++[(c,s)])
mEvent n a es []             = ([],a):mEvent n 0     es []
mEvent n _ [] q		     =  (q,s):mEvent n 0     [] (mServe n s q)
	where s = mTimeStep n q
mEvent n a es q =
   if a < s
     then (q,a) : mEvent n 0     es (mServe n a q)
     else (q,s) : mEvent n (a-s) es (mServe n s q)
	where s = mTimeStep n q


-- | decrease served customers remaining time by specified amount
mServe :: Int -> Int -> Queue a -> Queue a
mServe _ _ [] = []
mServe 0 _ x = x
mServe n c ((a,t):es) =
   if t > c
     then (a,t-c) : mServe (n-1) c es
     else mServe (n-1) c es

-- | time until next completion
mTimeStep :: Int -> Queue a -> Int
mTimeStep _ ((_,t):[]) = t
mTimeStep 1 ((_,t):_)  = t
mTimeStep n ((_,t):es) = min t (mTimeStep (n-1) es)
mTimeStep _ _ = error "Queuing.mTimeStep: queue must be non-empty"

mSystem :: Int -> Events a -> System a
mSystem n es = map (\(q,t)->(map fst q,t)) $ mEvent n 0 es []


-- * random

type RProfile = (Dist Time, Trans Time)

type REvent a = (a, RProfile)

type REvents a = [REvent a]

rSystem :: Int -> REvents a -> Rnd.T (System a)
rSystem n re = do
		e <- rBuildEvents re
		return (mSystem n e)

rBuildEvents :: REvents a -> Rnd.T (Events a)
rBuildEvents ((a,(dt,tt)):ex) = do
			rest <- rBuildEvents ex
			t <- Rnd.pick dt
			nt <- Rnd.pick $ tt t
			return ((a,(t,nt)):rest)
rBuildEvents [] = return []

rmSystem :: Ord a => Int -> Int -> REvents a -> RDist (System a)
rmSystem c n re = Rnd.dist $ replicate c (rSystem n re)

evalSystem :: (Ord a, Ord b) =>
   Int -> Int -> REvents a -> (System a -> b) -> RDist b
evalSystem c n re ef =
   do
      rds <- rmSystem c n re
      return (Dist.map ef rds)

unit :: b -> ((), b)
unit = (\p->((),p)) -- Dist.map (\p->((),p))


-- * evaluation

maxQueue :: Ord a => System a -> Int
maxQueue s = maximum [length q | (q,_) <- s]

allWaiting :: Ord a => Int -> System a -> [a]
allWaiting n s = nub $ sort $ concat [ drop n q | (q,_) <- s]


countWaiting :: Ord a => Int -> System a -> Int
countWaiting n = length . allWaiting n

waiting :: Int -> System a -> Time
waiting n s = sum [ t*length (drop n q) | (q,t) <- s]

inSystem :: System a -> Time
inSystem s = sum [ t*length q | (q,t) <- s]

total :: System a -> Time
total = sum . map snd

server :: Int -> System a -> Time
server n s = sum [ t*length (take n q) | (q,t) <- s]

idle :: Int -> System a -> Time
idle n s = sum [ t*(n - length q) | (q,t) <- s, length q <= n]

idleAvgP :: Int -> System a -> Float
idleAvgP n s = (fromIntegral $ idle n s) / (fromIntegral $ server n s)
