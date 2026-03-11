{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Mikael Johansson 2006
Maintainer   :   mik@math.uni-jena.de
Stability    :   provisional
Portability  :   requires multi-parameter type classes

Permutation of Integers represented by cycles.
-}

module MathObj.Permutation.CycleList where

import Data.Set(Set)
import qualified Data.Set as Set

import Data.List (unfoldr)
import Data.Array(Ix)
import qualified Data.Array as Array

import qualified Data.List.Match as Match
import Data.Maybe.HT (toMaybe)
import NumericPrelude.Numeric (fromInteger)
import NumericPrelude.Base


type Cycle i = [i]
type T i = [Cycle i]



fromFunction :: (Ix i) =>
   (i, i) -> (i -> i) -> T i
fromFunction rng f =
   let extractCycle available =
          do el <- choose available
             let orb = orbit f el
             return (orb, Set.difference available (Set.fromList orb))
       cycles = unfoldr extractCycle (Set.fromList (Array.range rng))
   in  keepEssentials cycles



-- right action of a cycle
cycleRightAction :: (Eq i) => i -> Cycle i -> i
x `cycleRightAction` c = cycleAction c x

-- left action of a cycle
cycleLeftAction :: (Eq i) => Cycle i -> i -> i
c `cycleLeftAction` x = cycleAction (reverse c) x

cycleAction :: (Eq i) => [i] -> i -> i
cycleAction cyc x =
   case dropWhile (x/=) (cyc ++ [head cyc]) of
      _:y:_ -> y
      _ -> x


cycleOrbit :: (Ord i) => Cycle i -> i -> [i]
cycleOrbit cyc = orbit (flip cycleRightAction cyc)

{- |
Right (left?) group action on the Integers.
Close to, but not the same as the module action in Algebra.Module.
-}
(*>) :: (Eq i) => T i -> i -> i
p *> x = foldr (flip cycleRightAction) x p

cyclesOrbit ::(Ord i) => T i -> i -> [i]
cyclesOrbit p = orbit (p *>)

orbit :: (Ord i) => (i -> i) -> i -> [i]
orbit op x0 = takeUntilRepetition (iterate op x0)

-- | candidates for Utility ?
takeUntilRepetition :: Ord a => [a] -> [a]
takeUntilRepetition xs =
   let accs = scanl (flip Set.insert) Set.empty xs
       lenlist = takeWhile not (zipWith Set.member xs accs)
   in  Match.take lenlist xs

takeUntilRepetitionSlow :: Eq a => [a] -> [a]
takeUntilRepetitionSlow xs =
   let accs = scanl (flip (:)) [] xs
       lenlist = takeWhile not (zipWith elem xs accs)
   in  Match.take lenlist xs


{-
Alternative to Data.Set.minView in GHC-6.6.
-}
choose :: Set a -> Maybe a
choose set =
   toMaybe (not (Set.null set)) (Set.findMin set)

keepEssentials :: T i -> T i
keepEssentials = filter isEssential

-- is more lazy than (length cyc > 1)
isEssential :: Cycle i -> Bool
isEssential = not . null . drop 1

inverse :: T i -> T i
inverse = map reverse
