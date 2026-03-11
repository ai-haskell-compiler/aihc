{-# LANGUAGE RebindableSyntax #-}
{- |
Copyright    :   (c) Henning Thielemann 2006
Maintainer   :   numericprelude@henning-thielemann.de
Stability    :   provisional
Portability  :

Permutation represented by an array of the images.
-}

module MathObj.Permutation.Table where

import qualified MathObj.Permutation as Perm

import Data.Set(Set)
import qualified Data.Set as Set

import Data.Array(Array,(!),(//),Ix)
import qualified Data.Array as Array

import Data.List ((\\), nub, unfoldr, )

import Data.Tuple.HT (swap, )
import Data.Maybe.HT (toMaybe, )

import NumericPrelude.Base hiding (cycle)


type T i = Array i i


fromFunction :: (Ix i) =>
   (i, i) -> (i -> i) -> T i
fromFunction rng f =
   Array.listArray rng (map f (Array.range rng))

toFunction :: (Ix i) => T i -> (i -> i)
toFunction = (!)

{-
Create a permutation in table form
from any other permutation representation.
-}
fromPermutation :: (Ix i, Perm.C p) => p i -> T i
fromPermutation x =
   let rng = Perm.domain x
   in  Array.listArray rng (map (Perm.apply x) (Array.range rng))

fromCycles :: (Ix i) => (i, i) -> [[i]] -> T i
fromCycles rng = foldl (flip cycle) (identity rng)


identity :: (Ix i) => (i, i) -> T i
identity rng = Array.listArray rng (Array.range rng)

cycle :: (Ix i) => [i] -> T i -> T i
cycle cyc p =
   p // zipWith (\i j -> (j,p!i)) cyc (tail (cyc++cyc))

inverse :: (Ix i) => T i -> T i
inverse p =
   let rng = Array.bounds p
   in  Array.array rng (map swap (Array.assocs p))

compose :: (Ix i) => T i -> T i -> T i
compose p q =
   let pRng = Array.bounds p
       qRng = Array.bounds q
   in  if pRng==qRng
         then fmap (p!) q
         else error "compose: ranges differ"
--                     ++ show pRng ++ " /= " ++ show qRng)


{- |
Extremely naïve algorithm
to generate a list of all elements in a group.
Should be replaced by a Schreier-Sims system
if this code is ever used for anything bigger than .. say ..
groups of order 512 or so.
-}
{-
Alternative to Data.Set.minView in GHC-6.6.
-}
choose :: Set a -> Maybe (a, Set a)
choose set =
   toMaybe (not (Set.null set)) (Set.deleteFindMin set)

closure :: (Ix i) => [T i] -> [T i]
closure [] = []
closure generators@(gen:_) =
   let genSet = Set.fromList generators
       idSet  = Set.singleton (identity (Array.bounds gen))
       generate (registered, candidates) =
          do (cand, remCands) <- choose candidates
             let newCands =
                    flip Set.difference registered $
                    Set.map (compose cand) genSet
             return (cand, (Set.union registered newCands,
                            Set.union remCands newCands))
   in  unfoldr generate (idSet, idSet)

closureSlow :: (Ix i) => [T i] -> [T i]
closureSlow [] = []
closureSlow generators@(gen:_) =
   let addElts grp [] = grp
       addElts grp cands@(cand:remCands) =
          let group'   = grp ++ [cand]
              newCands = map (compose cand) generators
              cands'   = nub (remCands ++ newCands) \\ (grp ++ cands)
          in  addElts group' cands'
   in  addElts [] [identity (Array.bounds gen)]
