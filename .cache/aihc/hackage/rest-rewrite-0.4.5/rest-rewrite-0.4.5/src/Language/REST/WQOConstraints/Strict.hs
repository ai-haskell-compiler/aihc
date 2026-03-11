{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines an implemenation for representing constraints on a 'WQO';
--   in this case represented by a set of "extendable" WQOs each satisfying the constraints.
--   For more details see 'StrictOC'
module Language.REST.WQOConstraints.Strict (
      strictOC
    , strictOC'
    , difference
    , isUnsatisfiable
    , noConstraints
    , permits
    , StrictOC
    ) where

import Control.Monad.Identity
import GHC.Generics (Generic)
import Data.Hashable
import Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S

import qualified Language.REST.WQOConstraints as OC
import qualified Language.REST.Internal.WQO as WQO

type WQO = WQO.WQO

-- Represents a set of constraints on a WQO on type `a`

-- The constraints are represented as a set ws of WQOs
-- The constraints permit any WQO w that is a valid extension of some (w' in wqos)

-- | @StrictOC ws@ represents constraints on a WQO. Each element of @ws@ is a WQO
--   that satisfies the constraints. @StrictOC ws@ permits a WQO @w@ if there exists
--   a @w'@ in @ws@ such that @w'@ can be extended to yield @w@.
--
--   This implementation is similar to disjunctive normal form representation of
--   logical formulas; except in this case each "conjunction" is a valid WQO, and thus
--   "satisfiable". Therefore @StrictOC ws@ satisfies /some/ WQO iff @ws@ is not empty.
--
--   Two potential downsides to this implementation are:
--   1. The size of @ws@ can grow quickly; an inherent issue of DNF
--   2. Related, calculating the entire set @ws@ is computationally expensive,
--      and often unnecessary for RESTs use-case, where continuing the path only
--      requires knowing if /any/ WQO is permitted.
newtype StrictOC a = StrictOC (S.Set (WQO a))
  deriving (Eq, Ord, Generic, Hashable)

instance (Show a, Eq a, Ord a, Hashable a) => Show (StrictOC a) where
  show (StrictOC cs) | S.null cs             = "unsatisfiable"
  show (StrictOC cs) | S.member WQO.empty cs = "no constraints"
  show (StrictOC cs) = L.intercalate " ∨ \n" (map show (S.toList cs))

getOrdering :: StrictOC a -> Maybe (WQO a)
getOrdering (StrictOC o) =
  listToMaybe (S.toList o)

-- | Constraints that permit any 'WQO'. In this case implemented by
--   a singleton set containing an empty WQO.
noConstraints :: forall a. (Eq a, Ord a, Hashable a) => StrictOC a
noConstraints = StrictOC (S.singleton WQO.empty)

unsatisfiable :: StrictOC a
unsatisfiable = StrictOC S.empty

-- | Returns @true@ iff @strictOC ws@ does not permit any WQOs; i.e., if @ws@ is empty.
isUnsatisfiable :: Eq a => StrictOC a -> Bool
isUnsatisfiable c = c == unsatisfiable

isSatisfiable :: Eq a => StrictOC a -> Bool
isSatisfiable c = c /= unsatisfiable

notStrongerThan :: forall m a. (Monad m, Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> m Bool
notStrongerThan (StrictOC _lhs) (StrictOC _rhs) = return False

-- The difference of two constraints `a` and `b` is new constraints such that
-- intersect (diff a b) b = a
difference :: (Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
difference (StrictOC lhs) (StrictOC rhs) =
    StrictOC (S.difference lhs rhs)

-- The union  of two constraints `a` and `b` is new constraints that only
-- permits an ordering if permitted by either `a` or `b`
union :: (Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
union (StrictOC lhs) (StrictOC rhs) =
  fromSet $ S.union lhs rhs

fromSet :: (Eq a, Ord a, Hashable a) => S.Set (WQO a) -> StrictOC a
fromSet oc = -- StrictOC oc
  StrictOC $ go [] (L.sortOn (length . WQO.elems) $ S.toList oc)
  where
    go include []       = S.fromList include
    go include (x : xs) =
        if any (`WQO.notStrongerThan` x) (include ++ xs)
            then go include xs
            else go (x : include) xs


-- | The intersection of two constraints `a` and `b` is new constraints that only
--   permits the orderings permitted by both `a` and `b`
intersect :: (Show a, Eq a, Ord a, Hashable a) => StrictOC a -> StrictOC a -> StrictOC a
intersect (StrictOC lhs) (StrictOC rhs) = result
  -- trace (printf "%s intersect %s yields %s" (show lhs) (show rhs) (show result)) result
    where
      result = fromSet $ S.fromList $
        do
          lhs' <- S.toList lhs
          rhs' <- S.toList rhs
          maybeToList (WQO.merge lhs' rhs')

addConstraint :: (Eq a, Ord a, Hashable a) => WQO a -> StrictOC a -> StrictOC a
addConstraint c (StrictOC oc) = StrictOC $ S.fromList $ do
  c'  <-  S.toList oc
  maybeToList $ WQO.merge c c'

-- | @StrictOC ws@ permits a 'WQO' @w@ if there exists a @w'@ in @ws@
--   that can be extended to equal @w@
permits :: (Eq a, Ord a, Hashable a) => StrictOC a -> WQO a -> Bool
permits (StrictOC permitted) desired =
  any (`WQO.notStrongerThan` desired) (S.toList permitted)

-- | An implementation of 'StrictOC'; for any computational context
strictOC :: Monad m => OC.WQOConstraints StrictOC m
strictOC = OC.OC
  addConstraint
  intersect
  (return . isSatisfiable)
  notStrongerThan
  noConstraints
  permits
  union
  unsatisfiable
  getOrdering

-- | An implementation of 'StrictOC' in the 'Identity' monad; usable in pure
--   computations.
strictOC' :: OC.WQOConstraints StrictOC Identity
strictOC' = strictOC
