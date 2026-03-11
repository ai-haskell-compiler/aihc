{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE FlexibleContexts #-}

-- | This module includes a typeclass for implementations of constraints on 'WQO's
module Language.REST.WQOConstraints
  (
    WQOConstraints(..)
  , ConstraintGen
  , liftC
  , cmapConstraints
  , isUnsatisfiable
  , intersectAll
  , unionAll
  , intersectRelation
  , runStateConstraints
  , singleton
  )  where

import Control.Monad.Identity
import Control.Monad.State.Strict
import qualified Data.List as L
import Data.Hashable

import Prelude hiding (GT, EQ)

import qualified Language.REST.Internal.WQO as WQO
import Language.REST.Types
import Language.REST.SMT (ToSMTVar)

type WQO = WQO.WQO

-- | @WQOConstraints impl m@ defines an implementation for tracking and checking
--   satisfiability of constraints on arbitrary type @a@. Namely, instances of
--   @impl a@ are used to keep track of constraints. Satisfiability checking and
--   other computations are embedded in a computational context @m@.
data WQOConstraints impl m = OC
  {
    -- | @addConstraint wqo c@ adds constraints to @c@ to also permit the WQO @w@.
    addConstraint       :: forall a. (Eq a, Ord a, Hashable a) => WQO a -> impl a -> impl a

    -- | @intersect c1 c2@ returns constraints to permit only WQOs permitted by both @c1@ and
    --   @c2@. Therefore the resulting constraints are stronger (less likely to be
    --   satisifiable).
  , intersect           :: forall a. (Show a, Eq a, Ord a, Hashable a) => impl a -> impl a -> impl a
    -- | @isSatisfiable c@ returns true iff @c@ permits any WQO
  , isSatisfiable       :: forall a. (ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => impl a -> m Bool

    -- | @c1 `notStrongerThan` c2@ iff any ordering permitted by @c1@ is also permitted
    --   by @c2@
  , notStrongerThan     :: forall a. (ToSMTVar a Int, Eq a, Ord a, Hashable a) => impl a -> impl a -> m Bool
    -- | @noConstraints@ returns an instance of constraints that permits any WQO
  , noConstraints       :: forall a. (Eq a, Ord a, Hashable a) => impl a

  , permits             :: forall a. (Show a, Eq a, Ord a, Hashable a) => impl a -> WQO a -> Bool
    -- | @c1 `union` c2@ returns constraints that permit WQOs permitted by /either/
    --  @c1@ or @c2@. The resulting constraints are therefore weaker (more likely to
    --  be satisfiable)
  , union               :: forall a. (Eq a, Ord a, Hashable a) => impl a -> impl a -> impl a

    -- | @unsatisfiable@ returns an instance of constraints that does not permit any WQO
  , unsatisfiable       :: forall a. impl a

    -- | @getOrdering c@ returns a concrete ordering satisfying the constraints, if one exists
  , getOrdering         :: forall a. impl a -> Maybe (WQO a)
  }

-- | Returns true iff the constraints do not permit any WQO.
isUnsatisfiable :: (Functor m, ToSMTVar a Int, Show a, Eq a, Ord a, Hashable a) => WQOConstraints oc m -> oc a -> m Bool
isUnsatisfiable OC{isSatisfiable} c = not <$> isSatisfiable c

-- | Returns the constraints that permit a given WQO
singleton :: (Eq a, Ord a, Hashable a) => WQOConstraints oc m -> WQO a -> oc a
singleton OC{addConstraint, noConstraints} c = addConstraint c noConstraints

-- | Given a list of constraints @ocs@, returns constraints that permit only the WQOs
--   permitted by each @oc@ in @ocs@
intersectAll :: (Eq a, Ord a, Hashable a, Show a, Show (oc a)) => WQOConstraints oc m -> [oc a] -> oc a
intersectAll OC{noConstraints} []     = noConstraints
intersectAll OC{intersect}     (x:xs) = L.foldl' intersect x xs

-- | Given a list of constraints @ocs@, returns constraints that permit the WQOs
--   permitted by any @oc@ in @ocs@
unionAll :: (Eq a, Ord a, Hashable a, Show a, Show (oc a)) => WQOConstraints oc m -> [oc a] -> oc a
unionAll OC{unsatisfiable} []     = unsatisfiable
unionAll OC{union}         (x:xs) = L.foldl' union x xs

-- | @intersectRelation oc impl (f, g, r)@ strengthens constraints represented by @impl@
--   to also ensure that @f@ and @g@ are related via relation @r@ in permitted WQOs.
intersectRelation ::
  (Ord a, Eq a, Ord a, Hashable a, Show a) =>
  WQOConstraints oc m -> oc a -> (a, a, Relation) -> oc a
intersectRelation oc impl (f, g, r) =
  case nc r of
    Just impl' -> intersect oc impl impl'
    Nothing    -> unsatisfiable oc
  where
    nc GT  = fmap (singleton oc) (WQO.singleton (f, g, WQO.QGT))
    nc EQ  = fmap (singleton oc) (WQO.singleton (f, g, WQO.QEQ))
    nc GTE = do
      wqo1 <- WQO.singleton (f, g, WQO.QGT)
      wqo2 <- WQO.singleton (f, g, WQO.QEQ)
      return $ union oc (singleton oc wqo1) (singleton oc wqo2)

-- | ConstraintGen impl R >= t u returns the constraints on >= that guarantee
-- the resulting relation >=', we have:
--   1. x >= y implies x >=' y
--   2. t lift(R(>=')) u
-- Where R generates { == , >=, > } from the underlying ordering
-- R is used to enable optimizations
type ConstraintGen oc base lifted m =
  forall m' . (WQOConstraints oc m' -> Relation -> oc base -> lifted -> lifted -> m (oc base))

-- | @cmapConstraints@ takes a transformation @f@ from @lifted' to lifted@, and transforms
-- a constraint generator on terms of types @lifted@ into one on terms of types @lifted'@
cmapConstraints :: (lifted' -> lifted) -> ConstraintGen oc base lifted m -> ConstraintGen oc base lifted' m
cmapConstraints f cgen impl r oc t u = cgen impl r oc (f t) (f u)

-- | @liftc f imp@ lifts the computations of @imp@ from context @m@ to context @m'@
liftC :: (m Bool  -> m' Bool) -> WQOConstraints impl m -> WQOConstraints impl m'
liftC f oc = oc{
    isSatisfiable   = isSatisfiable'
  , notStrongerThan = notStrongerThan'
  }
  where
    isSatisfiable'   c1    = f (isSatisfiable oc c1)
    notStrongerThan' c1 c2 = f (notStrongerThan oc c1 c2)

-- @runStateConstriants initState cgen@ transforms a constraint generator in the 'State'
-- monad to one in the 'Identity' monad by using initial state @initState@'
runStateConstraints :: ConstraintGen oc base lifted (State a) -> a -> ConstraintGen oc base lifted Identity
runStateConstraints cgen initState impl r oc t u = Identity $ evalState (cgen impl r oc t u) initState
