{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | This module defines "Lazy" constraints on a WQO; the intention is that
--   computations on this type do only the necessary amount of work to determine
--   satisfiability (deferring further computations in a thunk).
module Language.REST.WQOConstraints.Lazy (
      lazyOC
    , addConstraint
    , isSatisfiable
    , noConstraints
    , LazyOC
    ) where

import Text.Printf
import GHC.Generics (Generic)
import Data.Hashable

import qualified Language.REST.Internal.WQO as WQO
import qualified Language.REST.WQOConstraints as OC
import qualified Language.REST.WQOConstraints.ADT as ADT

type WQO = WQO.WQO

-- Partially lazy ordering constraints:
-- thunks computation after showing satisfiability

type Thunk a = ADT.ConstraintsADT a

-- | Implementation of "Lazy" ordering constraints.
data LazyOC a =
    Unsat
    -- @Sat wqo thunk@ represent satisfiable constraints; @wqo@ is a candidate.
    -- @Thunk@ represents the other satisfiable constraints, if any.
  | Sat (WQO a) (Thunk a)
  deriving (Eq, Ord, Generic, Hashable)

getOrdering :: LazyOC a -> Maybe (WQO a)
getOrdering (Sat wqo _) = Just wqo
getOrdering _           = Nothing

eval :: (Eq a, Ord a, Hashable a) => ADT.ConstraintsADT a -> LazyOC a
eval (ADT.Sat w)   = Sat w ADT.Unsat
eval ADT.Unsat     = Unsat
eval (ADT.Union lhs rhs) =
  case eval lhs of
    Sat w t1' -> Sat w (ADT.union t1' rhs)
    Unsat     -> eval rhs

eval (ADT.Intersect t1 t2)       =
  case (eval t1, eval t2) of
    (Sat c1 t1', Sat c2 t2') ->
      let
        rest =
          ADT.intersect (ADT.Sat c1) t2' `ADT.union`
          ADT.intersect (ADT.Sat c2) t1' `ADT.union`
          ADT.intersect t1' t2'
      in
        case WQO.merge c1 c2 of
          Just c' -> Sat c' rest
          Nothing -> eval rest
    _ -> Unsat


toADT :: Eq a => LazyOC a -> ADT.ConstraintsADT a
toADT Unsat     = ADT.Unsat
toADT (Sat w r) = ADT.union (ADT.Sat w) r

instance (Show a, Eq a, Ord a, Hashable a) => Show (LazyOC a) where
  show Unsat     = "⊥"
  show (Sat s r) = printf "%s ∨ lazy(%s)" (show s) (show r)

-- | Returns a new instance of 'LazyOC' permitting all WQOs
noConstraints :: LazyOC a
noConstraints = Sat WQO.empty ADT.Unsat

unsatisfiable :: LazyOC a
unsatisfiable = Unsat

union :: Eq a => LazyOC a -> LazyOC a -> LazyOC a
union Unsat s                 = s
union s Unsat                 = s
union (Sat s _)    _          | s == WQO.empty = noConstraints
union _           (Sat s _)   | s == WQO.empty = noConstraints
union (Sat s1 r1) (Sat s2 r2) = Sat s1 (ADT.union (ADT.Sat s2) (ADT.union r1 r2))

intersect :: (Ord a, Hashable a) => LazyOC a -> LazyOC a -> LazyOC a
intersect t1 t2 = eval $ ADT.intersect (toADT t1) (toADT t2)

-- | Returns @true@ if any orderings are permitted
isSatisfiable :: LazyOC a -> Bool
isSatisfiable (Sat _ _) = True
isSatisfiable Unsat     = False

notStrongerThan :: (Monad m, Eq a) => LazyOC a -> LazyOC a -> m Bool
notStrongerThan _     Unsat = return True
notStrongerThan t1    t2    = return $ t1 == t2

-- | @addConstraint o c@ strengthes @c@ to also contain every relation in @o@
addConstraint :: (Ord a, Hashable a) => WQO a -> LazyOC a -> LazyOC a
addConstraint o c = eval $ ADT.addConstraint o (toADT c)

permits :: (Ord a, Hashable a) => LazyOC a -> WQO.WQO a -> Bool
permits Unsat _            = False
permits (Sat s1 thunk) wqo = s1 `WQO.notStrongerThan` wqo || permits (eval thunk) wqo

-- | See 'LazyOC'
lazyOC :: Monad m => OC.WQOConstraints LazyOC m
lazyOC = OC.OC
  addConstraint
  intersect
  (return . isSatisfiable)
  notStrongerThan
  noConstraints
  permits
  union
  unsatisfiable
  getOrdering
