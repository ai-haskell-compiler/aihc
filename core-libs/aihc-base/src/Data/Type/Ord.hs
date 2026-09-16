{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Ordering of type-level literals.
--
-- GHC declares @Compare@ with one instance per sort of literal, chosen by
-- the kind of its arguments. The solver here reads the sort from the
-- literal and computes the comparison, so the family has no instances.
module Data.Type.Ord
  ( Compare,
    OrdCond,
    type (<=),
    type (<=?),
    type (<),
    type (<?),
    type (>),
    type (>?),
    type (>=),
    type (>=?),
    Max,
    Min,
  )
where

import GHC.TypeError (Assert, ErrorMessage (..), TypeError)
import GHC.Types (Bool (..), Constraint, Ordering (..))

-- | The ordering of two type-level literals of the same sort.
type Compare :: k -> k -> Ordering
type family Compare a b

-- | Pick a branch by an ordering.
type OrdCond :: Ordering -> k -> k -> k -> k
type family OrdCond o lt eq gt where
  OrdCond 'LT lt _ _ = lt
  OrdCond 'EQ _ eq _ = eq
  OrdCond 'GT _ _ gt = gt

type (<=?) :: k -> k -> Bool
type (<=?) x y = OrdCond (Compare x y) 'True 'True 'False

type (<?) :: k -> k -> Bool
type (<?) x y = OrdCond (Compare x y) 'True 'False 'False

type (>=?) :: k -> k -> Bool
type (>=?) x y = OrdCond (Compare x y) 'False 'True 'True

type (>?) :: k -> k -> Bool
type (>?) x y = OrdCond (Compare x y) 'False 'False 'True

-- The comparison constraints report the comparison that failed, as GHC's
-- do, rather than leaving an unsolved equality.
type (<=) :: k -> k -> Constraint
type (<=) x y = Assert (x <=? y) (LeErrMsg x y)

type LeErrMsg :: k -> k -> Constraint
type LeErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " <= " ':<>: 'ShowType y)

type (<) :: k -> k -> Constraint
type (<) x y = Assert (x <? y) (LtErrMsg x y)

type LtErrMsg :: k -> k -> Constraint
type LtErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " < " ':<>: 'ShowType y)

type (>=) :: k -> k -> Constraint
type (>=) x y = Assert (x >=? y) (GeErrMsg x y)

type GeErrMsg :: k -> k -> Constraint
type GeErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " >= " ':<>: 'ShowType y)

type (>) :: k -> k -> Constraint
type (>) x y = Assert (x >? y) (GtErrMsg x y)

type GtErrMsg :: k -> k -> Constraint
type GtErrMsg x y = TypeError ('Text "Cannot satisfy: " ':<>: 'ShowType x ':<>: 'Text " > " ':<>: 'ShowType y)

type Max :: k -> k -> k
type Max x y = OrdCond (Compare x y) y y x

type Min :: k -> k -> k
type Min x y = OrdCond (Compare x y) x x y

infix 4 <=, <, >=, >, <=?, <?, >=?, >?
