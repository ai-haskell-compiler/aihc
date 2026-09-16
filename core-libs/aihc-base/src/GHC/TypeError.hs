{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- | Custom type errors.
--
-- GHC declares these in @GHC.Internal.TypeError@ and re-exports them here.
-- @aihc-internal@ depends on @aihc-base@ rather than the other way round,
-- so the declarations live here and the internal module re-exports them.
module GHC.TypeError
  ( ErrorMessage (..),
    TypeError,
    Assert,
    Unsatisfiable,
    unsatisfiable,
  )
where

import GHC.Types (Bool (..), Constraint, Symbol, Type)

-- | The parts a custom type error is written from. It is used only as a
-- kind: a message is a promoted value.
type ErrorMessage :: Type
data ErrorMessage
  = -- | Literal text.
    Text Symbol
  | -- | A type, rendered as the error message shows it.
    forall t. ShowType t
  | -- | Both messages, side by side.
    ErrorMessage :<>: ErrorMessage
  | -- | Both messages, one above the other.
    ErrorMessage :$$: ErrorMessage

infixl 6 :<>:

infixl 5 :$$:

-- | A constraint or a type that reports its message instead of being
-- solved. The family has no equations: the solver recognises it.
type TypeError :: forall b. ErrorMessage -> b
type family TypeError a where

-- | The constraint, when the check fails.
type Assert :: Bool -> Constraint -> Constraint
type family Assert check errMsg where
  Assert 'True _ = ()
  Assert _ errMsg = errMsg

-- | A constraint that cannot be solved, and reports its message when it
-- is demanded.
-- The method is deliberately not exported, as in GHC: a user writes no
-- instance of this class.
type Unsatisfiable :: ErrorMessage -> Constraint
class Unsatisfiable msg where
  unsatisfiableLifted :: a

-- | The value of any type, from an unsatisfiable constraint.
--
-- GHC quantifies the result over its runtime representation. This one is
-- lifted only, which is a divergence recorded in
-- @docs/type-level-naturals.md@.
unsatisfiable :: forall msg a. (Unsatisfiable msg) => a
unsatisfiable = unsatisfiableLifted @msg @a
