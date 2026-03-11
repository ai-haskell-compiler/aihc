{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE TypeFamilies #-}

-- | This module provides type aliases that emulate the behavior of GHC's constraint
-- tuple syntax. Unlike GHC's built-in constraint tuples, the type aliases in this
-- library can be partially applied.
--
-- The type aliases in this module are defined by way of type families
-- decompose applications of constraint tuple type constructors to their arguments.
module Data.Tuple.Constraint.TypeFamily
  ( -- * Constraint tuples
    CTuple0
  , CTuple1
  , CUnit
  , CSolo
  , CTuple2
  , CTuple3
  , CTuple4
  , CTuple5
  , CTuple6
  , CTuple7
  , CTuple8
  , CTuple9
  , CTuple10
  , CTuple11
  , CTuple12
  , CTuple13
  , CTuple14
  , CTuple15
  , CTuple16
  , CTuple17
  , CTuple18
  , CTuple19
  , CTuple20
  , CTuple21
  , CTuple22
  , CTuple23
  , CTuple24
  , CTuple25
  , CTuple26
  , CTuple27
  , CTuple28
  , CTuple29
  , CTuple30
  , CTuple31
  , CTuple32
  , CTuple33
  , CTuple34
  , CTuple35
  , CTuple36
  , CTuple37
  , CTuple38
  , CTuple39
  , CTuple40
  , CTuple41
  , CTuple42
  , CTuple43
  , CTuple44
  , CTuple45
  , CTuple46
  , CTuple47
  , CTuple48
  , CTuple49
  , CTuple50
  , CTuple51
  , CTuple52
  , CTuple53
  , CTuple54
  , CTuple55
  , CTuple56
  , CTuple57
  , CTuple58
  , CTuple59
  , CTuple60
  , CTuple61
  , CTuple62
#if __GLASGOW_HASKELL__ >= 902
  , CTuple63
#endif
#if __GLASGOW_HASKELL__ >= 902
  , CTuple64
#endif
  ) where

import Data.Tuple.Constraint (CTuple0, CTuple1, CSolo)
import Data.Kind (Constraint)

-- | A type alias for a constraint tuple with 0 arguments.
type CUnit = Decomposer0 (() :: Constraint)
type family   Decomposer0 (x :: Constraint) :: Constraint
type instance Decomposer0 (f ) = f

-- | A type alias for a constraint tuple with 2 arguments.
type CTuple2 = Decomposer2 (((), ()) :: Constraint)
type family   Decomposer2 (x :: Constraint) :: Constraint -> Constraint -> Constraint
type instance Decomposer2 (f c1 c2) = f

-- | A type alias for a constraint tuple with 3 arguments.
type CTuple3 = Decomposer3 (((), (), ()) :: Constraint)
type family   Decomposer3 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer3 (f c1 c2 c3) = f

-- | A type alias for a constraint tuple with 4 arguments.
type CTuple4 = Decomposer4 (((), (), (), ()) :: Constraint)
type family   Decomposer4 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer4 (f c1 c2 c3 c4) = f

-- | A type alias for a constraint tuple with 5 arguments.
type CTuple5 = Decomposer5 (((), (), (), (), ()) :: Constraint)
type family   Decomposer5 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer5 (f c1 c2 c3 c4 c5) = f

-- | A type alias for a constraint tuple with 6 arguments.
type CTuple6 = Decomposer6 (((), (), (), (), (), ()) :: Constraint)
type family   Decomposer6 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer6 (f c1 c2 c3 c4 c5 c6) = f

-- | A type alias for a constraint tuple with 7 arguments.
type CTuple7 = Decomposer7 (((), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer7 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer7 (f c1 c2 c3 c4 c5 c6 c7) = f

-- | A type alias for a constraint tuple with 8 arguments.
type CTuple8 = Decomposer8 (((), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer8 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer8 (f c1 c2 c3 c4 c5 c6 c7 c8) = f

-- | A type alias for a constraint tuple with 9 arguments.
type CTuple9 = Decomposer9 (((), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer9 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer9 (f c1 c2 c3 c4 c5 c6 c7 c8 c9) = f

-- | A type alias for a constraint tuple with 10 arguments.
type CTuple10 = Decomposer10 (((), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer10 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer10 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10) = f

-- | A type alias for a constraint tuple with 11 arguments.
type CTuple11 = Decomposer11 (((), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer11 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer11 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11) = f

-- | A type alias for a constraint tuple with 12 arguments.
type CTuple12 = Decomposer12 (((), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer12 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer12 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12) = f

-- | A type alias for a constraint tuple with 13 arguments.
type CTuple13 = Decomposer13 (((), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer13 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer13 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13) = f

-- | A type alias for a constraint tuple with 14 arguments.
type CTuple14 = Decomposer14 (((), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer14 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer14 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14) = f

-- | A type alias for a constraint tuple with 15 arguments.
type CTuple15 = Decomposer15 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer15 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer15 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15) = f

-- | A type alias for a constraint tuple with 16 arguments.
type CTuple16 = Decomposer16 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer16 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer16 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16) = f

-- | A type alias for a constraint tuple with 17 arguments.
type CTuple17 = Decomposer17 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer17 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer17 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17) = f

-- | A type alias for a constraint tuple with 18 arguments.
type CTuple18 = Decomposer18 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer18 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer18 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18) = f

-- | A type alias for a constraint tuple with 19 arguments.
type CTuple19 = Decomposer19 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer19 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer19 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19) = f

-- | A type alias for a constraint tuple with 20 arguments.
type CTuple20 = Decomposer20 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer20 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer20 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20) = f

-- | A type alias for a constraint tuple with 21 arguments.
type CTuple21 = Decomposer21 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer21 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer21 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21) = f

-- | A type alias for a constraint tuple with 22 arguments.
type CTuple22 = Decomposer22 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer22 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer22 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22) = f

-- | A type alias for a constraint tuple with 23 arguments.
type CTuple23 = Decomposer23 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer23 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer23 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23) = f

-- | A type alias for a constraint tuple with 24 arguments.
type CTuple24 = Decomposer24 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer24 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer24 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24) = f

-- | A type alias for a constraint tuple with 25 arguments.
type CTuple25 = Decomposer25 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer25 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer25 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25) = f

-- | A type alias for a constraint tuple with 26 arguments.
type CTuple26 = Decomposer26 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer26 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer26 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26) = f

-- | A type alias for a constraint tuple with 27 arguments.
type CTuple27 = Decomposer27 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer27 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer27 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27) = f

-- | A type alias for a constraint tuple with 28 arguments.
type CTuple28 = Decomposer28 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer28 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer28 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28) = f

-- | A type alias for a constraint tuple with 29 arguments.
type CTuple29 = Decomposer29 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer29 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer29 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29) = f

-- | A type alias for a constraint tuple with 30 arguments.
type CTuple30 = Decomposer30 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer30 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer30 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30) = f

-- | A type alias for a constraint tuple with 31 arguments.
type CTuple31 = Decomposer31 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer31 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer31 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31) = f

-- | A type alias for a constraint tuple with 32 arguments.
type CTuple32 = Decomposer32 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer32 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer32 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32) = f

-- | A type alias for a constraint tuple with 33 arguments.
type CTuple33 = Decomposer33 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer33 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer33 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33) = f

-- | A type alias for a constraint tuple with 34 arguments.
type CTuple34 = Decomposer34 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer34 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer34 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34) = f

-- | A type alias for a constraint tuple with 35 arguments.
type CTuple35 = Decomposer35 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer35 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer35 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35) = f

-- | A type alias for a constraint tuple with 36 arguments.
type CTuple36 = Decomposer36 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer36 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer36 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36) = f

-- | A type alias for a constraint tuple with 37 arguments.
type CTuple37 = Decomposer37 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer37 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer37 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37) = f

-- | A type alias for a constraint tuple with 38 arguments.
type CTuple38 = Decomposer38 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer38 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer38 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38) = f

-- | A type alias for a constraint tuple with 39 arguments.
type CTuple39 = Decomposer39 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer39 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer39 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39) = f

-- | A type alias for a constraint tuple with 40 arguments.
type CTuple40 = Decomposer40 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer40 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer40 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40) = f

-- | A type alias for a constraint tuple with 41 arguments.
type CTuple41 = Decomposer41 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer41 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer41 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41) = f

-- | A type alias for a constraint tuple with 42 arguments.
type CTuple42 = Decomposer42 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer42 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer42 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42) = f

-- | A type alias for a constraint tuple with 43 arguments.
type CTuple43 = Decomposer43 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer43 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer43 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43) = f

-- | A type alias for a constraint tuple with 44 arguments.
type CTuple44 = Decomposer44 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer44 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer44 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44) = f

-- | A type alias for a constraint tuple with 45 arguments.
type CTuple45 = Decomposer45 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer45 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer45 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45) = f

-- | A type alias for a constraint tuple with 46 arguments.
type CTuple46 = Decomposer46 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer46 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer46 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46) = f

-- | A type alias for a constraint tuple with 47 arguments.
type CTuple47 = Decomposer47 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer47 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer47 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47) = f

-- | A type alias for a constraint tuple with 48 arguments.
type CTuple48 = Decomposer48 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer48 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer48 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48) = f

-- | A type alias for a constraint tuple with 49 arguments.
type CTuple49 = Decomposer49 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer49 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer49 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49) = f

-- | A type alias for a constraint tuple with 50 arguments.
type CTuple50 = Decomposer50 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer50 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer50 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50) = f

-- | A type alias for a constraint tuple with 51 arguments.
type CTuple51 = Decomposer51 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer51 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer51 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51) = f

-- | A type alias for a constraint tuple with 52 arguments.
type CTuple52 = Decomposer52 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer52 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer52 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52) = f

-- | A type alias for a constraint tuple with 53 arguments.
type CTuple53 = Decomposer53 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer53 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer53 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53) = f

-- | A type alias for a constraint tuple with 54 arguments.
type CTuple54 = Decomposer54 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer54 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer54 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54) = f

-- | A type alias for a constraint tuple with 55 arguments.
type CTuple55 = Decomposer55 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer55 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer55 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55) = f

-- | A type alias for a constraint tuple with 56 arguments.
type CTuple56 = Decomposer56 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer56 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer56 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56) = f

-- | A type alias for a constraint tuple with 57 arguments.
type CTuple57 = Decomposer57 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer57 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer57 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57) = f

-- | A type alias for a constraint tuple with 58 arguments.
type CTuple58 = Decomposer58 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer58 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer58 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58) = f

-- | A type alias for a constraint tuple with 59 arguments.
type CTuple59 = Decomposer59 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer59 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer59 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59) = f

-- | A type alias for a constraint tuple with 60 arguments.
type CTuple60 = Decomposer60 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer60 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer60 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60) = f

-- | A type alias for a constraint tuple with 61 arguments.
type CTuple61 = Decomposer61 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer61 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer61 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61) = f

-- | A type alias for a constraint tuple with 62 arguments.
type CTuple62 = Decomposer62 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer62 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer62 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62) = f

#if __GLASGOW_HASKELL__ >= 902
-- | A type alias for a constraint tuple with 63 arguments.
type CTuple63 = Decomposer63 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer63 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer63 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63) = f
#endif

#if __GLASGOW_HASKELL__ >= 902
-- | A type alias for a constraint tuple with 64 arguments.
type CTuple64 = Decomposer64 (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint)
type family   Decomposer64 (x :: Constraint) :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint
type instance Decomposer64 (f c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63 c64) = f
#endif

