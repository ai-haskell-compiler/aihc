{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE Safe #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
#if __GLASGOW_HASKELL__ < 806
{-# LANGUAGE TypeInType #-}
#endif

-- | This module provides type aliases that emulate the behavior of GHC's constraint
-- tuple syntax. Unlike GHC's built-in constraint tuples, the type aliases in this
-- library can be partially applied.
--
-- The type aliases in this module are defined by way of type synonyms that
-- decompose applications of constraint tuple type constructors to their arguments.
module Data.Tuple.Constraint.TypeSynonym
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
import Data.Proxy (Proxy(..))

-- | A type alias for a constraint tuple with 0 arguments.
type CUnit = Decomposer0 ('Proxy :: Proxy (() :: Constraint))
type Decomposer0 (x :: Proxy ((f :: Constraint) )) = f

-- | A type alias for a constraint tuple with 2 arguments.
type CTuple2 = Decomposer2 ('Proxy :: Proxy (((), ()) :: Constraint))
type Decomposer2 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint) c1 c2)) = f

-- | A type alias for a constraint tuple with 3 arguments.
type CTuple3 = Decomposer3 ('Proxy :: Proxy (((), (), ()) :: Constraint))
type Decomposer3 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3)) = f

-- | A type alias for a constraint tuple with 4 arguments.
type CTuple4 = Decomposer4 ('Proxy :: Proxy (((), (), (), ()) :: Constraint))
type Decomposer4 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4)) = f

-- | A type alias for a constraint tuple with 5 arguments.
type CTuple5 = Decomposer5 ('Proxy :: Proxy (((), (), (), (), ()) :: Constraint))
type Decomposer5 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5)) = f

-- | A type alias for a constraint tuple with 6 arguments.
type CTuple6 = Decomposer6 ('Proxy :: Proxy (((), (), (), (), (), ()) :: Constraint))
type Decomposer6 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6)) = f

-- | A type alias for a constraint tuple with 7 arguments.
type CTuple7 = Decomposer7 ('Proxy :: Proxy (((), (), (), (), (), (), ()) :: Constraint))
type Decomposer7 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7)) = f

-- | A type alias for a constraint tuple with 8 arguments.
type CTuple8 = Decomposer8 ('Proxy :: Proxy (((), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer8 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8)) = f

-- | A type alias for a constraint tuple with 9 arguments.
type CTuple9 = Decomposer9 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer9 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9)) = f

-- | A type alias for a constraint tuple with 10 arguments.
type CTuple10 = Decomposer10 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer10 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10)) = f

-- | A type alias for a constraint tuple with 11 arguments.
type CTuple11 = Decomposer11 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer11 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11)) = f

-- | A type alias for a constraint tuple with 12 arguments.
type CTuple12 = Decomposer12 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer12 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12)) = f

-- | A type alias for a constraint tuple with 13 arguments.
type CTuple13 = Decomposer13 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer13 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13)) = f

-- | A type alias for a constraint tuple with 14 arguments.
type CTuple14 = Decomposer14 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer14 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14)) = f

-- | A type alias for a constraint tuple with 15 arguments.
type CTuple15 = Decomposer15 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer15 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15)) = f

-- | A type alias for a constraint tuple with 16 arguments.
type CTuple16 = Decomposer16 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer16 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16)) = f

-- | A type alias for a constraint tuple with 17 arguments.
type CTuple17 = Decomposer17 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer17 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17)) = f

-- | A type alias for a constraint tuple with 18 arguments.
type CTuple18 = Decomposer18 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer18 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18)) = f

-- | A type alias for a constraint tuple with 19 arguments.
type CTuple19 = Decomposer19 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer19 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19)) = f

-- | A type alias for a constraint tuple with 20 arguments.
type CTuple20 = Decomposer20 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer20 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20)) = f

-- | A type alias for a constraint tuple with 21 arguments.
type CTuple21 = Decomposer21 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer21 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21)) = f

-- | A type alias for a constraint tuple with 22 arguments.
type CTuple22 = Decomposer22 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer22 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22)) = f

-- | A type alias for a constraint tuple with 23 arguments.
type CTuple23 = Decomposer23 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer23 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23)) = f

-- | A type alias for a constraint tuple with 24 arguments.
type CTuple24 = Decomposer24 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer24 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24)) = f

-- | A type alias for a constraint tuple with 25 arguments.
type CTuple25 = Decomposer25 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer25 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25)) = f

-- | A type alias for a constraint tuple with 26 arguments.
type CTuple26 = Decomposer26 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer26 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26)) = f

-- | A type alias for a constraint tuple with 27 arguments.
type CTuple27 = Decomposer27 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer27 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27)) = f

-- | A type alias for a constraint tuple with 28 arguments.
type CTuple28 = Decomposer28 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer28 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28)) = f

-- | A type alias for a constraint tuple with 29 arguments.
type CTuple29 = Decomposer29 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer29 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29)) = f

-- | A type alias for a constraint tuple with 30 arguments.
type CTuple30 = Decomposer30 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer30 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30)) = f

-- | A type alias for a constraint tuple with 31 arguments.
type CTuple31 = Decomposer31 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer31 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31)) = f

-- | A type alias for a constraint tuple with 32 arguments.
type CTuple32 = Decomposer32 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer32 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32)) = f

-- | A type alias for a constraint tuple with 33 arguments.
type CTuple33 = Decomposer33 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer33 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33)) = f

-- | A type alias for a constraint tuple with 34 arguments.
type CTuple34 = Decomposer34 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer34 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34)) = f

-- | A type alias for a constraint tuple with 35 arguments.
type CTuple35 = Decomposer35 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer35 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35)) = f

-- | A type alias for a constraint tuple with 36 arguments.
type CTuple36 = Decomposer36 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer36 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36)) = f

-- | A type alias for a constraint tuple with 37 arguments.
type CTuple37 = Decomposer37 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer37 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37)) = f

-- | A type alias for a constraint tuple with 38 arguments.
type CTuple38 = Decomposer38 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer38 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38)) = f

-- | A type alias for a constraint tuple with 39 arguments.
type CTuple39 = Decomposer39 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer39 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39)) = f

-- | A type alias for a constraint tuple with 40 arguments.
type CTuple40 = Decomposer40 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer40 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40)) = f

-- | A type alias for a constraint tuple with 41 arguments.
type CTuple41 = Decomposer41 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer41 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41)) = f

-- | A type alias for a constraint tuple with 42 arguments.
type CTuple42 = Decomposer42 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer42 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42)) = f

-- | A type alias for a constraint tuple with 43 arguments.
type CTuple43 = Decomposer43 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer43 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43)) = f

-- | A type alias for a constraint tuple with 44 arguments.
type CTuple44 = Decomposer44 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer44 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44)) = f

-- | A type alias for a constraint tuple with 45 arguments.
type CTuple45 = Decomposer45 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer45 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45)) = f

-- | A type alias for a constraint tuple with 46 arguments.
type CTuple46 = Decomposer46 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer46 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46)) = f

-- | A type alias for a constraint tuple with 47 arguments.
type CTuple47 = Decomposer47 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer47 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47)) = f

-- | A type alias for a constraint tuple with 48 arguments.
type CTuple48 = Decomposer48 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer48 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48)) = f

-- | A type alias for a constraint tuple with 49 arguments.
type CTuple49 = Decomposer49 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer49 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49)) = f

-- | A type alias for a constraint tuple with 50 arguments.
type CTuple50 = Decomposer50 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer50 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50)) = f

-- | A type alias for a constraint tuple with 51 arguments.
type CTuple51 = Decomposer51 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer51 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51)) = f

-- | A type alias for a constraint tuple with 52 arguments.
type CTuple52 = Decomposer52 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer52 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52)) = f

-- | A type alias for a constraint tuple with 53 arguments.
type CTuple53 = Decomposer53 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer53 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53)) = f

-- | A type alias for a constraint tuple with 54 arguments.
type CTuple54 = Decomposer54 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer54 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54)) = f

-- | A type alias for a constraint tuple with 55 arguments.
type CTuple55 = Decomposer55 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer55 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55)) = f

-- | A type alias for a constraint tuple with 56 arguments.
type CTuple56 = Decomposer56 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer56 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56)) = f

-- | A type alias for a constraint tuple with 57 arguments.
type CTuple57 = Decomposer57 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer57 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57)) = f

-- | A type alias for a constraint tuple with 58 arguments.
type CTuple58 = Decomposer58 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer58 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58)) = f

-- | A type alias for a constraint tuple with 59 arguments.
type CTuple59 = Decomposer59 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer59 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59)) = f

-- | A type alias for a constraint tuple with 60 arguments.
type CTuple60 = Decomposer60 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer60 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60)) = f

-- | A type alias for a constraint tuple with 61 arguments.
type CTuple61 = Decomposer61 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer61 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61)) = f

-- | A type alias for a constraint tuple with 62 arguments.
type CTuple62 = Decomposer62 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer62 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62)) = f

#if __GLASGOW_HASKELL__ >= 902
-- | A type alias for a constraint tuple with 63 arguments.
type CTuple63 = Decomposer63 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer63 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63)) = f
#endif

#if __GLASGOW_HASKELL__ >= 902
-- | A type alias for a constraint tuple with 64 arguments.
type CTuple64 = Decomposer64 ('Proxy :: Proxy (((), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), (), ()) :: Constraint))
type Decomposer64 (x :: Proxy ((f :: Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint -> Constraint) c1 c2 c3 c4 c5 c6 c7 c8 c9 c10 c11 c12 c13 c14 c15 c16 c17 c18 c19 c20 c21 c22 c23 c24 c25 c26 c27 c28 c29 c30 c31 c32 c33 c34 c35 c36 c37 c38 c39 c40 c41 c42 c43 c44 c45 c46 c47 c48 c49 c50 c51 c52 c53 c54 c55 c56 c57 c58 c59 c60 c61 c62 c63 c64)) = f
#endif

