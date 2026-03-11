{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ExplicitForAll #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedTuples #-}

module Arithmetic.Lt
  ( -- * Special Inequalities
    zero
  , zero#

    -- * Substitution
  , substituteL
  , substituteR

    -- * Increment
  , incrementL
  , incrementL#
  , incrementR
  , incrementR#

    -- * Decrement
  , decrementL
  , decrementL#
  , decrementR
  , decrementR#

    -- * Weaken
  , weakenL
  , weakenL#
  , weakenR
  , weakenR#
  , weakenLhsL#
  , weakenLhsR#

    -- * Composition
  , plus
  , plus#
  , transitive
  , transitive#
  , transitiveNonstrictL
  , transitiveNonstrictL#
  , transitiveNonstrictR
  , transitiveNonstrictR#

    -- * Multiplication and Division
  , reciprocalA
  , reciprocalB

    -- * Convert to Inequality
  , toLteL
  , toLteR

    -- * Absurdities
  , absurd

    -- * Integration with GHC solver
  , constant
  , constant#

    -- * Lift and Unlift
  , lift
  , unlift
  ) where

import Arithmetic.Unsafe (type (:=:) (Eq), type (<) (Lt), type (<#) (Lt#), type (<=) (Lte), type (<=#))
import GHC.TypeNats (CmpNat, type (+))

import qualified GHC.TypeNats as GHC

toLteR :: (a < b) -> (a + 1 <= b)
{-# INLINE toLteR #-}
toLteR Lt = Lte

toLteL :: (a < b) -> (1 + a <= b)
{-# INLINE toLteL #-}
toLteL Lt = Lte

{- | Replace the left-hand side of a strict inequality
with an equal number.
-}
substituteL :: (b :=: c) -> (b < a) -> (c < a)
{-# INLINE substituteL #-}
substituteL Eq Lt = Lt

{- | Replace the right-hand side of a strict inequality
with an equal number.
-}
substituteR :: (b :=: c) -> (a < b) -> (a < c)
{-# INLINE substituteR #-}
substituteR Eq Lt = Lt

-- | Add a strict inequality to a nonstrict inequality.
plus :: (a < b) -> (c <= d) -> (a + c < b + d)
{-# INLINE plus #-}
plus Lt Lte = Lt

plus# :: (a <# b) -> (c <=# d) -> (a + c <# b + d)
{-# INLINE plus# #-}
plus# _ _ = Lt# (# #)

{- | Add a constant to the left-hand side of both sides of
the strict inequality.
-}
incrementL ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) ->
  (c + a < c + b)
{-# INLINE incrementL #-}
incrementL Lt = Lt

incrementL# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <# b) ->
  (c + a <# c + b)
{-# INLINE incrementL# #-}
incrementL# _ = Lt# (# #)

{- | Add a constant to the right-hand side of both sides of
the strict inequality.
-}
incrementR ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) ->
  (a + c < b + c)
{-# INLINE incrementR #-}
incrementR Lt = Lt

incrementR# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <# b) ->
  (a + c <# b + c)
{-# INLINE incrementR# #-}
incrementR# _ = Lt# (# #)

{- | Subtract a constant from the left-hand side of both sides of
the inequality. This is the opposite of 'incrementL'.
-}
decrementL ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (c + a < c + b) ->
  (a < b)
{-# INLINE decrementL #-}
decrementL Lt = Lt

decrementL# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  ((c + a) <# (c + b)) ->
  (a <# b)
{-# INLINE decrementL# #-}
decrementL# _ = Lt# (# #)

{- | Subtract a constant from the right-hand side of both sides of
the inequality. This is the opposite of 'incrementR'.
-}
decrementR ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a + c < b + c) ->
  (a < b)
{-# INLINE decrementR #-}
decrementR Lt = Lt

decrementR# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a + c <# b + c) ->
  (a <# b)
{-# INLINE decrementR# #-}
decrementR# _ = Lt# (# #)

{- | Add a constant to the left-hand side of the right-hand side of
the strict inequality.
-}
weakenL ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) ->
  (a < c + b)
{-# INLINE weakenL #-}
weakenL Lt = Lt

weakenL# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <# b) ->
  (a <# c + b)
{-# INLINE weakenL# #-}
weakenL# _ = Lt# (# #)

weakenLhsL# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (c + a <# b) ->
  (a <# b)
{-# INLINE weakenLhsL# #-}
weakenLhsL# _ = Lt# (# #)

weakenLhsR# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a + c <# b) ->
  (a <# b)
{-# INLINE weakenLhsR# #-}
weakenLhsR# _ = Lt# (# #)

{- | Add a constant to the right-hand side of the right-hand side of
the strict inequality.
-}
weakenR ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a < b) ->
  (a < b + c)
{-# INLINE weakenR #-}
weakenR Lt = Lt

weakenR# ::
  forall (c :: GHC.Nat) (a :: GHC.Nat) (b :: GHC.Nat).
  (a <# b) ->
  (a <# b + c)
{-# INLINE weakenR# #-}
weakenR# _ = Lt# (# #)

-- | Compose two strict inequalities using transitivity.
transitive :: (a < b) -> (b < c) -> (a < c)
{-# INLINE transitive #-}
transitive Lt Lt = Lt

transitive# :: (a <# b) -> (b <# c) -> (a <# c)
{-# INLINE transitive# #-}
transitive# _ _ = Lt# (# #)

{- | Compose a strict inequality (the first argument) with a nonstrict
inequality (the second argument).
-}
transitiveNonstrictR :: (a < b) -> (b <= c) -> (a < c)
{-# INLINE transitiveNonstrictR #-}
transitiveNonstrictR Lt Lte = Lt

transitiveNonstrictR# :: (a <# b) -> (b <=# c) -> (a <# c)
{-# INLINE transitiveNonstrictR# #-}
transitiveNonstrictR# _ _ = Lt# (# #)

transitiveNonstrictL :: (a <= b) -> (b < c) -> (a < c)
{-# INLINE transitiveNonstrictL #-}
transitiveNonstrictL Lte Lt = Lt

transitiveNonstrictL# :: (a <=# b) -> (b <# c) -> (a <# c)
{-# INLINE transitiveNonstrictL# #-}
transitiveNonstrictL# _ _ = Lt# (# #)

-- | Zero is less than one.
zero :: 0 < 1
{-# INLINE zero #-}
zero = Lt

zero# :: (# #) -> 0 <# 1
{-# INLINE zero# #-}
zero# _ = Lt# (# #)

-- | Nothing is less than zero.
absurd :: n < 0 -> void
{-# INLINE absurd #-}
absurd Lt = errorWithoutStackTrace "Arithmetic.Nat.absurd: n < 0"

{- | Use GHC's built-in type-level arithmetic to prove
that one number is less than another. The type-checker
only reduces 'CmpNat' if both arguments are constants.
-}
constant :: forall a b. (CmpNat a b ~ 'LT) => (a < b)
{-# INLINE constant #-}
constant = Lt

constant# :: forall a b. (CmpNat a b ~ 'LT) => (# #) -> (a <# b)
{-# INLINE constant# #-}
constant# _ = Lt# (# #)

-- | Given that @m < n/p@, we know that @p*m < n@.
reciprocalA ::
  forall (m :: GHC.Nat) (n :: GHC.Nat) (p :: GHC.Nat).
  (m < GHC.Div n p) ->
  (p GHC.* m) < n
{-# INLINE reciprocalA #-}
reciprocalA _ = Lt

-- | Given that @m < roundUp(n/p)@, we know that @p*m < n@.
reciprocalB ::
  forall (m :: GHC.Nat) (n :: GHC.Nat) (p :: GHC.Nat).
  (m < GHC.Div (n GHC.- 1) p + 1) ->
  (p GHC.* m) < n
{-# INLINE reciprocalB #-}
reciprocalB _ = Lt

unlift :: (a < b) -> (a <# b)
unlift _ = Lt# (# #)

lift :: (a <# b) -> (a < b)
lift _ = Lt
