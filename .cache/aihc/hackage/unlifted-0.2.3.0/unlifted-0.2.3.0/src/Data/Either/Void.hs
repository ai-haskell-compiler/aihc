{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedNewtypes #-}
{-# LANGUAGE ViewPatterns #-}

module Data.Either.Void
  ( EitherVoid# (..)
  , pattern LeftVoid#
  , pattern RightVoid#
  ) where

import GHC.Exts

{- | Unboxed variant of @Either@. The thing possibly contained by @Just@
has a void runtime representation. Rather than using a sum, like the
more general @Either#@ does, this represents @Left@ with 0 and
@Right@ with 1.

It is recommended that the data constructor not be used directly.
Prefer the two pattern synonyms.
-}
newtype EitherVoid# :: TYPE ('TupleRep '[]) -> TYPE ('TupleRep '[]) -> TYPE 'WordRep where
  EitherVoid# :: forall (a :: TYPE ('TupleRep '[])) (b :: TYPE ('TupleRep '[])). Word# -> EitherVoid# a b

{-# COMPLETE RightVoid#, LeftVoid# #-}

pattern RightVoid# :: b -> EitherVoid# a b
pattern RightVoid# a <- (helperRight -> (# 1##, a #))
  where
    RightVoid# _ = EitherVoid# 1##

helperRight ::
  forall (a :: TYPE ('TupleRep '[])) (b :: TYPE ('TupleRep '[])).
  EitherVoid# a b ->
  (# Word#, b #)
{-# INLINE helperRight #-}
helperRight (EitherVoid# x) =
  (# x, (unsafeCoerce# :: (# #) -> b) (# #) #)

pattern LeftVoid# :: a -> EitherVoid# a b
pattern LeftVoid# a <- (helperLeft -> (# 0##, a #))
  where
    LeftVoid# _ = EitherVoid# 0##

helperLeft ::
  forall (a :: TYPE ('TupleRep '[])) (b :: TYPE ('TupleRep '[])).
  EitherVoid# a b ->
  (# Word#, a #)
{-# INLINE helperLeft #-}
helperLeft (EitherVoid# x) =
  (# x, (unsafeCoerce# :: (# #) -> a) (# #) #)
