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

module Data.Maybe.Void
  ( MaybeVoid# (..)
  , pattern JustVoid#
  , pattern NothingVoid#
  ) where

import GHC.Exts

{- | Unboxed variant of @Maybe@. The thing possibly contained by @Just@
has a void runtime representation. Rather than using a sum, like the
more general @Maybe#@ does, this represents @Nothing@ with 0 and
@Just@ with 1.

It is recommended that the data constructor not be used directly.
Prefer the two pattern synonyms.
-}
newtype MaybeVoid# :: TYPE ('TupleRep '[]) -> TYPE 'WordRep where
  MaybeVoid# :: forall (a :: TYPE ('TupleRep '[])). Word# -> MaybeVoid# a

pattern JustVoid# :: a -> MaybeVoid# a
pattern JustVoid# a <- (helper -> (# 1##, a #))
  where
    JustVoid# _ = MaybeVoid# 1##

helper ::
  forall (a :: TYPE ('TupleRep '[])).
  MaybeVoid# a ->
  (# Word#, a #)
{-# INLINE helper #-}
helper (MaybeVoid# x) =
  (# x, (unsafeCoerce# :: (# #) -> a) (# #) #)

pattern NothingVoid# :: MaybeVoid# a
pattern NothingVoid# = MaybeVoid# 0##
