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

module Data.Maybe.Word16
  ( MaybeWord16# (..)
  , pattern JustWord16#
  , pattern NothingWord16#
  ) where

import GHC.Exts

-- The value 65536 is used to mean Nothing. Values above this
-- should not be used.
newtype MaybeWord16# :: TYPE 'WordRep where
  MaybeWord16# :: Word# -> MaybeWord16#

pattern JustWord16# :: Word16# -> MaybeWord16#
pattern JustWord16# a <- (helper -> (# 0#, a #))
  where
    JustWord16# w = MaybeWord16# (word16ToWord# w)

helper :: MaybeWord16# -> (# Int#, Word16# #)
{-# INLINE helper #-}
helper (MaybeWord16# x) = (# eqWord# x 65536##, wordToWord16# x #)

pattern NothingWord16# :: MaybeWord16#
pattern NothingWord16# = MaybeWord16# 65536##
