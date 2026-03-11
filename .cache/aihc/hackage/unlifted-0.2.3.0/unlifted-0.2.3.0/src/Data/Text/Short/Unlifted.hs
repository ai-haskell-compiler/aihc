{-# LANGUAGE GADTSyntax #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnliftedNewtypes #-}

module Data.Text.Short.Unlifted
  ( ShortText# (..)
  , pattern Empty#
  , lift
  , unlift
  ) where

import GHC.Exts (Int#)
import Data.ByteString.Short.Internal as TS
import Data.Text.Short (ShortText)
import Data.Unlifted (ShortText# (ShortText#))
import GHC.Exts ((==#))

import qualified Data.Text.Short as TS
import qualified Data.Text.Short.Unsafe as TS
import qualified GHC.Exts as Exts

pattern Empty# :: ShortText#
pattern Empty# <- (null# -> 1#)
  where
  Empty# = empty# (# #)

empty# :: (# #) -> ShortText#
empty# _ =
  ShortText# (Exts.runRW# (\s0 -> case Exts.newByteArray# 0# s0 of { (# s1, b #) -> case Exts.unsafeFreezeByteArray# b s1 of { (# _, y #) -> y}}))

null# :: ShortText# -> Int#
null# (ShortText# x) = Exts.sizeofByteArray# x ==# 0#

lift :: ShortText# -> ShortText
lift (ShortText# x) = TS.fromShortByteStringUnsafe (SBS x)

unlift :: ShortText -> ShortText#
unlift t = case TS.toShortByteString t of
  SBS x -> ShortText# x
