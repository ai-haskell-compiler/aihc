{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Data.Bytes.Builder.Bounded.Class
  ( ToBoundedBuilder (..)
  ) where

import Data.Int
import Data.Word

import qualified Data.Bytes.Builder.Bounded as Bounded
import qualified GHC.TypeNats as GHC

{- | Variant of To that can be encoded as a builder. Human-readable encodings
are used when possible. For example, numbers are encoded an ascii-encoded
decimal characters. UTF-8 is preferred for textual types. For types
that represent arbitrary bytes (e.g. Bytes, ByteString), the bytes
are preserved.

The goal of this typeclass is to reduce the size of builders produced
by quasiquotation.
-}
class ToBoundedBuilder a where
  type BoundedBuilderLength a :: GHC.Nat
  toBuilder :: a -> Bounded.Builder (BoundedBuilderLength a)

-- | Identity
instance ToBoundedBuilder (Bounded.Builder n) where
  type BoundedBuilderLength (Bounded.Builder n) = n
  toBuilder = id

-- | Uses @int64Dec@.
instance ToBoundedBuilder Int64 where
  type BoundedBuilderLength Int64 = 20
  toBuilder = Bounded.int64Dec

-- | Uses @int32Dec@.
instance ToBoundedBuilder Int32 where
  type BoundedBuilderLength Int32 = 11
  toBuilder = Bounded.int32Dec

-- | Uses @int16Dec@.
instance ToBoundedBuilder Int16 where
  type BoundedBuilderLength Int16 = 6
  toBuilder = Bounded.int16Dec

-- | Uses @int8Dec@.
instance ToBoundedBuilder Int8 where
  type BoundedBuilderLength Int8 = 4
  toBuilder = Bounded.int8Dec

-- | Uses @intDec@.
instance ToBoundedBuilder Int where
  type BoundedBuilderLength Int = 20
  toBuilder = Bounded.intDec

-- | Uses @word64Dec@.
instance ToBoundedBuilder Word64 where
  type BoundedBuilderLength Word64 = 19
  toBuilder = Bounded.word64Dec

-- | Uses @word32Dec@.
instance ToBoundedBuilder Word32 where
  type BoundedBuilderLength Word32 = 10
  toBuilder = Bounded.word32Dec

-- | Uses @word16Dec@.
instance ToBoundedBuilder Word16 where
  type BoundedBuilderLength Word16 = 5
  toBuilder = Bounded.word16Dec

-- | Uses @word8Dec@.
instance ToBoundedBuilder Word8 where
  type BoundedBuilderLength Word8 = 3
  toBuilder = Bounded.word8Dec

-- | Uses @wordDec@.
instance ToBoundedBuilder Word where
  type BoundedBuilderLength Word = 19
  toBuilder = Bounded.wordDec
