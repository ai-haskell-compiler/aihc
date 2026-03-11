{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Data.Bytes.Builder.Class
  ( ToBuilder (..)
  ) where

import Data.ByteString.Short (ShortByteString)
import Data.Bytes (Bytes)
import Data.Bytes.Builder (Builder)
import Data.Int
import Data.Primitive.ByteArray (ByteArray)
import Data.Text.Short (ShortText)
import Data.Word
import Foreign.C.Types (CInt)

import qualified Data.Bytes.Builder as Builder

{- | Types that can be encoded as a builder. Human-readable encodings
are used when possible. For example, numbers are encoded an ascii-encoded
decimal characters. UTF-8 is preferred for textual types. For types
that represent arbitrary bytes (e.g. Bytes, ByteString), the bytes
are preserved.

The goal of this typeclass is to reduce the size of builders produced
by quasiquotation.
-}
class ToBuilder a where
  toBuilder :: a -> Builder

-- | Identity
instance ToBuilder Builder where
  toBuilder = id

-- | Uses @bytes@.
instance ToBuilder Bytes where
  toBuilder = Builder.bytes

-- | Uses @byteArray@
instance ToBuilder ByteArray where
  toBuilder = Builder.byteArray

-- | Uses @shortByteString@
instance ToBuilder ShortByteString where
  toBuilder = Builder.shortByteString

-- | Uses @shortTextUtf8@.
instance ToBuilder ShortText where
  toBuilder = Builder.shortTextUtf8

-- | Uses @stringUtf8@
instance ToBuilder String where
  toBuilder = Builder.stringUtf8

-- | Uses @int64Dec@.
instance ToBuilder Int64 where
  toBuilder = Builder.int64Dec

-- | Uses @int32Dec@.
instance ToBuilder Int32 where
  toBuilder = Builder.int32Dec

-- | Uses @int16Dec@.
instance ToBuilder Int16 where
  toBuilder = Builder.int16Dec

-- | Uses @int8Dec@.
instance ToBuilder Int8 where
  toBuilder = Builder.int8Dec

-- | Uses @intDec@.
instance ToBuilder Int where
  toBuilder = Builder.intDec

-- | Uses @word64Dec@.
instance ToBuilder Word64 where
  toBuilder = Builder.word64Dec

-- | Uses @word32Dec@.
instance ToBuilder Word32 where
  toBuilder = Builder.word32Dec

-- | Uses @word16Dec@.
instance ToBuilder Word16 where
  toBuilder = Builder.word16Dec

-- | Uses @word8Dec@.
instance ToBuilder Word8 where
  toBuilder = Builder.word8Dec

-- | Uses @wordDec@.
instance ToBuilder Word where
  toBuilder = Builder.wordDec

-- | uses @doubleDec@
instance ToBuilder Double where
  toBuilder = Builder.doubleDec

instance ToBuilder CInt where
  toBuilder = Builder.int64Dec . fromIntegral @CInt @Int64
