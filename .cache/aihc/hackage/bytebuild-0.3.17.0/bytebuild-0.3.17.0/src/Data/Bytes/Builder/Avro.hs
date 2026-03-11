{-# LANGUAGE BangPatterns #-}

{- | Builders for encoding data with Apache Avro. Most functions in this
module are just aliases for other functions. Avro uses zig-zag LEB128
for all integral types.
-}
module Data.Bytes.Builder.Avro
  ( int
  , int32
  , int64
  , word16
  , word32
  , word128
  , bytes
  , chunks
  , text

    -- * Maps
  , map2
  ) where

import Data.Bytes (Bytes)
import Data.Bytes.Builder (Builder)
import Data.Bytes.Chunks (Chunks)
import Data.Int
import Data.Text (Text)
import Data.WideWord (Word128)
import Data.Word

import qualified Data.Bytes as Bytes
import qualified Data.Bytes.Builder as B
import qualified Data.Bytes.Chunks as Chunks
import qualified Data.Bytes.Text.Utf8 as Utf8

int32 :: Int32 -> Builder
int32 = B.int32LEB128

int64 :: Int64 -> Builder
int64 = B.int64LEB128

int :: Int -> Builder
int = B.intLEB128

{- | Note: This results in a zigzag encoded number. Avro does not have
unsigned types.
-}
word16 :: Word16 -> Builder
word16 = B.int32LEB128 . fromIntegral

{- | Note: This results in a zigzag encoded number. Avro does not have
unsigned types.
-}
word32 :: Word32 -> Builder
word32 = B.int64LEB128 . fromIntegral

{- | Note: This results in a @fixed@ encoded value of length 16. In the
schema, the type must be @{"type": "fixed", "name": "...", "size": 16}@.
A big-endian encoding is used.
-}
word128 :: Word128 -> Builder
word128 = B.word128BE

bytes :: Bytes -> Builder
bytes !b = int (Bytes.length b) <> B.bytes b

chunks :: Chunks -> Builder
chunks !b = int (Chunks.length b) <> B.chunks b

text :: Text -> Builder
text = bytes . Utf8.fromText

{- | Encode a map with exactly two key-value pairs. The keys are text.
This is commonly used to encode the header in an avro file, which has
a map with two keys: @avro.schema@ and @avro.codec@.
-}
map2 ::
  -- | First key
  Text ->
  -- | First value (already encoded)
  Builder ->
  -- | Second key
  Text ->
  -- | Second value (already encoded)
  Builder ->
  Builder
{-# INLINE map2 #-}
map2 k1 v1 k2 v2 = B.word8 0x04 <> text k1 <> v1 <> text k2 <> v2 <> B.word8 0x00
