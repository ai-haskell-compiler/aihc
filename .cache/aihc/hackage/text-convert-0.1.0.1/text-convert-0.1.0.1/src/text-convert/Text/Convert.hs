{- | Convert between various textual representations.

This module exports seven one-method type classes. The methods are
designed to be input-type polymorphic and output-type monomorphic, to aid with
readability and type inference. The classes are:
- `ToString`, with method `asString`;
- `ToByteString`, with method `asByteString`;
- `ToLazyByteString`, with method `asLazyByteString`;
- `ToByteStringBuilder`, with method `asByteStringBuilder`;
- `ToText`, with method `asText`;
- `ToLazyText`, with method `asLazyText`; and
- `ToTextBuilder`, with method `asTextBuilder`.

Design goals in order of importance:
1. correctness (including totality);
2. efficiency; and
3. ease of use.
The author hopes that the design achieves these goals to a high degree.

To comment on correctness, we are using lenient UTF-8 decoding when converting
between `ByteString`s and `Text`s, which means the conversions are correct in
the sense that they are total, but they are incorrect in the sense that they may
replace some unicode characters with the usual replacement character. Since this
library serves a very general purpose, users will not necessarily place any
restrictions on the inputs of these conversion functions, so the author thought
that totality was important. Notably, this choice agrees with the behavior of
many text editors and readers that use similar decoding strategies.

To comment on efficiency, we avoid unnecessary intermediate conversions where
possible. For example, instead of converting `Text` to `ByteString` through
`String` (a design admirable in that it is simple, correct, and a quantity of
code that is linear in number of supported datatypes), we choose to use a
bespoke conversion function for every pair of supported datatypes wherever
possible (resulting in a quadratic quantity of code). We also avoid unnecessary
allocation. For example, instead of converting `LazyByteString` to `Text`
through `LazyText` (an approach that would entail creating a new rope of `Text`
that would immediately be converted into a single array), we convert
`LazyByteString` to `Text` through `ByteString` (which is a single array). We
don't have any benchmarks to support this design choice, admittedly, as we
consider the cost of creating a benchmark suite overrides the benefits of having
one at this time. Should interest in this library grow, we may revisit this
decision.
-}
module Text.Convert (
  ToString(..),
  ToByteString(..),
  ToLazyByteString(..),
  ToByteStringBuilder(..),
  ToText(..),
  ToLazyText(..),
  ToTextBuilder(..),
  String,
  ByteString,
  LazyByteString,
  Text,
  LazyText,
) where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.ByteString.Builder qualified as BB
import Data.ByteString.Char8 qualified as BC
import Data.ByteString.Lazy.Char8 qualified as BLC
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error qualified as TEE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TLB
import Data.Text.Lazy.Encoding qualified as TLE

type LazyByteString = BLC.ByteString
type LazyText = TL.Text

class ToString a where
  asString :: a -> String

class ToByteString a where
  asByteString :: a -> BC.ByteString

class ToLazyByteString a where
  asLazyByteString :: a -> BLC.ByteString

class ToByteStringBuilder a where
  asByteStringBuilder :: a -> BB.Builder

class ToText a where
  asText :: a -> T.Text

class ToLazyText a where
  asLazyText :: a -> TL.Text

class ToTextBuilder a where
  asTextBuilder :: a -> TLB.Builder

-- ToString instances

instance ToString String where
  asString = id
  {-# INLINE asString #-}

instance ToString BC.ByteString where
  asString = BC.unpack
  {-# INLINE asString #-}

instance ToString BLC.ByteString where
  asString = BLC.unpack
  {-# INLINE asString #-}

instance ToString BB.Builder where
  asString = BLC.unpack . BB.toLazyByteString
  {-# INLINE asString #-}

instance ToString T.Text where
  asString = T.unpack
  {-# INLINE asString #-}

instance ToString TL.Text where
  asString = TL.unpack
  {-# INLINE asString #-}

instance ToString TLB.Builder where
  asString = TL.unpack . TLB.toLazyText
  {-# INLINE asString #-}

-- ToByteString instances

instance ToByteString String where
  asByteString = BC.pack
  {-# INLINE asByteString #-}

instance ToByteString BC.ByteString where
  asByteString = id
  {-# INLINE asByteString #-}

instance ToByteString BLC.ByteString where
  asByteString = BLC.toStrict
  {-# INLINE asByteString #-}

instance ToByteString BB.Builder where
  asByteString = BLC.toStrict . BB.toLazyByteString
  {-# INLINE asByteString #-}

instance ToByteString T.Text where
  asByteString = TE.encodeUtf8
  {-# INLINE asByteString #-}

instance ToByteString TL.Text where
  asByteString = TE.encodeUtf8 . TL.toStrict
  {-# INLINE asByteString #-}

instance ToByteString TLB.Builder where
  asByteString = TE.encodeUtf8 . TL.toStrict . TLB.toLazyText
  {-# INLINE asByteString #-}

-- ToLazyByteString instances

instance ToLazyByteString String where
  asLazyByteString = BLC.pack
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString BC.ByteString where
  asLazyByteString = BLC.fromStrict
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString BLC.ByteString where
  asLazyByteString = id
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString BB.Builder where
  asLazyByteString = BB.toLazyByteString
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString T.Text where
  asLazyByteString = BLC.fromStrict . TE.encodeUtf8
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString TL.Text where
  asLazyByteString = TLE.encodeUtf8
  {-# INLINE asLazyByteString #-}

instance ToLazyByteString TLB.Builder where
  asLazyByteString = TLE.encodeUtf8 . TLB.toLazyText
  {-# INLINE asLazyByteString #-}

-- ToByteStringBuilder instances

instance ToByteStringBuilder String where
  asByteStringBuilder = BB.stringUtf8
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder BC.ByteString where
  asByteStringBuilder = BB.byteString
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder BLC.ByteString where
  asByteStringBuilder = BB.lazyByteString
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder BB.Builder where
  asByteStringBuilder = id
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder T.Text where
  asByteStringBuilder = BB.byteString . TE.encodeUtf8
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder TL.Text where
  asByteStringBuilder = BB.lazyByteString . TLE.encodeUtf8
  {-# INLINE asByteStringBuilder #-}

instance ToByteStringBuilder TLB.Builder where
  asByteStringBuilder = BB.lazyByteString . TLE.encodeUtf8 . TLB.toLazyText
  {-# INLINE asByteStringBuilder #-}

-- ToText instances

instance ToText String where
  asText = T.pack
  {-# INLINE asText #-}

instance ToText BC.ByteString where
  asText = TE.decodeUtf8With TEE.lenientDecode
  {-# INLINE asText #-}

instance ToText BLC.ByteString where
  asText = undefined
  {-# INLINE asText #-}

instance ToText BB.Builder where
  asText = undefined
  {-# INLINE asText #-}

instance ToText T.Text where
  asText = id
  {-# INLINE asText #-}

instance ToText TL.Text where
  asText = TL.toStrict
  {-# INLINE asText #-}

instance ToText TLB.Builder where
  asText = TL.toStrict . TLB.toLazyText
  {-# INLINE asText #-}

-- ToLazyText instances

instance ToLazyText String where
  asLazyText = TL.pack
  {-# INLINE asLazyText #-}

instance ToLazyText BC.ByteString where
  asLazyText = TL.fromStrict . TE.decodeUtf8With TEE.lenientDecode
  {-# INLINE asLazyText #-}

instance ToLazyText BLC.ByteString where
  asLazyText = TLE.decodeUtf8With TEE.lenientDecode
  {-# INLINE asLazyText #-}

instance ToLazyText BB.Builder where
  asLazyText = TLE.decodeUtf8With TEE.lenientDecode . BB.toLazyByteString
  {-# INLINE asLazyText #-}

instance ToLazyText T.Text where
  asLazyText = TL.fromStrict
  {-# INLINE asLazyText #-}

instance ToLazyText TL.Text where
  asLazyText = id
  {-# INLINE asLazyText #-}

instance ToLazyText TLB.Builder where
  asLazyText = TLB.toLazyText
  {-# INLINE asLazyText #-}

-- ToTextBuilder instances

instance ToTextBuilder String where
  asTextBuilder = TLB.fromString
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder BC.ByteString where
  asTextBuilder = TLB.fromText . TE.decodeUtf8With TEE.lenientDecode
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder BLC.ByteString where
  asTextBuilder = TLB.fromLazyText . TLE.decodeUtf8With TEE.lenientDecode
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder BB.Builder where
  asTextBuilder = TLB.fromLazyText . TLE.decodeUtf8With TEE.lenientDecode . BB.toLazyByteString
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder T.Text where
  asTextBuilder = TLB.fromText
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder TL.Text where
  asTextBuilder = TLB.fromLazyText
  {-# INLINE asTextBuilder #-}

instance ToTextBuilder TLB.Builder where
  asTextBuilder = id
  {-# INLINE asTextBuilder #-}
