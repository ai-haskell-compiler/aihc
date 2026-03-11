module ByteString.StrictBuilder
  ( Builder,
    builderBytes,
    builderChunksBuilder,
    builderLength,
    builderPtrFiller,
    bytes,
    lazyBytes,
    asciiIntegral,
    asciiChar,
    utf8Char,
    storable,
    word8,
    word16BE,
    word32BE,
    word64BE,
    int8,
    int16BE,
    int32BE,
    int64BE,
  )
where

import qualified ByteString.StrictBuilder.Population as A
import ByteString.StrictBuilder.Prelude
import qualified ByteString.StrictBuilder.UTF8 as E
import qualified Data.ByteString as B
import qualified Data.ByteString.Builder.Internal as G
import qualified Data.ByteString.Internal as C
import qualified Data.ByteString.Lazy as F

-- * Rewrite rules

{-# RULES "builderBytes/bytes" forall a. builderBytes (bytes a) = a #-}

-- * Builder

data Builder
  = Builder !Int !A.Population

instance Semigroup Builder where
  (<>) (Builder leftSize leftPopulation) (Builder rightSize rightPopulation) =
    Builder (leftSize + rightSize) (leftPopulation <> rightPopulation)
  {-# INLINE sconcat #-}
  sconcat builders =
    Builder size population
    where
      size =
        foldl' (\acc (Builder x _) -> acc + x) 0 builders
      population =
        foldMap (\(Builder _ x) -> x) builders

instance Monoid Builder where
  {-# INLINE mempty #-}
  mempty =
    Builder 0 mempty
  {-# INLINE mconcat #-}
  mconcat builders =
    Builder size population
    where
      size =
        foldl' (\acc (Builder x _) -> acc + x) 0 builders
      population =
        foldMap (\(Builder _ x) -> x) builders

instance IsString Builder where
  fromString =
    bytes . fromString

instance Show Builder where
  show =
    show . builderBytes

-------------------------

-- |
-- Efficiently constructs a strict bytestring.
{-# NOINLINE builderBytes #-}
builderBytes :: Builder -> ByteString
builderBytes (Builder size population) =
  C.unsafeCreate size $ \ptr -> A.populationPtrUpdate population ptr $> ()

-- |
-- Converts into the standard lazy bytestring builder.
-- Does so efficiently using the internal APIs of \"bytestring\",
-- without producing any intermediate representation.
{-# INLINE builderChunksBuilder #-}
builderChunksBuilder :: Builder -> G.Builder
builderChunksBuilder (Builder size population) =
  G.ensureFree size <> A.populationChunksBuilder population

-- |
-- /O(1)/. Gets the size of the bytestring that is to be produced.
{-# INLINE builderLength #-}
builderLength :: Builder -> Int
builderLength (Builder size population) =
  size

-- |
-- Use the builder to populate a buffer.
-- It is your responsibility to ensure that the bounds are not exceeded.
{-# INLINE builderPtrFiller #-}
builderPtrFiller ::
  Builder ->
  -- | A continuation on the amount of bytes to be written and the action populating the pointer.
  (Int -> (Ptr Word8 -> IO ()) -> result) ->
  result
builderPtrFiller (Builder size (A.Population ptrUpdate)) cont =
  cont size (void . ptrUpdate)

-- * Primitives

-------------------------

{-# NOINLINE bytes #-}
bytes :: ByteString -> Builder
bytes bytes =
  Builder (B.length bytes) (A.bytes bytes)

{-# INLINE lazyBytes #-}
lazyBytes :: F.ByteString -> Builder
lazyBytes =
  F.foldlChunks (\builder -> mappend builder . bytes) mempty

{-# INLINE byte #-}
byte :: Word8 -> Builder
byte =
  word8

-- * Extras

-------------------------

{-# INLINEABLE asciiIntegral #-}
asciiIntegral :: (Integral a) => a -> Builder
asciiIntegral =
  \case
    0 ->
      byte 48
    x ->
      bool ((<>) (byte 45)) id (x >= 0)
        $ loop mempty
        $ abs x
  where
    loop builder remainder =
      case remainder of
        0 ->
          builder
        _ ->
          case quotRem remainder 10 of
            (quot, rem) ->
              loop (byte (48 + fromIntegral rem) <> builder) quot

{-# INLINE asciiChar #-}
asciiChar :: Char -> Builder
asciiChar =
  byte . fromIntegral . ord

{-# INLINE CONLIKE storable #-}
storable :: (Storable a) => a -> Builder
storable value =
  Builder size (A.storable size value)
  where
    size =
      sizeOf value

{-# INLINE word8 #-}
word8 :: Word8 -> Builder
word8 =
  Builder 1 . A.word8

{-# INLINE word16BE #-}
word16BE :: Word16 -> Builder
word16BE =
  Builder 2 . A.word16BE

{-# INLINE word32BE #-}
word32BE :: Word32 -> Builder
word32BE =
  Builder 4 . A.word32BE

{-# INLINE word64BE #-}
word64BE :: Word64 -> Builder
word64BE =
  Builder 8 . A.word64BE

{-# INLINE int8 #-}
int8 :: Int8 -> Builder
int8 =
  Builder 1 . A.int8

{-# INLINE int16BE #-}
int16BE :: Int16 -> Builder
int16BE =
  Builder 2 . A.int16BE

{-# INLINE int32BE #-}
int32BE :: Int32 -> Builder
int32BE =
  Builder 4 . A.int32BE

{-# INLINE int64BE #-}
int64BE :: Int64 -> Builder
int64BE =
  Builder 8 . A.int64BE

{-# INLINE utf8Char #-}
utf8Char :: Char -> Builder
utf8Char x =
  E.char x bytes_1 bytes_2 bytes_3 bytes_4

{-# INLINE bytes_1 #-}
bytes_1 :: Word8 -> Builder
bytes_1 b1 =
  Builder 1 (A.bytes_1 b1)

{-# INLINE bytes_2 #-}
bytes_2 :: Word8 -> Word8 -> Builder
bytes_2 b1 b2 =
  Builder 2 (A.bytes_2 b1 b2)

{-# INLINE bytes_3 #-}
bytes_3 :: Word8 -> Word8 -> Word8 -> Builder
bytes_3 b1 b2 b3 =
  Builder 3 (A.bytes_3 b1 b2 b3)

{-# INLINE bytes_4 #-}
bytes_4 :: Word8 -> Word8 -> Word8 -> Word8 -> Builder
bytes_4 b1 b2 b3 b4 =
  Builder 4 (A.bytes_4 b1 b2 b3 b4)
