-- | The CBOR primitives that the store artifacts use.
--
-- The artifact encoders write a small subset of CBOR: arrays, text strings,
-- byte strings, unsigned integers, negative integers, and big integers.
-- This module holds one copy of that subset for each artifact module.
module Aihc.Cbor
  ( Cbor (..),
    encodeCbor,
    decodeCbor,
    expectArray,
    cborArray,
    cborText,
    cborBytes,
    cborWord,
    cborInt,
    cborInteger,
    cborMajor,
    getArrayLength,
    getText,
    getBytes,
    getWord,
    getInt,
    getInteger,
    getMajor,
    (<*!>),
  )
where

import Control.Monad (replicateM, unless, (<$!>))
import Data.Binary.Get qualified as Get
import Data.Bits (shiftL, shiftR, (.&.), (.|.))
import Data.ByteString qualified as BS
import Data.ByteString.Builder qualified as Builder
import Data.ByteString.Lazy qualified as BL
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Word (Word64, Word8)

cborArray :: Int -> Builder.Builder
cborArray = cborMajor 4 . fromIntegral

cborText :: Text -> Builder.Builder
cborText value = cborMajor 3 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes
  where
    bytes = TE.encodeUtf8 value

cborBytes :: BS.ByteString -> Builder.Builder
cborBytes bytes = cborMajor 2 (fromIntegral (BS.length bytes)) <> Builder.byteString bytes

-- | An integer of any size. A value outside the 64-bit range of the two
-- integer major types is a big integer: tag 2 or tag 3 on its big-endian
-- magnitude, as RFC 8949 section 3.4.3 gives.
cborInteger :: Integer -> Builder.Builder
cborInteger value
  | value >= 0 && value <= maxWord = cborMajor 0 (fromInteger value)
  | value < 0 && value >= -1 - maxWord = cborMajor 1 (fromInteger (-1 - value))
  | value >= 0 = cborMajor 6 2 <> cborBytes (integerBytes value)
  | otherwise = cborMajor 6 3 <> cborBytes (integerBytes (-1 - value))
  where
    maxWord = toInteger (maxBound :: Word64)

integerBytes :: Integer -> BS.ByteString
integerBytes = BS.pack . reverse . go
  where
    go remaining
      | remaining == 0 = []
      | otherwise = fromInteger (remaining .&. 255) : go (remaining `shiftR` 8)

cborWord :: Word64 -> Builder.Builder
cborWord = cborMajor 0

cborInt :: Int -> Builder.Builder
cborInt value
  | value >= 0 = cborMajor 0 (fromIntegral value)
  | otherwise = cborMajor 1 (fromIntegral (-1 - value))

cborMajor :: Word8 -> Word64 -> Builder.Builder
cborMajor major value
  | value < 24 = Builder.word8 (major * 32 + fromIntegral value)
  | value <= 255 = Builder.word8 (major * 32 + 24) <> Builder.word8 (fromIntegral value)
  | value <= 65535 = Builder.word8 (major * 32 + 25) <> Builder.word16BE (fromIntegral value)
  | value <= 4294967295 = Builder.word8 (major * 32 + 26) <> Builder.word32BE (fromIntegral value)
  | otherwise = Builder.word8 (major * 32 + 27) <> Builder.word64BE value

getArrayLength :: Get.Get Int
getArrayLength = fromIntegral <$!> getMajor 4

getText :: Get.Get Text
getText = do
  length' <- getMajor 3
  -- Decoded here, so the text does not keep the artifact bytes alive.
  bytes <- Get.getByteString (fromIntegral length')
  either (fail . show) pure (TE.decodeUtf8' bytes)

-- | Apply a decoded function to a decoded value and evaluate the result.
-- 'Get' builds every '<$>' and '<*>' result lazily, so a decoder written
-- with them returns a tree of thunks that lives as long as the decoded
-- value does; the artifact decoders use this operator and '<$!>' instead.
(<*!>) :: Get.Get (a -> b) -> Get.Get a -> Get.Get b
getFunction <*!> getArgument = do
  function <- getFunction
  argument <- getArgument
  pure $! function argument

infixl 4 <*!>

getBytes :: Get.Get BS.ByteString
getBytes = do
  length' <- getMajor 2
  -- Copied, so the bytes do not keep the artifact bytes alive.
  BS.copy <$!> Get.getByteString (fromIntegral length')

getInteger :: Get.Get Integer
getInteger = do
  initial <- Get.lookAhead Get.getWord8
  case initial `shiftR` 5 of
    0 -> toInteger <$!> getMajor 0
    1 -> (\magnitude -> -1 - toInteger magnitude) <$!> getMajor 1
    6 -> do
      tag <- getMajor 6
      magnitude <- BS.foldl' (\total byte -> total `shiftL` 8 .|. toInteger byte) 0 <$!> getBytes
      case tag of
        2 -> pure $! magnitude
        3 -> pure $! (-1 - magnitude)
        _ -> fail "unexpected CBOR tag"
    _ -> fail "unexpected CBOR integer"

getWord :: Get.Get Word64
getWord = getMajor 0

getInt :: Get.Get Int
getInt = do
  initial <- Get.lookAhead Get.getWord8
  let major = initial `shiftR` 5
  value <- getMajor major
  case major of
    0 -> pure $! fromIntegral value
    1 -> pure $! (-1 - fromIntegral value)
    _ -> fail "unexpected CBOR integer"

getMajor :: Word8 -> Get.Get Word64
getMajor expected = do
  initial <- Get.getWord8
  let major = initial `shiftR` 5
      info = initial `mod` 32
  unless (major == expected) (fail "unexpected CBOR major type")
  case info of
    value | value < 24 -> pure $! fromIntegral value
    24 -> fromIntegral <$!> Get.getWord8
    25 -> fromIntegral <$!> Get.getWord16be
    26 -> fromIntegral <$!> Get.getWord32be
    27 -> Get.getWord64be
    _ -> fail "unsupported CBOR length"

-- | A value with a CBOR representation.
class Cbor a where
  toCbor :: a -> Builder.Builder
  fromCbor :: Get.Get a

encodeCbor :: (Cbor a) => a -> BL.ByteString
encodeCbor = Builder.toLazyByteString . toCbor

decodeCbor :: (Cbor a) => BL.ByteString -> Either String a
decodeCbor bytes =
  case Get.runGetOrFail fromCbor bytes of
    Left (_, _, message) -> Left message
    Right (remaining, _, value)
      | BL.null remaining -> Right value
      | otherwise -> Left "invalid trailing data"

expectArray :: Int -> Get.Get ()
expectArray expected = do
  actual <- getArrayLength
  unless (actual == expected) (fail "unexpected CBOR array length")

instance Cbor Text where
  toCbor = cborText
  fromCbor = getText

instance Cbor Int where
  toCbor = cborInt
  fromCbor = getInt

instance Cbor Integer where
  toCbor = cborInteger
  fromCbor = getInteger

instance Cbor Bool where
  toCbor value = Builder.word8 (if value then 245 else 244)
  fromCbor = do
    value <- Get.getWord8
    case value of
      244 -> pure False
      245 -> pure True
      _ -> fail "unexpected CBOR boolean"

instance {-# OVERLAPPING #-} Cbor String where
  toCbor = cborText . T.pack
  fromCbor = T.unpack <$!> getText

instance {-# OVERLAPPABLE #-} (Cbor a) => Cbor [a] where
  toCbor values = cborArray (length values) <> foldMap toCbor values
  fromCbor = getArrayLength >>= (`replicateM` fromCbor)

instance (Cbor a, Cbor b) => Cbor (a, b) where
  toCbor (left, right) = cborArray 2 <> toCbor left <> toCbor right
  fromCbor = expectArray 2 >> ((,) <$!> fromCbor <*!> fromCbor)

instance (Cbor a) => Cbor (Maybe a) where
  toCbor Nothing = cborArray 0
  toCbor (Just value) = cborArray 1 <> toCbor value
  fromCbor = do
    count <- getArrayLength
    case count of
      0 -> pure Nothing
      1 -> Just <$!> fromCbor
      _ -> fail "unexpected CBOR optional value"

instance (Ord key, Cbor key, Cbor value) => Cbor (Map key value) where
  toCbor = toCbor . Map.toAscList
  fromCbor = Map.fromList <$!> fromCbor
