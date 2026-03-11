module Data.Base32String ( Base32String
                         , b32String
                         , b32String'
                         , fromBinary
                         , toBinary
                         , fromBytes
                         , toBytes
                         , toText
                         , fromText ) where

import           Control.Applicative   (pure, (<$>))
import           Control.Monad         (liftM)

import           Data.Bits             (shiftL, shiftR, (.|.))
import           Data.Char             (chr, ord)
import           Data.List             (unfoldr)

import           Data.Maybe            (fromJust, fromMaybe, isJust,
                                        listToMaybe)

import           Data.String           (fromString)
import           Data.Word             (Word8)
import           Numeric               (readInt, showIntAtBase)

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy  as BSL

import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE

import qualified Data.Binary           as B (Binary, decode, encode)

-- | Represents a Base32 string. Guarantees that all characters it contains
--   are valid base32 characters.
data Base32String =
  Base32String BS.ByteString
  deriving ( Show, Eq, Ord )

-- | Smart constructor which validates that all the text are actually
--   base-32 characters.
b32String :: BS.ByteString -- ^ Our Base32 mapping table
          -> BS.ByteString -- ^ Our Base32 string
          -> Base32String
b32String table bs =
  if   BS.all (isValidBase32 table) bs
  then Base32String bs
  else error ("Not a valid base32 string: " ++ show bs)

-- | Case insensitive variant of 'b32String', which converts all characters
--   to upper case.
b32String' :: BS.ByteString -- ^ Our Base32 mapping table
           -> BS.ByteString -- ^ Our Base32 string
           -> Base32String
b32String' table bs =
  b32String table (TE.encodeUtf8 . T.toUpper . TE.decodeUtf8 $ bs)

-- | Converts a 'B.Binary' to a 'Base32String' value
fromBinary :: B.Binary a
           => BS.ByteString -- ^ Our Base32 mapping table
           -> a             -- ^ Input object that is convertable to binary
           -> Base32String  -- ^ Base32 representation of binary data
fromBinary table = b32String table . b32Encode table . BSL.toStrict . B.encode

-- | Converts a 'Base32String' to a 'B.Binary' value
toBinary :: B.Binary a
         => BS.ByteString -- ^ Base32 mapping table
         -> Base32String  -- ^ Base32 representation
         -> a             -- ^ Converted object
toBinary table (Base32String bs) = B.decode . BSL.fromStrict . fromMaybe (error "not a valid base32 input") $ b32Decode table bs

-- | Reads a 'BS.ByteString' as raw bytes and converts to base32 representation. We
--   cannot use the instance Binary of 'BS.ByteString' because it provides
--   a leading length, which is not what we want when dealing with raw bytes.
fromBytes :: BS.ByteString -- ^ Our Base32 mapping table
          -> BS.ByteString -- ^ Raw binary bytes
          -> Base32String  -- ^ Base32 representation of raw binary bytes
fromBytes table = b32String table . b32Encode table

-- | Access to the raw bytes in a 'BS.ByteString' format.
toBytes :: BS.ByteString -- ^ Base32 mapping table
        -> Base32String  -- ^ Base32 string we wish to get binary data from
        -> BS.ByteString -- ^ Raw binary representation
toBytes table (Base32String bs) = fromMaybe (error "not a valid base32 input") $ b32Decode table bs

-- | Access to a 'T.Text' representation of the 'Base32String'
toText :: Base32String -> T.Text
toText (Base32String bs) = TE.decodeUtf8 bs

-- | Converts a 'T.Text' representation to a 'Base32String'
fromText :: BS.ByteString -- ^ Base32 mapping table
         -> T.Text        -- ^ Text representation
         -> Base32String  -- ^ Base32 classified representation
fromText table = b32String table . TE.encodeUtf8

isValidBase32 :: BS.ByteString -> Word8 -> Bool
isValidBase32 table c =
  c `BS.elem` table

b32 :: BS.ByteString -> Word8 -> Word8
b32 table i = BS.index table (fromIntegral i)

b32' :: BS.ByteString -> Word8 -> Maybe Word8
b32' table w = fromIntegral <$> BS.elemIndex w table

b32EncodeInt :: BS.ByteString -- ^ Base32 mapping table
             -> Integer
             -> BS.ByteString
b32EncodeInt table i =
    fromString $ showIntAtBase (32 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . b32 table . fromIntegral

b32DecodeInt :: BS.ByteString -- ^ Base32 mapping table
             -> BS.ByteString
             -> Maybe Integer
b32DecodeInt table s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = b32' table . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 32 p f (BS8.unpack s)

b32Encode :: BS.ByteString -- ^ Base32 mapping table
          -> BS.ByteString
          -> BS.ByteString
b32Encode table input = BS.append l r
  where
    (z,b) = BS.span (== 0) input
    l = BS.map (b32 table) z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = b32EncodeInt table $ bsToInteger b

b32Decode :: BS.ByteString -- ^ Base32 mapping table
          -> BS.ByteString
          -> Maybe BS.ByteString
b32Decode table input = liftM (BS.append prefix) r
  where
    (z,b)  = BS.span (== b32 table 0) input
    prefix = BS.map (fromJust . b32' table) z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> b32DecodeInt table b

-- | Decode a big endian Integer from a bytestring
bsToInteger :: BS.ByteString -> Integer
bsToInteger = foldr f 0 . reverse . BS.unpack
  where
    f w n = toInteger w .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> BS.ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.pack $ reverse $ unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just (fromInteger x :: Word8, x `shiftR` 8)
