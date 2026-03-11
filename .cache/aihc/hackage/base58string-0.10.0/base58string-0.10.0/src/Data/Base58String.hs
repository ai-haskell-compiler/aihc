module Data.Base58String ( Base58String
                         , b58String
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

-- | Represents a Base58 string. Guarantees that all characters it contains
--   are valid base58 characters.
data Base58String =
  Base58String BS.ByteString
  deriving ( Show, Eq, Ord )

-- | Smart constructor which validates that all the text are actually
--   base-58 characters.
b58String :: BS.ByteString -- ^ Our Base58 mapping table
          -> BS.ByteString -- ^ Our Base58 string
          -> Base58String
b58String table bs =
  if   BS.all (isValidBase58 table) bs
  then Base58String bs
  else error ("Not a valid base58 string: " ++ show bs)

-- | Converts a 'B.Binary' to a 'Base58String' value
fromBinary :: B.Binary a
           => BS.ByteString -- ^ Our Base58 mapping table
           -> a             -- ^ Input object that is convertable to binary
           -> Base58String  -- ^ Base58 representation of binary data
fromBinary table = (b58String table) . (b58Encode table) . BSL.toStrict . B.encode

-- | Converts a 'Base58String' to a 'B.Binary' value
toBinary :: B.Binary a
         => BS.ByteString -- ^ Base58 mapping table
         -> Base58String  -- ^ Base58 representation
         -> a             -- ^ Converted object
toBinary table (Base58String bs) = B.decode . BSL.fromStrict . fromMaybe (error "not a valid base58 input") $ b58Decode table bs

-- | Reads a 'BS.ByteString' as raw bytes and converts to base58 representation. We
--   cannot use the instance Binary of 'BS.ByteString' because it provides
--   a leading length, which is not what we want when dealing with raw bytes.
fromBytes :: BS.ByteString -- ^ Our Base58 mapping table
          -> BS.ByteString -- ^ Raw binary bytes
          -> Base58String  -- ^ Base58 representation of raw binary bytes
fromBytes table = (b58String table) . (b58Encode table)

-- | Access to the raw bytes in a 'BS.ByteString' format.
toBytes :: BS.ByteString -- ^ Base58 mapping table
        -> Base58String  -- ^ Base58 string we wish to get binary data from
        -> BS.ByteString -- ^ Raw binary representation
toBytes table (Base58String bs) = fromMaybe (error "not a valid base58 input") $ b58Decode table bs

-- | Access to a 'T.Text' representation of the 'Base58String'
toText :: Base58String -> T.Text
toText (Base58String bs) = TE.decodeUtf8 bs

-- | Converts a 'T.Text' representation to a 'Base58String'
fromText :: BS.ByteString -- ^ Base58 mapping table
         -> T.Text        -- ^ Text representation
         -> Base58String  -- ^ Base58 classified representation
fromText table = (b58String table) . TE.encodeUtf8

isValidBase58 :: BS.ByteString -> Word8 -> Bool
isValidBase58 table c =
  BS.elem c table

b58 :: BS.ByteString -> Word8 -> Word8
b58 table i = BS.index table (fromIntegral i)

b58' :: BS.ByteString -> Word8 -> Maybe Word8
b58' table w = fromIntegral <$> BS.elemIndex w table

b58EncodeInt :: BS.ByteString -- ^ Base58 mapping table
             -> Integer
             -> BS.ByteString
b58EncodeInt table i =
    fromString $ showIntAtBase (58 :: Integer) f (fromIntegral i) ""
  where
    f = chr . fromIntegral . (b58 table) . fromIntegral

b58DecodeInt :: BS.ByteString -- ^ Base58 mapping table
             -> BS.ByteString
             -> Maybe Integer
b58DecodeInt table s = case go of
    Just (r,[]) -> Just r
    _           -> Nothing
  where
    c = (b58' table) . fromIntegral . ord
    p = isJust . c
    f = fromIntegral . fromJust . c
    go = listToMaybe $ readInt 58 p f (BS8.unpack s)

b58Encode :: BS.ByteString -- ^ Base58 mapping table
          -> BS.ByteString
          -> BS.ByteString
b58Encode table input = BS.append l r
  where
    (z,b) = BS.span (== 0) input
    l = BS.map (b58 table) z -- preserve leading 0's
    r | BS.null b = BS.empty
      | otherwise = b58EncodeInt table $ bsToInteger b

b58Decode :: BS.ByteString -- ^ Base58 mapping table
          -> BS.ByteString
          -> Maybe BS.ByteString
b58Decode table input = liftM (BS.append prefix) r
  where
    (z,b)  = BS.span (== (b58 table) 0) input
    prefix = BS.map (fromJust . (b58' table)) z -- preserve leading 1's
    r | BS.null b = Just BS.empty
      | otherwise = integerToBS <$> (b58DecodeInt table) b

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
