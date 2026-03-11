module Data.ByteString.Base58.Internal where

import Data.Bits
import Data.ByteString ( ByteString )
import Data.String
import Data.Typeable ( Typeable )
import Data.Word
import GHC.Generics ( Generic )

import qualified Data.ByteString as BS
import qualified Data.List as L


newtype Alphabet =
    Alphabet
    { unAlphabet  :: ByteString
    } deriving (Ord, Eq, Show, Typeable, Generic, IsString)


bitcoinAlphabet :: Alphabet
bitcoinAlphabet =
    "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

rippleAlphabet :: Alphabet
rippleAlphabet =
    "rpshnaf39wBUDNEGHJKLM4PQRST7VWXYZ2bcdeCg65jkm8oFqi1tuvAxyz"

flickrAlphabet :: Alphabet
flickrAlphabet =
    "123456789abcdefghijkmnopqrstuvwxyzABCDEFGHJKLMNPQRSTUVWXYZ"

-- | Take 'i' byte from alphabet
b58 :: Alphabet -> Int -> Word8
b58 a i = BS.index (unAlphabet a) i

-- | Lookup position of byte 'w' in alphabet
b58' :: Alphabet -> Word8 -> Maybe Int
b58' a w = BS.elemIndex w (unAlphabet a)


-- | Decode a big endian Integer from a bytestring
bsToInteger :: ByteString -> Integer
bsToInteger = (L.foldl' f 0) . BS.unpack
  where
    f n w = (toInteger w) .|. shiftL n 8

-- | Encode an Integer to a bytestring as big endian
integerToBS :: Integer -> ByteString
integerToBS 0 = BS.pack [0]
integerToBS i
    | i > 0     = BS.pack $ reverse $ L.unfoldr f i
    | otherwise = error "integerToBS not defined for negative values"
  where
    f 0 = Nothing
    f x = Just $ (fromInteger x :: Word8, x `shiftR` 8)
