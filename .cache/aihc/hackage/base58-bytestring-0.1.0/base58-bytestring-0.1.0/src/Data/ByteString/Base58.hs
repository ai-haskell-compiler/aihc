module Data.ByteString.Base58
       ( -- * Alphabet
         Alphabet(..)
       , bitcoinAlphabet
       , rippleAlphabet
       , flickrAlphabet
       -- * Encoding and decoding bytestrings
       , encodeBase58
       , decodeBase58
       -- * Encoding and decoding integers
       , encodeBase58I
       , decodeBase58I
       ) where

import Control.Applicative
import Data.ByteString ( ByteString )
import Data.ByteString.Base58.Internal
import Data.Char (chr, ord)
import Data.Maybe
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BC


encodeBase58I :: Alphabet -> Integer -> ByteString
encodeBase58I alpha i =
    BC.pack $ showIntAtBase 58 f i ""
  where
    f :: Int -> Char
    f = chr . fromIntegral . b58 alpha . fromIntegral

decodeBase58I :: Alphabet -> ByteString -> Maybe Integer
decodeBase58I alpha s =
    let c = b58' alpha . fromIntegral . ord
        p = isJust . c
        f = fromIntegral . fromJust . c
    in case readInt 58 p f (BC.unpack s) of
        [(r,[])] -> Just r
        _        -> Nothing

-- | Encode a bytestring to a base 58 representation.
encodeBase58 :: Alphabet -> ByteString -> ByteString
encodeBase58 alpha bs =
    let (z, b) = BS.span (== 0) bs
        l = BS.pack
            $ replicate (BS.length z)
            $ b58 alpha 0
        r | BS.null b = BS.empty
          | otherwise = encodeBase58I alpha
                        $ bsToInteger b
    in BS.append l r

-- | Decode a base 58 encoded bytestring. This can fail if the input bytestring
-- contains invalid base 58 characters such as 0,O,l,I
decodeBase58 :: Alphabet -> ByteString -> Maybe ByteString
decodeBase58 alpha bs =
    let (z, b) = BS.span (== (b58 alpha 0)) bs
        prefix = BS.pack
                 $ replicate (BS.length z) 0
        r | BS.null b = Just BS.empty
          | otherwise = integerToBS <$> decodeBase58I alpha b
    in BS.append prefix <$> r
