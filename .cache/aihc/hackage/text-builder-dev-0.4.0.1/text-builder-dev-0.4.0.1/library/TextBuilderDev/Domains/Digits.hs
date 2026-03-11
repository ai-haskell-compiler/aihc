module TextBuilderDev.Domains.Digits where

import qualified Data.ByteString as ByteString
import qualified Data.List.Split as Split
import TextBuilder
import TextBuilderDev.Prelude hiding (intercalate)

-- | Decimal digit.
{-# INLINE decimalDigit #-}
decimalDigit :: (Integral a) => a -> TextBuilder
decimalDigit (fromIntegral -> n) =
  unicodeCodepoint (n + 48)

-- | Hexadecimal digit.
{-# INLINE hexadecimalDigit #-}
hexadecimalDigit :: (Integral a) => a -> TextBuilder
hexadecimalDigit (fromIntegral -> n) =
  if n <= 9
    then unicodeCodepoint (n + 48)
    else unicodeCodepoint (n + 87)

{-# INLINE signed #-}
signed :: (Ord a, Num a) => (a -> TextBuilder) -> a -> TextBuilder
signed onUnsigned i =
  if i >= 0
    then onUnsigned i
    else unicodeCodepoint 45 <> onUnsigned (negate i)

-- | Hexadecimal readable representation of binary data.
--
-- >>> byteStringHexEncoding "Hello"
-- "4865 6c6c 6f"
{-# INLINE byteStringHexEncoding #-}
byteStringHexEncoding :: ByteString -> TextBuilder
byteStringHexEncoding =
  intercalate " "
    . fmap mconcat
    . Split.chunksOf 2
    . fmap hexadecimal
    . ByteString.unpack
