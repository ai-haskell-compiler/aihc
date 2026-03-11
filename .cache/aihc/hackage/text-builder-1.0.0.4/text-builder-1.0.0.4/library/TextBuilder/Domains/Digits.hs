module TextBuilder.Domains.Digits where

import qualified Data.Text.Array as TextArray
import qualified TextBuilder.Domains.Digits.Codepoints as Codepoints
import TextBuilder.Prelude
import TextBuilderCore

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

{-# INLINE customFixedNumeralSystem #-}
customFixedNumeralSystem ::
  (FiniteBits a, Integral a) =>
  -- | Number of bits per digit.
  Int ->
  -- | Projection to codepoint with handling of overflow.
  (a -> a) ->
  -- | Value.
  a ->
  TextBuilder
customFixedNumeralSystem bitsPerDigit digitCodepoint val =
  let size = div (finiteBitSize val + bitsPerDigit - 1) bitsPerDigit
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex (fromIntegral (digitCodepoint val))
                  go (unsafeShiftR val bitsPerDigit) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)

-- |
-- [Two's complement](https://en.wikipedia.org/wiki/Two%27s_complement) binary representation of a value.
--
-- Bits of a statically sized value padded from the left according to the size.
-- If it's a negatable integer, the sign is reflected in the bits.
--
-- >>> binary @Int8 0
-- "00000000"
--
-- >>> binary @Int8 4
-- "00000100"
--
-- >>> binary @Int8 (-1)
-- "11111111"
--
-- >>> binary @Word8 255
-- "11111111"
--
-- >>> binary @Int16 4
-- "0000000000000100"
--
-- >>> binary @Int16 (-4)
-- "1111111111111100"
{-# INLINE binary #-}
binary :: (FiniteBits a) => a -> TextBuilder
binary val =
  let size = finiteBitSize val
   in TextBuilder size \array arrayStartIndex ->
        let go val arrayIndex =
              if arrayIndex >= arrayStartIndex
                then do
                  TextArray.unsafeWrite array arrayIndex if testBit val 0 then 49 else 48
                  go (unsafeShiftR val 1) (pred arrayIndex)
                else return indexAfter
            indexAfter =
              arrayStartIndex + size
         in go val (pred indexAfter)

-- |
-- Same as 'binary', but with the \"0b\" prefix.
--
-- >>> prefixedBinary @Int8 0
-- "0b00000000"
{-# INLINE prefixedBinary #-}
prefixedBinary :: (FiniteBits a) => a -> TextBuilder
prefixedBinary = mappend "0b" . binary

-- | Octal representation of an integer.
--
-- >>> octal @Int32 123456
-- "00000361100"
--
-- >>> octal @Int32 (-123456)
-- "77777416700"
{-# INLINE octal #-}
octal :: (FiniteBits a, Integral a) => a -> TextBuilder
octal = customFixedNumeralSystem 3 (Codepoints.octalDigit . fromIntegral)

-- |
-- Same as 'octal', but with the \"0o\" prefix.
--
-- >>> prefixedOctal @Int8 0
-- "0o000"
{-# INLINE prefixedOctal #-}
prefixedOctal :: (FiniteBits a, Integral a) => a -> TextBuilder
prefixedOctal = mappend "0o" . octal

-- | Integer in hexadecimal notation with a fixed number of digits determined by the size of the type.
--
-- >>> hexadecimal @Int8 0
-- "00"
--
-- >>> hexadecimal @Int8 4
-- "04"
--
-- >>> hexadecimal @Int8 (-128)
-- "80"
--
-- >>> hexadecimal @Int8 (-1)
-- "ff"
--
-- >>> hexadecimal @Word8 255
-- "ff"
--
-- >>> hexadecimal @Int32 123456
-- "0001e240"
--
-- >>> hexadecimal @Int32 (-123456)
-- "fffe1dc0"
{-# INLINE hexadecimal #-}
hexadecimal :: (FiniteBits a, Integral a) => a -> TextBuilder
hexadecimal = customFixedNumeralSystem 4 (Codepoints.hexDigit . fromIntegral)

-- |
-- Same as 'hexadecimal', but with the \"0x\" prefix.
--
-- >>> prefixedHexadecimal @Int8 0
-- "0x00"
{-# INLINE prefixedHexadecimal #-}
prefixedHexadecimal :: (FiniteBits a, Integral a) => a -> TextBuilder
prefixedHexadecimal = mappend "0x" . hexadecimal

-- * Signed Numbers

{-# INLINE signed #-}
signed :: (Integral a) => (forall a. (Integral a) => a -> TextBuilder) -> a -> TextBuilder
signed onUnsigned a =
  if a >= 0
    then onUnsigned a
    else
      unicodeCodepoint 45
        <> let negated = negate a
            in if negated /= a
                 then onUnsigned negated
                 else
                   -- This is a special case for the minimum value of signed types.
                   -- The negation of the minimum value is not representable in the same type.
                   -- For example, for Int8, -128 is not representable as a positive number.
                   onUnsigned (negate (fromIntegral a :: Integer))

-- | Signed decimal representation of an integer.
--
-- >>> decimal 123456
-- "123456"
--
-- >>> decimal (-123456)
-- "-123456"
--
-- >>> decimal 0
-- "0"
--
-- >>> decimal (-2 :: Int8)
-- "-2"
--
-- >>> decimal (-128 :: Int8)
-- "-128"
{-# INLINE decimal #-}
decimal :: (Integral a) => a -> TextBuilder
decimal = signed unsignedDecimal

-- * Unsigned Numbers

-- | Render a number in the given radix.
{-# INLINE digitsByRadix #-}
digitsByRadix :: (Integral a) => a -> (a -> a) -> a -> TextBuilder
digitsByRadix radix digitCodepoint =
  go 0 []
  where
    go !offset !digits x = case divMod x radix of
      (next, digit) ->
        if next <= 0
          then finish (succ offset) (digit : digits)
          else go (succ offset) (digit : digits) next

    finish size digits =
      TextBuilder size action
      where
        action :: TextArray.MArray s -> Int -> ST s Int
        action array =
          go digits
          where
            go digits offset = case digits of
              [] -> return offset
              (digit : digits) -> do
                TextArray.unsafeWrite array offset (fromIntegral (digitCodepoint digit))
                go digits (succ offset)

-- | Unsigned octal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
--
-- >>> unsignedOctal 7
-- "7"
--
-- >>> unsignedOctal 9
-- "11"
--
-- >>> unsignedOctal 16
-- "20"
{-# INLINE unsignedOctal #-}
unsignedOctal :: (Integral a) => a -> TextBuilder
unsignedOctal =
  digitsByRadix 8 (+ 48)

-- | Unsigned decimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedDecimal 123456
-- "123456"
--
-- >>> unsignedDecimal 0
-- "0"
{-# INLINE unsignedDecimal #-}
unsignedDecimal :: (Integral a) => a -> TextBuilder
unsignedDecimal =
  digitsByRadix 10 (+ 48)

-- | Unsigned hexadecimal representation of a non-negative integral value.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedHexadecimal 123456
-- "1e240"
--
-- >>> unsignedHexadecimal 0
-- "0"
{-# INLINE unsignedHexadecimal #-}
unsignedHexadecimal :: (Integral a) => a -> TextBuilder
unsignedHexadecimal =
  digitsByRadix 16 (\digit -> if digit <= 9 then digit + 48 else digit + 87)

-- * Other

-- | Fixed-length decimal without sign.
-- Padded with zeros or trimmed depending on whether it's shorter or longer
-- than specified.
--
-- >>> fixedLengthDecimal 5 123
-- "00123"
--
-- >>> fixedLengthDecimal 5 123456
-- "23456"
--
-- >>> fixedLengthDecimal 5 (-123456)
-- "23456"
--
-- >>> fixedLengthDecimal 7 (-123456)
-- "0123456"
--
-- >>> fixedLengthDecimal 0 123
-- ""
--
-- >>> fixedLengthDecimal (-2) 123
-- ""
{-# INLINEABLE fixedLengthDecimal #-}
fixedLengthDecimal :: (Integral a) => Int -> a -> TextBuilder
fixedLengthDecimal (max 0 -> size) (abs -> val) =
  TextBuilder size $ \array startOffset ->
    let offsetAfter = startOffset + size
        writeValue val offset =
          if offset >= startOffset
            then
              if val /= 0
                then case divMod val 10 of
                  (val, digit) -> do
                    TextArray.unsafeWrite array offset $ 48 + fromIntegral digit
                    writeValue val (pred offset)
                else writePadding offset
            else return offsetAfter
        writePadding offset =
          if offset >= startOffset
            then do
              TextArray.unsafeWrite array offset 48
              writePadding (pred offset)
            else return offsetAfter
     in writeValue val (pred offsetAfter)

-- | Decimal representation of an integral value with thousands separated by the specified character.
--
-- >>> thousandSeparatedDecimal ',' 1234567890
-- "1,234,567,890"
--
-- >>> thousandSeparatedDecimal ' ' (-1234567890)
-- "-1 234 567 890"
{-# INLINEABLE thousandSeparatedDecimal #-}
thousandSeparatedDecimal :: (Integral a) => Char -> a -> TextBuilder
thousandSeparatedDecimal separatorChar =
  signed (unsignedThousandSeparatedDecimal separatorChar)

-- | Decimal representation of an unsigned integral value with thousands separated by the specified character.
--
-- __Warning:__ It is your responsibility to ensure that the value is non-negative.
--
-- >>> unsignedThousandSeparatedDecimal ',' 1234567890
-- "1,234,567,890"
--
-- >>> unsignedThousandSeparatedDecimal ' ' 1234567890
-- "1 234 567 890"
--
-- >>> unsignedThousandSeparatedDecimal ',' 0
-- "0"
{-# INLINEABLE unsignedThousandSeparatedDecimal #-}
unsignedThousandSeparatedDecimal :: (Integral a) => Char -> a -> TextBuilder
unsignedThousandSeparatedDecimal separatorChar =
  processRightmostDigit
  where
    processRightmostDigit value =
      case divMod value 10 of
        (value, digit) ->
          processAnotherDigit [decimalDigit digit] (1 :: Int) value
    processAnotherDigit builders index value =
      if value == 0
        then mconcat builders
        else case divMod value 10 of
          (value, digit) ->
            if mod index 3 == 0
              then
                processAnotherDigit
                  (decimalDigit digit : char separatorChar : builders)
                  (succ index)
                  value
              else
                processAnotherDigit
                  (decimalDigit digit : builders)
                  (succ index)
                  value
