module TextBuilderDev.Domains.Padding where

import qualified Data.Text as Text
import TextBuilder
import TextBuilderDev.Prelude

-- | Pad a builder from the left side to the specified length with the specified character.
--
-- >>> padFromLeft 5 '0' "123"
-- "00123"
--
-- >>> padFromLeft 5 '0' "123456"
-- "123456"
{-# INLINEABLE padFromLeft #-}
padFromLeft :: Int -> Char -> TextBuilder -> TextBuilder
padFromLeft paddedLength paddingChar =
  textPaddedFromLeft paddedLength paddingChar . toText

-- | Pad a builder from the right side to the specified length with the specified character.
--
-- >>> padFromRight 5 ' ' "123"
-- "123  "
--
-- >>> padFromRight 5 ' ' "123456"
-- "123456"
{-# INLINEABLE padFromRight #-}
padFromRight :: Int -> Char -> TextBuilder -> TextBuilder
padFromRight paddedLength paddingChar =
  textPaddedFromRight paddedLength paddingChar . toText

-- | Pad a text from the left side to the specified length with the specified character.
--
-- >>> textPaddedFromLeft 5 '0' "123"
-- "00123"
--
-- >>> textPaddedFromLeft 5 '0' "123456"
-- "123456"
{-# INLINEABLE textPaddedFromLeft #-}
textPaddedFromLeft :: Int -> Char -> Text -> TextBuilder
textPaddedFromLeft paddedLength paddingChar input =
  let actualLength = Text.length input
   in if paddedLength <= actualLength
        then text input
        else foldMap char (replicate (paddedLength - actualLength) paddingChar) <> text input

-- | Pad a text from the right side to the specified length with the specified character.
--
-- >>> textPaddedFromRight 5 '0' "123"
-- "12300"
--
-- >>> textPaddedFromRight 5 '0' "123456"
-- "123456"
{-# INLINEABLE textPaddedFromRight #-}
textPaddedFromRight :: Int -> Char -> Text -> TextBuilder
textPaddedFromRight paddedLength paddingChar inputText =
  let actualLength = Text.length inputText
   in if paddedLength <= actualLength
        then text inputText
        else text inputText <> foldMap char (replicate (paddedLength - actualLength) paddingChar)
