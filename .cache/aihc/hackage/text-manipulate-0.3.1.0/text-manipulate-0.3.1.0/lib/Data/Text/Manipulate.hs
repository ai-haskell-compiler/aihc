{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

-- Module      : Data.Text.Manipulate
-- Copyright   : (c) 2014-2020 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Manipulate identifiers and structurally non-complex pieces
-- of text by delimiting word boundaries via a combination of whitespace,
-- control-characters, and case-sensitivity.
--
-- Assumptions have been made about word boundary characteristics inherint
-- in predominantely English text, please see individual function documentation
-- for further details and behaviour.
module Data.Text.Manipulate
  ( -- * Strict vs lazy types
    -- $strict

    -- * Unicode
    -- $unicode

    -- * Fusion
    -- $fusion

    -- * Subwords

    -- ** Removing words
    takeWord,
    dropWord,
    stripWord,

    -- ** Breaking on words
    breakWord,
    splitWords,

    -- * Character manipulation
    lowerHead,
    upperHead,
    mapHead,

    -- * Line manipulation
    indentLines,
    prependLines,

    -- * Ellipsis
    toEllipsis,
    toEllipsisWith,

    -- * Acronyms
    toAcronym,

    -- * Ordinals
    toOrdinal,

    -- * Casing
    toTitle,
    toCamel,
    toPascal,
    toSnake,
    toSpinal,
    toTrain,

    -- * Boundary predicates
    isBoundary,
    isWordBoundary,
  )
where

import qualified Data.Char as Char
import Data.List (intersperse)
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LText
import qualified Data.Text.Lazy.Manipulate as LMan
import Data.Text.Manipulate.Internal.Fusion (strict)
import qualified Data.Text.Manipulate.Internal.Fusion as Fusion
import Data.Text.Manipulate.Internal.Types

-- $strict
-- This library provides functions for manipulating both strict and lazy Text types.
-- The strict functions are provided by the "Data.Text.Manipulate" module, while the lazy
-- functions are provided by the "Data.Text.Lazy.Manipulate" module.

-- $unicode
-- While this library supports Unicode in a similar fashion to the
-- underlying <http://hackage.haskell.org/package/text text> library,
-- more explicit Unicode handling of word boundaries can be found in the
-- <http://hackage.haskell.org/package/text-icu text-icu> library.

-- $fusion
-- Many functions in this module are subject to fusion, meaning that
-- a pipeline of such functions will usually allocate at most one Text value.
--
-- Functions that can be fused by the compiler are documented with the
-- phrase /Subject to fusion/.

-- DEBUG:
-- import Data.Text.Internal.Fusion        (stream)
-- import Data.Text.Internal.Fusion.Common (unstreamList)
-- tokens = unstreamList . Fusion.tokenise . stream

-- FIXME:
-- dropWord "ALong" == ""

-- | Lowercase the first character of a piece of text.
--
-- >>> lowerHead "Title Cased"
-- "title Cased"
lowerHead :: Text -> Text
lowerHead = mapHead Char.toLower

-- | Uppercase the first character of a piece of text.
--
-- >>> upperHead "snake_cased"
-- "Snake_cased"
upperHead :: Text -> Text
upperHead = mapHead Char.toUpper

-- | Apply a function to the first character of a piece of text.
mapHead :: (Char -> Char) -> Text -> Text
mapHead f x =
  case Text.uncons x of
    Just (c, cs) -> Text.singleton (f c) <> cs
    Nothing -> x

-- | Indent newlines by the given number of spaces.
--
-- /See:/ 'prependLines'
indentLines :: Int -> Text -> Text
indentLines n = prependLines (Text.replicate n " ")

-- | Prepend newlines with the given separator
prependLines :: Text -> Text -> Text
prependLines sep = mappend sep . Text.unlines . intersperse sep . Text.lines

-- | O(n) Truncate text to a specific length.
-- If the text was truncated the ellipsis sign "..." will be appended.
--
-- /See:/ 'toEllipsisWith'
toEllipsis :: Int -> Text -> Text
toEllipsis n = toEllipsisWith n "..."

-- | O(n) Truncate text to a specific length.
-- If the text was truncated the given ellipsis sign will be appended.
toEllipsisWith ::
  -- | Length.
  Int ->
  -- | Ellipsis.
  Text ->
  Text ->
  Text
toEllipsisWith n suf x
  | Text.length x > n = Text.take n x <> suf
  | otherwise = x

-- | O(n) Returns the first word, or the original text if no word
-- boundary is encountered. /Subject to fusion./
takeWord :: Text -> Text
takeWord = strict Fusion.takeWord

-- | O(n) Return the suffix after dropping the first word. If no word
-- boundary is encountered, the result will be empty. /Subject to fusion./
dropWord :: Text -> Text
dropWord = strict Fusion.dropWord

-- | Break a piece of text after the first word boundary is encountered.
--
-- >>> breakWord "PascalCasedVariable"
-- ("Pacal", "CasedVariable")
--
-- >>> breakWord "spinal-cased-variable"
-- ("spinal", "cased-variable")
breakWord :: Text -> (Text, Text)
breakWord x = (takeWord x, dropWord x)

-- | O(n) Return the suffix after removing the first word, or 'Nothing'
-- if no word boundary is encountered.
--
-- >>> stripWord "HTML5Spaghetti"
-- Just "Spaghetti"
--
-- >>> stripWord "noboundaries"
-- Nothing
stripWord :: Text -> Maybe Text
stripWord x
  | Text.length y < Text.length x = Just y
  | otherwise = Nothing
  where
    y = dropWord x

-- | O(n) Split into a list of words delimited by boundaries.
--
-- >>> splitWords "SupercaliFrag_ilistic"
-- ["Supercali","Frag","ilistic"]
splitWords :: Text -> [Text]
splitWords = go
  where
    go x = case breakWord x of
      (h, t)
        | Text.null h -> go t
        | Text.null t -> [h]
        | otherwise -> h : go t

-- | O(n) Create an adhoc acronym from a piece of cased text.
--
-- >>> toAcronym "AmazonWebServices"
-- Just "AWS"
--
-- >>> toAcronym "Learn-You Some_Haskell"
-- Just "LYSH"
--
-- >>> toAcronym "this_is_all_lowercase"
-- Nothing
toAcronym :: Text -> Maybe Text
toAcronym (Text.filter Char.isUpper -> x)
  | Text.length x > 1 = Just x
  | otherwise = Nothing

-- | Render an ordinal used to denote the position in an ordered sequence.
--
-- >>> toOrdinal (101 :: Int)
-- "101st"
--
-- >>> toOrdinal (12 :: Int)
-- "12th"
toOrdinal :: Integral a => a -> Text
toOrdinal = LText.toStrict . LMan.toOrdinal

-- | O(n) Convert casing to @Title Cased Phrase@. /Subject to fusion./
toTitle :: Text -> Text
toTitle = strict Fusion.toTitle

-- | O(n) Convert casing to @camelCasedPhrase@. /Subject to fusion./
toCamel :: Text -> Text
toCamel = strict Fusion.toCamel

-- | O(n) Convert casing to @PascalCasePhrase@. /Subject to fusion./
toPascal :: Text -> Text
toPascal = strict Fusion.toPascal

-- | O(n) Convert casing to @snake_cased_phrase@. /Subject to fusion./
toSnake :: Text -> Text
toSnake = strict Fusion.toSnake

-- | O(n) Convert casing to @spinal-cased-phrase@. /Subject to fusion./
toSpinal :: Text -> Text
toSpinal = strict Fusion.toSpinal

-- | O(n) Convert casing to @Train-Cased-Phrase@. /Subject to fusion./
toTrain :: Text -> Text
toTrain = strict Fusion.toTrain
