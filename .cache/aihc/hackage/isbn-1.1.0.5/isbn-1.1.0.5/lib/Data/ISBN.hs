{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}


module Data.ISBN
    ( -- * Introduction
      -- $introduction

      -- * Documentation
      ISBN
      -- $isbn-import
    , renderISBN
      -- * ISBN Validation
    , validateISBN
      -- *** ISBN Validation Errors
    , ISBNValidationError(..)
    , renderISBNValidationError

      -- * Validating only ISBN-10
    , validateISBN10
      -- *** ISBN-13 Validation Errors
    , ISBN10ValidationError
    , renderISBN10ValidationError

      -- * Validating only ISBN-13
    , validateISBN13
      -- *** ISBN-10 Validation Errors
    , ISBN13ValidationError(..)
    , renderISBN13ValidationError

      -- * Conversion between ISBN-10 and ISBN-13
      -- $conversion
    , convertISBN10toISBN13
    , convertISBN13toISBN10
      -- * ISBN Helpers
    , isISBN10
    , isISBN13
      -- * Creating ISBN values without validation
      -- $unsafe
    , unsafeToISBN10
    , unsafeToISBN13
    ) where

import           Data.ISBN.ISBN10
import           Data.ISBN.ISBN13

import           Control.Monad
import           Data.Text        ( Text )
import qualified Data.Text        as Text

-- $introduction
--
-- This library contains tools for validating and working with
-- [International Standard Book Numbers (ISBNs)](https://en.wikipedia.org/wiki/International_Standard_Book_Number).
-- An ISBN value can be in either the format of a ten digit ISBN-10 or a
-- thirteen digit ISBN-13. This library uses a single 'ISBN' data type with two
-- value constructors to represent ISBN values, so that a single entry point can
-- be used to validate text inputs as either an 'ISBN10', 'ISBN13', or neither.
--
-- For most users, importing only the @Data.ISBN@ module is necessary, as this
-- module re-exports all of the data types and functions necessary for
-- validating, creating, and converting between ISBN values.


------------------------------------

-- $isbn-import
--
-- __NOTE:__ This module does not export the constructors in the @ISBN@ type.
-- For access to the constructors, import the @Data.Types.ISBN@ module.


------------------------------------


-- | Used to safely create 'ISBN' values. Assumes that the 'Data.Text.Text'
-- input is an ISBN-10 or ISBN-13 string, either with or without hyphens.
--
-- Will return either a validated @ISBN@ or an 'ISBNValidationError', which can be
-- rendered as a descriptive string using 'renderISBNValidationError'.
--
-- /Examples:/
--
-- @
-- validateISBN "0345816021"        == Right (ISBN10 "0345816021")
-- validateISBN "0-807-01429-X"     == Right (ISBN10 "080701429X")
-- validateISBN "9780807014295"     == Right (ISBN13 "9780807014295")
-- validateISBN "978-0-306-40615-7" == Right (ISBN13 "9780306406157")
-- validateISBN "0-345-816"         == Left InvalidISBNInputLength
-- validateISBN "X-345-81602-1"     == Left IllegalCharactersInISBN10Body
-- validateISBN "0-345-81602-B"     == Left IllegalCharacterAsISBN10CheckDigit
-- validateISBN "0-345-81602-3"     == Left InvalidISBN10CheckDigit
-- validateISBN "00000000000000"    == Left InvalidISBNInputLength
-- validateISBN "9780807014299"     == Left InvalidISBN13CheckDigit
-- validateISBN "0X00000000000"     == Left IllegalCharactersInISBN13Input
-- @
validateISBN :: Text -> Either ISBNValidationError ISBN
validateISBN isbn = do
    let isbn10result = validateISBN10 isbn
        isbn13result = validateISBN13 isbn

    case (isbn10result, isbn13result) of
        (Right isbn10, _) ->
            Right isbn10

        (_, Right isbn13) ->
            Right isbn13

        (Left ISBN10InvalidInputLength, Left ISBN13InvalidInputLength) ->
            Left InvalidISBNInputLength

        (Left ISBN10IllegalCharactersInBody, _) ->
            Left IllegalCharactersInISBN10Body

        (Left ISBN10IllegalCharacterAsCheckDigit, _) ->
            Left IllegalCharacterAsISBN10CheckDigit

        (_ , Left ISBN13IllegalCharactersInInput) ->
            Left IllegalCharactersInISBN13Input

        (Left ISBN10InvalidCheckDigit, _) ->
            Left InvalidISBN10CheckDigit

        (_, Left ISBN13InvalidCheckDigit) ->
            Left InvalidISBN13CheckDigit


-- | Convert an 'ISBN' value to a 'Text' string. Can be used when displaying an
-- ISBN in an application interface or before storing the plain ISBN text values
-- in a database.
--
-- 'ISBN' values created using 'validateISBN', 'validateISBN10', or
-- 'validateISBN13' will never contain hyphens.
--
-- /Examples:/
--
-- @
-- renderISBN (ISBN10 "080701429X")    == "080701429X"
-- renderISBN (ISBN13 "9780060899226") == "9780060899226"
-- @
renderISBN :: ISBN -> Text
renderISBN (ISBN10 i) = i
renderISBN (ISBN13 i) = i


-- | Possible validation errors resulting from ISBN validation. Can be
-- rendered as a descriptive error message using 'renderISBNValidationError'.
data ISBNValidationError
    = InvalidISBNInputLength             -- ^ The length of the input string is not 10 or 13 characters, not counting hyphens
    | IllegalCharactersInISBN10Body      -- ^ The first nine characters of the ISBN-10 input contain non-numeric characters
    | IllegalCharactersInISBN13Input     -- ^ The ISBN-13 input contains non-numeric characters
    | IllegalCharacterAsISBN10CheckDigit -- ^ The check digit of the ISBN-10 is not a valid character (@0-9@ or @\'X\'@)
    | InvalidISBN10CheckDigit            -- ^ The check digit is not valid for the given ISBN-10
    | InvalidISBN13CheckDigit            -- ^ The check digit is not valid for the given ISBN-13
    deriving (Show, Eq)



-- | Convert an 'ISBNValidationError' into a human-friendly error message.
renderISBNValidationError :: ISBNValidationError -> Text
renderISBNValidationError validationError =
    case validationError of
        InvalidISBNInputLength ->
            "An ISBN must be 10 or 13 characters, not counting hyphens"

        IllegalCharactersInISBN10Body ->
            "The first nine non-hyphen characters of an ISBN-10 must all be numbers"

        IllegalCharactersInISBN13Input ->
            "Every non-hyphen character of an ISBN-13 must be a number"

        IllegalCharacterAsISBN10CheckDigit ->
            "The last character of an ISBN-10 must be a number or the letter 'X'"

        InvalidISBN10CheckDigit ->
            "The supplied ISBN-10 is not valid"

        InvalidISBN13CheckDigit ->
            "The supplied ISBN-13 is not valid"




-- $conversion
--
-- ISBN values can be converted from ISBN-10 to ISBN-13 and vise versa.


-- | Convert an ISBN-10 to an ISBN-13. Since all ISBN-10s can be converted to
-- ISBN-13s, this operation cannot fail.
--
-- /Example:/
--
-- @
-- convertISBN10toISBN13 (unsafeToISBN10 "0060899220") == ISBN13 "9780060899226"
-- @
convertISBN10toISBN13 :: ISBN -> ISBN
convertISBN10toISBN13 isbn10 =
    unsafeToISBN13 $ isbn13Body <> isbn13CheckDigit
      where
        isbn13CheckDigit = Text.singleton . numericValueToISBN13Char $ calculateISBN13CheckDigitValue isbn13Body
        isbn13Body = "978" <> isbn10Body
        isbn10Body = Text.init $ renderISBN isbn10


-- | Convert an ISBN-13 to an ISBN-10. Since only ISBN-13s starting with '978'
-- can be converted, this operation may fail.
--
-- /Example:/
--
-- @
-- convertISBN13toISBN10 (unsafeToISBN13 "9780060899226") == Just (ISBN10 "0060899220")
-- @
convertISBN13toISBN10 :: ISBN -> Maybe ISBN
convertISBN13toISBN10 isbn13 = do
    let isbn13Text = renderISBN isbn13
    unless ("978" `Text.isPrefixOf` isbn13Text)
        Nothing -- "Only ISBN-13s that begin with '978' can be converted to ISBN-10s"

    let isbn10Body = Text.init $ Text.drop 3 isbn13Text
        isbn10CheckDigit = Text.singleton . numericValueToISBN10Char $ calculateISBN10CheckDigitValue isbn10Body

    pure $ unsafeToISBN10 $ isbn10Body <> isbn10CheckDigit

-- $unsafe
--
-- In most cases, creating 'ISBN10' and 'ISBN13' values should be performed
-- using the 'validateISBN', 'validateISBN10', or 'validateISBN13' functions,
-- which ensure the ISBN values they produce are valid.
--
-- The functions below allow for the creation of ISBN values without any
-- validation. They should only be used in specific cases. For example, when
-- loading already-validated ISBN values stored in a text column in a database.
