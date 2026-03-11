{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN13
    ( ISBN(..)
    , validateISBN13
      -- * Validation Errors
    , renderISBN13ValidationError
    , ISBN13ValidationError(..)
      -- * Helpers
    , confirmISBN13CheckDigit
    , calculateISBN13CheckDigitValue
    , numericValueToISBN13Char
    , isISBN13
      -- * Unsafe Coercion
    , unsafeToISBN13
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text       ( Text )
import qualified Data.Text       as Text

import           Data.ISBN.Types ( ISBN (ISBN13) )



-- | Used to safely create 'ISBN13' values represented by the 'ISBN' data type.
-- Assumes that the 'Data.Text.Text' input is an ISBN-13 string, either with or
-- without hyphens.
--
-- Will return either a validated ISBN-13 or an 'ISBN13ValidationError', which can be
-- rendered as a descriptive string using 'renderISBN13ValidationError'.
--
-- /Examples:/
--
-- @
-- validateISBN13 "9780345816023"     == Right (ISBN13 "9780345816023")
-- validateISBN13 "9780807014295"     == Right (ISBN13 "9780807014295")
-- validateISBN13 "9780306406157"     == Right (ISBN13 "9780306406157")
-- validateISBN13 "978-0-306-40615-7" == Right (ISBN13 "9780306406157")
-- validateISBN13 "9780345816029"     == Left ISBN13InvalidCheckDigit
-- validateISBN13 "9780807014299"     == Left ISBN13InvalidCheckDigit
-- validateISBN13 "00000000000000"    == Left ISBN13InvalidInputLength
-- validateISBN13 "0X00000000000"     == Left ISBN13IllegalCharactersInInput
-- @
validateISBN13 :: Text -> Either ISBN13ValidationError ISBN
validateISBN13 input = do
    -- Make a copy of the text input before further manipulation to prevent
    -- space leaks if input text is a slice of a larger string
    let inputWithoutHyphens = Text.filter (/= '-') $ Text.copy input

    unless (Text.length inputWithoutHyphens == 13) $
        Left ISBN13InvalidInputLength

    let illegalCharacters = Text.filter (not . isNumericCharacter) inputWithoutHyphens

    unless (Text.length illegalCharacters == 0) $
        Left ISBN13IllegalCharactersInInput

    unless (confirmISBN13CheckDigit inputWithoutHyphens) $
        Left ISBN13InvalidCheckDigit

    pure $ ISBN13 inputWithoutHyphens



-- | Possible validation errors resulting from ISBN-13 validation.
data ISBN13ValidationError
    = ISBN13InvalidInputLength       -- ^ The length of the input string is not 13 characters, not counting hyphens
    | ISBN13IllegalCharactersInInput -- ^ The ISBN-13 input contains non-numeric characters
    | ISBN13InvalidCheckDigit        -- ^ The check digit is not valid for the given ISBN-13
    deriving (Show, Eq)

-- | Convert an 'ISBN13ValidationError' into a human-friendly error message.
renderISBN13ValidationError :: ISBN13ValidationError -> Text
renderISBN13ValidationError validationError =
    case validationError of
        ISBN13InvalidInputLength ->
            "An ISBN-13 must be 13 characters, not counting hyphens"

        ISBN13IllegalCharactersInInput ->
            "Every non-hyphen character of an ISBN-13 must be a number"

        ISBN13InvalidCheckDigit ->
            "The supplied ISBN-13 is not valid"


-- | Determines whether a character is numeric (e.g. in the range of @0-9@).
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)

-- | Confirms that the check digit of an ISBN-13 is correct. Assumes that the
-- input consists of 12 numeric characters followed by a legal check digit
-- character (@0-9@).
--
-- /Examples:/
--
-- @
-- confirmISBN13CheckDigit "9780306406157" == True
-- confirmISBN13CheckDigit "9780345816029" == False
-- @
confirmISBN13CheckDigit :: Text -> Bool
confirmISBN13CheckDigit isbn13 =
    calculateISBN13CheckDigitValue (Text.init isbn13) == isbn13CharToNumericValue (Text.last isbn13)

-- | Calculates an ISBN-13 check digit value using the standard check digit
-- calculation formula. Assumes that the input is 12 numeric characters. The
-- check digit value will be a number from 0 to 9.
--
-- See: <https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-13_check_digit_calculation>
--
-- /Examples:/
--
-- @
-- calculateISBN13CheckDigitValue "978030640615" == 7
-- calculateISBN13CheckDigitValue "978151915024" == 0
-- @
calculateISBN13CheckDigitValue :: Text -> Int
calculateISBN13CheckDigitValue input =
    go 1 (Text.unpack input) 0
      where
        go w charList acc =
            case charList of
              [] -> (10 - (acc `mod` 10)) `mod` 10
              c:clist -> go ((w + 2) `mod` 4) clist (acc + w * isbn13CharToNumericValue c)


-- | Converts an ISBN-13 character to a numeric value. Valid input characters
-- include @0-9@ as well as @X@.
isbn13CharToNumericValue :: Char -> Int
isbn13CharToNumericValue = digitToInt

-- | Converts a numeric value to an ISBN-13 character. Valid input values
-- are the numbers from 0 to 10.
numericValueToISBN13Char :: Int -> Char
numericValueToISBN13Char c = Text.head $ Text.pack $ show c

-- | Determines whether an 'ISBN' value is an ISBN-13.
--
-- /Examples:/
--
-- @
-- isISBN13 (unsafeToISBN10 "0060899220")    == False
-- isISBN13 (unsafeToISBN13 "9780060899226") == True
-- @
--
-- /Since: 1.1.0.0/
isISBN13 :: ISBN -> Bool
isISBN13 (ISBN13 _) = True
isISBN13 _          = False


-- | Will create an 'ISBN13' value without any validation.
unsafeToISBN13 :: Text -> ISBN
unsafeToISBN13 = ISBN13
