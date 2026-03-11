{-# LANGUAGE OverloadedStrings #-}

module Data.ISBN.ISBN10
    ( ISBN(..)
    , validateISBN10
      -- * Validation Errors
    , ISBN10ValidationError(..)
    , renderISBN10ValidationError
      -- * Helpers
    , confirmISBN10CheckDigit
    , calculateISBN10CheckDigitValue
    , isbn10CharToNumericValue
    , numericValueToISBN10Char
    , isValidISBN10CheckDigit
    , isNumericCharacter
    , isISBN10
      -- * Unsafe Coercion
    , unsafeToISBN10
    ) where

import           Control.Monad
import           Data.Char
import           Data.Text       ( Text )
import qualified Data.Text       as Text

import           Data.ISBN.Types ( ISBN (ISBN10) )



-- | Used to safely create 'ISBN10' values represented by the 'ISBN' data type.
-- Assumes that the 'Data.Text.Text' input is an ISBN-10 string, either with or
-- without hyphens.
--
-- Will return either a validated ISBN-10 or an 'ISBN10ValidationError', which can be
-- rendered as a descriptive string using 'renderISBN10ValidationError'.
--
-- /Examples:/
--
-- @
-- validateISBN10 "0-345-81602-1" == Right (ISBN10 "0345816021")
-- validateISBN10 "0345816021"    == Right (ISBN10 "0345816021")
-- validateISBN10 "0-807-01429-X" == Right (ISBN10 "080701429X")
-- validateISBN10 "0-345-816"     == Left ISBN10InvalidInputLength
-- validateISBN10 "X-345-81602-1" == Left ISBN10IllegalCharactersInBody
-- validateISBN10 "0-345-81602-B" == Left ISBN10IllegalCharacterAsCheckDigit
-- validateISBN10 "0-345-81602-3" == Left ISBN10InvalidCheckDigit
-- @
validateISBN10 :: Text -> Either ISBN10ValidationError ISBN
validateISBN10 input = do
    -- Make a copy of the text input before further manipulation to prevent
    -- space leaks if input text is a slice of a larger string
    let inputWithoutHyphens = Text.filter (/= '-') $ Text.copy input

    unless (Text.length inputWithoutHyphens == 10) $
        Left ISBN10InvalidInputLength

    let invalidBodyCharacters = Text.filter (not . isNumericCharacter) (Text.init inputWithoutHyphens)

    unless (Text.length invalidBodyCharacters == 0) $
        Left ISBN10IllegalCharactersInBody

    unless (isValidISBN10CheckDigit $ Text.last inputWithoutHyphens) $
        Left ISBN10IllegalCharacterAsCheckDigit

    unless (confirmISBN10CheckDigit inputWithoutHyphens) $
        Left ISBN10InvalidCheckDigit

    pure $ ISBN10 inputWithoutHyphens



-- | Possible validation errors resulting from ISBN-10 validation.
data ISBN10ValidationError
    = ISBN10InvalidInputLength           -- ^ The length of the input string is not 10 characters, not counting hyphens
    | ISBN10IllegalCharactersInBody      -- ^ The first nine characters of the ISBN-10 input contain non-numeric characters
    | ISBN10IllegalCharacterAsCheckDigit -- ^ The check digit of the ISBN-10 is not a valid character (@0-9@ or @\'X\'@)
    | ISBN10InvalidCheckDigit            -- ^ The check digit is not valid for the given ISBN-10
    deriving (Show, Eq)


-- | Convert an 'ISBN10ValidationError' into a human-friendly error message.
renderISBN10ValidationError :: ISBN10ValidationError -> Text
renderISBN10ValidationError validationError =
    case validationError of
        ISBN10InvalidInputLength ->
            "An ISBN-10 must be 10 characters, not counting hyphens"

        ISBN10IllegalCharactersInBody ->
            "The first nine characters of an ISBN-10 must all be numbers"

        ISBN10IllegalCharacterAsCheckDigit ->
            "The last character of the supplied ISBN-10 must be a number or the letter 'X'"

        ISBN10InvalidCheckDigit ->
            "The supplied ISBN-10 is not valid"


-- | Confirms that the check digit of an ISBN-10 is correct. Assumes that the
-- input consists of 9 numeric characters followed by a legal check digit
-- character (@0-9@ or @X@).
--
-- /Examples:/
--
-- @
-- confirmISBN10CheckDigit "0345816021" == True
-- confirmISBN10CheckDigit "080701429X" == True
-- @
confirmISBN10CheckDigit :: Text -> Bool
confirmISBN10CheckDigit isbn10 =
    calculateISBN10CheckDigitValue (Text.init isbn10) == isbn10CharToNumericValue (Text.last isbn10)


-- | Calculates an ISBN-10 check digit value using the standard check digit
-- calculation formula. Assumes that the input is 9 numeric characters. The
-- check digit value can be any number in the range 0 to 10, the last of
-- which is represented by the symbol \'X\' in an ISBN-10.
--
-- See: <https://en.wikipedia.org/wiki/International_Standard_Book_Number#ISBN-10_check_digits>
--
-- /Examples:/
--
-- @
-- calculateISBN10CheckDigitValue "034581602" == 1
-- calculateISBN10CheckDigitValue "080701429" == 10
-- @
calculateISBN10CheckDigitValue :: Text -> Int
calculateISBN10CheckDigitValue input =
    go 10 (Text.unpack input) 0
      where
        go n charList acc =
            case charList of
              []      -> (11 - (acc `mod` 11)) `mod` 11
              c:clist -> go (n - 1) clist (acc + isbn10CharToNumericValue c * n)


-- | Converts an ISBN-10 character to a numeric value. Valid input characters
-- include @0-9@ as well as @X@.
isbn10CharToNumericValue :: Char -> Int
isbn10CharToNumericValue 'X' = 10
isbn10CharToNumericValue  c  = digitToInt c

-- | Converts a numeric value to an ISBN-10 character. Valid input values
-- are the numbers from 0 to 10.
numericValueToISBN10Char :: Int -> Char
numericValueToISBN10Char 10 = 'X'
numericValueToISBN10Char c  = Text.head $ Text.pack $ show c


-- | Validates a character as a valid ISBN-10 check digit character.  ISBN-10
-- check digit characters include @0-9@ as well as the symbol @'X'@. The lowercase
-- letter \'x\' is not considered valid.
isValidISBN10CheckDigit :: Char -> Bool
isValidISBN10CheckDigit char = char `elem` ("1234567890X" :: String)

-- | Determines whether a character is numeric (e.g. in the range of @0-9@).
isNumericCharacter :: Char -> Bool
isNumericCharacter char = char `elem` ("1234567890" :: String)

-- | Determines whether an 'ISBN' value is an ISBN-10.
--
-- /Examples:/
--
-- @
-- isISBN10 (unsafeToISBN10 "0060899220")    == True
-- isISBN10 (unsafeToISBN13 "9780060899226") == False
-- @
--
-- /Since: 1.1.0.0/
isISBN10 :: ISBN -> Bool
isISBN10 (ISBN10 _) = True
isISBN10 _          = False


-- | Will create an 'ISBN10' value without any validation.
unsafeToISBN10 :: Text -> ISBN
unsafeToISBN10 = ISBN10
