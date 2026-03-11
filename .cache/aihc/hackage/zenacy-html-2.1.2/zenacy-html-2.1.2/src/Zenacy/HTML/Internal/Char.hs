-- | Functions for identifying and manipulating character codes.
module Zenacy.HTML.Internal.Char
  ( ctow
  , chrWord8
  , chrUTF8
  , chrSurrogate
  , chrScalar
  , chrNonCharacter
  , chrASCIIDigit
  , chrASCIIUpperHexDigit
  , chrASCIILowerHexDigit
  , chrASCIIHexDigit
  , chrASCIIUpperAlpha
  , chrASCIILowerAlpha
  , chrASCIIAlpha
  , chrASCIIAlphanumeric
  , chrWhitespace
  , chrC0Control
  , chrControl
  , chrToUpper
  , chrToLower
  , chrAmpersand
  , chrEOF
  , chrExclamation
  , chrGreater
  , chrLess
  , chrQuestion
  , chrSolidus
  , chrTab
  , chrLF
  , chrFF
  , chrCR
  , chrSpace
  , chrEqual
  , chrQuote
  , chrApostrophe
  , chrGrave
  , chrNumberSign
  , chrHyphen
  , chrBracketRight
  , chrSemicolon
  , chrUpperX
  , chrLowerX
  ) where

import qualified Data.ByteString as S
  ( unpack
  )
import Data.Char
  ( chr
  , ord
  )
import qualified Data.Text as Text
  ( singleton
  )
import qualified Data.Text.Encoding as Text
  ( encodeUtf8
  )
import Data.Word8

-- | Converts a character to a Word8.
ctow :: Char -> Word8
ctow x = fromIntegral (ord x)

-- | Determines if a character code is in the range of a Word8.
chrWord8 :: Int -> Bool
chrWord8 x = x >= 0 && x <= 0xFF

-- | Decodes a UTF8 unicode character.
chrUTF8 :: Int -> [Word8]
chrUTF8 = S.unpack . Text.encodeUtf8 . Text.singleton . chr

-- | Determines if a character code is a surrogate.
chrSurrogate :: Int -> Bool
chrSurrogate x = x >= 0xD800 && x <= 0xDFFF

-- | Determines if a character code is a scalar.
chrScalar :: Int -> Bool
chrScalar = not . chrSurrogate

-- | Determines if a code is a not a character code.
chrNonCharacter :: Int -> Bool
chrNonCharacter x =
  (x >= 0xFDD0 && x <= 0xFDEF) ||
  any (==x)
    [ 0xFFFE, 0xFFFF, 0x1FFFE, 0x1FFFF
    , 0x2FFFE, 0x2FFFF, 0x3FFFE, 0x3FFFF
    , 0x4FFFE, 0x4FFFF, 0x5FFFE, 0x5FFFF
    , 0x6FFFE, 0x6FFFF, 0x7FFFE, 0x7FFFF
    , 0x8FFFE, 0x8FFFF, 0x9FFFE, 0x9FFFF
    , 0xAFFFE, 0xAFFFF, 0xBFFFE, 0xBFFFF
    , 0xCFFFE, 0xCFFFF, 0xDFFFE, 0xDFFFF
    , 0xEFFFE, 0xEFFFF, 0xFFFFE, 0xFFFFF
    , 0x10FFFE, 0x10FFFF
    ]

-- | Determines if a character is an ASCII digit.
chrASCIIDigit :: Word8 -> Bool
chrASCIIDigit x = x >= 0x30 && x <= 0x39

-- | Determines if a character is an ASCII uppercase hex digit.
chrASCIIUpperHexDigit :: Word8 -> Bool
chrASCIIUpperHexDigit x = chrASCIIDigit x || (x >= 0x41 && x <= 0x46)

-- | Determines if a character is an ASCII lowercase hex digit.
chrASCIILowerHexDigit :: Word8 -> Bool
chrASCIILowerHexDigit x = chrASCIIDigit x || (x >= 0x61 && x <= 0x66)

-- | Determines if a character is an ASCII hex digit (any case).
chrASCIIHexDigit :: Word8 -> Bool
chrASCIIHexDigit x = chrASCIIUpperHexDigit x || chrASCIILowerHexDigit x

-- | Determines if a character is an ASCII uppercase alpha character.
chrASCIIUpperAlpha :: Word8 -> Bool
chrASCIIUpperAlpha x = x >= 0x41 && x <= 0x5A

-- | Determines if a character is an ASCII lowercase alpha character.
chrASCIILowerAlpha :: Word8 -> Bool
chrASCIILowerAlpha x = x >= 0x61 && x <= 0x7A

-- | Determines if a character is an ASCII alpha character (any case).
chrASCIIAlpha :: Word8 -> Bool
chrASCIIAlpha x = chrASCIIUpperAlpha x || chrASCIILowerAlpha x

-- | Determines if a character is an ASCII alphanumeric character (any case).
chrASCIIAlphanumeric :: Word8 -> Bool
chrASCIIAlphanumeric x = chrASCIIDigit x || chrASCIIAlpha x

-- | Determines if a character is a whitespace character.
chrWhitespace :: Word8 -> Bool
chrWhitespace x =
  x == chrTab ||
  x == chrLF ||
  x == chrFF ||
  x == chrCR ||
  x == chrSpace

-- | Determines if a character is a C0 control character.
chrC0Control :: Word8 -> Bool
chrC0Control x = x >= 0x00 && x <= 0x1F

-- | Determines if a character is a control character.
chrControl :: Word8 -> Bool
chrControl x = chrC0Control x || (x >= 0x7F && x <= 0x9F)

-- | Converts a character to uppercase.
chrToUpper :: Word8 -> Word8
chrToUpper = toUpper

-- | Converts a character to lowercase.
chrToLower :: Word8 -> Word8
chrToLower = toLower

-- | Character code for ampersand.
chrAmpersand :: Word8
chrAmpersand = _ampersand

-- | Character code for EOF.
chrEOF :: Word8
chrEOF = _nul

-- | Character code for exclamation.
chrExclamation :: Word8
chrExclamation = _exclam

-- | Character code for greater.
chrGreater :: Word8
chrGreater = _greater

-- | Character code for less.
chrLess :: Word8
chrLess = _less

-- | Character code for question.
chrQuestion :: Word8
chrQuestion = _question

-- | Character code for solidus (slash).
chrSolidus :: Word8
chrSolidus = _slash

-- | Character code for tab.
chrTab :: Word8
chrTab = _tab

-- | Character code for line feed.
chrLF :: Word8
chrLF = _lf

-- | Character code for form feed.
chrFF :: Word8
chrFF = _np

-- | Character code for carraige return.
chrCR :: Word8
chrCR = _cr

-- | Character code for space.
chrSpace :: Word8
chrSpace = _space

-- | Character code for equal.
chrEqual :: Word8
chrEqual = _equal

-- | Character code for quote.
chrQuote :: Word8
chrQuote = _quotedbl

-- | Character code for apostrophe.
chrApostrophe :: Word8
chrApostrophe = _quotesingle

-- | Character code for grave.
chrGrave :: Word8
chrGrave = _grave

-- | Character code for number sign.
chrNumberSign :: Word8
chrNumberSign = _numbersign

-- | Character code for hyphen.
chrHyphen :: Word8
chrHyphen = _hyphen

-- | Character code for right bracket.
chrBracketRight :: Word8
chrBracketRight = _bracketright

-- | Character code for semicolon.
chrSemicolon :: Word8
chrSemicolon = _semicolon

-- | Character code for upper x.
chrUpperX :: Word8
chrUpperX = 0x58

-- | Character code for lower x.
chrLowerX :: Word8
chrLowerX = 0x78
