{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}

-- | C's @printf@ for Haskell.
--
-- 'printf' takes a format string and as many arguments as the format string
-- needs. Its result is a 'String' or an action that writes to the standard
-- output. 'hPrintf' writes to a given handle. The 'PrintfArg' class and the
-- field formatters let a library give its own string or number type a
-- @printf@ conversion.
module Text.Printf
  ( printf,
    hPrintf,
    PrintfType,
    HPrintfType,
    FieldFormat (..),
    FieldFormatter,
    FormatAdjustment (..),
    FormatParse (..),
    FormatSign (..),
    IsChar (..),
    ModifierParser,
    PrintfArg (..),
    formatChar,
    formatInt,
    formatInteger,
    formatRealFloat,
    formatString,
    vFmt,
    perror,
    errorBadArgument,
    errorBadFormat,
    errorMissingArgument,
    errorShortFormat,
  )
where

import Data.Char (chr, intToDigit, isDigit, ord, toUpper)
import GHC.Float qualified as Float (FFFormat (..), formatRealFloat)
import GHC.Int (Int16, Int32, Int64, Int8)
import GHC.Word (Word16, Word32, Word64, Word8)
import Numeric (showIntAtBase)
import System.IO (Handle, hPutStr, putStr)
import Prelude
  ( Bool (..),
    Bounded (..),
    Char,
    Double,
    Eq (..),
    Float,
    IO,
    Int,
    Integer,
    Integral (..),
    Maybe (..),
    Num (..),
    Ord (..),
    RealFloat,
    Show (..),
    ShowS,
    String,
    Word,
    errorWithoutStackTrace,
    id,
    map,
    negate,
    otherwise,
    replicate,
    reverse,
    showChar,
    showString,
    span,
    toInteger,
    (++),
    (.),
  )

-- | How a field that is narrower than its width is padded.
data FormatAdjustment = LeftAdjust | ZeroPad

-- | What a non-negative number carries in place of a minus sign.
data FormatSign = SignPlus | SignSpace

-- | One conversion of a format string, as the format parser saw it.
data FieldFormat = FieldFormat
  { -- | The minimum field width.
    fmtWidth :: Maybe Int,
    -- | Digits after the point, or the maximum length of a string.
    fmtPrecision :: Maybe Int,
    -- | Left adjustment or zero padding.
    fmtAdjust :: Maybe FormatAdjustment,
    -- | The sign of a non-negative number.
    fmtSign :: Maybe FormatSign,
    -- | The @#@ flag, which asks for the alternate form.
    fmtAlternate :: Bool,
    -- | The modifier characters that preceded the conversion character.
    fmtModifiers :: String,
    -- | The conversion character.
    fmtChar :: Char
  }

-- | The conversion character of a format, with the modifiers that came
-- before it and the rest of the format string.
data FormatParse = FormatParse
  { fpModifiers :: String,
    fpChar :: Char,
    fpRest :: String
  }

-- | Render one argument under a format.
type FieldFormatter = FieldFormat -> ShowS

-- | Take the modifiers and the conversion character off a format string.
type ModifierParser = String -> FormatParse

-- | A type that @printf@ can render.
class PrintfArg a where
  formatArg :: a -> FieldFormatter

  parseFormat :: a -> ModifierParser
  parseFormat _ format =
    case format of
      [] -> errorShortFormat
      character : rest -> FormatParse "" character rest

-- | A character-like type, so that a formatter can work on any string.
class IsChar c where
  toChar :: c -> Char
  fromChar :: Char -> c

instance IsChar Char where
  toChar = id
  fromChar = id

instance PrintfArg Char where
  formatArg = formatChar

instance (IsChar c) => PrintfArg [c] where
  formatArg = formatString

instance PrintfArg Int where
  formatArg = formatInt

instance PrintfArg Int8 where
  formatArg = formatInt

instance PrintfArg Int16 where
  formatArg = formatInt

instance PrintfArg Int32 where
  formatArg = formatInt

instance PrintfArg Int64 where
  formatArg = formatInt

instance PrintfArg Word where
  formatArg = formatInt

instance PrintfArg Word8 where
  formatArg = formatInt

instance PrintfArg Word16 where
  formatArg = formatInt

instance PrintfArg Word32 where
  formatArg = formatInt

instance PrintfArg Word64 where
  formatArg = formatInt

instance PrintfArg Integer where
  formatArg = formatInteger

instance PrintfArg Float where
  formatArg = formatRealFloat

instance PrintfArg Double where
  formatArg = formatRealFloat

-- | Fix the conversion character of a format that asks for the default
-- conversion of its argument.
vFmt :: Char -> FieldFormat -> FieldFormat
vFmt conversion format =
  case fmtChar format == 'v' of
    True -> format {fmtChar = conversion}
    False -> format

-- | Render a string under @%s@.
formatString :: (IsChar a) => [a] -> FieldFormatter
formatString value format =
  case fmtChar (vFmt 's' format) of
    's' -> showString (padPlain format (truncateTo (fmtPrecision format) (map toChar value)))
    other -> errorBadFormat other

-- | Render a character under @%c@, or its code point under a number
-- conversion.
formatChar :: Char -> FieldFormatter
formatChar value format = formatIntegral (toInteger (ord value)) (vFmt 'c' format)

-- | Render a bounded integral type. A negative value under @%u@ wraps
-- around, as it does in C.
formatInt :: (Integral a, Bounded a) => a -> FieldFormatter
formatInt value format =
  case fmtChar wanted of
    'u' ->
      case toInteger value < 0 of
        True -> formatIntegral (toInteger value + wrapAround value) wanted
        False -> formatIntegral (toInteger value) wanted
    _ -> formatIntegral (toInteger value) wanted
  where
    wanted = vFmt 'd' format

-- | The size of the value range of a bounded integral type.
wrapAround :: (Integral a, Bounded a) => a -> Integer
wrapAround value = toInteger (maxBound `asTypeOfValue` value) - toInteger (minBound `asTypeOfValue` value) + 1

asTypeOfValue :: a -> a -> a
asTypeOfValue value _ = value

-- | Render an 'Integer'.
formatInteger :: Integer -> FieldFormatter
formatInteger value format = formatIntegral value (vFmt 'd' format)

-- | Render a floating-point number under @%e@, @%f@ or @%g@.
formatRealFloat :: (RealFloat a) => a -> FieldFormatter
formatRealFloat value format =
  case fmtChar (vFmt 'g' format) of
    'e' -> render Float.FFExponent id
    'E' -> render Float.FFExponent (map toUpper)
    'f' -> render Float.FFFixed id
    'F' -> render Float.FFFixed (map toUpper)
    'g' -> render Float.FFGeneric id
    'G' -> render Float.FFGeneric (map toUpper)
    other -> errorBadFormat other
  where
    render style caseOf =
      showString
        ( padNumber
            format
            (signPrefix format (isNegativeValue value))
            (caseOf (Float.formatRealFloat style (fmtPrecision format) (absoluteValue value)))
        )

isNegativeValue :: (RealFloat a) => a -> Bool
isNegativeValue value = value < 0

absoluteValue :: (RealFloat a) => a -> a
absoluteValue value =
  case value < 0 of
    True -> negate value
    False -> value

-- | The shared rendering of every integral conversion.
formatIntegral :: Integer -> FieldFormatter
formatIntegral value format =
  case fmtChar format of
    'c' -> showString (padPlain format [chr (integerToInt value)])
    'd' -> signed 10 intToDigit
    'i' -> signed 10 intToDigit
    'u' -> signed 10 intToDigit
    'x' -> based 16 intToDigit "0x"
    'X' -> based 16 (toUpper . intToDigit) "0X"
    'o' -> based 8 intToDigit "0"
    'b' -> based 2 intToDigit "0b"
    other -> errorBadFormat other
  where
    digits base toDigit = showIntAtBase base toDigit (absoluteInteger value) ""
    signed base toDigit =
      showString (padNumber format (signPrefix format (value < 0)) (digits base toDigit))
    based base toDigit marker =
      showString (padNumber format (alternatePrefix format marker) (digits base toDigit))

absoluteInteger :: Integer -> Integer
absoluteInteger value =
  case value < 0 of
    True -> negate value
    False -> value

integerToInt :: Integer -> Int
integerToInt = fromInteger

-- | The sign that precedes a number.
signPrefix :: FieldFormat -> Bool -> String
signPrefix format negative =
  case negative of
    True -> "-"
    False ->
      case fmtSign format of
        Just SignPlus -> "+"
        Just SignSpace -> " "
        Nothing -> ""

-- | The @0x@-style marker that the @#@ flag asks for.
alternatePrefix :: FieldFormat -> String -> String
alternatePrefix format marker =
  case fmtAlternate format of
    True -> marker
    False -> ""

-- | Pad a string to the field width with spaces.
padPlain :: FieldFormat -> String -> String
padPlain format text =
  case fmtAdjust format of
    Just LeftAdjust -> text ++ replicate missing ' '
    _ -> replicate missing ' ' ++ text
  where
    missing = missingWidth format text

-- | Pad a rendered number to the field width. Zero padding goes after
-- the sign and the alternate-form marker.
padNumber :: FieldFormat -> String -> String -> String
padNumber format prefix digits =
  case fmtAdjust format of
    Just LeftAdjust -> prefix ++ digits ++ replicate missing ' '
    Just ZeroPad -> prefix ++ replicate missing '0' ++ digits
    Nothing -> replicate missing ' ' ++ prefix ++ digits
  where
    missing = missingWidth format (prefix ++ digits)

-- | How many characters a field is short of its width.
missingWidth :: FieldFormat -> String -> Int
missingWidth format text =
  case fmtWidth format of
    Nothing -> 0
    Just width ->
      case width - charCount text of
        missing
          | missing <= 0 -> 0
          | otherwise -> missing

-- | Keep at most the given number of characters.
truncateTo :: Maybe Int -> String -> String
truncateTo Nothing text = text
truncateTo (Just count) text =
  case count <= 0 of
    True -> ""
    False ->
      case text of
        [] -> []
        character : rest -> character : truncateTo (Just (count - 1)) rest

charCount :: String -> Int
charCount [] = 0
charCount (_ : rest) = 1 + charCount rest

perror :: String -> a
perror message = errorWithoutStackTrace ("printf: " ++ message)

errorBadFormat :: Char -> a
errorBadFormat conversion = perror ("bad formatting char " ++ show conversion)

errorShortFormat :: a
errorShortFormat = perror "formatting string ended prematurely"

errorMissingArgument :: a
errorMissingArgument = perror "argument list ended prematurely"

errorBadArgument :: a
errorBadArgument = perror "bad argument"

-- | One argument of 'printf', with the two operations that the format
-- interpreter needs from it.
data UPrintf = UPrintf ModifierParser FieldFormatter

toUPrintf :: (PrintfArg a) => a -> UPrintf
toUPrintf value = UPrintf (parseFormat value) (formatArg value)

argumentParser :: UPrintf -> ModifierParser
argumentParser (UPrintf parser _) = parser

argumentFormatter :: UPrintf -> FieldFormatter
argumentFormatter (UPrintf _ formatter) = formatter

-- | A result type of 'printf'.
--
-- The instances give 'printf' its variable number of arguments. The result is
-- a string, or an action that writes to the standard output.
class PrintfType t where
  spr :: String -> [UPrintf] -> t

-- | A result type of 'hPrintf'.
class HPrintfType t where
  hspr :: Handle -> String -> [UPrintf] -> t

instance (IsChar c) => PrintfType [c] where
  spr format arguments = map fromChar (uprintf format (reverse arguments))

instance (a ~ ()) => PrintfType (IO a) where
  spr format arguments = putStr (uprintf format (reverse arguments))

instance (a ~ ()) => HPrintfType (IO a) where
  hspr handle format arguments = hPutStr handle (uprintf format (reverse arguments))

instance (PrintfArg a, PrintfType r) => PrintfType (a -> r) where
  spr format arguments value = spr format (toUPrintf value : arguments)

instance (PrintfArg a, HPrintfType r) => HPrintfType (a -> r) where
  hspr handle format arguments value = hspr handle format (toUPrintf value : arguments)

-- | Format the arguments and give a string, or write them to the standard
-- output.
--
-- The format string holds text and conversions. A conversion starts with
-- @%@ and ends with a conversion character. Between them it can have flags,
-- a field width, and a precision. @%%@ gives one @%@ character.
printf :: (PrintfType r) => String -> r
printf format = spr format []

-- | Format the arguments and write them to a handle.
hPrintf :: (HPrintfType r) => Handle -> String -> r
hPrintf handle format = hspr handle format []

-- | Apply a format string to the arguments that it names.
uprintf :: String -> [UPrintf] -> String
uprintf format arguments = renderFormat format arguments ""

renderFormat :: String -> [UPrintf] -> ShowS
renderFormat [] [] = id
renderFormat [] (_ : _) = errorShortFormat
renderFormat ('%' : '%' : rest) arguments = showChar '%' . renderFormat rest arguments
renderFormat ('%' : rest) arguments = renderConversion rest arguments
renderFormat (character : rest) arguments = showChar character . renderFormat rest arguments

-- | The flags that come directly after the @%@ character.
data Flags = Flags
  { flagLeft :: Bool,
    flagZero :: Bool,
    flagPlus :: Bool,
    flagSpace :: Bool,
    flagAlternate :: Bool
  }

noFlags :: Flags
noFlags = Flags False False False False False

-- | Left adjustment has priority over zero padding, as it has in C.
flagsAdjustment :: Flags -> Maybe FormatAdjustment
flagsAdjustment flags
  | flagLeft flags = Just LeftAdjust
  | flagZero flags = Just ZeroPad
  | otherwise = Nothing

-- | A plus sign has priority over a space, as it has in C.
flagsSign :: Flags -> Maybe FormatSign
flagsSign flags
  | flagPlus flags = Just SignPlus
  | flagSpace flags = Just SignSpace
  | otherwise = Nothing

readFlags :: Flags -> String -> (Flags, String)
readFlags flags ('-' : rest) = readFlags flags {flagLeft = True} rest
readFlags flags ('0' : rest) = readFlags flags {flagZero = True} rest
readFlags flags ('+' : rest) = readFlags flags {flagPlus = True} rest
readFlags flags (' ' : rest) = readFlags flags {flagSpace = True} rest
readFlags flags ('#' : rest) = readFlags flags {flagAlternate = True} rest
readFlags flags rest = (flags, rest)

-- | Read the field width. A @*@ takes the width from the next argument.
readWidth :: String -> [UPrintf] -> (Maybe Int, String, [UPrintf])
readWidth ('*' : rest) arguments =
  case arguments of
    [] -> errorMissingArgument
    argument : remaining -> (Just (argumentInt argument), rest, remaining)
readWidth text arguments =
  case span isDigit text of
    ([], _) -> (Nothing, text, arguments)
    (digits, rest) -> (Just (decimalValue digits), rest, arguments)

-- | Read the precision. A @*@ takes the precision from the next argument.
readPrecision :: String -> [UPrintf] -> (Maybe Int, String, [UPrintf])
readPrecision ('.' : '*' : rest) arguments =
  case arguments of
    [] -> errorMissingArgument
    argument : remaining -> (Just (argumentInt argument), rest, remaining)
readPrecision ('.' : text) arguments =
  case span isDigit text of
    (digits, rest) -> (Just (decimalValue digits), rest, arguments)
readPrecision text arguments = (Nothing, text, arguments)

-- | Render one conversion and then the rest of the format string.
renderConversion :: String -> [UPrintf] -> ShowS
renderConversion text arguments =
  case readFlags noFlags text of
    (flags, afterFlags) ->
      case readWidth afterFlags arguments of
        (width, afterWidth, widthArguments) ->
          case readPrecision afterWidth widthArguments of
            (precision, afterPrecision, precisionArguments) ->
              case precisionArguments of
                [] -> errorMissingArgument
                argument : remaining ->
                  case argumentParser argument afterPrecision of
                    FormatParse modifiers conversion rest ->
                      argumentFormatter
                        argument
                        FieldFormat
                          { fmtWidth = width,
                            fmtPrecision = precision,
                            fmtAdjust = flagsAdjustment flags,
                            fmtSign = flagsSign flags,
                            fmtAlternate = flagAlternate flags,
                            fmtModifiers = modifiers,
                            fmtChar = conversion
                          }
                        . renderFormat rest remaining

-- | The value of an argument that gives a width or a precision.
argumentInt :: UPrintf -> Int
argumentInt argument = signedDecimalValue (argumentFormatter argument plainIntegerFormat "")

-- | The format that renders an argument as a plain decimal number.
plainIntegerFormat :: FieldFormat
plainIntegerFormat =
  FieldFormat
    { fmtWidth = Nothing,
      fmtPrecision = Nothing,
      fmtAdjust = Nothing,
      fmtSign = Nothing,
      fmtAlternate = False,
      fmtModifiers = "",
      fmtChar = 'd'
    }

signedDecimalValue :: String -> Int
signedDecimalValue ('-' : digits) = negate (decimalValue digits)
signedDecimalValue digits = decimalValue digits

decimalValue :: String -> Int
decimalValue = go 0
  where
    go total [] = total
    go total (character : rest) = go (total * 10 + (ord character - ord '0')) rest
