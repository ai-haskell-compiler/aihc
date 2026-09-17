{-# LANGUAGE MagicHash #-}

module GHC.Unicode
  ( GeneralCategory (..),
    generalCategory,
    isAlpha,
    isAlphaNum,
    isAscii,
    isAsciiLower,
    isAsciiUpper,
    isControl,
    isDigit,
    isHexDigit,
    isLatin1,
    isLower,
    isLowerCase,
    isOctDigit,
    isPrint,
    isPunctuation,
    isSpace,
    isSymbol,
    isUpper,
    isUpperCase,
    toLower,
    toTitle,
    toUpper,
  )
where

import Data.Maybe (fromMaybe)
import GHC.Base (ord)
import GHC.Enum (boundedEnumFrom, boundedEnumFromThen, toEnumError)
import GHC.Int (Int (..))
import GHC.Ix (Ix (..), indexError)
import GHC.Prim (Int#)
import GHC.Prim.Unicode
  ( generalCategory#,
    isLowercase#,
    isUppercase#,
    unicodeToLower,
    unicodeToTitle,
    unicodeToUpper,
  )
import Prelude

data GeneralCategory
  = UppercaseLetter
  | LowercaseLetter
  | TitlecaseLetter
  | ModifierLetter
  | OtherLetter
  | NonSpacingMark
  | SpacingCombiningMark
  | EnclosingMark
  | DecimalNumber
  | LetterNumber
  | OtherNumber
  | ConnectorPunctuation
  | DashPunctuation
  | OpenPunctuation
  | ClosePunctuation
  | InitialQuote
  | FinalQuote
  | OtherPunctuation
  | MathSymbol
  | CurrencySymbol
  | ModifierSymbol
  | OtherSymbol
  | Space
  | LineSeparator
  | ParagraphSeparator
  | Control
  | Format
  | Surrogate
  | PrivateUse
  | NotAssigned
  deriving (Show, Eq, Ord, Bounded)

-- | Stock 'Enum' and 'Ix' deriving are not available yet, so the
-- instances GHC derives are written out, numbering the constructors in
-- declaration order like the derived ones.
instance Enum GeneralCategory where
  toEnum (I# value) =
    case categoryFromNumber value of
      Just category -> category
      Nothing -> toEnumError "GeneralCategory" (I# value) (UppercaseLetter, NotAssigned)
  fromEnum category = I# (categoryNumber category)
  enumFrom = boundedEnumFrom
  enumFromThen = boundedEnumFromThen

instance Ix GeneralCategory where
  range (lower, upper) = enumFromTo lower upper
  unsafeIndex (lower, _) value = fromEnum value - fromEnum lower
  index bounds value =
    case inRange bounds value of
      True -> unsafeIndex bounds value
      False -> indexError bounds value "GeneralCategory"
  inRange (lower, upper) value = fromEnum lower <= fromEnum value && fromEnum value <= fromEnum upper

generalCategory :: Char -> GeneralCategory
generalCategory (C# value) = fromMaybe NotAssigned (categoryFromNumber (generalCategory# value))

-- | The constructor at a category number, in the order the Unicode
-- primitive and the derived 'Enum' instance both use.
categoryFromNumber :: Int# -> Maybe GeneralCategory
categoryFromNumber value =
  case value of
    0# -> Just UppercaseLetter
    1# -> Just LowercaseLetter
    2# -> Just TitlecaseLetter
    3# -> Just ModifierLetter
    4# -> Just OtherLetter
    5# -> Just NonSpacingMark
    6# -> Just SpacingCombiningMark
    7# -> Just EnclosingMark
    8# -> Just DecimalNumber
    9# -> Just LetterNumber
    10# -> Just OtherNumber
    11# -> Just ConnectorPunctuation
    12# -> Just DashPunctuation
    13# -> Just OpenPunctuation
    14# -> Just ClosePunctuation
    15# -> Just InitialQuote
    16# -> Just FinalQuote
    17# -> Just OtherPunctuation
    18# -> Just MathSymbol
    19# -> Just CurrencySymbol
    20# -> Just ModifierSymbol
    21# -> Just OtherSymbol
    22# -> Just Space
    23# -> Just LineSeparator
    24# -> Just ParagraphSeparator
    25# -> Just Control
    26# -> Just Format
    27# -> Just Surrogate
    28# -> Just PrivateUse
    29# -> Just NotAssigned
    _ -> Nothing

categoryNumber :: GeneralCategory -> Int#
categoryNumber category =
  case category of
    UppercaseLetter -> 0#
    LowercaseLetter -> 1#
    TitlecaseLetter -> 2#
    ModifierLetter -> 3#
    OtherLetter -> 4#
    NonSpacingMark -> 5#
    SpacingCombiningMark -> 6#
    EnclosingMark -> 7#
    DecimalNumber -> 8#
    LetterNumber -> 9#
    OtherNumber -> 10#
    ConnectorPunctuation -> 11#
    DashPunctuation -> 12#
    OpenPunctuation -> 13#
    ClosePunctuation -> 14#
    InitialQuote -> 15#
    FinalQuote -> 16#
    OtherPunctuation -> 17#
    MathSymbol -> 18#
    CurrencySymbol -> 19#
    ModifierSymbol -> 20#
    OtherSymbol -> 21#
    Space -> 22#
    LineSeparator -> 23#
    ParagraphSeparator -> 24#
    Control -> 25#
    Format -> 26#
    Surrogate -> 27#
    PrivateUse -> 28#
    NotAssigned -> 29#

isAscii :: Char -> Bool
isAscii value = ord value < 128

isLatin1 :: Char -> Bool
isLatin1 value = ord value <= 255

isAsciiLower :: Char -> Bool
isAsciiLower value =
  let codePoint = ord value
   in codePoint >= 97 && codePoint <= 122

isAsciiUpper :: Char -> Bool
isAsciiUpper value =
  let codePoint = ord value
   in codePoint >= 65 && codePoint <= 90

isControl :: Char -> Bool
isControl value =
  case generalCategory value of
    Control -> True
    _ -> False

isPrint :: Char -> Bool
isPrint value =
  case generalCategory value of
    LineSeparator -> False
    ParagraphSeparator -> False
    Control -> False
    Format -> False
    Surrogate -> False
    PrivateUse -> False
    NotAssigned -> False
    _ -> True

isSpace :: Char -> Bool
isSpace value =
  let codePoint = ord value
   in (codePoint >= 9 && codePoint <= 13)
        || case generalCategory value of
          Space -> True
          _ -> False

isUpper :: Char -> Bool
isUpper value =
  case generalCategory value of
    UppercaseLetter -> True
    TitlecaseLetter -> True
    _ -> False

isUpperCase :: Char -> Bool
isUpperCase (C# value) = intHashToBool (isUppercase# value)

isLower :: Char -> Bool
isLower value =
  case generalCategory value of
    LowercaseLetter -> True
    _ -> False

isLowerCase :: Char -> Bool
isLowerCase (C# value) = intHashToBool (isLowercase# value)

isAlpha :: Char -> Bool
isAlpha = isLetter

isDigit :: Char -> Bool
isDigit value =
  let codePoint = ord value
   in codePoint >= 48 && codePoint <= 57

isOctDigit :: Char -> Bool
isOctDigit value =
  let codePoint = ord value
   in codePoint >= 48 && codePoint <= 55

isHexDigit :: Char -> Bool
isHexDigit value =
  let codePoint = ord value
   in isDigit value
        || (codePoint >= 65 && codePoint <= 70)
        || (codePoint >= 97 && codePoint <= 102)

isAlphaNum :: Char -> Bool
isAlphaNum value = isAlpha value || isNumber value

isPunctuation :: Char -> Bool
isPunctuation value =
  case generalCategory value of
    ConnectorPunctuation -> True
    DashPunctuation -> True
    OpenPunctuation -> True
    ClosePunctuation -> True
    InitialQuote -> True
    FinalQuote -> True
    OtherPunctuation -> True
    _ -> False

isSymbol :: Char -> Bool
isSymbol value =
  case generalCategory value of
    MathSymbol -> True
    CurrencySymbol -> True
    ModifierSymbol -> True
    OtherSymbol -> True
    _ -> False

toUpper :: Char -> Char
toUpper (C# value) = C# (unicodeToUpper value)

toLower :: Char -> Char
toLower (C# value) = C# (unicodeToLower value)

toTitle :: Char -> Char
toTitle (C# value) = C# (unicodeToTitle value)

intHashToBool :: Int# -> Bool
intHashToBool value =
  case value of
    0# -> False
    _ -> True

-- Data.Char exports these predicates; GHC.Unicode only uses them.
isLetter :: Char -> Bool
isLetter value =
  case generalCategory value of
    UppercaseLetter -> True
    LowercaseLetter -> True
    TitlecaseLetter -> True
    ModifierLetter -> True
    OtherLetter -> True
    _ -> False

isMark :: Char -> Bool
isMark value =
  case generalCategory value of
    NonSpacingMark -> True
    SpacingCombiningMark -> True
    EnclosingMark -> True
    _ -> False

isNumber :: Char -> Bool
isNumber value =
  case generalCategory value of
    DecimalNumber -> True
    LetterNumber -> True
    OtherNumber -> True
    _ -> False

isSeparator :: Char -> Bool
isSeparator value =
  case generalCategory value of
    Space -> True
    LineSeparator -> True
    ParagraphSeparator -> True
    _ -> False
