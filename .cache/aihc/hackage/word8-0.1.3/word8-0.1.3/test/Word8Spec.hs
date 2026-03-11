module Word8Spec where

import qualified Data.Char as C
import Data.Word8
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "isControl" $ do
        prop "behaves like model" $ \w ->
            isControl w == C.isControl (word8ToChar w)

    describe "isSpace" $ do
        prop "behaves like model" $ \w ->
            isSpace w == C.isSpace (word8ToChar w)

    describe "isLower" $ do
        prop "behaves like model" $ \w ->
            isLower w == C.isLower (word8ToChar w)

    describe "isUpper" $ do
        prop "behaves like model" $ \w ->
            isUpper w == C.isUpper (word8ToChar w)

    describe "isAlpha" $ do
        prop "behaves like model" $ \w ->
            isAlpha w == C.isAlpha (word8ToChar w)

    describe "isAlphaNum" $ do
        prop "behaves like model" $ \w ->
            isAlphaNum w == C.isAlphaNum (word8ToChar w)

    describe "isPrint" $ do
        prop "behaves like model" $ \w ->
            isPrint w == C.isPrint (word8ToChar w)

    describe "isDigit" $ do
        prop "behaves like model" $ \w ->
            isDigit w == C.isDigit (word8ToChar w)

    describe "isOctDigit" $ do
        prop "behaves like model" $ \w ->
            isOctDigit w == C.isOctDigit (word8ToChar w)

    describe "isHexDigit" $ do
        prop "behaves like model" $ \w ->
            isHexDigit w == C.isHexDigit (word8ToChar w)

    describe "isLetter" $ do
        prop "behaves like model" $ \w ->
            isLetter w == C.isLetter (word8ToChar w)

    describe "isMark" $ do
        prop "behaves like model" $ \w ->
            isMark w == C.isMark (word8ToChar w)

    describe "isNumber" $ do
        prop "behaves like model" $ \w ->
            isNumber w == C.isNumber (word8ToChar w)

    describe "isPunctuation" $ do
        prop "behaves like model" $ \w ->
            isPunctuation w == C.isPunctuation (word8ToChar w)

    describe "isSymbol" $ do
        prop "behaves like model" $ \w ->
            isSymbol w == C.isSymbol (word8ToChar w)

    describe "isSeparator" $ do
        prop "behaves like model" $ \w ->
            isSeparator w == C.isSeparator (word8ToChar w)

    describe "isAscii" $ do
        prop "behaves like model" $ \w ->
            isAscii w == C.isAscii (word8ToChar w)

    describe "isLatin1" $ do
        prop "behaves like model" $ \w ->
            isLatin1 w == C.isLatin1 (word8ToChar w)

    describe "isAsciiUpper" $ do
        prop "behaves like model" $ \w ->
            isAsciiUpper w == C.isAsciiUpper (word8ToChar w)

    describe "isAsciiLower" $ do
        prop "behaves like model" $ \w ->
            isAsciiLower w == C.isAsciiLower (word8ToChar w)

    describe "toUpper" $ do
        prop "behaves like model" $ prop_toUpper

    describe "toLower" $ do
        prop "behaves like model" $ \w ->
            word8ToChar (toLower w) == C.toLower (word8ToChar w)

    describe "toTitle" $ do
        prop "behaves like model" $ prop_toTitle

prop_toUpper :: Word8 -> Bool
prop_toUpper w
  | w == _mu        = True
  | w == _ydieresis = True
  | otherwise       = word8ToChar (toUpper w) == C.toUpper (word8ToChar w)

prop_toTitle :: Word8 -> Bool
prop_toTitle w
  | w == _mu  = True
  | w == _ydieresis = True
  | otherwise = word8ToChar (toTitle w) == C.toTitle (word8ToChar w)

----------------------------------------------------------------

word8ToChar :: Word8 -> Char
word8ToChar = C.chr . fromIntegral
