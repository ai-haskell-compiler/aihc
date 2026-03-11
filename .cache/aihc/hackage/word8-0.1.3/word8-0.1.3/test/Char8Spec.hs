module Char8Spec where

import qualified Data.Char as C
import Data.Char8
import Data.Word (Word8)
import Word8Spec (word8ToChar)
import Test.Hspec
import Test.Hspec.QuickCheck

spec :: Spec
spec = do
    describe "isControl" $ do
        prop "behaves like model" $ ensure $ \c ->
            isControl c == C.isControl c

    describe "isSpace" $ do
        prop "behaves like model" $ ensure $ \c ->
            isSpace c == C.isSpace c

    describe "isLower" $ do
        prop "behaves like model" $ ensure $ \c ->
            isLower c == C.isLower c

    describe "isUpper" $ do
        prop "behaves like model" $ ensure $ \c ->
            isUpper c == C.isUpper c

    describe "isAlpha" $ do
        prop "behaves like model" $ ensure $ \c ->
            isAlpha c == C.isAlpha c

    describe "isAlphaNum" $ do
        prop "behaves like model" $ ensure $ \c ->
            isAlphaNum c == C.isAlphaNum c

    describe "isPrint" $ do
        prop "behaves like model" $ ensure $ \c ->
            isPrint c == C.isPrint c

    describe "isDigit" $ do
        prop "behaves like model" $ ensure $ \c ->
            isDigit c == C.isDigit c

    describe "isOctDigit" $ do
        prop "behaves like model" $ ensure $ \c ->
            isOctDigit c == C.isOctDigit c

    describe "isHexDigit" $ do
        prop "behaves like model" $ ensure $ \c ->
            isHexDigit c == C.isHexDigit c

    describe "isLetter" $ do
        prop "behaves like model" $ ensure $ \c ->
            isLetter c == C.isLetter c

    describe "isMark" $ do
        prop "behaves like model" $ ensure $ \c ->
            isMark c == C.isMark c

    describe "isNumber" $ do
        prop "behaves like model" $ ensure $ \c ->
            isNumber c == C.isNumber c

    describe "isPunctuation" $ do
        prop "behaves like model" $ ensure $ \c ->
            isPunctuation c == C.isPunctuation c

    describe "isSymbol" $ do
        prop "behaves like model" $ ensure $ \c ->
            isSymbol c == C.isSymbol c

    describe "isSeparator" $ do
        prop "behaves like model" $ ensure $ \c ->
            isSeparator c == C.isSeparator c

    describe "isAscii" $ do
        prop "behaves like model" $ ensure $ \c ->
            isAscii c == C.isAscii c

    describe "isLatin1" $ do
        prop "behaves like model" $ ensure $ \c ->
            isLatin1 c == C.isLatin1 c

    describe "isAsciiUpper" $ do
        prop "behaves like model" $ ensure $ \c ->
            isAsciiUpper c == C.isAsciiUpper c

    describe "isAsciiLower" $ do
        prop "behaves like model" $ ensure $ \c ->
            isAsciiLower c == C.isAsciiLower c

    describe "toUpper" $ do
        prop "behaves like model" $ ensure $ prop_toUpper

    describe "toLower" $ do
        prop "behaves like model" $ ensure $ \c ->
            toLower c == C.toLower c

    describe "toTitle" $ do
        prop "behaves like model" $ ensure $ prop_toTitle

prop_toUpper :: Char -> Bool
prop_toUpper c
  | c == _mu        = True
  | c == _ydieresis = True
  | otherwise       = toUpper c == C.toUpper c

prop_toTitle :: Char -> Bool
prop_toTitle c
  | c == _mu  = True
  | c == _ydieresis = True
  | otherwise = toTitle c == C.toTitle c

----------------------------------------------------------------

_mu, _ydieresis :: Char
_mu        = '\xb5'
_ydieresis = '\xff'

----------------------------------------------------------------

ensure :: (Char -> Bool) -> Word8 -> Bool
ensure body w = body (word8ToChar w)
