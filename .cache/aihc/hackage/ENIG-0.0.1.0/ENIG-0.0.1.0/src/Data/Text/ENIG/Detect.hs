module Data.Text.ENIG.Detect where

import Data.Text.ENIG.Config
import Data.Text.ENIG.Data


import Data.Char

import           Data.Text (Text)
import qualified Data.Text as T

import Data.Text.Normalize

import qualified Data.Vector.Unboxed as V


-- | Detect whether the last component is an last consonant or not
isLastConsonant :: Int -> Bool
isLastConsonant hangulCode =
  (head lastConsonantCodeList <= hangulCode) && (hangulCode <= last lastConsonantCodeList)

-- | Detect whether the last component is an vowel or not
--   To avoid the issue with the first consonant, like 'ㄱ'.
isLastVowel :: Int -> Bool
isLastVowel hangulCode =
  (head vowelCodeList <= hangulCode) && (hangulCode <= last vowelCodeList)

-- | Detect whether the last component is the last consonant 'ㄹ'
isLastR :: Int -> Bool
isLastR hangulCode = hangulCode == 4527

-- | Detect whether the given code is an Hangul component or not
isHangul :: Int -> Bool
isHangul mHangulCode =
  (head hangulComponentCodeList <= mHangulCode) && (mHangulCode <= last hangulComponentCodeList)

-- | Get the last component's code of given Hangel
getLastComponentCode :: Text -> Int
getLastComponentCode str = ord aComponent
  where
    lastChar = T.singleton . T.last $ str
    aComponent = T.last . normalize NFKD $ lastChar
