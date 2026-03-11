{-# LANGUAGE OverloadedStrings #-}

module Data.Text.ENIG where

import Data.Text.ENIG.Config
import Data.Text.ENIG.Data
import Data.Text.ENIG.Detect
import Data.Text.ENIG.Show


import Data.Char

import           Data.Text (Text)
import qualified Data.Text as T

import Data.Text.Normalize

import qualified Data.Vector.Unboxed as V


-- | Return proper and minimal PPP about given text
--   주어진 단어와 조사의 종류에 대해서 최소한의 적절한 조사의 문자열을 반환함
--
-- >>> enigPPP "과자" WG
-- "와"
-- >>> enigPPP "무엇" WG
-- "과"
enigPPP :: Text -> PPPCategory -> Text
enigPPP inputStr pppCa
  | isHangul lastComponent =
    tShowPPPId . (toEnum :: Int -> PPPIdentity) $
      if isSecondType
        then snd selectedPPPIPair
        else fst selectedPPPIPair
  | isDigit lastComponent = error "enigPPPByDigit is not implemented"
  | otherwise = tShowPPPCa pppCa
  where
    isSecondType = isLastVowel lastComponent || (pppCa == EuX && isLastR lastComponent)
    selectedPPPIPair = pppidVector V.! fromEnum pppCa
    -- preprocessForPPP for handling parenthesis, quatation, etc.
    preprocessForPPP = id
    preprocessed = preprocessForPPP inputStr
    lastComponent = getLastComponentCode preprocessed
    -- handling PPP with digit is not implemented
    isDigit _ = False

-- | Return proper PPP about given text with post text
--   주어진 단어와 조사의 종류에 대해서 적절한 조사의 문자열을 반환함
--
-- >>> enigPPP "과자" EuX "로"
-- "로"
-- >>> enigPPP "무엇" EuX "로"
-- "으로"
enigPPPWithPost :: Text -> PPPCategory -> Text -> Text
enigPPPWithPost inputStr pppCa = T.append (enigPPP inputStr pppCa)


{-


# Pattern

* 무엇(으)로
* 무엇을(를)

# Problems

* How to Identify pattern without false positive
  * Case #1: Not 'IX'/'EuX'
  * Case #2: 'IX'/'EuX'

# Tasks

* Try to use 'Builder' or producer/consumer model

# Development Steps

1. Implement with 'List'
2. Implement with tail recursive with 'Char' and 'Text'
-}
-- | Find replacing pattern and apply enigPPP from given text automatically
-- TODO: Now working by reverse order.
enigAuto :: Text -> Text
-- ERROR: Qualified name in binding position: T.empty
-- enigAuto T.empty = T.empty
enigAuto "" = T.empty
enigAuto input =
  maybe
    (T.append target (enigAuto rest))
    (\(found, ppp, post) -> T.concat [found, enigPPP found ppp, post, enigAuto rest])
    result
  where
    (target,rest) = getTarget "" input
    result = findingPPPPattern target

getTarget :: Text -> Text -> (Text,Text)
getTarget prior "" = (prior,"")
getTarget prior input = if T.null target
  then getTarget (T.append prior (T.take 1 input)) (T.drop 1 input)
  else (T.append prior target,rest)
  where
    (target,rest) = T.break isDelimiter input

findingPPPPattern :: Text -> Maybe (Text,PPPCategory,Text)
findingPPPPattern input =
  maybe
    Nothing
    (\x -> Just (T.dropEnd (T.length . tFst $ x) input, tSnd x, tTrd x))
    (match' (\x -> T.isSuffixOf (tFst x) input) autoPatternList)
  where
   tFst (a,_,_) = a
   tSnd (_,b,_) = b
   tTrd (_,_,c) = c

match' _ [] = Nothing
match' condition (x:xs) = if condition x then Just x else match' condition xs

isDelimiter '(' = False
isDelimiter ')' = False
isDelimiter x = Prelude.any (\f -> f x) [isSpace, isPunctuation, isMark, isSeparator]
