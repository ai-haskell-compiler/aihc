{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ENIG.Detect where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base

import Data.Char
import qualified Data.Text as T
import Data.Text.Normalize

import Data.Text.ENIG.Data
import Data.Text.ENIG.Detect

tests = $(testGroupGenerator)


preHangulStrList = ["과자", "무엇", "한글", "ㄱ", "ㅏ"]


c_11 = 4449 @?= getLastComponentCode arg
  where arg = preHangulStrList !! 0
c_12 = 4538 @?= getLastComponentCode arg
  where arg = preHangulStrList !! 1
c_13 = 4527 @?= getLastComponentCode arg
  where arg = preHangulStrList !! 2

test_ComponentCode =
  [ testCase "lastComponent(과자) = ㅏ" c_11
  , testCase "lastComponent(무엇) = ㅅ" c_12
  , testCase "lastComponent(한글) = ㄹ" c_13
  ]

c_21 = assertBool "" (not . isLastConsonant . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 0
c_22 = assertBool "" (isLastConsonant . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 1
c_23 = assertBool "" (isLastConsonant . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 2

test_isLastConsonant =
  [ testCase "isLastConsonant(과자): False" c_21
  , testCase "isLastConsonant(무엇): True"  c_22
  , testCase "isLastConsonant(한글): True"  c_23
  ]

c_31 = assertBool "" (not . isLastR . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 0
c_32 = assertBool "" (not . isLastR . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 1
c_33 = assertBool "" (isLastR . getLastComponentCode $ arg)
  where arg = preHangulStrList !! 2

test_isLastR =
  [ testCase "isLastR(과자): False" c_31
  , testCase "isLastR(무엇): False" c_32
  , testCase "isLastR(한글): True"  c_33
  ]

