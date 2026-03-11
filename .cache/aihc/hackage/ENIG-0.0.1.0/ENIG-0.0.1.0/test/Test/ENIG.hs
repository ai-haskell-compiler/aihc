{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Test.ENIG where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base

import qualified Data.Text as T
import Data.Text.Normalize

import Data.Text.ENIG
import Data.Text.ENIG.Data
import Data.Text.ENIG.Show


tests = $(testGroupGenerator)


test_UsualCases =
  [ testCase "과자을(를) => 과자를" c_01
  , testCase "무엇을(를) => 무엇을" c_02
  , testCase "한글을(를) => 한글을" c_03
  ]

test_EuXCases =
  [ testCase "과자(으)로 => 과자로" c_11
  , testCase "무엇(으)로 => 무엇으로" c_12
  , testCase "한글(으)로 => 한글로" c_13
  ]

test_AutoBaseCases =
  [ testCase "enigAuto 과자을(를) = 과자를" c_a01
  , testCase "enigAuto 무엇을(를) = 무엇을" c_a02
  , testCase "enigAuto 한글을(를) = 한글을" c_a03
  , testCase "enigAuto 과자(으)로 = 과자로" c_a11
  , testCase "enigAuto 무엇(으)로 = 무엇으로" c_a12
  , testCase "enigAuto 한글(으)로 = 한글로" c_a13
  ]

test_AutoComplexPatternCases =
  [ testCase "enigAuto 과자을(를) = 과자를" c_acp01
  , testCase "enigAuto 무엇을(를) = 무엇을" c_acp02
  , testCase "enigAuto 한글을(를) = 한글을" c_acp03
  , testCase "enigAuto 과자(으)로 = 과자로" c_acp11
  , testCase "enigAuto 무엇(으)로 = 무엇으로" c_acp12
  , testCase "enigAuto 한글(으)로 = 한글로" c_acp13
  ]

preHangulStrList = ["과자", "무엇", "한글", "ㄱ", "ㅏ"]
preNotHangulStrList = ["1", "2", "test", "harm"]

c_01 = tShowPPPId Leul @?= (enigPPP  arg EL)
  where arg = preHangulStrList !! 0
c_02 = tShowPPPId Eul @?= (enigPPP  arg EL)
  where arg = preHangulStrList !! 1
c_03 = tShowPPPId Eul @?= (enigPPP  arg EL)
  where arg = preHangulStrList !! 3

c_11 = tShowPPPId X @?= (enigPPP  arg EuX)
  where arg = preHangulStrList !! 0
c_12 = tShowPPPId Eux @?= (enigPPP  arg EuX)
  where arg = preHangulStrList !! 1
c_13 = tShowPPPId X @?= (enigPPP  arg EuX)
  where arg = preHangulStrList !! 2

c_a01 = enigAuto "과자을(를)" @?= "과자를"
c_a02 = enigAuto "무엇을(를)" @?= "무엇을"
c_a03 = enigAuto "한글을(를)"  @?= "한글을"

c_a11 = enigAuto "과자(으)로" @?= "과자로"
c_a12 = enigAuto "무엇(으)로" @?= "무엇으로"
c_a13 = enigAuto "한글(으)로" @?= "한글로"

c_acp01 = enigAuto "나은(는) 과자을(를) 먹었다." @?= "나는 과자를 먹었다."
c_acp02 = enigAuto "너은(는) 무엇을(를) 하였느냐?" @?= "너는 무엇을 하였느냐?"
c_acp03 = enigAuto "네, 한글을(를) 다루었습니다."  @?= "네, 한글을 다루었습니다."

c_acp11 = enigAuto "배고픔을 과자(으)로 때웠다" @?= "배고픔을 과자로 때웠다"
c_acp12 = enigAuto "이것은, 무엇(으)로 만들었는가?" @?= "이것은, 무엇으로 만들었는가?"
c_acp13 = enigAuto "예(Yes)! 한글(으)로 썼습니다" @?= "예(Yes)! 한글로 썼습니다"

-- TODO: Add enigAuto test code
