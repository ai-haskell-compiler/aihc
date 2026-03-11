{-# LANGUAGE TemplateHaskell #-}

module Test.Flag where


import Data.Flag.Simple
import Data.Word

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.TH
import Test.HUnit.Base
import Test.QuickCheck

import Test.Flag.Env


tests = $(testGroupGenerator)

test_showFlag =
  [ testCase "\"0000000000000000000000000000000000000000000000000000000001001001\" == showFlag  73" c_sF01
  , testCase "\"0000000000000000000000000000000000000000000000000000000001111101\" == showFlag 125" c_sF02
  , testCase "\"0000000000000000000000000000000000000000000000000000000011001011\" == showFlag 203" c_sF03
  , testCase "\"0000000000000000000000000000000000000000000000000000000011001111\" == showFlag 207" c_sF04
  , testCase "\"0000000000000000000000000000000000000000000000000000000101001001\" == showFlag 329" c_sF05
  ]

c_sF01 = "0000000000000000000000000000000000000000000000000000000001001001" @=? showFlag  73
c_sF02 = "0000000000000000000000000000000000000000000000000000000001111101" @=? showFlag 125
c_sF03 = "0000000000000000000000000000000000000000000000000000000011001011" @=? showFlag 203
c_sF04 = "0000000000000000000000000000000000000000000000000000000011001111" @=? showFlag 207
c_sF05 = "0000000000000000000000000000000000000000000000000000000101001001" @=? showFlag 329

test_eqAbout =
  [ testCase "eqAbout 125 203 329 == True" c_eA01
  , testCase "eqAbout 125 207 329 == False" c_eA02
  ]

c_eA01 = assertBool ""        (eqAbout 125 203 329)
c_eA02 = assertBool "" (not $ eqAbout 125 207 329)

-- To understand `readFlag` with any character which is not '0' or '1', read the document.
filterFlag = readFlag "0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F0F"

test_includeAbout =
  [ testCase "includeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1110111011101110') == True" c_iA01
  , testCase "includeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1010101010101110') == True" c_iA02
  , testCase "includeAbout (Flag '0101010101010101') (Flag '1110111001000100') (Flag '1110111011101110') == True" c_iA03
  , testCase "includeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1110111011101111') == False" c_iA04
  , testCase "includeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1010101010101011') == False" c_iA05
  ]

c_iA01 = assertBool ""       (includeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_1_0_1_0_1_0_1_0"))
c_iA02 = assertBool ""       (includeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_1_0_0_0_0_0_1_0"))
c_iA03 = assertBool ""       (includeAbout filterFlag (readFlag "_1_0_1_001000100") (readFlag "_1_0_1_0_1_0_1_0"))
c_iA04 = assertBool "" (not $ includeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_1_0_1_0_1_0_1_1"))
c_iA05 = assertBool "" (not $ includeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_0_0_0_0_0_0_0_1"))

test_excludeAbout =
  [ testCase "excludeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1110111011101110') == True"  c_xA01
  , testCase "excludeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1010101010101110') == True"  c_xA02
  , testCase "excludeAbout (Flag '0101010101010101') (Flag '1110111001000100') (Flag '1110111011101110') == True"  c_xA03
  , testCase "excludeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1110111011101111') == False" c_xA04
  , testCase "excludeAbout (Flag '0101010101010101') (Flag '1110111011101110') (Flag '1010101010101011') == False" c_xA05
  ]

c_xA01 = assertBool ""       (excludeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_0_1_0_1_0_1_0_1"))
c_xA02 = assertBool ""       (excludeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_0_1_0_0_0_1_0_0"))
c_xA03 = assertBool ""       (excludeAbout filterFlag (readFlag "_1_0_1_001000100") (readFlag "_0_1_0_1_0_1_0_1"))
c_xA04 = assertBool "" (not $ excludeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_1_0_1_0_1_0_1_1"))
c_xA05 = assertBool ""       (excludeAbout filterFlag (readFlag "_1_0_1_0_1_0_1_0") (readFlag "_0_0_0_0_0_0_0_1"))

test_anyReq =
  [ testCase "anyReq (Flag '0000') (Flag '000') == True"  c_aR01
  , testCase "anyReq (Flag '0000') (Flag '001') == False" c_aR02
  , testCase "anyReq (Flag '0001') (Flag '001') == True"  c_aR03
  , testCase "anyReq (Flag '0001') (Flag '000') == True"  c_aR04
  , testCase "anyReq (Flag '0110') (Flag '001') == False" c_aR05
  , testCase "anyReq (Flag '0011') (Flag '001') == True"  c_aR06
  , testCase "anyReq (Flag '0011') (Flag '010') == True"  c_aR07
  , testCase "anyReq (Flag '0011') (Flag '100') == False" c_aR08
  , testCase "anyReq (Flag '0001') (Flag '101') == True"  c_aR09
  ]

c_aR01 = assertBool ""       (anyReq (readFlag "0000") (readFlag "000"))
c_aR02 = assertBool "" (not $ anyReq (readFlag "0000") (readFlag "001"))
c_aR03 = assertBool ""       (anyReq (readFlag "0001") (readFlag "001"))
c_aR04 = assertBool ""       (anyReq (readFlag "0001") (readFlag "000"))
c_aR05 = assertBool "" (not $ anyReq (readFlag "0110") (readFlag "001"))
c_aR06 = assertBool ""       (anyReq (readFlag "0011") (readFlag "001"))
c_aR07 = assertBool ""       (anyReq (readFlag "0011") (readFlag "010"))
c_aR08 = assertBool "" (not $ anyReq (readFlag "0011") (readFlag "100"))
c_aR09 = assertBool ""       (anyReq (readFlag "0001") (readFlag "101"))

test_allReq =
  [ testCase "allReq (Flag '0000') (Flag '000') == True"  c_AR01
  , testCase "allReq (Flag '0000') (Flag '001') == False" c_AR02
  , testCase "allReq (Flag '0001') (Flag '001') == True"  c_AR03
  , testCase "allReq (Flag '0001') (Flag '000') == True"  c_AR04
  , testCase "allReq (Flag '0110') (Flag '001') == False" c_AR05
  , testCase "allReq (Flag '0011') (Flag '001') == True"  c_AR06
  , testCase "allReq (Flag '0011') (Flag '010') == True"  c_AR07
  , testCase "allReq (Flag '0011') (Flag '100') == False" c_AR08
  , testCase "allReq (Flag '0101') (Flag '101') == True"  c_AR09
  ]

c_AR01 = assertBool ""       (allReq (readFlag "0000") (readFlag "000"))
c_AR02 = assertBool "" (not $ allReq (readFlag "0000") (readFlag "001"))
c_AR03 = assertBool ""       (allReq (readFlag "0001") (readFlag "001"))
c_AR04 = assertBool ""       (allReq (readFlag "0001") (readFlag "000"))
c_AR05 = assertBool "" (not $ allReq (readFlag "0110") (readFlag "001"))
c_AR06 = assertBool ""       (allReq (readFlag "0011") (readFlag "001"))
c_AR07 = assertBool ""       (allReq (readFlag "0011") (readFlag "010"))
c_AR08 = assertBool "" (not $ allReq (readFlag "0011") (readFlag "100"))
c_AR09 = assertBool ""       (allReq (readFlag "0101") (readFlag "101"))
c_AR10 = assertBool "" (not $ allReq (readFlag "0101") (readFlag "111"))
