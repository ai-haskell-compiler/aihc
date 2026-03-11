{-# LANGUAGE TemplateHaskell #-}

module Test.Flag.Phantom where


import Data.Flag.Phantom
import Data.Word

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2
import Test.Framework.TH
import Test.HUnit.Base
import Test.QuickCheck

import Test.Flag.Env


tests = $(testGroupGenerator)

test_isFlaggable =
  [ testCase "isFlaggable A  == True"  c_if01
  , testCase "isFlaggable X0 == False" c_if02
  , testCase "isFlaggable Y1 == True"  c_if03
  ]

c_if01 = assertBool "" (isFlaggable $ A)
c_if02 = assertBool "" (not . isFlaggable $ X0)
c_if03 = assertBool "" (isFlaggable $ Y1)

test_encDec =
  [ testProperty "(getFlag . encodeFlag . (decodeFlag :: PhantomFlag X -> [X]) . PhFlag) i == i" p_ged01
  , testProperty "(getFlag . encodeFlag . (decodeFlag :: PhantomFlag Y -> [Y]) . PhFlag) i == i" p_ged02
  ]

p_ged01 i = (getFlag . encodeFlag . (decodeFlag :: PhantomFlag X -> [X]) . PhFlag) i == i
  where types = (i :: Flag)
p_ged02 i = (getFlag . encodeFlag . (decodeFlag :: PhantomFlag Y -> [Y]) . PhFlag) i == i
  where types = (i :: Flag)

test_readShow =
  [ testProperty "(getFlag . readFlag . showFlag . PhFlag) i == i" p_rs01
  --, testProperty "(getFlag . readFlag . showFlagBy 16 . PhFlag) i == i" p_rs02
  , testProperty "(getFlag . readFlag . showFlagFit X0 . PhFlag) i == i" p_rs03
  , testProperty "(getFlag . readFlag . showFlagFit Y1 . PhFlag) i == i" p_rs04
  ]

p_rs01 i = (getFlag . readFlag . showFlag . PhFlag) i == i
  where types = (i :: Flag)
-- NOTE: This test should fail because input is not limited by -2^15~2^15-1
p_rs02 i = (getFlag . readFlag . showFlagBy 16 . PhFlag) i == i
  where types = (i :: Flag)
-- NOTE: This should not fail at test but assertion because input is limited in Flag representation
p_rs03 i = (getFlag . readFlag . showFlagFit X0 . PhFlag) i == i
  where types = (i :: Flag)
p_rs04 i = (getFlag . readFlag . showFlagFit Y1 . PhFlag) i == i
  where types = (i :: Flag)

test_showFlagBy =
  [ testCase "showFlagBy  0 (PhFlag 1) == \"\"" c_sFB01
  , testCase "showFlagBy  1 (PhFlag 1) == \"1\"" c_sFB02
  , testCase "showFlagBy  9 (PhFlag 1) == \"000000001\"" c_sFB03
  , testCase "showFlagBy 10 (PhFlag 1) == \"0000000001\"" c_sFB04
  , testCase "showFlagBy  1 (PhFlag 2) == \"0\"" c_sFB05
  , testCase "showFlagBy  2 (PhFlag 2) == \"10\"" c_sFB06
  , testCase "showFlagBy 10 (PhFlag 2) == \"0000000010\"" c_sFB07
  ]

c_sFB01 = showFlagBy  0 (PhFlag 1) @?= ""
c_sFB02 = showFlagBy  1 (PhFlag 1) @?= "1"
c_sFB03 = showFlagBy  9 (PhFlag 1) @?= "000000001"
c_sFB04 = showFlagBy 10 (PhFlag 1) @?= "0000000001"
c_sFB05 = showFlagBy  1 (PhFlag 2) @?= "0"
c_sFB06 = showFlagBy  2 (PhFlag 2) @?= "10"
c_sFB07 = showFlagBy 10 (PhFlag 2) @?= "0000000010"

test_encodeFlag =
  [ testCase "encodeFlag [X0,X1] == PhFlag 3" c_eF01
  ]

c_eF01 = PhFlag 3 @=? encodeFlag [X0,X1]

{-
  Can't test because of type error

_test_equal =
  [ testCase "x1 /= x2" c_eq01
  , testCase "x1 == y1" c_eq02
  ]

c_eq01 = x1 @?= x2
c_eq02 = x1 @?= y1

_test_include =
  [ testCase "include x1 y1" c_incl01
  , testCase "include x1 x2" c_incl02
  ]

c_incl01 = assertBool "" (include x1 y1)
c_incl02 = assertBool "" (include x1 x2)
-}

