{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Oper.Tests
  ( testOper
  ) where

import Zenacy.HTML
import Control.Monad
  ( (>=>)
  )
import Data.Map
  ( Map
  )
import qualified Data.Map as Map
  ( fromList
  , lookup
  )
import Data.Maybe
  ( fromJust
  )
import Test.Framework
  ( Test
  , testGroup
  )
import Test.Framework.Providers.HUnit
  ( testCase
  )
import Test.HUnit
  ( assertBool
  , assertEqual
  , assertFailure
  )

testOper :: Test
testOper = testGroup "Zenacy.HTML.Internal.Oper"
  [ testStyle
  ]

h = fromJust
  . htmlDocBody
  $ htmlParseEasy
  "<body>\
  \<h1></h1>\
  \<p><a href='bbb'>AAA</a><span></span><br><img></p>\
  \<p id=\"1\" style=\"display:none\"></p>\
  \<p id=\"2\" style=\"display:none;\"></p>\
  \<p id=\"3\" style=\"display: none;\"></p>\
  \<p id=\"4\" style=\"  display:  none; \"></p>\
  \<p id=\"5\" style=\"  display\"></p>\
  \<p id=\"6\" style=\" \"></p>\
  \<p id=\"7\" style=\"  display:  ; \"></p>\
  \<p id=\"8\" style=\";  ;display:  none;;\"></p>\
  \<div id=\"9\" style=\"background-image:url(https://a/b);\"></div>\
  \<div id=\"10\" style=\"background-image:url('https://a/b');\"></div>\
  \</body>"
f x = fromJust . htmlElemFindID x

testStyle :: Test
testStyle = testCase "oper style" $ do
  assertEqual "TEST 1" m0 $ g "1"
  assertEqual "TEST 2" m0 $ g "2"
  assertEqual "TEST 3" m0 $ g "3"
  assertEqual "TEST 4" m0 $ g "4"
  assertEqual "TEST 5" m1 $ g "5"
  assertEqual "TEST 6" m2 $ g "6"
  assertEqual "TEST 7" m1 $ g "7"
  assertEqual "TEST 8" m0 $ g "8"
  assertEqual "TEST 9"
    (Map.fromList [("background-image","url(https://a/b)")])
    (g "9")
  assertEqual "TEST 10"
    (Just "https://a/b")
    ((Map.lookup "background-image" >=> htmlElemStyleParseURL) (g "9"))
  assertEqual "TEST 11"
    (Just "https://a/b")
    ((Map.lookup "background-image" >=> htmlElemStyleParseURL) (g "10"))
  where
    g x = htmlElemStyles $ f x h
    m0 = Map.fromList [("display","none")]
    m1 = Map.fromList [("display","")]
    m2 = Map.fromList []
