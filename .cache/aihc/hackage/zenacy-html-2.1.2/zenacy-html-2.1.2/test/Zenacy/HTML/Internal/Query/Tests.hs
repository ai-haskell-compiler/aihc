{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Query.Tests
  ( testQuery
  ) where

import Zenacy.HTML
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

testQuery :: Test
testQuery = testGroup "Zenacy.HTML.Internal.Query"
  [ testFirst
  , testLast
  , testNext
  , testPrev
  , testUp
  , testIsFirst
  , testIsLast
  , testSave
  , testNode
  , testName
  , testAttr
  , testAttrVal
  , testId
  , testClass
  , testGeneral
  , testOnly
  ]

h = htmlParseEasy
    "<h1></h1><p>\
    \<a href='bbb'>AAA</a><span id='x' class='y z'></span><br><img>\
    \</p>"
b = fromJust $ htmlDocBody h

testFirst :: Test
testFirst = testCase "query first" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ do
      htmlQueryFirst
      htmlQueryName "h1"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ do
      htmlQueryFirst
      htmlQueryFirst
      htmlQuerySucc True

testLast :: Test
testLast = testCase "query last" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryLast >> htmlQueryName "p" >> htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ htmlQueryFirst >> htmlQueryLast >> htmlQuerySucc True

testNext :: Test
testNext = testCase "query next" $ do
  assertEqual "TEST 1" (Nothing) $
    htmlQueryRun b $ htmlQueryNext >> htmlQuerySucc True

testPrev :: Test
testPrev = testCase "query prev" $ do
  assertEqual "TEST 1" (Nothing) $
    htmlQueryRun b $ htmlQueryPrev >> htmlQuerySucc True

testUp :: Test
testUp = testCase "query up" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryLast >> htmlQueryLast >> htmlQueryName "img"
      >> htmlQueryUp >> htmlQueryName "p" >> htmlQueryUp >> htmlQueryName "body"
      >> htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ htmlQueryUp >> htmlQuerySucc True

testIsFirst :: Test
testIsFirst = testCase "query isfirst" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryIsFirst >> htmlQuerySucc True

testIsLast :: Test
testIsLast = testCase "query islast" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryIsLast >> htmlQuerySucc True

testSave :: Test
testSave = testCase "query save" $ do
  assertEqual "TEST 1" (Just "body") $
    htmlQueryRun b $ htmlQuerySave 4 >> htmlQueryGet 4 >>= \a -> htmlQuerySucc $ htmlElemName a

testNode :: Test
testNode = testCase "query node" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryNode >>= htmlQuerySucc . htmlElemHasName "body"

testName :: Test
testName = testCase "query name" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ htmlQueryName "body" >> htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ htmlQueryName "x" >> htmlQuerySucc True

testAttr :: Test
testAttr = testCase "query attr" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryAttr "id"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ do
      htmlQueryAttr "x"
      htmlQuerySucc True

testAttrVal :: Test
testAttrVal = testCase "query attr val" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryAttrVal "id" "x"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryAttrVal "id" "0"
      htmlQuerySucc True

testId :: Test
testId = testCase "query id" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryId "x"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryId "0"
      htmlQuerySucc True

testClass :: Test
testClass = testCase "query class" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryHasClass "z"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun b $ do
      htmlQueryLast
      htmlQueryFirst
      htmlQueryNext
      htmlQueryHasClass "0"
      htmlQuerySucc True

testGeneral :: Test
testGeneral = testCase "query general" $ do
  assertEqual "TEST 4" (Just "h1") $
    htmlQueryRun b $ do
      htmlQueryName "body"
      htmlQueryFirst
      htmlQueryName "h1"
      htmlQuerySave 1
      htmlQueryNext
      htmlQueryName "p"
      htmlQueryIsLast
      a <- htmlQueryGet 1
      htmlQuerySucc $ htmlElemName a

testOnly :: Test
testOnly = testCase "query only" $ do
  assertEqual "TEST 1" (Just True) $
    htmlQueryRun a $ do
      htmlQueryFirst
      htmlQueryNext
      htmlQueryOnly "span"
      htmlQueryOnly "a"
      htmlQuerySucc True
  assertEqual "TEST 2" (Nothing) $
    htmlQueryRun a $ do
      htmlQueryFirst
      htmlQueryNext
      htmlQueryOnly "a"
      htmlQueryOnly "a"
      htmlQuerySucc True
  assertEqual "TEST 3" (Nothing) $
    htmlQueryRun a $ do
      htmlQueryOnly "h1"
      htmlQuerySucc True
  where
    a = fromJust $ htmlDocBody $ htmlParseEasy
      "<h1></h1><p><span><a></a></span></p>"
