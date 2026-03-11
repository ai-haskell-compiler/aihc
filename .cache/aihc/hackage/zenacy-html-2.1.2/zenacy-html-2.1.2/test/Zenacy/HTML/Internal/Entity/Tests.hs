{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Entity.Tests
  ( testEntity
  ) where

import Zenacy.HTML.Internal.Entity
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

testEntity :: Test
testEntity = testGroup "Zenacy.HTML.Internal.Entity"
  [ testGeneral
  , testLongest
  ]

testGeneral :: Test
testGeneral = testCase "entity general" $ do
  assertEqual "TEST 1" Nothing                     $ entityMatch "aamp;b"
  assertEqual "TEST 2" (Just ("amp;", "&", "bbb")) $ entityMatch "amp;bbb"
  assertEqual "TEST 3" (Just ("amp;", "&", ""))    $ entityMatch "amp;"
  assertEqual "TEST 4" (Just ("amp", "&", "b"))    $ entityMatch "ampb"

testLongest :: Test
testLongest = testCase "entity longest" $ do
  assertEqual "TEST 1"
    -- TODO: is this the correct result?
    (Just ("DoubleLongLeftRightArrow;", "\226\159\186", "x"))
    (entityMatch "DoubleLongLeftRightArrow;x")
