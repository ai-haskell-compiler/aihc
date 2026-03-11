{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Trie.Tests
  ( testTrie
  ) where

import qualified Zenacy.HTML.Internal.Trie as Trie
import Control.Monad
  ( (>=>)
  )
import Control.Monad.Writer
  ( Writer(..)
  , execWriter
  , tell
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
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( concat
  )

testTrie :: Test
testTrie = testGroup "Zenacy.HTML.Internal.Trie"
  [ testMatch
  ]

testMatch :: Test
testMatch = testCase "trie match" $ do
  assertEqual "TEST 1" Nothing $ Trie.match t0 "z"
  assertEqual "TEST 2" (Just ("a", 1, "")) $ Trie.match t0 "a"
  assertEqual "TEST 3" (Just ("a", 1, "y")) $ Trie.match t0 "ay"
  assertEqual "TEST 4" (Just ("bbb", 5, "yyy")) $ Trie.match t2 "bbbyyy"
  assertEqual "TEST 5" (Just ("ccc", 7, "yyy")) $ Trie.match t2 "cccyyy"
  assertEqual "TEST 6" Nothing $ Trie.match t1 "z"
  assertEqual "TEST 7" (Just ("aa", 1, "")) $ Trie.match t1 "aa"
  assertEqual "TEST 8" (Just ("bb", 2, "")) $ Trie.match t1 "bb"
  assertEqual "TEST 9" (Just ("a", 1, "z")) $ Trie.match t2 "az"
  assertEqual "TEST 10" (Just ("aa", 2, "z")) $ Trie.match t2 "aaz"
  assertEqual "TEST 11" (Just ("aaa", 3, "z")) $ Trie.match t2 "aaaz"
  assertEqual "TEST 12" (Just ("bb", 4, "z")) $ Trie.match t2 "bbz"
  assertEqual "TEST 13" (Just ("aaax", 6, "z")) $ Trie.match t2 "aaaxz"

t0 = Trie.fromList
  [ ("a", 1)
  ]

t1 = Trie.fromList
  [ ("aa", 1)
  , ("bb", 2)
  ]

t2 = Trie.fromList
  [ ("a",    1)
  , ("aa",   2)
  , ("aaa",  3)
  , ("bb",   4)
  , ("bbb",  5)
  , ("aaax", 6)
  , ("ccc",  7)
  ]
