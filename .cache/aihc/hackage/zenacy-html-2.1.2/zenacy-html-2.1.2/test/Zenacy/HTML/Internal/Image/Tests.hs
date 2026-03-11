{-# LANGUAGE OverloadedStrings #-}

module Zenacy.HTML.Internal.Image.Tests
  ( testImage
  ) where

import Zenacy.HTML.Internal.BS
import Zenacy.HTML.Internal.Char
import Zenacy.HTML.Internal.Image
import Zenacy.HTML.Internal.Types
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( reverse
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

testImage :: Test
testImage = testGroup "Zenacy.HTML.Internal.Image"
  [ testParse
  , testRender
  , testURL
  , testMap
  , testMin
  , testMax
  ]

testParse :: Test
testParse = testCase "srcset parse" $ do
  assertEqual "TEST 1"
    (HTMLSrcset [])
    (htmlSrcsetParse "")
  assertEqual "TEST 2"
    (HTMLSrcset [HTMLSrcsetCandidate "/a/b/c" HTMLSrcsetNone])
    (htmlSrcsetParse "/a/b/c")
  assertEqual "TEST 3"
    (HTMLSrcset [HTMLSrcsetCandidate "/a/b/c" (HTMLSrcsetPixel 2)])
    (htmlSrcsetParse "/a/b/c 2x")
  assertEqual "TEST 4"
    (HTMLSrcset
      [HTMLSrcsetCandidate "/a" HTMLSrcsetNone
      ,HTMLSrcsetCandidate "/b" HTMLSrcsetNone
      ])
    (htmlSrcsetParse "/a,/b")
  assertEqual "TEST 5"
    (HTMLSrcset
      [HTMLSrcsetCandidate "/a" (HTMLSrcsetPixel 2)
      ,HTMLSrcsetCandidate "/b" (HTMLSrcsetWidth 2)
      ])
    (htmlSrcsetParse "/a 2x , /b 2w")
  assertEqual "TEST 6"
    (HTMLSrcset [])
    (htmlSrcsetParse ",")
  assertEqual "TEST 7"
    (HTMLSrcset [HTMLSrcsetCandidate "/a/b/c" HTMLSrcsetNone])
    (htmlSrcsetParse " /a/b/c ")
  assertEqual "TEST 8"
    (HTMLSrcset [HTMLSrcsetCandidate "/a/b/c" HTMLSrcsetNone])
    (htmlSrcsetParse " /a/b/c, ")
  assertEqual "TEST 9"
    (HTMLSrcset [HTMLSrcsetCandidate "/a/b/c" (HTMLSrcsetPixel 2)])
    (htmlSrcsetParse ",/a/b/c 2x,")
  assertEqual "TEST 10"
    (HTMLSrcset
      [HTMLSrcsetCandidate "/a" (HTMLSrcsetPixel 2)
      ,HTMLSrcsetCandidate "/b" (HTMLSrcsetWidth 2)
      ])
    (htmlSrcsetParse " /a 2x , /b 2w , ")

testRender :: Test
testRender = testCase "srcset render" $ do
  assertEqual "TEST 1" "" $ p ""
  assertEqual "TEST 2" "/a/b/c" $ p "/a/b/c"
  assertEqual "TEST 3" "/a/b/c 2x" $ p "/a/b/c 2x"
  assertEqual "TEST 4" "/a,/b" $ p "/a,/b"
  assertEqual "TEST 5" "/a 2x,/b 2w" $ p "/a 2x,/b 2w"
  assertEqual "TEST 6" "" $ p ","
  where
    p = htmlSrcsetRender
      . htmlSrcsetParse

testURL :: Test
testURL = testCase "srcset list" $ do
  assertEqual "TEST 1" [] $ p ""
  assertEqual "TEST 2" ["/a/b/c"] $ p "/a/b/c"
  assertEqual "TEST 3" ["/a/b/c"] $ p "/a/b/c 3x"
  assertEqual "TEST 4" ["/a","/b"] $  p "/a,/b"
  assertEqual "TEST 5" ["/a","/b"] $ p "/a 2x,/b 2w"
  where
    p = htmlSrcsetListURL
      . htmlSrcsetParse

testMap :: Test
testMap = testCase "srcset map" $ do
  assertEqual "TEST 1" "a/ 2x,b/ 2w" $ p "/a 2x,/b 2w"
  where
    p = htmlSrcsetRender
      . htmlSrcsetMapURL T.reverse
      . htmlSrcsetParse

testMin :: Test
testMin = testCase "srcset min" $ do
  assertEqual "TEST 1" "/a" $ p "/a,/b 2x"
  assertEqual "TEST 2" "/a" $ p "/a"
  assertEqual "TEST 3" "/a" $ p "/a 100w"
  assertEqual "TEST 4" "/a" $ p "/c 3x, /d 4x, /a, /b 2x"
  where
    p = htmlSrcsetImageMin
      . htmlSrcsetParse

testMax :: Test
testMax = testCase "srcset max" $ do
  assertEqual "TEST 1" "/b" $ p "/a,/b 2x"
  assertEqual "TEST 2" "/a" $ p "/a"
  assertEqual "TEST 3" "/a" $ p "/a 100w"
  assertEqual "TEST 4" "/d" $ p "/c 3x, /d 4x, /a, /b 2x"
  where
    p = htmlSrcsetImageMax
      . htmlSrcsetParse
