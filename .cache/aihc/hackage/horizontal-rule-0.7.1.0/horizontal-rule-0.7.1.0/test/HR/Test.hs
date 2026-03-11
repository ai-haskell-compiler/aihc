{-# LANGUAGE OverloadedStrings #-}

module HR.Test (tests) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (horizontal-rule)
import qualified HR

------------------------------------------------------------------------------

testRenderAscii :: TestTree
testRenderAscii = testGroup "renderAscii"
    [ testCase "fill" $
        "----------------------------------------" @=?
          HR.renderAscii 40 []
    , testCase "oneNote" $
        "--|test|--------------------------------" @=?
          HR.renderAscii 40 ["test"]
    , testCase "twoNotes" $
        "--|2021-05-27 17:29:42|-|test|----------" @=?
          HR.renderAscii 40 ["2021-05-27 17:29:42", "test"]
    , testCase "long" $
        "--|2021-05-27 17:29:42|-|this note is long|--" @=?
          HR.renderAscii 40 ["2021-05-27 17:29:42", "this note is long"]
    ]

------------------------------------------------------------------------------

testRenderUnicode :: TestTree
testRenderUnicode = testGroup "renderUnicode"
    [ testCase "fill" $
        "━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" @=?
          HR.renderUnicode 40 []
    , testCase "oneNote" $
        "━━┫test┣━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━" @=?
          HR.renderUnicode 40 ["test"]
    , testCase "twoNotes" $
        "━━┫2021-05-27 17:29:42┣━┫test┣━━━━━━━━━━" @=?
          HR.renderUnicode 40 ["2021-05-27 17:29:42", "test"]
    , testCase "long" $
        "━━┫2021-05-27 17:29:42┣━┫this note is long┣━━" @=?
          HR.renderUnicode 40 ["2021-05-27 17:29:42", "this note is long"]
    ]

------------------------------------------------------------------------------

testRender :: TestTree
testRender = testGroup "render"
    [ testCase "fill" $
        "════════════════════════════════════════" @=?
          HR.render parts 40 []
    , testCase "oneNote" $
        "══╣test╠════════════════════════════════" @=?
          HR.render parts 40 ["test"]
    , testCase "twoNotes" $
        "══╣2021-05-27 17:29:42╠═╣test╠══════════" @=?
          HR.render parts 40 ["2021-05-27 17:29:42", "test"]
    , testCase "long" $
        "══╣2021-05-27 17:29:42╠═╣this note is long╠══" @=?
          HR.render parts 40 ["2021-05-27 17:29:42", "this note is long"]
    ]
  where
    parts :: HR.Parts
    parts = HR.Parts
      { HR.leftPart  = "══╣"
      , HR.midPart   = "╠═╣"
      , HR.rightPart = "╠══"
      , HR.fillPart  = '═'
      }

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "HR"
    [ testRenderAscii
    , testRenderUnicode
    , testRender
    ]
