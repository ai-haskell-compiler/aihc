{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Test.Tasty as Tasty
import qualified Test.Tasty.Runners as Tasty
import qualified Test.Tasty.HUnit as HU
import           Test.Tasty.HUnit ((@?=))

import qualified Xmlbf as X
import qualified Xmlbf.Xeno as Xx

--------------------------------------------------------------------------------

main :: IO ()
main = Tasty.defaultMainWithIngredients
  [ Tasty.consoleTestReporter
  , Tasty.listingTests
  ] tt_main

--------------------------------------------------------------------------------

tt_main :: Tasty.TestTree
tt_main =
  -- All of the testcases suffixed "-BAD" below are actually undesired results
  -- provided by xeno. They are kept here as an acknowledgement of this
  -- known behavior.

  Tasty.testGroup "main"
  [ HU.testCase "1" $ do
     Xx.fromRawXml "" @?= Right []

  , HU.testCase "2" $ do
     Xx.fromRawXml " " @?= Right (X.text " ")

  , HU.testCase "3" $ do
     Xx.fromRawXml "<foo/>" @?= Right (X.element "foo" [] [])

  , HU.testCase "4" $ do
     Xx.fromRawXml "<foo></foo>" @?= Right (X.element "foo" [] [])

  , HU.testCase "5" $ do
     Xx.fromRawXml " <foo></foo>" @?= Right (X.text " " <> X.element "foo" [] [])

  , HU.testCase "6" $ do
     Xx.fromRawXml " <foo/>  "
       @?= Right (X.text " " <> X.element "foo" [] [] <> X.text "  ")

  , HU.testCase "7" $ do
     Xx.fromRawXml "<foo a=\"\"/>" @?= Right (X.element "foo" [("a", "")] [])

  , HU.testCase "8" $ do
     Xx.fromRawXml "<foo a=\"b\"></foo>" @?= Right (X.element "foo" [("a", "b")] [])

  , HU.testCase "9" $ do
     Xx.fromRawXml "<foo a=\"b\" c=\"\"/>"
       @?= Right (X.element "foo" [("a", "b"), ("c", "")] [])

  , HU.testCase "10" $ do
     Xx.fromRawXml "<foo a=\"b\" c=\"d\"/>"
       @?= Right (X.element "foo" [("a", "b"), ("c", "d")] [])

  , HU.testCase "11" $ do
     Xx.fromRawXml "<foo a=\"b\">bar</foo>"
       @?= Right (X.element "foo" [("a", "b")] (X.text "bar"))

  , HU.testCase "12" $ do
     Xx.fromRawXml "<foo a=\"b\"><bar/></foo>"
       @?= Right (X.element "foo" [("a", "b")] (X.element "bar" [] []))

  , HU.testCase "13-BAD" $ do
     -- This should return on either a 'Left', or a @'Right' [X.element "foo" [] []]@.
     -- The reason why it doesn't is because 'Xx.fromRawXml' wraps @"<foo>"@ as
     -- @"<x><foo></x>"@ before parsing, which causes the @x@ element to be
     -- successfully parsed, but not the inner @foo@. This is a problem in Xeno.
     Xx.fromRawXml "<foo>" @?= Right []

  , HU.testCase "14-BAD" $ do
     -- Similar to test case 13-BAD. This is wrong, but it is what Xeno returns.
     Xx.fromRawXml "<foo></bar>" @?= Right (X.element "foo" [] [])

  , HU.testCase "15" $ do
     Xx.fromRawXml "&lt;foo/&gt;" @?= Right (X.text "<foo/>")

  , HU.testCase "16" $ do
     Xx.fromRawXml "&amp;lt;" @?= Right (X.text "&lt;")

  , HU.testCase "17" $ do
     -- Test BOM presence
     Xx.fromRawXml "\xEF\xBB\xBF<foo/>" @?= Xx.fromRawXml "<foo/>"
  ]
