{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.Text (Text)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.XML
       (Document(..), Element(..), Name(..), Node(..), Prologue(..))

import Text.XML.QQ (xmlRaw, xmlUnsafe)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [xmlTests]

xmlTests :: TestTree
xmlTests = testGroup "xml" [xmlUnsafeTests, xmlRawTests]

xmlUnsafeTests :: TestTree
xmlUnsafeTests = testGroup "xmlUnsafe" [xmlUnsafeWorksCorrectly]

xmlUnsafeWorksCorrectly :: TestTree
xmlUnsafeWorksCorrectly = testCase "works correctly" $ doc @?= expectedDoc
  where
    doc :: Document
    doc =
      let a = "hello" :: Text
      in [xmlUnsafe|<html>#{a}</html>|]

    expectedDoc :: Document
    expectedDoc =
      Document
      { documentPrologue =
          Prologue
          {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
      , documentRoot =
          Element
          { elementName =
              Name
              { nameLocalName = "html"
              , nameNamespace = Nothing
              , namePrefix = Nothing
              }
          , elementAttributes = mempty
          , elementNodes = [NodeContent "hello"]
          }
      , documentEpilogue = []
      }

xmlRawTests :: TestTree
xmlRawTests = testGroup "xmlRaw" [xmlRawWorksCorrectly]

xmlRawWorksCorrectly :: TestTree
xmlRawWorksCorrectly = testCase "works correctly" $ doc @?= expectedDoc
  where
    doc :: Document
    doc = [xmlRaw|<html></html>|]

    expectedDoc :: Document
    expectedDoc =
      Document
      { documentPrologue =
          Prologue
          {prologueBefore = [], prologueDoctype = Nothing, prologueAfter = []}
      , documentRoot =
          Element
          {elementName = "html", elementAttributes = mempty, elementNodes = []}
      , documentEpilogue = []
      }
