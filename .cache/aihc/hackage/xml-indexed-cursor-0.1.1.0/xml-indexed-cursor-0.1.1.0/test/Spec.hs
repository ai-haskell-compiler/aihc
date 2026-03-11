{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.ByteString.Lazy (ByteString)
import Data.Maybe (catMaybes)
import Data.Monoid ((<>))
import Data.Text (Text, isPrefixOf)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit ((@?=), testCase)
import Text.XML (Element(..), Name(..), Node(..))
import Text.XML.Cursor.Generic (($.//), ($//), child, descendant, node)

import Text.XML.Cursor.Indexed
       (IndexedCursor, IndexedNode(..), attribute, attributeIs, check,
        checkElement, checkIndexedNode, checkName, content, element,
        hasAttribute, indexedCursorFromByteString_, laxAttribute)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests" [xmlCursorTests]

xmlCursorTests :: TestTree
xmlCursorTests =
  testGroup
    "XML Cursors"
    [ cursorCheck
    , cursorPredicate
    , cursorCheckIndexedNode
    , cursorCheckElement
    , cursorCheckName
    , cursorElement
    , cursorContent
    , cursorAttribute
    , cursorLaxAttribute
    , cursorHasAttribute
    , cursorAttributeIs
    ]

cursorCheck :: TestTree
cursorCheck =
  testCase "has correct check" $ null (cursor $.// check (const False)) @?= True

cursorPredicate :: TestTree
cursorPredicate =
  testCase "has correct check with lists" $
  (name $ cursor $.// check descendant) @?= ["foo", "bar2", "baz3", "bar3"]

cursorCheckIndexedNode :: TestTree
cursorCheckIndexedNode =
  testCase "has correct checkIndexedNode" $
  (name $ cursor $// checkIndexedNode f) @?= ["bar1", "bar2", "bar3"]
  where
    f :: IndexedNode -> Bool
    f IndexedNode{indexedNodeNode = NodeElement e} =
      "bar" `isPrefixOf` nameLocalName (elementName e)
    f _ = False

cursorCheckElement :: TestTree
cursorCheckElement =
  testCase "has correct checkElement" $
  (name $ cursor $// checkElement f) @?= ["bar1", "bar2", "bar3"]
  where
    f :: Element -> Bool
    f e = "bar" `isPrefixOf` nameLocalName (elementName e)


cursorCheckName :: TestTree
cursorCheckName =
  testCase "has correct checkName" $
  (name $ cursor $// checkName f) @?= ["bar1", "bar2", "bar3"]
  where
    f :: Name -> Bool
    f n = "bar" `isPrefixOf` nameLocalName n

cursorElement :: TestTree
cursorElement =
  testCase "has correct element" $
  (name $ cursor $// element "bar1") @?= ["bar1"]

cursorContent :: TestTree
cursorContent = testCase "has correct content" $ do
  content cursor @?= []
  (cursor $.// content) @?= ["a", "b"]

cursorAttribute :: TestTree
cursorAttribute =
  testCase "has correct attribute" $ attribute "attr" cursor @?= ["x"]

cursorLaxAttribute :: TestTree
cursorLaxAttribute =
  testCase "has correct laxAttribute" $
  (cursor $.// laxAttribute "Attr") @?= ["x", "y", "q"]

cursorHasAttribute :: TestTree
cursorHasAttribute =
  testCase "has correct hasAttribute" $
  (length $ cursor $.// hasAttribute "attr") @?= 2

cursorAttributeIs :: TestTree
cursorAttributeIs =
  testCase "has correct attributeIs" $
  (length $ cursor $.// attributeIs "attr" "y") @?= 1

-- | Pull out all the 'Name's of all the 'Element's pointed to by the input
-- list. Ignore all 'IndexedCursor's that aren't pointing to 'Element's.
name :: [IndexedCursor] -> [Text]
name = catMaybes . fmap getMaybeName
  where
    getMaybeName :: IndexedCursor -> Maybe Text
    getMaybeName c =
      case indexedNodeNode $ node c of
        NodeElement e -> Just . nameLocalName $ elementName e
        _ -> Nothing

cursor :: IndexedCursor
cursor = indexedCursorFromByteString_ input
  where
    input :: ByteString
    input =
        "<foo attr=\"x\">" <>
           "<bar1/>" <>
           "<bar2>" <>
              "<baz1/>" <>
              "<baz2 attr=\"y\"/>" <>
              "<baz3>a</baz3>" <>
           "</bar2>" <>
           "<bar3>" <>
              "<bin1/>" <>
              "b" <>
              "<bin2/>" <>
              "<bin3/>" <>
           "</bar3>" <>
           "<Bar1 xmlns=\"http://example.com\" Attr=\"q\"/>" <>
        "</foo>"

bar2 :: IndexedCursor
bar2 = child cursor !! 1

baz2 :: IndexedCursor
baz2 = child bar2 !! 1

bar3 :: IndexedCursor
bar3 = child cursor !! 2

bin2 :: IndexedCursor
bin2 = child bar3 !! 1
