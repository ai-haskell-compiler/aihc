{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}

module Samples
  ( testSamples
  ) where

import Zenacy.HTML
import Data.Default
  ( Default(..)
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
import Data.Maybe
  ( fromJust
  , fromMaybe
  )
import Data.Text
  ( Text
  )
import qualified Data.Text as T
  ( pack
  )
import Text.RawString.QQ

testSamples :: Test
testSamples = testGroup "Samples"
  [ testHello
  , testQuirks
  , testRewrite
  , testExtract
  , testQuery
  , testQuery2
  ]

testHello :: Test
testHello = testCase "sample hello" $ do
  flip (assertEqual "Sample 1")
    (htmlRender $ htmlParseEasy "<div>HelloWorld</div>")
    "<html><head></head><body><div>HelloWorld</div></body></html>"

  flip (assertEqual "Sample 2")
    (htmlParseEasy "<div>HelloWorld</div>")
    (HTMLDocument "" HTMLQuirksMode
      [ HTMLElement "html" HTMLNamespaceHTML []
          [ HTMLElement "head" HTMLNamespaceHTML [] []
          , HTMLElement "body" HTMLNamespaceHTML []
            [ HTMLElement "div" HTMLNamespaceHTML []
              [ HTMLText "HelloWorld" ] ] ] ])

  flip (assertEqual "Sample 3")
    (htmlParseEasy "<div>HelloWorld</div>")
    HTMLDocument
      { htmlDocumentName = ""
      , htmlDocumentMode = HTMLQuirksMode
      , htmlDocumentChildren =
        [ HTMLElement
          { htmlElementName = "html"
          , htmlElementNamespace = HTMLNamespaceHTML
          , htmlElementAttributes = []
          , htmlElementChildren =
            [ HTMLElement
              { htmlElementName = "head"
              , htmlElementNamespace = HTMLNamespaceHTML
              , htmlElementAttributes = []
              , htmlElementChildren = []
              }
            , HTMLElement
              { htmlElementName = "body"
              , htmlElementNamespace = HTMLNamespaceHTML
              , htmlElementAttributes = []
              , htmlElementChildren =
                [ HTMLElement
                  { htmlElementName = "div"
                  , htmlElementNamespace = HTMLNamespaceHTML
                  , htmlElementAttributes = []
                  , htmlElementChildren =
                    [ HTMLText
                      { htmlTextData = "HelloWorld" }
                    ] } ] } ] } ] }

testQuirks :: Test
testQuirks = testCase "sample quirks" $ do
  flip (assertEqual "Sample 1")
    (htmlDocumentMode $ htmlParseEasy "<!DOCTYPE html><div>HelloWorld</div>")
    HTMLQuirksNone
  flip (assertEqual "Sample 1")
    (htmlDocumentMode $ htmlParseEasy "<html><div>HelloWorld</div>")
    HTMLQuirksMode

testRewrite :: Test
testRewrite = testCase "sample rewrite" $ do
  flip (assertEqual "Sample 1")
    (rewrite "<span>Hello</span><span>World</span>")
    "<html><head></head><body><div>Hello</div><div>World</div></body></html>"

rewrite :: Text -> Text
rewrite = htmlRender . htmlMapElem f . fromJust . htmlDocHtml . htmlParseEasy
  where
    f x
      | htmlElemHasName "span" x = htmlElemRename "div" x
      | otherwise = x

testExtract :: Test
testExtract = testCase "sample extract" $ do
  flip (assertEqual "Sample 1")
    (extract "<a href=\"https://example1.com\"></a><a href=\"https://example2.com\"></a>")
    [ "https://example1.com"
    , "https://example2.com"
    ]

extract :: Text -> [Text]
extract = go . htmlParseEasy
  where
    go = \case
      HTMLDocument n m c ->
        concatMap go c
      e@(HTMLElement "a" s a c) ->
        case htmlElemAttrFind (htmlAttrHasName "href") e of
          Just (HTMLAttr n v s) ->
            v : concatMap go c
          Nothing ->
            concatMap go c
      HTMLElement n s a c ->
        concatMap go c
      _otherwise ->
        []

testQuery :: Test
testQuery = testCase "sample query" $ do
  assertEqual "Sample 1" (Just "AAA") $ query (b h)
  where
    h = [r|
      <p>
        <span id="x" class="y z"></span>
        <br>
        <a href="bbb">AAA</a>
        <img>
      </p>
      |]
    b = fromJust
      . htmlSpaceRemove
      . fromJust
      . htmlDocBody
      . htmlParseEasy

query :: HTMLNode -> Maybe Text
query = htmlQueryExec $ do
  htmlQueryName "body"
  htmlQueryFirst
  htmlQueryName "p"
  htmlQueryFirst
  htmlQueryId "x"
  htmlQueryNext
  htmlQueryNext
  htmlQueryName "a"
  a <- htmlQueryNode
  htmlQuerySucc $
    fromMaybe "" $ htmlElemText a

testQuery2 = testCase "sample query2" $ do
  assertEqual "Sample 1" (htmlRender h') $
    htmlRender $ htmlMapElem query2 h
  where
    h = b [r|
      <section><div><img src="aaa"></div></section>
      <section><div><img src="bbb"></div></section>
      <section><div><img src="ccc"></div></section>
      |]
    h' = b [r|
      <section><a href="aaa">aaa</a></section>
      <section><a href="bbb">bbb</a></section>
      <section><a href="ccc">ccc</a></section>
      |]
    b = fromJust
      . htmlSpaceRemove
      . fromJust
      . htmlDocBody
      . htmlParseEasy

query2 :: HTMLNode -> HTMLNode
query2 = htmlQueryTry $ do
  htmlQueryName "div"
  htmlQueryOnly "img"
  a <- htmlQueryNode
  let Just b = htmlElemGetAttr "src" a
  htmlQuerySucc $
    htmlElem "a" [ htmlAttr "href" b ]
      [ htmlText b ]
