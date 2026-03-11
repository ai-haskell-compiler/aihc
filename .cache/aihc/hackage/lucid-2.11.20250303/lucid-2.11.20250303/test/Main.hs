{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ExtendedDefaultRules #-}

-- | Test suite for Lucid.

module Main where

import Lucid
import Lucid.Base
import Lucid.Bootstrap

import Control.Monad
import Control.Applicative
import Control.Monad.State.Strict

import qualified Data.Text as T

import Example1

import Test.HUnit
import Test.Hspec

-- | Test suite entry point, returns exit failure if any test fails.
main :: IO ()
main = hspec spec

-- | Test suite.
spec :: Spec
spec = do
  describe "text" testText
  describe "elements" testElements
  describe "attributes" testAttributes
  describe "attributes-with" testAttributesWith
  describe "extension" testExtension
  describe "special-elements" testSpecials
  describe "self-closing" testSelfClosing
  describe "commuteHtmlT" testCommuteHtmlT
  describe "monadFix" testMonadFix

(==?*) :: (Eq a, Show a) => a -> [a] -> Assertion
x ==?* xs | x `elem` xs = return ()
          | otherwise   = assertFailure $ show x ++ " is not equal to any of " ++ show xs

-- | Test text/unicode.
testText :: Spec
testText =
  do it "simple"
        (renderText "foo" ==
         "foo")
     it "escaping"
        (renderText "'<>" ==
         "&#39;&lt;&gt;")
     it "unicode"
        (renderText "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381" ==
         "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381")

-- | Test basic elements and nesting.
testElements :: Spec
testElements =
  do it "simple"
        (renderText (p_ "foo") ==
         "<p>foo</p>")
     it "escaping"
        (renderText (p_ "'<>") ==
         "<p>&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText (p_ "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") ==
         ("<p>fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText (p_ (p_ "Hello!")) ==
         "<p><p>Hello!</p></p>")
     it "empty"
        (renderText (p_ (p_ "")) ==
         "<p><p></p></p>")
     it "mixed"
        (renderText (p_ (style_ "")) ==
         "<p><style></style></p>")
     it "no closing"
        (renderText (p_ (input_ [])) ==
         "<p><input></p>")
     it "no closing"
        (renderText (makeXmlElementNoEnd "p") ==
         "<p/>")

-- | Test that attribute assigning works properly.
testAttributes :: Spec
testAttributes =
  do it "simple"
        (renderText (p_ [class_ "foo"] "foo") ==
         "<p class=\"foo\">foo</p>")
     it "escaping"
        (renderText (p_ [class_ "foo"] "'<>") ==
         "<p class=\"foo\">&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText
           (p_ [class_ "foo"]
               "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") ==
         ("<p class=\"foo\">fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText
           (p_ [class_ "foo"]
               (p_ "Hello!")) ==
         "<p class=\"foo\"><p>Hello!</p></p>")
     it "empty"
        (renderText
           (p_ [class_ "foo"]
               (p_ "")) ==
         "<p class=\"foo\"><p></p></p>")
     it "mixed" $
        renderText
           (p_ [class_ "foo",style_ "attrib"]
               (do style_ ""
                   style_ "")) ==?*
        [ "<p style=\"attrib\" class=\"foo\"><style></style><style></style></p>"
        , "<p class=\"foo\" style=\"attrib\"><style></style><style></style></p>"
        ]
     it "no closing"
        (renderText (p_ [class_ "foo"] (input_ [])) ==
         "<p class=\"foo\"><input></p>")
     it "multiple" $
        renderText (p_ [class_ "foo",id_ "zot"] "foo") ==?*
        [ "<p id=\"zot\" class=\"foo\">foo</p>"
        , "<p class=\"foo\" id=\"zot\">foo</p>"
        ]
     it "encoded"
        (renderText (p_ [class_ "foo<>"] "foo") ==
         "<p class=\"foo&lt;&gt;\">foo</p>")
     it "nesting attributes"
        (renderText
           (with (p_ [class_ "foo"])
                 [class_ "bar"]
                 "foo") ==
         "<p class=\"foobar\">foo</p>")

-- | Test that the `with' combinator still works as expected.
testAttributesWith :: Spec
testAttributesWith =
  do it "simple"
        (renderText (with p_ [class_ "foo"] "foo") ==
         "<p class=\"foo\">foo</p>")
     it "escaping"
        (renderText (with p_ [class_ "foo"] "'<>") ==
         "<p class=\"foo\">&#39;&lt;&gt;</p>")
     it "unicode"
        (renderText
           (with p_
                 [class_ "foo"]
                 "fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381") ==
         ("<p class=\"foo\">fo\243o\333o(\4326\728\8995\728\4326) \9835\65381*:.\65377. .\65377.:*\65381</p>"))
     it "nesting"
        (renderText
           (with p_
                 [class_ "foo"]
                 (p_ "Hello!")) ==
         "<p class=\"foo\"><p>Hello!</p></p>")
     it "empty"
        (renderText
           (with p_
                 [class_ "foo"]
                 (p_ "")) ==
         "<p class=\"foo\"><p></p></p>")
     it "mixed" $
        renderText
           (with p_
                 [class_ "foo",style_ "attrib"]
                 (style_ "")) ==?*
        [ "<p style=\"attrib\" class=\"foo\"><style></style></p>"
        , "<p class=\"foo\" style=\"attrib\"><style></style></p>"
        ]
     it "no closing" $
        renderText (with p_ [class_ "foo"] (with (input_ [type_ "text"]) [class_ "zot"])) ==?*
        [ "<p class=\"foo\"><input type=\"text\" class=\"zot\"></p>"
        , "<p class=\"foo\"><input class=\"zot\" type=\"text\"></p>"
        ]
     it "multiple" $
        renderText (with p_ [class_ "foo",id_ "zot"] "foo") ==?*
        [ "<p id=\"zot\" class=\"foo\">foo</p>"
        , "<p class=\"foo\" id=\"zot\">foo</p>"
        ]
     it "encoded"
        (renderText (with p_ [class_ "foo<>"] "foo") ==
         "<p class=\"foo&lt;&gt;\">foo</p>")
     it "nesting attributes"
        (renderText (with (with p_ [class_ "foo"]) [class_ "bar"] "foo") ==
        "<p class=\"foobar\">foo</p>")

-- | Test that one can use elements with extensible attributes.
testExtension :: Spec
testExtension =
  do it "bootstrap"
        (renderText (container_ "Foo!") ==
         "<div class=\" container \">Foo!</div>")
     it "bootstrap-attributes-extended" $
        renderText (container_ [class_ "bar",id_ "zot"] "Foo!") ==?*
        [ "<div class=\" container bar\" id=\"zot\">Foo!</div>"
        , "<div class=\" container bar\" id=\"zot\">Foo!</div>"
        ]

-- | Test special elements that do something different to normal
-- elements.
testSpecials :: Spec
testSpecials =
  do it "script"
        (renderText (script_ "alert('Hello, World!')") ==
         "<script>alert('Hello, World!')</script>")
     it "style"
        (renderText (style_ "body{background:url('Hello, World!')}") ==
         "<style>body{background:url('Hello, World!')}</style>")

-- | Elements which do not contain children.
testSelfClosing :: Spec
testSelfClosing =
  do it "br" (renderText (br_ []) == "<br>")
     it "hr" (renderText (hr_ []) == "<hr>")
     it "input"
        (renderText (input_ []) ==
         "<input>")
     it "input"
        (renderText (input_ [type_ "text"]) ==
         "<input type=\"text\">")

testCommuteHtmlT :: Spec
testCommuteHtmlT =
  do it "makes using inner monads easy"
        (example == renderText expected)
  where
    example = renderText $ evalState (commuteHtmlT exampleHtml) 1

    exampleHtml :: (Applicative m, MonadState Int m) => HtmlT m ()
    exampleHtml = ul_ $ replicateM_ 5 $ do
      x <- get
      put (x + 1)
      li_ $ toHtml $ show x

    expected = ul_ $ do
      li_ "1"
      li_ "2"
      li_ "3"
      li_ "4"
      li_ "5"

testMonadFix :: Spec
testMonadFix =
  do it "mdo" (renderText example == renderText expected)
  where
   toSectionId i = T.pack $ "section_" ++ show i

   toSectionTitle i = T.pack $ "Section " ++ show i

   example = mdo
     forM_ sections $ \(sectionName, sectionId) ->
       a_ [href_ sectionId] $ toHtml sectionName
     sections <- forM [1 .. 2] $ \sectionNum -> do
       let sectionId = toSectionId sectionNum
           sectionTitle = toSectionTitle sectionNum
       h1_ [id_ sectionId] $ toHtml sectionTitle
       return (sectionTitle, sectionId)
     return ()

   expected = do
     forM_ [1 .. 2] $ \sectionNum ->
       a_ [href_ $ toSectionId sectionNum] $ toHtml $ toSectionTitle sectionNum
     forM_ [1 .. 2] $ \sectionNum ->
       h1_ [id_ $ toSectionId sectionNum] $ toHtml $ toSectionTitle sectionNum
