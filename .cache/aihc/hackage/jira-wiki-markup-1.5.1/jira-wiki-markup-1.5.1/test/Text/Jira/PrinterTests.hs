{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Text.Jira.PrinterTests
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki printer.
-}
module Text.Jira.PrinterTests (tests) where

import Prelude hiding (unlines)
import Data.String (IsString (fromString))
import Data.Text (Text, pack, unlines)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Jira.Markup
import Text.Jira.Printer

tests :: TestTree
tests = testGroup "Printer"
  [ testGroup "doc"
    [ testCase "empty document" $
      pretty (Doc []) @?= ""
    ]

  , testGroup "blocks"
    [ testCase "simple paragraph" $
      let para = Para [Str "Hello,", Space, Str "World!"]
      in renderBlock' para @?= "Hello, World!\n"

    , testCase "two paragraphs" $
      let para1 = Para [Str "First", Space, Str "paragraph."]
          para2 = Para [Str "Second", Space, Str "paragraph."]
      in prettyBlocks [para1, para2] @?= unlines
         [ "First paragraph."
         , ""
         , "Second paragraph."
         ]

    , testGroup "header"
      [ testCase "simple" $
        let header = Header 1 [Str "test", Space, Str "header"]
        in renderBlock' header @?= "h1. test header"

      , testCase "header" $
        let header = Header 5 [Str "test", Space, Str "header"]
        in renderBlock' header @?= "h5. test header"
      ]

    , testCase "horizontal rule" $
      renderBlock' HorizontalRule @?= "----"

    , testCase "color" $
      renderBlock' (Color (ColorName "blue") [Para [Str "yabadee"]]) @?=
      "{color:blue}\nyabadee\n{color}"

    , testGroup "list"
      [ testCase "simple list" $
        let list = List SquareBullets [ [Para [Str "first"]]
                                      , [Para [Str "second"]]
                                      , [Para [Str "third"]]
                                      ]
        in renderBlock' list @?= unlines
           [ "- first"
           , "- second"
           , "- third"
           ]

      , testCase "nested list" $
        let list = List CircleBullets
                   [ [Para [Str "first"]]
                   , [ List Enumeration   [ [Para [Str "second-1"]]
                                          , [Para [Str "second-2"]]]]
                   , [ Para [Str "third"]
                     , List CircleBullets [ [Para [Str "third-1"]]
                                          , [Para [Str "third-2"]]]]
                   ]
        in renderBlock' list @?= unlines
           [ "* first"
           , "*# second-1"
           , "*# second-2"
           , "* third"
           , "** third-1"
           , "** third-2"
           ]
      ]

    , testCase "table" $
      let headerRow = Row [ HeaderCell [Para [Str "one"]]
                          , HeaderCell [Para [Str "two"]]
                          ]
          bodyRow = Row [ BodyCell [Para [Str "1"]]
                        , BodyCell [Para [Str "2"]]
                        ]
          table = Table [headerRow, bodyRow]
      in renderBlock' table @?= "|| one || two ||\n| 1 | 2 |\n"

    , testCase "para after table" $
      let table = Table [Row [BodyCell [Para [Str "boring"]]]]
          para = Para [Str "after", Space, Str "table"]
      in prettyBlocks [table, para] @?= "| boring |\n\nafter table\n"

    , testCase "para after list" $
      let list = List Enumeration [[Para [Str "boring"]]]
          para = Para [Str "after", Space, Str "table"]
      in prettyBlocks [list, para] @?= "# boring\n\nafter table\n"

    , testGroup "block quote"
      [ testCase "single-line block quote" $
        let bq = BlockQuote [Para
                   [Str "Errare", Space, Str "humanum", Space, Str "est."]
                 ]
        in prettyBlocks [bq] @?= "bq. Errare humanum est."

      , testCase "multi-line block quote" $
        let bq = BlockQuote [Para [Str "Show", Linebreak, Str "me"]]
        in prettyBlocks [bq] @?= "{quote}\nShow\nme\n{quote}"

      , testCase "multi-paragraph block quote" $
        let bq = BlockQuote [Para [Str "Only."], Para [Str "You."]]
        in prettyBlocks [bq] @?= "{quote}\nOnly.\n\nYou.\n{quote}"
      ]

    , testGroup "panel"
      [ testCase "simple panel" $
        let panel = Panel [] [Para [Str "Contents!"]]
        in prettyBlocks [panel] @?= "{panel}\nContents!\n{panel}"

      , testCase "panel with title" $
        let panel = Panel [Parameter "title" "Gimme"] [Para [Str "Contents!"]]
        in prettyBlocks [panel] @?= "{panel:title=Gimme}\nContents!\n{panel}"
      ]
    ]

  , testGroup "isolated inline"
    [ testCase "SpecialChar" $
      renderInline (SpecialChar '*') @?= "\\*"

    , testCase "AutoLink" $
      renderInline (AutoLink (URL "https://example.org")) @?=
      "https://example.org"

    , testCase "citation" $
      renderInline (Citation [Str "John", Space, Str "Doe"]) @?=
      "??John Doe??"

    , testCase "Emoji" $
      renderInline (Emoji IconSmiling) @?= ":D"

    , testCase "thumbnail" $
      renderInline (Image [Parameter "thumbnail" ""] (URL "example.jpg")) @?=
      "!example.jpg|thumbnail!"

    , testCase "image attributes" $
      let params = [Parameter "align" "right", Parameter "vspace" "4"]
      in renderInline (Image params (URL "example.jpg")) @?=
         "!example.jpg|align=right, vspace=4!"

    , testGroup "link"
      [ testCase "external link" $
        renderInline (Link External [Str "example"] "https://example.org") @?=
        "[example|https://example.org]"

      , testCase "email link" $
        renderInline (Link Email [Str "example"] "me@example.org") @?=
        "[example|mailto:me@example.org]"

      , testCase "attachment" $
        renderInline (Link Attachment [Str "a", Space, Str "b"] "test.txt") @?=
        "[a b^test.txt]"

      , testCase "attachment without description" $
        renderInline (Link Attachment [] "something.txt") @?=
        "[^something.txt]"

      , testCase "attachment with space and Unicode" $
        renderInline (Link Attachment [] "Übergang links.txt") @?=
        "[^Übergang links.txt]"

      , testCase "user" $
        renderInline (Link User [Str "John", Space, Str "Doe"] "ab34-cdef") @?=
        "[John Doe|~ab34-cdef]"

      , testCase "smart link" $
        renderInline (Link SmartLink [Str "repo"]
                      "https://github.com/tarleb/jira-wiki-markup") @?=
        "[repo|https://github.com/tarleb/jira-wiki-markup|smart-link]"

      , testCase "smart card" $
        renderInline (Link SmartCard [Str "hslua"]
                      "https://github.com/hslua/hslua") @?=
        "[hslua|https://github.com/hslua/hslua|smart-card]"
      ]

    , testCase "Styled Emphasis" $
      renderInline (Styled Emphasis [Str "Hello,", Space, Str "World!"]) @?=
      "_Hello, World!_"

    , testCase "Styled Strong" $
      renderInline (Styled Strong [Str "Hello,", Space, Str "World!"]) @?=
      "*Hello, World!*"

    , testCase "Colored inlines" $
      renderInline (ColorInline (ColorName "red")
                    [Str "This", Space, Str "is", Space, Str "red."]) @?=
      "{color:red}This is red.{color}"
    ]

  , testGroup "combined inlines"
    [ testCase "opening brace between words" $
      prettyInlines [Str "a", SpecialChar '{', Str "b"] @?=
      "a\\{b"

    , testCase "other special char between words" $
      prettyInlines [Str "easy", SpecialChar '-', Str "peasy"] @?=
      "easy-peasy"

    , testCase "special char before word" $
      prettyInlines [SpecialChar '*', Str "star"] @?=
      "\\*star"

    , testCase "markup within word" $
      prettyInlines [Str "H", Styled Subscript [Str "2"], Str "O"] @?=
      "H{~}2{~}O"

    , testCase "markup followed by punctuation" $
      prettyInlines [Styled Emphasis [Str "Word"], Str "."] @?=
      "_Word_."

    , testCase "colon as last character" $
      prettyInlines [Str "end", SpecialChar ':'] @?=
      "end:"

    , testCase "semicolon is escaped before close paren" $
      -- would be treated as "winking smiley" otherwise
      prettyInlines [SpecialChar ';', Str ")"] @?=
      "\\;)"

    , testCase "colon is not escaped before space" $
      prettyInlines [SpecialChar ':', Space, Str "end"] @?=
      ": end"

    , testCase "colon is not escaped before non-emoji word" $
      prettyInlines [Space, SpecialChar ':', Str "PA"] @?=
      " :PA"

    , testCase "closing brace between spaces" $
      prettyInlines [Space, SpecialChar '}', Space] @?=
      " \\} "

    , testCase "colon not escaped before opening paren" $
      -- escaping the paren is enough
      prettyInlines [SpecialChar ':', SpecialChar '('] @?=
      ":\\("

    , testGroup "question marks"
      [ testCase "escaped if followed by question mark" $
        prettyInlines [SpecialChar '?', SpecialChar '?'] @?=
        "\\??"

      , testCase "unescaped before space" $
        prettyInlines [SpecialChar '?', Space, Str "foo"] @?=
        "? foo"
      ]
    ]
  ]

renderBlock' :: Block -> Text
renderBlock' = withDefault . renderBlock

instance IsString URL where
  fromString = URL . pack
