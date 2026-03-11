{-|
Module      : Text.Jira.ParserTests
Copyright   : © 2019–2023 Albert Krewinkel
License     : MIT

Maintainer  : Albert Krewinkel <tarleb@zeitkraut.de>
Stability   : alpha
Portability : portable

Tests for the jira wiki parser.
-}
module Text.Jira.ParserTests (tests) where

import Data.Text ()
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Text.Jira.Parser
import qualified Text.Jira.Parser.BlockTests
import qualified Text.Jira.Parser.InlineTests

tests :: TestTree
tests = testGroup "Parser"
  [ Text.Jira.Parser.InlineTests.tests
  , Text.Jira.Parser.BlockTests.tests
  , testGroup "doc"
    [ testCase "empty document" $
      parse "" @?=
      Right (Doc [])

    , testCase "simple document" $
      parse "h1. test\nThis is ok." @?=
      Right (Doc [ Header 1 [Str "test"]
                 , Para [Str "This", Space, Str "is", Space, Str "ok."]])

    , testCase "leading blank lines" $
      parse "\n\ntext\n" @?=
      Right (Doc [Para [Str "text"]])
    ]

  , testGroup "plainText"
    [ testCase "word" $
      plainText "kthxbye" @?=
      Right [Str "kthxbye"]

    , testCase "words" $
      plainText "be Berlin" @?=
      Right [Str "be", Space, Str "Berlin"]

    , testCase "smiley" $
      plainText ":)" @?=
      Right [Str "\\:)"]

    , testCase "smiley and text" $
      plainText ":D lol" @?=
      Right [Str "\\:D", Space, Str "lol"]

    , testCase "icon after word" $
      plainText "f(x)" @?=
      Right [Str "f\\(x)"]

    , testCase "icon-sequence at start of word" $
      plainText ":PA" @?=
      Right [SpecialChar ':', Str "PA"]

    , testCase "icon-sequence followed by digit" $
      plainText ":P2" @?=
      Right [SpecialChar ':', Str "P2"]


    , testCase "special chars" $
      plainText "*not strong*" @?=
      Right [SpecialChar '*', Str "not", Space, Str "strong", SpecialChar '*']
    ]
  ]
