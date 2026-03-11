{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}

-- Copyright : 2011-2014 Magnus Therning
-- License   : BSD3
module ReaderI (
    allTests,
) where

import Test.Tasty (TestTree)
import Test.Tasty.HUnit (Assertion, testCase, (@=?))
import Test.Tasty.TH (testGroupGenerator)
import Text.ParserCombinators.Parsec as P (Parser, parse)

import Data.Ini.Reader.Internals (
    IniFile (CommentL, OptionContL, OptionL, SectionL),
    iniParser,
    noiseParser,
    optContParser,
    optLineParser,
    secParser,
 )

-- Convenience function that translates a parser result to something that's
-- easier to check.
p2E :: Parser a -> String -> String -> Either String a
p2E p s t =
    let
        res = P.parse p s t
     in
        case res of
            Left _ -> Left "bad"
            Right e -> Right e

-- {{{1 secParser
case_secParserAllowedChars1 :: Assertion
case_secParserAllowedChars1 =
    let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[foo]\n"
     in
        expected @=? actual

case_secParserAllowedChars2 :: Assertion
case_secParserAllowedChars2 =
    let
        expected = Right $ SectionL "FooBar"
        actual = p2E secParser "sec" "[FooBar]\n"
     in
        expected @=? actual

case_secParserAllowedChars3 :: Assertion
case_secParserAllowedChars3 =
    let
        expected = Right $ SectionL "@Foo/Bar-"
        actual = p2E secParser "sec" "[@Foo/Bar-]\n"
     in
        expected @=? actual

case_secParserAllowedChars4 :: Assertion
case_secParserAllowedChars4 =
    let
        expected = Right $ SectionL "foo123"
        actual = p2E secParser "sec" "[foo123]\n"
     in
        expected @=? actual

case_secParserAllowedChars5 :: Assertion
case_secParserAllowedChars5 =
    let
        expected = Right $ SectionL "_foo"
        actual = p2E secParser "sec" "[_foo]\n"
     in
        expected @=? actual

case_secParserDropSpace :: Assertion
case_secParserDropSpace =
    let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[ \tfoo\t ]\n"
     in
        expected @=? actual

case_secParserDropTrailing :: Assertion
case_secParserDropTrailing =
    let
        expected = Right $ SectionL "foo"
        actual = p2E secParser "sec" "[foo]  \t foobar\n"
     in
        expected @=? actual

case_secParserAllowGit1 :: Assertion
case_secParserAllowGit1 =
    let
        expected = Right $ SectionL "branch \"master\""
        actual = p2E secParser "sec" "[branch \"master\"]\n"
     in
        expected @=? actual

case_secParserAllowGit2 :: Assertion
case_secParserAllowGit2 =
    let
        expected = Right $ SectionL "foo \"bar.baz\""
        actual = p2E secParser "sec" "[foo \"bar.baz\"]\n"
     in
        expected @=? actual

-- {{{1 optLineParser
case_optLineParserAllowedChars1 :: Assertion
case_optLineParserAllowedChars1 =
    let
        expected = Right $ OptionL "foo" "bar"
        actual = p2E optLineParser "optLine" "foo=bar\n"
     in
        expected @=? actual

case_optLineParserAllowedChars2 :: Assertion
case_optLineParserAllowedChars2 =
    let
        expected = Right $ OptionL "Foo" "bAr"
        actual = p2E optLineParser "optLine" "Foo=bAr\n"
     in
        expected @=? actual

case_optLineParserAllowedChars3 :: Assertion
case_optLineParserAllowedChars3 =
    let
        expected = Right $ OptionL "foo@/foo-" "bar"
        actual = p2E optLineParser "optLine" "foo@/foo-=bar\n"
     in
        expected @=? actual

case_optLineParserAllowedChars4 :: Assertion
case_optLineParserAllowedChars4 =
    let
        expected = Right $ OptionL "foo123" "bar"
        actual = p2E optLineParser "optLine" "foo123=bar\n"
     in
        expected @=? actual

case_optLineParserAllowedChars5 :: Assertion
case_optLineParserAllowedChars5 =
    let
        expected = Right $ OptionL "_foo" "bar"
        actual = p2E optLineParser "optLine" "_foo=bar\n"
     in
        expected @=? actual

case_optLineParserAllowedChars6 :: Assertion
case_optLineParserAllowedChars6 =
    let
        expected = Right $ OptionL "foo bar" "baz"
        actual = p2E optLineParser "optLine" "foo bar=baz\n"
     in
        expected @=? actual

case_optLineParserDisallowedChars1 :: Assertion
case_optLineParserDisallowedChars1 =
    let
        expected = Left "bad"
        actual = p2E optLineParser "optLine" "foo.bar=baz\n"
     in
        expected @=? actual

case_optLineParserDropSpace1 :: Assertion
case_optLineParserDropSpace1 =
    let
        expected = Right $ OptionL "foo" "bar"
        actual = p2E optLineParser "optLine" " foo =  bar\n"
     in
        expected @=? actual

case_optLineParserDropSpace2 :: Assertion
case_optLineParserDropSpace2 =
    let
        expected = Right $ OptionL "foo" "bar"
        actual = p2E optLineParser "optLine" " \tfoo\t \t=\t \t bar\n"
     in
        expected @=? actual

case_optLineParserKeepSpace :: Assertion
case_optLineParserKeepSpace =
    let
        expected = Right $ OptionL "foo" "bar \t \t"
        actual = p2E optLineParser "optLine" "foo\t \t=\t \t bar \t \t\n"
     in
        expected @=? actual

-- {{{1 optContParser
case_optContParserSpace :: Assertion
case_optContParserSpace =
    let
        expected = Right $ OptionContL "foo"
        actual = p2E optContParser "optCont" " foo\n"
     in
        expected @=? actual

case_optContParserTab :: Assertion
case_optContParserTab =
    let
        expected = Right $ OptionContL "foo"
        actual = p2E optContParser "optCont" "\tfoo\n"
     in
        expected @=? actual

case_optContParserKeepTrailing :: Assertion
case_optContParserKeepTrailing =
    let
        expected = Right $ OptionContL "foo  \t\t"
        actual = p2E optContParser "optCont" "\tfoo  \t\t\n"
     in
        expected @=? actual

-- {{{1 noiseParser
case_noiseParserEmptyLine :: Assertion
case_noiseParserEmptyLine =
    let
        expected = Right CommentL
        actual = p2E noiseParser "noise" "\n"
     in
        expected @=? actual

case_noiseParserComment1 :: Assertion
case_noiseParserComment1 =
    let
        expected = Right CommentL
        actual = p2E noiseParser "noise" "# a comment\n"
     in
        expected @=? actual

case_noiseParserComment2 :: Assertion
case_noiseParserComment2 =
    let
        expected = Right CommentL
        actual = p2E noiseParser "noise" "; another comment\n"
     in
        expected @=? actual

case_noiseParserNonEmpty :: Assertion
case_noiseParserNonEmpty =
    let
        expected = Left "bad"
        actual = p2E noiseParser "noise" " \n"
     in
        expected @=? actual

-- {{{1 iniParser
case_iniParserEmpty :: Assertion
case_iniParserEmpty = expected @=? actual
  where
    expected = Right []
    actual = p2E iniParser "parsing empty file" ""

-- {{{1 buildConfig
-- TBD

allTests :: TestTree
allTests = $(testGroupGenerator)
