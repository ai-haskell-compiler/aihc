{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as B
import Data.Maybe
import qualified Data.Vector as V
import Test.HUnit

import Regex.RE2

main :: IO Counts
main = runTestTT tests

tests :: Test
tests = TestList
  [ test_CompileSuccess
  , test_CompileFailure
  , test_PatternGroups
  , test_Match
  , test_Find
  , test_Replace
  , test_ReplaceAll
  , test_Extract
  , test_OptionEncoding
  , test_QuoteMeta
  ]

test_CompileSuccess :: Test
test_CompileSuccess = TestLabel "compile.success" $ TestCase $
  compT (compile (b "^foo$")) $ \p ->
    assertEqual "compiled" (patternInput p) (b "^foo$")

test_CompileFailure :: Test
test_CompileFailure = TestLabel "compile.failure" $ TestCase $
  case compile (b "^f(oo$") of
    Right _ -> assertBool "should not compile" False
    Left err -> do
      assertEqual "err message" (errorMessage err) "missing ): ^f(oo$"
      assertEqual "err code" (errorCode err) ErrorMissingParen

test_PatternGroups :: Test
test_PatternGroups = TestLabel "patternGroups" $ TestCase $
  compT (compile (b "^(foo)(?P<named1>bar)(?P<named2>baz)$")) $ \p ->
    assertEqual "groups" (patternGroups p)
      (V.fromList
        [ Nothing
        , Just (b "named1")
        , Just (b "named2")
        ])

test_Match :: Test
test_Match = TestLabel "match" $ TestCase $
  compT (compile (b "(ba)r")) $ \p -> do
    case match p (b foobar) 0 (length foobar) Nothing 0 of
      Nothing -> assertBool "should have matched" False
      Just m -> assertEqual "match" (matchGroups m) (V.fromList [])

    let found = match p (b foobar) 0 (length foobar) (Just AnchorStart) 0
    assertBool "no match" (isNothing found)

    case match p (b foobar) 0 (length foobar) Nothing 1 of
      Nothing -> assertBool "should have matched" True
      Just m ->
        assertEqual "match" (matchGroups m)
        (V.fromList
            [ Just (b "bar")
            ])
  where
    foobar :: String
    foobar = "foo bar"

test_Find :: Test
test_Find = TestLabel "find" $ TestCase $
  compT (compile (b "(foo)|(\\d)(\\d)\\d")) $ \p ->
    case find p (b "abc 123") of
      Nothing -> assertBool "should have matched" False
      Just m -> assertEqual "match" (matchGroups m)
        (V.fromList
          [ Just (b "123")
          , Nothing
          , Just (b "1")
          , Just (b "2")
          ])

test_Replace :: Test
test_Replace = TestLabel "replace" $ TestCase $
  compT (compile (b "foo")) $ \p -> do
    assertEqual "no match" (replace p (b "no match") (b "baz")) (b "no match", False)
    assertEqual "replacement" (replace p (b "foo bar foo bar") (b "baz")) (b "baz bar foo bar", True)
    assertEqual "escaped" (replace p (b "foo bar foo bar") (b "b\\1az")) (b "baz bar foo bar", True)

test_ReplaceAll :: Test
test_ReplaceAll = TestLabel "replaceAll" $ TestCase $
  compT (compile (b "foo")) $ \p -> do
    assertEqual "no match" (replaceAll p (b "no match") (b "baz")) (b "no match", 0)
    assertEqual "replacement" (replaceAll p (b "foo bar foo bar") (b "baz")) (b "baz bar baz bar", 2)

test_Extract :: Test
test_Extract = TestLabel "extract" $ TestCase $
  compT (compile (b "(foo)")) $ \p -> do
    assertEqual "no match" (extract p (b "no match") (b "baz")) Nothing
    assertEqual "replacement" (extract p (b "foo bar foo bar") (b "baz")) (Just (b "baz"))
    assertEqual "escaped" (extract p (b "foo bar foo bar") (b "\\1baz")) (Just (b "foobaz"))

test_OptionEncoding :: Test
test_OptionEncoding = TestLabel "optionEncoding" $ TestCase $ do
  compT (compileWith utfOpts (b "^(.)")) $ \utfP ->
    assertEqual "utf" (extract utfP (b "\xCE\xBB") (b "\\1")) (Just (b "\xCE\xBB"))

  compT (compileWith latinOpts (b "^(.)")) $ \latinP ->
    assertEqual "latin" (extract latinP (b "\xCE\xBB") (b "\\1")) (Just (b "\xCE"))
    
  where
    utfOpts = defaultOptions
    latinOpts = defaultOptions { optionEncoding = EncodingLatin1 }

test_QuoteMeta :: Test
test_QuoteMeta = TestLabel "quoteMeta" $ TestCase $ do
  assertEqual "quote" (quoteMeta (b "^foo$")) (b "\\^foo\\$")
  assertEqual "escaped" (quoteMeta (b "^f\NULoo$")) (b "\\^f\\x00oo\\$")

b :: String -> B.ByteString
b = B.pack

compT :: Either Error Pattern -> (Pattern -> IO ()) -> IO ()
compT r act =
  case r of
    Left _ -> assertBool "should have compiled" False
    Right p -> act p