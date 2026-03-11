{-# LANGUAGE OverloadedStrings #-}

module Redact.Markdown.Test (tests) where

-- https://hackage.haskell.org/package/tasty
import Test.Tasty (TestTree, testGroup)

-- https://hackage.haskell.org/package/tasty-hunit
import Test.Tasty.HUnit ((@=?), testCase)

-- (redact)
import Redact.Markdown
  ( Line(NormalLine, RedactLine), Part(Redact, Stet), redact, redact'
  , redactLine, redactLine'
  )

------------------------------------------------------------------------------

testRedactLine :: TestTree
testRedactLine = testGroup "redactLine"
    [ testCase "empty" $ Just (NormalLine []) @=? redactLine ""
    , testCase "TT" $ Just (NormalLine [Stet "``"]) @=? redactLine "``"
    , testCase "T" $ Nothing @=? redactLine "`"
    , testCase "TTT" $ Nothing @=? redactLine "```"
    , testCase "a" $ Just (NormalLine [Stet "a"]) @=? redactLine "a"
    , testCase "TaT" $
        Just (NormalLine [Stet "`", Redact "a", Stet "`"]) @=?
          redactLine "`a`"
    , testCase "TTa" $ Just (NormalLine [Stet "``a"]) @=? redactLine "``a"
    , testCase "aTT" $ Just (NormalLine [Stet "a``"]) @=? redactLine "a``"
    , testCase "aT" $ Nothing @=? redactLine "a`"
    , testCase "Ta" $ Nothing @=? redactLine "`a"
    , testCase "TTaT" $ Nothing @=? redactLine "``a`"
    , testCase "TaTT" $ Nothing @=? redactLine "`a``"
    , testCase "TaTb" $
        Just (NormalLine [Stet "`", Redact "a", Stet "`b"]) @=?
          redactLine "`a`b"
    , testCase "aTbT" $
        Just (NormalLine [Stet "a`", Redact "b", Stet "`"]) @=?
          redactLine "a`b`"
    , testCase "aTTb" $ Just (NormalLine [Stet "a``b"]) @=? redactLine "a``b"
    , testCase "TaTbT" $ Nothing @=? redactLine "`a`b`"
    , testCase "aTbTc" $
        Just (NormalLine [Stet "a`", Redact "b", Stet "`c"]) @=?
          redactLine "a`b`c"
    , testCase "TaTbTcT" $
        Just
          ( NormalLine
              [Stet "`", Redact "a", Stet "`b`", Redact "c", Stet "`"]
          ) @=?
          redactLine "`a`b`c`"
    , testCase "TaTTbTc" $
        Just
          ( NormalLine
              [Stet "`", Redact "a", Stet "``", Redact "b", Stet "`c"]
          ) @=?
          redactLine "`a``b`c"
    , testCase "TaTTbTTcT" $
        Just
          ( NormalLine
              [ Stet "`", Redact "a", Stet "``", Redact "b", Stet "``"
              , Redact "c" , Stet "`"
              ]
          ) @=? redactLine "`a``b``c`"
    , testCase "TaTbTcTd" $
        Just
          ( NormalLine
              [Stet "`", Redact "a", Stet "`b`", Redact "c", Stet "`d"]
          ) @=?
          redactLine "`a`b`c`d"
    , testCase "aTbTTcTd" $
        Just
          ( NormalLine
              [Stet "a`", Redact "b", Stet "``", Redact "c", Stet "`d"]
          ) @=?
          redactLine "a`b``c`d"
    , testCase "aTbTcTcT" $
        Just
          ( NormalLine
              [Stet "a`", Redact "b", Stet "`c`", Redact "d", Stet "`"]
          ) @=?
          redactLine "a`b`c`d`"
    ]

------------------------------------------------------------------------------

testRedactLine' :: TestTree
testRedactLine' = testGroup "redactLine'"
    [ testCase "empty" $ NormalLine [] @=? redactLine' ""
    , testCase "TT" $ NormalLine [Stet "``"] @=? redactLine' "``"
    , testCase "T" $ NormalLine [Stet "`"] @=? redactLine' "`"
    , testCase "TTT" $ NormalLine [Stet "```"] @=? redactLine' "```"
    , testCase "a" $ NormalLine [Stet "a"] @=? redactLine' "a"
    , testCase "TaT" $
        NormalLine [Stet "`", Redact "a", Stet "`"] @=? redactLine' "`a`"
    , testCase "TTa" $ NormalLine [Stet "``a"] @=? redactLine' "``a"
    , testCase "aTT" $ NormalLine [Stet "a``"] @=? redactLine' "a``"
    , testCase "aT" $ NormalLine [Stet "a`"] @=? redactLine' "a`"
    , testCase "Ta" $ NormalLine [Stet "`", Redact "a"] @=? redactLine' "`a"
    , testCase "TTaT" $ NormalLine [Stet "``a`"] @=? redactLine' "``a`"
    , testCase "TaTT" $
        NormalLine [Stet "`", Redact "a", Stet "``"] @=? redactLine' "`a``"
    , testCase "TaTb" $
        NormalLine [Stet "`", Redact "a", Stet "`b"] @=? redactLine' "`a`b"
    , testCase "aTbT" $
        NormalLine [Stet "a`", Redact "b", Stet "`"] @=? redactLine' "a`b`"
    , testCase "aTTb" $ NormalLine [Stet "a``b"] @=? redactLine' "a``b"
    , testCase "TaTbT" $
        NormalLine [Stet "`", Redact "a", Stet "`b`"] @=? redactLine' "`a`b`"
    , testCase "aTbTc" $
        NormalLine [Stet "a`", Redact "b", Stet "`c"] @=? redactLine' "a`b`c"
    , testCase "TaTbTcT" $
        NormalLine [Stet "`", Redact "a", Stet "`b`", Redact "c", Stet "`"]
          @=? redactLine' "`a`b`c`"
    , testCase "TaTTbTc" $
        NormalLine [Stet "`", Redact "a", Stet "``", Redact "b", Stet "`c"]
          @=? redactLine' "`a``b`c"
    , testCase "TaTTbTTcT" $
        NormalLine
          [ Stet "`", Redact "a", Stet "``", Redact "b", Stet "``"
          , Redact "c", Stet "`"
          ]
          @=? redactLine' "`a``b``c`"
    , testCase "TaTbTcTd" $
        NormalLine [Stet "`", Redact "a", Stet "`b`", Redact "c", Stet "`d"]
        @=? redactLine' "`a`b`c`d"
    , testCase "aTbTTcTd" $
        NormalLine [Stet "a`", Redact "b", Stet "``", Redact "c", Stet "`d"]
        @=? redactLine' "a`b``c`d"
    , testCase "aTbTcTcT" $
        NormalLine [Stet "a`", Redact "b", Stet "`c`", Redact "d", Stet "`"]
        @=? redactLine' "a`b`c`d`"
    ]

------------------------------------------------------------------------------

testRedact :: TestTree
testRedact = testGroup "redact"
    [ testCase "empty" $ Right [] @=? redact ""
    , testCase "newline" $ Right [NormalLine []] @=? redact "\n"
    , testCase "newlines" $
        Right [NormalLine [], NormalLine []] @=? redact "\n\n"
    , testCase "inline_single" $
        Right [NormalLine [Stet "a`", Redact "b", Stet "`c"]] @=?
          redact "a`b`c\n"
    , testCase "inline_multiple" $
        Right
          [ NormalLine [Stet "a`", Redact "b", Stet "`c"]
          , NormalLine
              [Stet "`", Redact "d", Stet "`e`", Redact "f", Stet "`"]
          ] @=? redact "a`b`c\n`d`e`f`\n"
    , testCase "inline_invalid" $
        Left "inline code not terminated (line 2)" @=? redact "a`b`c\nd`e\n\n"
    , testCase "fenced_three" $
        Right
          [ NormalLine [Stet "```"]
          , RedactLine "a`b`c"
          , NormalLine [Stet "```"]
          ] @=? redact "```\na`b`c\n```\n"
    , testCase "fenced_extra" $
        Right
          [ NormalLine [Stet "`````"]
          , RedactLine  "a`b`c"
          , NormalLine [Stet "```````"]
          ] @=? redact "`````\na`b`c\n```````\n"
    , testCase "fenced_multiple" $
        Right
          [ NormalLine [Stet "a"]
          , NormalLine [Stet "```"]
          , RedactLine "b"
          , RedactLine "c"
          , NormalLine [Stet "```"]
          , NormalLine [Stet "```"]
          , RedactLine "d"
          , NormalLine [Stet "```"]
          ] @=? redact "a\n```\nb\nc\n```\n```\nd\n```\n"
    , testCase "fenced_language" $
        Right
          [ NormalLine [Stet "a"]
          , NormalLine [Stet "```haskell"]
          , RedactLine "--"
          , NormalLine [Stet "```"]
          ] @=? redact "a\n```haskell\n--\n```\n"
    , testCase "fenced_invalid" $
        Left "fenced code not terminated (line 4)" @=? redact "a\n```\nb\n"
    ]

------------------------------------------------------------------------------

testRedact' :: TestTree
testRedact' = testGroup "redact'"
    [ testCase "empty" $ [] @=? redact' ""
    , testCase "newline" $ [NormalLine []] @=? redact' "\n"
    , testCase "newlines" $ [NormalLine [], NormalLine []] @=? redact' "\n\n"
    , testCase "inline_single" $
        [NormalLine [Stet "a`", Redact "b", Stet "`c"]] @=? redact' "a`b`c\n"
    , testCase "inline_multiple" $
        [ NormalLine [Stet "a`", Redact "b", Stet "`c"]
        , NormalLine [Stet "`", Redact "d", Stet "`e`", Redact "f", Stet "`"]
        ] @=? redact' "a`b`c\n`d`e`f`\n"
    , testCase "inline_invalid" $
        [ NormalLine [Stet "a`", Redact "b", Stet "`c"]
        , NormalLine [Stet "d`", Redact "e"]
        , NormalLine []
        ] @=? redact' "a`b`c\nd`e\n\n"
    , testCase "fenced_three" $
        [ NormalLine [Stet "```"]
        , RedactLine "a`b`c"
        , NormalLine [Stet "```"]
        ] @=? redact' "```\na`b`c\n```\n"
    , testCase "fenced_extra" $
        [ NormalLine [Stet "`````"]
        , RedactLine "a`b`c"
        , NormalLine [Stet "```````"]
        ] @=? redact' "`````\na`b`c\n```````\n"
    , testCase "fenced_multiple" $
        [ NormalLine [Stet "a"]
        , NormalLine [Stet "```"]
        , RedactLine "b"
        , RedactLine "c"
        , NormalLine [Stet "```"]
        , NormalLine [Stet "```"]
        , RedactLine "d"
        , NormalLine [Stet "```"]
        ] @=? redact' "a\n```\nb\nc\n```\n```\nd\n```\n"
    , testCase "fenced_language" $
        [ NormalLine [Stet "a"]
        , NormalLine [Stet "```haskell"]
        , RedactLine "--"
        , NormalLine [Stet "```"]
        ] @=? redact' "a\n```haskell\n--\n```\n"
    , testCase "fenced_invalid" $
        [ NormalLine [Stet "a"]
        , NormalLine [Stet "```"]
        , RedactLine "b"
        ] @=? redact' "a\n```\nb\n"
    ]

------------------------------------------------------------------------------

tests :: TestTree
tests = testGroup "Redact.Markdown"
    [ testRedactLine
    , testRedactLine'
    , testRedact
    , testRedact'
    ]
