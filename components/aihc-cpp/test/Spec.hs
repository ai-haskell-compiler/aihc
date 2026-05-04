{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Aihc.Cpp (Config (..), Result (..), Step (..), defaultConfig, preprocess)
import qualified Data.Map.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Test.Progress (CaseMeta (..), Outcome (..), evaluateCase, loadManifest)
import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.HUnit (Assertion, assertFailure, testCase)
import qualified Test.Tasty.QuickCheck as QC

main :: IO ()
main = do
  cases <- loadManifest
  checks <- mapM mkCase cases
  defaultMain
    ( testGroup
        "cpp-oracle"
        ( checks
            <> [linePragmaTest, dateTimeTest, functionMacroArgumentTest, functionMacroUnclosedCallTest, definedConditionSpacingTest, stringContinuationTests, tokenPastingTests, ccallLineCommentTest]
            <> [pragmaOnceTest]
            <> [QC.testProperty "dummy quickcheck property" prop_dummy]
        )
    )

-- | Dummy QuickCheck property that always passes.
-- Added so that --quickcheck-tests flag is accepted by the test suite.
prop_dummy :: Bool
prop_dummy = True

dateTimeTest :: TestTree
dateTimeTest =
  testGroup
    "__DATE__ and __TIME__"
    [ testCase "expands to provided values" $ do
        let cfg =
              defaultConfig
                { configMacros =
                    M.fromList
                      [ ("__DATE__", "\"Mar 15 2026\""),
                        ("__TIME__", "\"12:00:00\"")
                      ]
                }
            input = TE.encodeUtf8 "__DATE__ __TIME__"
        case preprocess cfg input of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\"Mar 15 2026\" \"12:00:00\"\n"
          _ -> assertFailure "expected Done",
      testCase "defaults to unix epoch" $ do
        let cfg = defaultConfig
            input = TE.encodeUtf8 "__DATE__ __TIME__"
        case preprocess cfg input of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\"Jan  1 1970\" \"00:00:00\"\n"
          _ -> assertFailure "expected Done"
    ]

(@?=) :: (Eq a, Show a) => a -> a -> Assertion
actual @?= expected =
  if actual == expected
    then pure ()
    else assertFailure ("expected: " <> show expected <> "\n but got: " <> show actual)

mkCase :: CaseMeta -> IO TestTree
mkCase meta =
  pure $ testCase (caseId meta) (assertCase meta)

assertCase :: CaseMeta -> Assertion
assertCase meta = do
  (_, outcome, details) <- evaluateCase meta
  case outcome of
    OutcomeFail ->
      assertFailure
        ( "cpp regression in "
            <> caseId meta
            <> " ["
            <> caseCategory meta
            <> "]: "
            <> details
        )
    _ -> pure ()

linePragmaTest :: TestTree
linePragmaTest =
  testCase "include emits line pragmas" $
    case preprocess defaultConfig {configInputFile = "root.hs"} (TE.encodeUtf8 "before\n#include \"nested.inc\"\nafter") of
      NeedInclude _ k ->
        case k (Just "inside") of
          Done result -> do
            let out = T.lines (resultOutput result)
                hasIncludePragma = any (T.isSuffixOf "nested.inc\"") out
            if hasIncludePragma && "#line 3 \"root.hs\"" `elem` out
              then pure ()
              else assertFailure "expected include line pragmas in output"
          NeedInclude {} -> assertFailure "unexpected nested include in line pragma test"
      Done _ -> assertFailure "expected include continuation step"

functionMacroArgumentTest :: TestTree
functionMacroArgumentTest =
  testCase "function-like macro keeps nested argument text" $
    case preprocess defaultConfig (TE.encodeUtf8 "#define PAIR(x,y) x + y\nPAIR((1 + 2), 3)") of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\n(1 + 2) + 3\n"
      _ -> assertFailure "expected Done"

functionMacroUnclosedCallTest :: TestTree
functionMacroUnclosedCallTest =
  testCase "unterminated function-like call does not expand macro" $
    case preprocess defaultConfig (TE.encodeUtf8 "#define ID() replaced\nID(") of
      Done result ->
        resultOutput result @?= "#line 1 \"<input>\"\n\nID(\n"
      _ -> assertFailure "expected Done"

definedConditionSpacingTest :: TestTree
definedConditionSpacingTest =
  testCase "defined handles whitespace around parenthesized name" $
    case preprocess defaultConfig (TE.encodeUtf8 "#define FLAG 1\n#if defined   ( FLAG )\nok\n#else\nbad\n#endif") of
      Done result ->
        if "ok\n" `T.isInfixOf` resultOutput result && not ("bad\n" `T.isInfixOf` resultOutput result)
          then pure ()
          else assertFailure ("expected ok branch to be active, output was: " <> show (resultOutput result))
      _ -> assertFailure "expected Done"

stringContinuationTests :: TestTree
stringContinuationTests =
  testGroup
    "Haskell string continuations"
    [ testCase "ordinary string accepts GCC double-backslash continuation" $
        assertPreprocessOutput
          gccStringContinuationInput
          (T.unlines ["#line 1 \"<input>\"", "x = \"a\\       \\b\""]),
      testCase "ordinary string preserves GHC single-backslash gap" $
        assertPreprocessOutput
          ghcStringGapInput
          (T.unlines ["#line 1 \"<input>\"", "x = \"a\\", "       \\b\""]),
      testCase "function macro argument accepts GCC double-backslash continuation" $
        assertPreprocessOutput
          gccStringContinuationMacroInput
          (T.unlines ["#line 1 \"<input>\"", "", "x = \"a\\       \\b\""]),
      testCase "function macro argument preserves GHC single-backslash gap" $
        assertPreprocessOutput
          ghcStringGapMacroInput
          (T.unlines ["#line 1 \"<input>\"", "", "x = \"a\\", "       \\b\""]),
      testCase "GCC continuation tracks string gaps across concatenation" $
        assertPreprocessOutput
          gccStringContinuationConcatInput
          (T.unlines ["#line 1 \"<input>\"", "x = \"a\\       \\\" <> y <> \"\\n\\       \\b\""]),
      testCase "double backslash outside strings is not line-spliced" $
        assertPreprocessOutput
          nonStringDoubleBackslashInput
          (T.unlines ["#line 1 \"<input>\"", "", "x = foo \\\\", "       bar"])
    ]

assertPreprocessOutput :: T.Text -> T.Text -> Assertion
assertPreprocessOutput input expected =
  case preprocess defaultConfig (TE.encodeUtf8 input) of
    Done result -> resultOutput result @?= expected
    _ -> assertFailure "expected Done"

tokenPastingTests :: TestTree
tokenPastingTests =
  testGroup
    "token pasting"
    [ testCase "CCALL macro expands stringizing and token pasting" $
        case preprocess defaultConfig (TE.encodeUtf8 ccallMacroInput) of
          Done result ->
            if "foreign import ccall unsafe \"foo\"" `T.isInfixOf` resultOutput result
              && "c_foo :: Int -> IO Int" `T.isInfixOf` resultOutput result
              then pure ()
              else assertFailure ("expected CCALL expansion in output, got: " <> show (resultOutput result))
          _ -> assertFailure "expected Done",
      testCase "token pasting joins both sides without expanding arguments first" $
        case preprocess defaultConfig (TE.encodeUtf8 tokenPasteRawArgInput) of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\n\nXY\n"
          _ -> assertFailure "expected Done",
      testCase "token pasting result is rescanned for further macro expansion" $
        case preprocess defaultConfig (TE.encodeUtf8 tokenPasteRescanInput) of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\n\n42\n"
          _ -> assertFailure "expected Done",
      testCase "token pasting supports prefix and suffix forms" $
        case preprocess defaultConfig (TE.encodeUtf8 tokenPasteAffixInput) of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\n\nleft right\n"
          _ -> assertFailure "expected Done",
      testCase "token pasting supports chained concatenation" $
        case preprocess defaultConfig (TE.encodeUtf8 tokenPasteChainedInput) of
          Done result ->
            resultOutput result @?= "#line 1 \"<input>\"\n\nfoobar\n"
          _ -> assertFailure "expected Done",
      testCase "token pasting survives Haskell block comments in arguments" $
        case preprocess defaultConfig (TE.encodeUtf8 tokenPasteHsCommentInput) of
          Done result ->
            resultOutput result
              @?= "#line 1 \"<input>\"\n\n{-# INLINE _bar #-}; _bar :: LensP Foo Baz{-comment-}; _bar = lens bar $ \\ Foo {..} bar_ -> Foo {bar = bar_, ..}\n"
          _ -> assertFailure "expected Done"
    ]

ccallLineCommentTest :: TestTree
ccallLineCommentTest =
  testCase "CCALL macro with -- comment in argument list" $
    case preprocess defaultConfig (TE.encodeUtf8 ccallLineCommentInput) of
      Done result ->
        if "foreign import ccall unsafe \"xls_wb_sheetcount\"" `T.isInfixOf` resultOutput result
          && "c_xls_wb_sheetcount :: XLSWorkbook -> IO CInt" `T.isInfixOf` resultOutput result
          && " -- Int32" `T.isInfixOf` resultOutput result
          then pure ()
          else assertFailure ("expected CCALL expansion with line comment, got: " <> show (resultOutput result))
      _ -> assertFailure "expected Done"

pragmaOnceTest :: TestTree
pragmaOnceTest =
  testCase "#pragma once skips repeated includes" $
    case preprocess defaultConfig {configInputFile = "root.hs"} (TE.encodeUtf8 "#include \"guarded.inc\"\n#include \"guarded.inc\"\nafter") of
      NeedInclude _ k1 ->
        case k1 (Just "#pragma once\ninside") of
          NeedInclude {} -> assertFailure "second include should be skipped"
          Done result -> do
            let output = resultOutput result
            if T.count "inside" output == 1 && "after\n" `T.isSuffixOf` output
              then pure ()
              else assertFailure ("expected guarded include once, got: " <> show output)
      Done _ -> assertFailure "expected include continuation step"

ccallLineCommentInput :: T.Text
ccallLineCommentInput =
  T.unlines
    [ "#define CCALL(name,signature) \\",
      "foreign import ccall unsafe #name \\",
      "    c_##name :: signature",
      "",
      "CCALL(xls_wb_sheetcount, XLSWorkbook -> IO CInt -- Int32)"
    ]

ccallMacroInput :: T.Text
ccallMacroInput =
  T.unlines
    [ "#define CCALL(name,signature) \\",
      "foreign import ccall unsafe #name \\",
      "    c_##name :: signature",
      "",
      "CCALL(foo, Int -> IO Int)"
    ]

tokenPasteRawArgInput :: T.Text
tokenPasteRawArgInput =
  T.unlines
    [ "#define X Y",
      "#define JOIN(a,b) a##b",
      "JOIN(X,Y)"
    ]

tokenPasteRescanInput :: T.Text
tokenPasteRescanInput =
  T.unlines
    [ "#define VALUE 42",
      "#define JOIN(a,b) a##b",
      "JOIN(VAL,UE)"
    ]

tokenPasteAffixInput :: T.Text
tokenPasteAffixInput =
  T.unlines
    [ "#define PREFIX(name) left##name",
      "#define SUFFIX(name) name##right",
      "PREFIX() SUFFIX()"
    ]

tokenPasteChainedInput :: T.Text
tokenPasteChainedInput =
  T.unlines
    [ "#define CHAIN(a,b,c) a##b##c",
      "CHAIN(foo,bar,)"
    ]

tokenPasteHsCommentInput :: T.Text
tokenPasteHsCommentInput =
  T.unlines
    [ "#define LENS(S,F,A) {-# INLINE _/**/F #-}; _/**/F :: LensP S A; _/**/F = lens F $ \\ S {..} F/**/_ -> S {F = F/**/_, ..}",
      "LENS(Foo,bar,Baz{-comment-})"
    ]

gccStringContinuationInput :: T.Text
gccStringContinuationInput =
  T.unlines
    [ "x = \"a\\\\",
      "       \\b\""
    ]

ghcStringGapInput :: T.Text
ghcStringGapInput =
  T.unlines
    [ "x = \"a\\",
      "       \\b\""
    ]

gccStringContinuationMacroInput :: T.Text
gccStringContinuationMacroInput =
  T.unlines
    [ "#define ID(x) x",
      "x = ID(\"a\\\\",
      "       \\b\")"
    ]

ghcStringGapMacroInput :: T.Text
ghcStringGapMacroInput =
  T.unlines
    [ "#define ID(x) x",
      "x = ID(\"a\\",
      "       \\b\")"
    ]

gccStringContinuationConcatInput :: T.Text
gccStringContinuationConcatInput =
  T.unlines
    [ "x = \"a\\\\",
      "       \\\" <> y <> \"\\n\\\\",
      "       \\b\""
    ]

nonStringDoubleBackslashInput :: T.Text
nonStringDoubleBackslashInput =
  T.unlines
    [ "#define ID(x) x",
      "x = ID(foo \\\\",
      "       bar)"
    ]
