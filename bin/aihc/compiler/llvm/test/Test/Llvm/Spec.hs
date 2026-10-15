{-# LANGUAGE OverloadedStrings #-}

module Test.Llvm.Spec (tests) where

import Aihc.Cli.Backend (BackendOutput (..))
import Aihc.Lir.Lower (posixTarget64)
import Aihc.Lir.Parser (parseModule)
import Aihc.Llvm.Lir (compileLirModule)
import Aihc.Native (NativeTarget (Llvm))
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import System.Info (arch, os)
import Test.Lir.NativeSuite (NativeBackend (..))
import Test.Lir.NativeSuite qualified as NativeSuite
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertBool, assertEqual, assertFailure, testCase)

-- | The LLVM backend compiles for the host, so the programs run wherever
-- Clang runs. The snapshot fixtures carry one allocation count per host.
tests :: IO TestTree
tests = do
  suite <-
    NativeSuite.tests
      NativeBackend
        { backendName = "aihc-llvm",
          backendTarget = Llvm,
          backendLowerTarget = posixTarget64,
          backendClangArguments = ["-Wno-override-module"],
          backendRuns = (arch == "aarch64" && os == "darwin") || (arch == "x86_64" && os == "linux"),
          backendAllocationKey = if os == "darwin" then "macos-arm64" else "linux-amd64",
          backendSourceExtension = ".ll",
          backendCompile = either (Left . show) (Right . BackendSource) . compileLirModule
        }
  pure
    ( testGroup
        "LLVM"
        [ suite,
          testCase "an access in a region carries alias metadata of its region" test_regionAliasMetadata
        ]
    )

-- | A stack load and a heap store get alias tags of two different scalar
-- types, and an access in no region gets no tag.
test_regionAliasMetadata :: Assertion
test_regionAliasMetadata = do
  lirModule <-
    either (assertFailure . show) pure $
      parseModule
        ( T.unlines
            [ "export func @main(%frame: ptr, %object: ptr) -> i64 {",
              "entry:",
              "  %saved = load i64 [%frame + 8] align 8 region stack",
              "  store i64 %saved, [%object + 8] align 8 region heap",
              "  %header = load i64 [%object] align 8",
              "  return %header",
              "}"
            ]
        )
  rendered <- either (assertFailure . show) pure (compileLirModule lirModule)
  let ir = T.lines rendered
      tagsOf prefix = mapMaybe (tbaaTag prefix) ir
      definitions name = [line | line <- ir, ("!\"aihc " <> name <> "\"") `T.isInfixOf` line]
  stackTag <- single "stack load" (tagsOf "load i64")
  heapTag <- single "heap store" (tagsOf "store i64")
  assertBool "the stack load and the heap store have different tags" (stackTag /= heapTag)
  assertEqual "one scalar type node names the stack" 1 (length (definitions "stack"))
  assertEqual "one scalar type node names the heap" 1 (length (definitions "heap"))
  assertEqual "the untagged load has no alias metadata" 1 (length [line | line <- ir, "load i64" `T.isInfixOf` line, not ("!tbaa" `T.isInfixOf` line)])
  where
    -- The alias tag of an instruction line that starts with the prefix.
    tbaaTag :: Text -> Text -> Maybe Text
    tbaaTag prefix line
      | prefix `T.isInfixOf` line, (_, rest) <- T.breakOn "!tbaa " line, not (T.null rest) = Just (T.takeWhile (/= ',') (T.drop 6 rest))
      | otherwise = Nothing
    single name tags =
      case tags of
        [tag] -> pure tag
        _ -> assertFailure ("expected one tagged " <> name <> ", found " <> show tags)
