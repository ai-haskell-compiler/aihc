module Test.ExtensionMapping.Suite
  ( extensionMappingTests,
  )
where

import Data.List (intercalate)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Language.Haskell.Extension as Cabal
import qualified Language.Haskell.TH.Syntax as TH
import qualified Parser.Ast as Ast
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, assertFailure, testCase)

extensionMappingTests :: TestTree
extensionMappingTests =
  testGroup
    "extension-mapping"
    [ testCase "maps overlapping Cabal KnownExtension constructors" test_cabalKnownExtensionCoverage,
      testCase "maps overlapping TemplateHaskell Extension constructors" test_templateHaskellExtensionCoverage,
      testCase "maps Cabal Rank2Types to RankNTypes" test_cabalRank2TypesAlias,
      testCase "maps Template Haskell RankNTypes to RankNTypes" test_templateHaskellRankNTypes
    ]

test_cabalKnownExtensionCoverage :: IO ()
test_cabalKnownExtensionCoverage = do
  let missing =
        [ show ext
        | ext <- [minBound .. maxBound] :: [Cabal.KnownExtension],
          show ext `Set.member` parserExtensionNames,
          isNothing (toParserExtension ext)
        ]
  assertNoMissing "Cabal.KnownExtension" missing

test_templateHaskellExtensionCoverage :: IO ()
test_templateHaskellExtensionCoverage = do
  let missing =
        [ show ext
        | ext <- [minBound .. maxBound] :: [TH.Extension],
          show ext `Set.member` parserExtensionNames,
          isNothing (toParserExtension ext)
        ]
  assertNoMissing "Language.Haskell.TH.Syntax.Extension" missing

test_cabalRank2TypesAlias :: IO ()
test_cabalRank2TypesAlias = do
  let matches = [ext | ext <- [minBound .. maxBound] :: [Cabal.KnownExtension], show ext == "Rank2Types"]
  case matches of
    [rank2] -> assertEqual "Cabal Rank2Types should map to RankNTypes" (Just Ast.RankNTypes) (toParserExtension rank2)
    [] -> assertFailure "Cabal.KnownExtension does not expose Rank2Types"
    _ -> assertFailure "Cabal.KnownExtension exposes duplicate Rank2Types constructors"

test_templateHaskellRankNTypes :: IO ()
test_templateHaskellRankNTypes = do
  let matches = [ext | ext <- [minBound .. maxBound] :: [TH.Extension], show ext == "RankNTypes"]
  case matches of
    [rankN] -> assertEqual "Template Haskell RankNTypes should map to RankNTypes" (Just Ast.RankNTypes) (toParserExtension rankN)
    [] -> assertFailure "Language.Haskell.TH.Syntax.Extension does not expose RankNTypes"
    _ -> assertFailure "Language.Haskell.TH.Syntax.Extension exposes duplicate RankNTypes constructors"

toParserExtension :: (Show a) => a -> Maybe Ast.Extension
toParserExtension = Ast.parseExtensionName . T.pack . show

parserExtensionNames :: Set.Set String
parserExtensionNames =
  Set.fromList
    ( map (T.unpack . Ast.extensionName) Ast.allKnownExtensions
        <> ["Cpp", "GeneralisedNewtypeDeriving", "Rank2Types", "Safe", "Unsafe"]
    )

assertNoMissing :: String -> [String] -> IO ()
assertNoMissing _ [] = pure ()
assertNoMissing source missing =
  assertFailure
    ( source
        <> " constructors missing Parser.Ast mapping: "
        <> intercalate ", " missing
    )
