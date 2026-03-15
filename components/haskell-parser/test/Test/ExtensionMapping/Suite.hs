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
import Test.Tasty.HUnit (assertFailure, testCase)

extensionMappingTests :: TestTree
extensionMappingTests =
  testGroup
    "extension-mapping"
    [ testCase "maps overlapping Cabal KnownExtension constructors" test_cabalKnownExtensionCoverage,
      testCase "maps overlapping TemplateHaskell Extension constructors" test_templateHaskellExtensionCoverage
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
