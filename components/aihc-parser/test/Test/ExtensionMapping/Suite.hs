module Test.ExtensionMapping.Suite
  ( extensionMappingTests,
  )
where

import qualified Aihc.Parser.Syntax as Syntax
import Data.List (intercalate)
import Data.Maybe (isNothing)
import qualified Data.Text as T
import qualified Language.Haskell.Extension as Cabal
import qualified Language.Haskell.TH.Syntax as TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

extensionMappingTests :: TestTree
extensionMappingTests =
  testGroup
    "extension-mapping"
    [ testCase "maps all Cabal KnownExtension constructors" test_cabalKnownExtensionCoverage,
      testCase "maps all TemplateHaskell Extension constructors" test_templateHaskellExtensionCoverage
    ]

test_cabalKnownExtensionCoverage :: IO ()
test_cabalKnownExtensionCoverage = do
  let missing =
        [ show ext
        | ext <- [minBound .. maxBound] :: [Cabal.KnownExtension],
          isNothing (toParserExtension ext)
        ]
  assertNoMissing "Cabal.KnownExtension" missing

test_templateHaskellExtensionCoverage :: IO ()
test_templateHaskellExtensionCoverage = do
  let missing =
        [ show ext
        | ext <- [minBound .. maxBound] :: [TH.Extension],
          isNothing (toParserExtension ext)
        ]
  assertNoMissing "Language.Haskell.TH.Syntax.Extension" missing

toParserExtension :: (Show a) => a -> Maybe Syntax.Extension
toParserExtension = Syntax.parseExtensionName . T.pack . show

assertNoMissing :: String -> [String] -> IO ()
assertNoMissing _ [] = pure ()
assertNoMissing source missing =
  assertFailure
    ( source
        <> " constructors missing Parser.Syntax mapping: "
        <> intercalate ", " missing
    )
