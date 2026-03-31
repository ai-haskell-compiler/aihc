module Test.ExtensionMapping.Suite
  ( extensionMappingTests,
  )
where

import qualified Aihc.Parser.Syntax as Syntax
import Data.List (intercalate, sort)
import Data.Maybe (isNothing)
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified GHC.Driver.DynFlags as DynFlags
import qualified GHC.LanguageExtensions.Type as GHC
import qualified Language.Haskell.Extension as Cabal
import qualified Language.Haskell.TH.Syntax as TH
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

extensionMappingTests :: TestTree
extensionMappingTests =
  testGroup
    "extension-mapping"
    [ testCase "maps all Cabal KnownExtension constructors" test_cabalKnownExtensionCoverage,
      testCase "maps all TemplateHaskell Extension constructors" test_templateHaskellExtensionCoverage,
      testGroup
        "language-edition-extensions"
        [ testCase "Haskell98 extensions match GHC" test_haskell98Extensions,
          testCase "Haskell2010 extensions match GHC" test_haskell2010Extensions,
          testCase "GHC2021 extensions match GHC" test_ghc2021Extensions,
          testCase "GHC2024 extensions match GHC" test_ghc2024Extensions
        ]
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

-- | Test that our Haskell98 extension list matches GHC's.
test_haskell98Extensions :: IO ()
test_haskell98Extensions =
  compareEditionExtensions Syntax.Haskell98Edition DynFlags.Haskell98

-- | Test that our Haskell2010 extension list matches GHC's.
test_haskell2010Extensions :: IO ()
test_haskell2010Extensions =
  compareEditionExtensions Syntax.Haskell2010Edition DynFlags.Haskell2010

-- | Test that our GHC2021 extension list matches GHC's.
test_ghc2021Extensions :: IO ()
test_ghc2021Extensions =
  compareEditionExtensions Syntax.GHC2021Edition DynFlags.GHC2021

-- | Test that our GHC2024 extension list matches GHC's.
test_ghc2024Extensions :: IO ()
test_ghc2024Extensions =
  compareEditionExtensions Syntax.GHC2024Edition DynFlags.GHC2024

-- | Compare our edition extensions against GHC's.
-- The test extracts the canonical extension names from both sources and compares them.
compareEditionExtensions :: Syntax.LanguageEdition -> DynFlags.Language -> IO ()
compareEditionExtensions ourEdition ghcLang = do
  let ourExts = Set.fromList (map (T.unpack . Syntax.extensionName) (Syntax.languageEditionExtensions ourEdition))
      ghcExts = Set.fromList (map ghcExtToName (DynFlags.languageExtensions (Just ghcLang)))
      -- Extensions in GHC but not in our list
      missingInOurs = Set.difference ghcExts ourExts
      -- Extensions in our list but not in GHC
      extraInOurs = Set.difference ourExts ghcExts

  case (Set.null missingInOurs, Set.null extraInOurs) of
    (True, True) -> pure ()
    _ ->
      assertFailure $
        "Extension mismatch for "
          <> show ghcLang
          <> ":\n"
          <> (if Set.null missingInOurs then "" else "  Missing (in GHC but not ours): " <> intercalate ", " (sort (Set.toList missingInOurs)) <> "\n")
          <> (if Set.null extraInOurs then "" else "  Extra (in ours but not GHC): " <> intercalate ", " (sort (Set.toList extraInOurs)) <> "\n")

-- | Convert a GHC extension to its canonical name for comparison.
-- Handles the spelling difference for Cpp (GHC) vs CPP (ours).
ghcExtToName :: GHC.Extension -> String
ghcExtToName ext =
  case ext of
    GHC.Cpp -> "CPP"
    _ -> show ext
