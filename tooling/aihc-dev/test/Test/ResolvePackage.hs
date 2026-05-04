{-# LANGUAGE OverloadedStrings #-}

module Test.ResolvePackage
  ( resolvePackageTests,
  )
where

import Aihc.Parser.Syntax (NameType (..), mkQualifiedName, mkUnqualifiedName)
import Aihc.Resolve (ResolvedName (..), Scope (..))
import Data.ByteString.Lazy.Char8 qualified as BL8
import Data.List (isInfixOf)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import ResolvePackage
  ( ResolveModuleIface (..),
    ResolvePackageIface (..),
    dependencyClosure,
    formatDependencyFailure,
    interfaceFromExports,
    renderInterfaceJSON,
    targetLayers,
  )
import ResolveStackageProgress (PackageStatus (..))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertEqual, testCase)

resolvePackageTests :: TestTree
resolvePackageTests =
  testGroup
    "resolve package"
    [ testCase "selects transitive dependency closure" test_dependencyClosure,
      testCase "orders only target dependency layers" test_targetLayers,
      testCase "renders stable sorted interface JSON" test_renderInterfaceJSON,
      testCase "formats dependency failures separately from target failures" test_formatDependencyFailure
    ]

test_dependencyClosure :: IO ()
test_dependencyClosure = do
  let graph =
        Map.fromList
          [ ("target", ["a", "b"]),
            ("a", ["base"]),
            ("b", ["a"]),
            ("base", []),
            ("unrelated", ["base"])
          ]
  assertEqual
    "closure"
    (Set.fromList ["target", "a", "b", "base"])
    (dependencyClosure "target" graph)

test_targetLayers :: IO ()
test_targetLayers = do
  let graph =
        Map.fromList
          [ ("target", ["a", "b"]),
            ("a", ["base"]),
            ("b", ["a"]),
            ("base", []),
            ("unrelated", [])
          ]
  assertEqual
    "target layers"
    [["base"], ["a"], ["b"], ["target"]]
    (targetLayers "target" graph)

test_renderInterfaceJSON :: IO ()
test_renderInterfaceJSON = do
  let iface =
        interfaceFromExports
          "demo"
          ( Map.fromList
              [ ("Z", mkScope ["zterm"] ["Zed"]),
                ("A", (mkScope ["beta", "alpha"] ["Thing"]) {scopeConstructors = Map.singleton "Thing" ["MkThing"], scopeMethods = Map.singleton "Classy" ["method"]})
              ]
          )
      rendered = BL8.unpack (renderInterfaceJSON iface)
  assertEqual
    "structured interface"
    ( ResolvePackageIface
        "demo"
        [ ResolveModuleIface "A" ["alpha", "beta"] ["Thing"] (Map.singleton "Thing" ["MkThing"]) (Map.singleton "Classy" ["method"]),
          ResolveModuleIface "Z" ["zterm"] ["Zed"] Map.empty Map.empty
        ]
    )
    iface
  assertBool "JSON includes constructors" ("\"MkThing\"" `isInfixOf` rendered)
  assertBool "JSON includes methods" ("\"method\"" `isInfixOf` rendered)
  assertBool "JSON includes first sorted module" ("\"module\": \"A\"" `isInfixOf` rendered)
  assertBool "JSON includes sorted terms" ("\"alpha\"" `isInfixOf` rendered && "\"beta\"" `isInfixOf` rendered)

test_formatDependencyFailure :: IO ()
test_formatDependencyFailure = do
  let graph =
        Map.fromList
          [ ("target", ["dep", "other"]),
            ("dep", []),
            ("other", ["dep"])
          ]
      results =
        Map.fromList
          [ ("dep", PkgFailed "dep failed\nextra detail\nmore detail\nmore detail\nmore detail\nhidden"),
            ("other", PkgSkipped),
            ("target", PkgSkipped)
          ]
      rendered = formatDependencyFailure "target" graph results
  assertBool "mentions dependency failure" ("dep failed" `isInfixOf` rendered)
  assertBool "mentions skipped dependency" ("Skipped because dependencies failed: other" `isInfixOf` rendered)
  assertBool "does not report target as dependency" (not ("target:" `isInfixOf` rendered))

mkScope :: [Text] -> [Text] -> Scope
mkScope terms types =
  Scope
    { scopeTerms = Map.fromList [(name, resolve name) | name <- terms],
      scopeTypes = Map.fromList [(name, resolve name) | name <- types],
      scopeConstructors = Map.empty,
      scopeRecordFields = Map.empty,
      scopeMethods = Map.empty,
      scopeQualifiedModules = Map.empty
    }
  where
    resolve name =
      ResolvedTopLevel (mkQualifiedName (mkUnqualifiedName NameVarId name) (Just "M"))
