{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Test.Tasty
import Test.Tasty.HUnit

import Control.Monad
import Data.Aeson
import qualified Data.ByteString.Lazy as B

import Web.Bower.PackageMeta

-- Decode any JSON value, not just arrays/objects.
-- this is a bit of a hack, but the 'proper' way is just too much effort.
decodeValue :: FromJSON a => B.ByteString -> Maybe a
decodeValue = the <=< decode . ("[" <>) . (<> "]")
  where
  the [x] = Just x
  the _ = Nothing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "tests"
  [ testGroup "FromJSON Author instance" authorTests
  , testGroup "optional keys" optionalKeyTests
  , testGroup "round trips" roundTripTests
  , testGroup "real bower.json" realBowerJsonTests
  ]

authorTests :: [TestTree]
authorTests =
  [ testCase "As string without homepage/email" $ do
      Just authorWithoutOptionalAttrs @=?
        decodeValue "\"Harry Garrood\""

      -- should not be sensitive to extra whitespace
      Just authorWithoutOptionalAttrs @=?
        decodeValue "\" Harry Garrood \""

  , testCase "As string with homepage/email" $ do
      Just authorWithEmail @=?
        decodeValue "\"Harry Garrood <harry@garrood.me>\""

      Just authorWithHomepage @=?
        decodeValue "\"Harry Garrood (http://harry.garrood.me)\""

      Just authorWithBoth @=?
        decodeValue "\"Harry Garrood <harry@garrood.me> (http://harry.garrood.me)\""

  , testCase "As object" $ do
      Just authorWithoutOptionalAttrs @=?
        decode "{\"name\": \"Harry Garrood\"}"

      Just authorWithEmail @=?
        decode "{\"name\": \"Harry Garrood\", \"email\": \"harry@garrood.me\"}"

      Just authorWithHomepage @=?
        decode "{\"name\": \"Harry Garrood\", \"homepage\": \"http://harry.garrood.me\"}"

      Just authorWithBoth @=?
        decode "{\"name\": \"Harry Garrood\", \"email\": \"harry@garrood.me\", \"homepage\": \"http://harry.garrood.me\"}"
  ]

authorWithoutOptionalAttrs = Author "Harry Garrood" Nothing Nothing
authorWithEmail = Author "Harry Garrood" (Just "harry@garrood.me") Nothing
authorWithHomepage = Author "Harry Garrood" Nothing (Just "http://harry.garrood.me")
authorWithBoth = Author "Harry Garrood" (Just "harry@garrood.me") (Just "http://harry.garrood.me")

optionalKeyTests :: [TestTree]
optionalKeyTests =
  [ testCase "Missing keys should become empty lists, missing private key means not private" $ do
      Just basic @=? decode "{\"name\": \"test-package\"}"

  , testCase "Empty objects should turn into empty lists" $ do
      Just basic @=? decode "{\"name\": \"test-package\", \"dependencies\": {}}"

  , testCase "Nonempty objects should be parsed" $ do
      Just basicWithDeps @=?
        decode "{\"name\": \"test-package\", \"dependencies\": {\"dependency-package\": \">= 1.0\"}}"

  , testCase "Empty arrays should be parsed as empty lists" $ do
      Just basic @=? decode "{\"name\": \"test-package\", \"main\": []}"

  , testCase "Arrays with values should be parsed" $ do
      Just basicWithModuleType @=?
        decode "{\"name\": \"test-package\", \"moduleType\": [\"amd\"]}"
  ]
  where

Right pkgName = mkPackageName "test-package"
Right depPkgName = mkPackageName "dependency-package"
basic = PackageMeta pkgName Nothing [] [] [] [] [] [] Nothing Nothing [] [] [] False
basicWithDeps = basic { bowerDependencies = [(depPkgName, VersionRange ">= 1.0")] }
basicWithModuleType = basic { bowerModuleType = [AMD] }

complex =
  basicWithDeps
    { bowerDescription     = Just "hello, world"
    , bowerMain            = ["foo.js"]
    , bowerModuleType      = [Globals, Node]
    , bowerLicense         = ["MIT"]
    , bowerIgnore          = []
    , bowerKeywords        = ["purescript"]
    , bowerAuthors         = [authorWithoutOptionalAttrs, authorWithEmail, authorWithBoth]
    , bowerHomepage        = Nothing
    , bowerRepository      = Just (Repository "git://github.com/hdgarrood/test-package" "git")
    , bowerDevDependencies = []
    , bowerResolutions     = []
    }

complexPrivate = complex { bowerPrivate = True }

allPkgs =
  [ ("basic", basic)
  , ("basicWithDeps", basicWithDeps)
  , ("basicWithModuleType", basicWithModuleType)
  , ("complex", complex)
  , ("complexPrivate", complexPrivate)
  ]

roundTripTests :: [TestTree]
roundTripTests =
  map (\(name, b) -> testCase name (Just b @=? decode (encode b)))
      allPkgs

realBowerJsonTests :: [TestTree]
realBowerJsonTests = [go]
  where
  go =
    testCase "parses a real bower.json correctly" $ do
      res <- decodeFile "./test-resources/bower.json"
      case res of
        Right PackageMeta{..} ->
          ["MIT"] @=? bowerLicense
        Left err ->
          assertBool ("Failed to parse ./test-resources/bower.json: " ++ show err) False
