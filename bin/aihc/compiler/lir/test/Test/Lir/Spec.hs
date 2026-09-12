{-# LANGUAGE OverloadedStrings #-}

module Test.Lir.Spec (tests) where

import Aihc.Grin hiding (renderParseError)
import Aihc.Grin qualified as Grin
import Aihc.Lir
import Aihc.Lir.Lower (lowerModule, posixTarget64)
import Control.Monad (unless)
import Data.List (sort)
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import System.Directory (listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import Test.Lir.Arbitrary (prop_lirPrettyRoundTrip)
import Test.Lir.RegAllocSpec qualified as RegAllocSpec
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, assertFailure, testCase)
import Test.Tasty.Hedgehog (testProperty)

tests :: IO TestTree
tests = do
  root <- fixtureRoot
  evalCases <- loadFixtures (root </> "eval")
  lintCases <- loadFixtures (root </> "lint")
  regAlloc <- RegAllocSpec.tests (root </> "eval")
  pure
    ( testGroup
        "aihc-lir"
        [ testProperty "generated Lir pretty-printer round-trip" prop_lirPrettyRoundTrip,
          testGroup "evaluation fixtures" (map evalTest evalCases),
          testGroup "lint error fixtures" (map lintTest lintCases),
          testCase "emits the primitive bounds checks only when asked" test_primitiveBoundsChecks,
          regAlloc
        ]
    )

-- | The array primitives are unchecked loads and stores, as in GHC. With
-- the bounds checks on, an element access compares the index against the
-- length and reaches the runtime failure when it is out of bounds.
test_primitiveBoundsChecks :: Assertion
test_primitiveBoundsChecks = do
  program <-
    either (assertFailure . Grin.renderParseError) pure $
      parseProgram
        ( T.unlines
            [ "primitive newByteArray#%1 :: BoxedRep Unlifted/1",
              "primitive readWordArray#%2 :: WordRep/2",
              "",
              "$entry -> WordRep =",
              "  (array%3 :: BoxedRep Unlifted) <- primitive-call @(BoxedRep Unlifted) newByteArray# (8 :: IntRep)",
              "  primitive-call @WordRep readWordArray# (array%3 :: BoxedRep Unlifted) (4 :: IntRep)"
            ]
        )
  gc <- either (assertFailure . show) (pure . lowerGc) (toCpsGrin program)
  let externsWith check = do
        lowered <- either (assertFailure . show) pure (lowerModule posixTarget64 check gc)
        assertEqual "Lir lint" [] (lintModule lowered)
        pure [externFunctionName extern | ItemExternFunction extern <- moduleItems lowered]
      failure = Symbol "aihc_byte_array_bounds_fail"
  unchecked <- externsWith False
  checked <- externsWith True
  assertEqual "unchecked access calls no failure" False (failure `elem` unchecked)
  assertEqual "checked access can reach the failure" True (failure `elem` checked)

fixtureRoot :: IO FilePath
fixtureRoot = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  pure (root </> "bin" </> "aihc" </> "compiler" </> "lir" </> "test" </> "Test" </> "Fixtures" </> "lir")

data Fixture = Fixture
  { fixtureName :: !String,
    fixturePath :: !FilePath,
    fixtureSource :: !Text
  }

loadFixtures :: FilePath -> IO [Fixture]
loadFixtures directory = do
  names <- sort . filter ((== ".lir") . takeExtension) <$> listDirectory directory
  mapM (\name -> Fixture name (directory </> name) <$> TIO.readFile (directory </> name)) names

-- | The values of every header comment @; key: value@.
headerValues :: Text -> Text -> [Text]
headerValues key source =
  mapMaybe (T.stripPrefix ("; " <> key <> ": ")) (T.lines source)

parseFixture :: Fixture -> IO Module
parseFixture fixture =
  case parseModule (fixtureSource fixture) of
    Left err -> assertFailure (renderParseError err)
    Right lirModule -> do
      -- Every fixture also round-trips through the pretty-printer.
      case parseModule (renderModule lirModule) of
        Left err -> assertFailure ("pretty-printer output does not parse:\n" <> renderParseError err)
        Right reparsed -> assertEqual "pretty-printer round-trip" lirModule reparsed
      either (assertFailure . renderLoadError) pure =<< expandIncludes TIO.readFile (fixturePath fixture) lirModule

-- | A module that declares an extern function, which the backends link and
-- this interpreter cannot call (see @docs/lir.md@). Such a fixture is still
-- parsed and linted here, and the backend groups run it.
declaresExternFunction :: Module -> Bool
declaresExternFunction lirModule = any isExtern (moduleItems lirModule)
  where
    isExtern item =
      case item of
        ItemExternFunction _ -> True
        _ -> False

evalTest :: Fixture -> TestTree
evalTest fixture = testCase (fixtureName fixture) $ do
  lirModule <- parseFixture fixture
  case lintModule lirModule of
    [] -> pure ()
    errors -> assertFailure ("lint errors:\n" <> T.unpack (T.unlines (map renderLintError errors)))
  unless (declaresExternFunction lirModule) (runEvalFixture fixture lirModule)

runEvalFixture :: Fixture -> Module -> Assertion
runEvalFixture fixture lirModule = do
  let entry = Symbol "main"
      resultTypes = concat [functionResults function | ItemFunction function <- moduleItems lirModule, functionName function == entry]
      expected = headerValues "expect" (fixtureSource fixture)
      expectedTrap = headerValues "expect-trap" (fixtureSource fixture)
  case (runFunction lirModule entry [], expected, expectedTrap) of
    (Right values, [want], []) -> assertEqual "result" want (renderValues resultTypes values)
    (Left (InterpretTrap message), [], [want]) -> assertEqual "trap" want message
    (Right values, _, _) -> assertFailure ("unexpected result " <> T.unpack (renderValues resultTypes values))
    (Left err, _, _) -> assertFailure (T.unpack (renderInterpretError err))

lintTest :: Fixture -> TestTree
lintTest fixture = testCase (fixtureName fixture) $ do
  lirModule <- parseFixture fixture
  let expected = headerValues "error" (fixtureSource fixture)
  assertEqual "lint errors" expected (map renderLintError (lintModule lirModule))
