{-# LANGUAGE OverloadedStrings #-}

-- | GRIN-to-Lir golden fixtures.
--
-- The GRIN goldens stop at GRIN and the assembly goldens start at
-- hand-written Lir, which left the lowering itself with no textual
-- assertion: a change to the Lir a construct produces was visible only
-- through the lint, a pretty-printer round-trip, and the behaviour of the
-- compiled program. These fixtures show the Lir.
--
-- A fixture names the GRIN program and the Lir the lowering must produce.
-- The neighbouring @lower@ directory is the wasm data-layout suite, which
-- takes the same GRIN through to wasm and asserts the shape of a global
-- rather than the Lir text.
-- Because the lowering takes a 'LowerTarget' rather than an architecture,
-- and every 64-bit POSIX target shares one, a fixture selects a target by
-- word size and host and not by architecture. See @docs/lir.md@.
module Test.Lir.LowerSuite (tests) where

import Aihc.Grin (lowerGc, parseProgram, renderParseError, toCpsGrin)
import Aihc.Lir.Lower (LowerTarget, lowerModule, posixTarget64, wasip3Target)
import Aihc.Lir.Pretty (renderModule)
import Data.Aeson ((.!=), (.:), (.:?))
import Data.Aeson.Types (Parser, Value, parseEither, withObject)
import Data.List (sort)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertFailure, testCase)

data LowerFixture = LowerFixture
  { fixtureProgram :: !Text,
    fixtureTarget :: !LowerTarget,
    -- | Whether the lowering emits the primitive bounds checks.
    fixtureCheckPrimBounds :: !Bool,
    fixtureExpected :: !Text
  }

tests :: FilePath -> IO TestTree
tests directory = do
  names <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  pure (testGroup "GRIN lowering fixtures" (map (fixtureTest directory) names))

fixtureTest :: FilePath -> FilePath -> TestTree
fixtureTest directory name = testCase name $ do
  value <- either (assertFailure . Y.prettyPrintParseException) pure =<< Y.decodeFileEither (directory </> name)
  fixture <- either assertFailure pure (parseEither parseFixture value)
  actual <- either assertFailure pure (lowerFixture fixture)
  -- The whole rendered module is the expectation, so a mismatch prints
  -- something that can be pasted straight back into the fixture.
  if actual == T.stripEnd (fixtureExpected fixture)
    then pure ()
    else
      assertFailure
        ( "lowered Lir mismatch\nexpected:\n"
            <> T.unpack (T.stripEnd (fixtureExpected fixture))
            <> "\nactual:\n"
            <> T.unpack actual
        )

parseFixture :: Value -> Parser LowerFixture
parseFixture =
  withObject "Lir lowering fixture" $ \object -> do
    status <- object .: "status" :: Parser Text
    if status == "pass"
      then pure ()
      else fail ("Lir lowering fixtures must have status pass: " <> T.unpack status)
    targetName <- object .:? "target" .!= "posix64"
    target <-
      case targetName :: Text of
        "posix64" -> pure posixTarget64
        "wasip3" -> pure wasip3Target
        _ -> fail ("Unknown Lir lowering target: " <> T.unpack targetName)
    LowerFixture
      <$> object .: "program"
      <*> pure target
      <*> object .:? "check-prim-bounds" .!= False
      <*> object .: "expected"

-- | The GRIN pipeline a native target takes, stopping at the Lir text.
lowerFixture :: LowerFixture -> Either String Text
lowerFixture fixture = do
  program <- either (Left . renderParseError) Right (parseProgram (fixtureProgram fixture))
  cps <- either (Left . show) Right (toCpsGrin program)
  lir <- either (Left . show) Right (lowerModule (fixtureTarget fixture) (fixtureCheckPrimBounds fixture) (lowerGc cps))
  pure (T.stripEnd (renderModule lir))
