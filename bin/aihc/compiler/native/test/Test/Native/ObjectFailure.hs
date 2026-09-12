{-# LANGUAGE OverloadedStrings #-}

-- | Source fixtures for object-output failures on both native backends.
module Test.Native.ObjectFailure (failureFixtures) where

import Aihc.Cli.Backend (compileGrinTo, compileLirTo)
import Aihc.Grin qualified as Grin
import Aihc.Lir qualified as Lir
import Aihc.Native (NativeTarget)
import Control.Exception (SomeException, displayException, try)
import Control.Monad (forM, forM_, void, when)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.ByteString qualified as BS
import Data.List (sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Yaml qualified as Y
import System.Directory (doesFileExist, listDirectory)
import System.Environment (lookupEnv)
import System.FilePath (takeExtension, (</>))
import System.IO.Temp (withSystemTempDirectory)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertBool, assertFailure, testCase, (@?=))

data FailureFixture = FailureFixture !Text !Text !Text

instance FromJSON FailureFixture where
  parseJSON = withObject "FailureFixture" $ \object -> FailureFixture <$> object .: "language" <*> object .: "error" <*> object .: "source"

failureFixtures :: NativeTarget -> IO TestTree
failureFixtures target = do
  root <- fromMaybe "." <$> lookupEnv "AIHC_TEST_ROOT"
  let directory = root </> "bin/aihc/compiler/native/test/Test/Fixtures/object-fail"
  names <- sort . filter ((== ".yaml") . takeExtension) <$> listDirectory directory
  cases <- forM names $ \name -> do
    fixture <- BS.readFile (directory </> name) >>= checked . Y.decodeEither'
    pure (testCase name (checkFailure fixture))
  pure (testGroup "object failure fixtures" cases)
  where
    checked :: (Show error) => Either error value -> IO value
    checked = either (assertFailure . show) pure
    checkFailure (FailureFixture language expected source) = do
      compile <- case language of
        "lir" -> compileLirTo False target <$> checked (Lir.parseModule source)
        "grin" -> do
          program <- checked (Grin.parseProgram source)
          gc <- Grin.lowerGc <$> checked (Grin.toCpsGrin program)
          pure (compileGrinTo False False target Nothing gc)
        _ -> assertFailure "Unknown source language."
      forM_ [False, True] $ \existing ->
        withSystemTempDirectory "aihc-object-failure" $ \directory -> do
          let path = directory </> "module.o"
          when existing (BS.writeFile path "previous object")
          result <- try (void (compile path)) :: IO (Either SomeException ())
          case result of
            Right () -> assertFailure "Compilation did not fail."
            Left err -> assertBool (displayException err) (expected `T.isInfixOf` T.pack (displayException err))
          doesFileExist path >>= (@?= existing)
          when existing (BS.readFile path >>= (@?= "previous object"))
          listDirectory directory >>= (@?= ["module.o" | existing])
