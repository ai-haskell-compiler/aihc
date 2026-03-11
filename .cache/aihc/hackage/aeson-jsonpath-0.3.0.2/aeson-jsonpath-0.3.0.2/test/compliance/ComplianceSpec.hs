{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
module ComplianceSpec
  ( spec
  , TestSuite (..))
  where

import qualified Data.Aeson                    as JSON
import qualified Data.Vector                   as V
import qualified Text.ParserCombinators.Parsec as P

import Control.Monad              (mzero)
import Data.Aeson                 ((.:),(.:?),Value)
import Data.Aeson.JSONPath        (query, queryLocated)
import Data.Aeson.JSONPath.Parser (pQuery)
import Data.Either                (isLeft, fromRight)
import Data.Vector                (Vector)

import Test.Hspec
import Prelude

newtype TestSuite = TestSuite
  { tests :: [TestCase]
  } deriving (Show)

instance JSON.FromJSON TestSuite where
  parseJSON (JSON.Object o) =
    TestSuite
    <$> o .: "tests"

  parseJSON _ = mzero

data TestCase = TestCase
  { name         :: String
  , selector     :: String
  , document     :: Maybe Value
  , result       :: Maybe (Vector Value)
  , results      :: Maybe [Vector Value]
  , resultPaths  :: Maybe (Vector String)
  , resultsPaths :: Maybe [Vector String]
  , invalidSel   :: Maybe Bool
  , tags         :: Maybe [String]
  } deriving (Show)

instance JSON.FromJSON TestCase where
  parseJSON (JSON.Object o) =
    TestCase
    <$> o .: "name"
    <*> o .: "selector"
    <*> o .:? "document"
    <*> o .:? "result"
    <*> o .:? "results"
    <*> o .:? "result_paths"
    <*> o .:? "results_paths"
    <*> o .:? "invalid_selector"
    <*> o .:? "tags"

  parseJSON _ = mzero

spec :: TestSuite -> Spec
spec TestSuite{tests} = do
  describe "compliance tests" $ do
    mapM_ runTestCase tests


runTestCase :: TestCase -> SpecWith ()
-- skip function extension tests
runTestCase tc@TestCase{tags=Just xs, ..} =
  if "function" `elem` xs
    then xit name pending
  else
    runTestCase tc{tags=Nothing}

-- invalid selector should not parse correctly
runTestCase TestCase{invalidSel=(Just True), ..} =
  it name $
    P.parse pQuery "" selector `shouldSatisfy` isLeft

-- if result is deterministic (one json)
runTestCase TestCase{result=(Just r), resultPaths=(Just rp), document=(Just doc), ..} =
  it name $ do
    query selector doc `shouldBe` Right r
    queryLocated selector doc `shouldBe` Right (V.zip rp r)

-- if result is non-deterministic (any json from the list of results)
runTestCase TestCase{results=(Just rs), resultsPaths=(Just rsp), document=(Just doc), ..} = do
  it name $ do
    query selector doc `shouldSatisfy` (\x -> fromRight V.empty x `elem` rs)
    queryLocated selector doc `shouldSatisfy` (\x -> fromRight V.empty x `elem` vecList)
    where
      vecList = [ V.zip rp r | rp <- rsp, r <- rs]

runTestCase _ = pure ()
