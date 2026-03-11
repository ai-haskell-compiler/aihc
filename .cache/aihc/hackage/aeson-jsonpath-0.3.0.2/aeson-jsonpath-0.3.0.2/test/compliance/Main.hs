module Main
  ( main )
  where

import qualified Data.Aeson         as JSON
import qualified Test.Hspec         as HS
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as TIO

import Data.Either                  (fromRight)

import qualified ComplianceSpec
import qualified Paths_aeson_jsonpath as Paths

import ComplianceSpec   (TestSuite (..))

import Test.Hspec.Runner
import Prelude

compliance :: Either String TestSuite -> Spec
compliance cts = do
  HS.describe "Run compliance tests" $
    ComplianceSpec.spec (fromRight TestSuite{tests=[]} cts)

readCtsFile :: FilePath -> IO (Either String TestSuite)
readCtsFile filePath = do
  contents <- TIO.readFile filePath
  return $ JSON.eitherDecodeStrict' $ T.encodeUtf8 contents

main :: IO ()
main = do
  file <- Paths.getDataFileName "jsonpath-compliance-test-suite/cts.json"
  cts <- readCtsFile file
  summary <- hspecWithResult defaultConfig 
    { configColorMode = ColorAuto
    } (compliance cts)
  
  putStrLn $ "Total tests: " ++ show (summaryExamples summary)
  putStrLn $ "Failures: "    ++ show (summaryFailures summary)
