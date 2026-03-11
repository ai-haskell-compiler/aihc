import Prelude hiding (unlines)

import Control.Monad (when)
import Data.ByteString (hGetContents)
import Data.ByteString.Char8 (unpack)
import Data.List.Extra (splitOn)
import Data.Text (Text, unlines)
import qualified Data.Text (unpack)
import qualified Data.Text.Lazy as LazyText
import System.Directory (setCurrentDirectory)
import System.Environment (lookupEnv)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.Process (StdStream(CreatePipe), createProcess, proc, std_err, std_out, waitForProcess)
import Test.Hspec (describe, expectationFailure, hspec, it, shouldBe)

import SpecUp (invokeTemplateOnSpec)

main = do
  runExtendedTests <- testVarSet
  hspec $ do

    when runExtendedTests $
        it "should call the binary with the example in the README" $
          usingTestData "example-from-readme" $
            do res <- callSpecUpWithArgs "-s animal.yaml -t template.m"
               specupStandardErr res `shouldBe` ""
               (specupStandardOut res `shouldBe`) =<< readFile "output.txt"
               specupExitCode res `shouldBe` ExitSuccess

    it "should work with a template with a simple list" $
      invokeTemplateOnSpec "{{# .}}# {{{ fieldName }}}\n{{/ .}}"
                           "- fieldName: value1\n- fieldName: value2"
        `shouldReturnMarkupOf`
      [ "# value1"
      , "# value2"
      ]

data SpecupResult = SpecupResult {
  specupStandardOut :: !String
, specupStandardErr :: !String
, specupExitCode :: !ExitCode
} deriving (Show, Eq)

output `shouldReturnMarkupOf` expectedMarkup =
    either
      (expectationFailure . ("expected Right but got Left " <>))
      (`shouldBe` LazyText.unlines expectedMarkup)
      output

callSpecUpWithArgs :: String -> IO SpecupResult
callSpecUpWithArgs args =
         do (_,Just mOut, Just mErr, procHandle) <- createProcess $ (proc "specup" (splitOn " " args)) {std_out = CreatePipe, std_err = CreatePipe}
            SpecupResult <$> (unpack <$> hGetContents mOut)
                         <*> (unpack <$> hGetContents mErr)
                         <*> waitForProcess procHandle

testVarSet =
  (== Just "1") <$> lookupEnv "TEST_BINARY"

usingTestData testData payload =
  do setCurrentDirectory $ "test/resources/" <> testData
     res <- payload
     setCurrentDirectory "../.."
     return res
