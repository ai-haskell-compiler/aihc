{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module Main where

--------------------------------------------------------------------------
-- imports

import qualified Data.Text                  as T
import           EasyLogger
import           System.Directory
import           Test.QuickCheck

-- file Spec.hs
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Test.QuickCheck.Assertions
import           Test.QuickCheck.Monadic
import           Test.QuickCheck.Property

--------------------------------------------------------------------------

getLogMessages :: FilePath -> IO [String]
getLogMessages fp = do
  txt <- readFile fp
  let ls = lines txt
  return $ map getLogMsg ls
  where
    getLogMsg = T.unpack . T.strip . T.takeWhile (/= '@') . T.tail . T.dropWhile (/= ']') . T.pack

--------------------------------------------------------------------------

prop_Logging :: Property
prop_Logging = ioProperty $ do
  writeFile "test/files/prop_Logging.log" ""
  $(initLogger) (LogFile ("test/files/prop_Logging.log" :: FilePath))
  $(logDebug) "test run"
  finalizeAllLoggers
  lastLog <- getLogMessages "test/files/prop_Logging.log"
  return (["test run"] === lastLog)

prop_NoInit :: IO ()
prop_NoInit = do
  writeFile "test/files/prop_NoInit.log" ""
  $(logDebug) "test run"


--------------------------------------------------------------------------
-- main

return []
-- prop_conj :: Property
-- prop_conj = counterexample "Simon Thompson" $(monomorphic 'prop_SimonThompson) .&&.
--             counterexample "reverse" $(monomorphic 'prop_Reverse)

-- prop_disj :: Property
-- prop_disj = counterexample "reverse" $(monomorphic 'prop_Reverse) .||.
--             counterexample "Simon Thompson" $(monomorphic 'prop_SimonThompson)
-- return []


main :: IO ()
main = do
  createDirectoryIfMissing True "test/files"
  hspec $ do
    describe "prop_Logging" $ do
      it "logs to file and reads content" $ prop_Logging

    describe "initialization" $ do
      it "throws an exception if not initialized" $
        prop_NoInit `shouldThrow` anyException

