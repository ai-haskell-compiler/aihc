module ProcessSpec (processSpec) where

import System.PosixCompat
import Test.HUnit
import Test.Hspec

processSpec :: Spec
processSpec = do
  describe "getProcessID" $ do
    it "should work on Windows and Unix" $ do
      pid <- getProcessID
      assert $ pid > 0
