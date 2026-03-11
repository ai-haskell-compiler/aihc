module Terminal.Game.Layer.Object.TestSpec where

import Terminal.Game.Layer.Imperative
import Terminal.Game.Layer.Object
import Alone

import Test.Hspec

import qualified GHC.Exts as E

spec :: Spec
spec = do

  describe "runTest" $ do
    it "logs exceptions without failing" $ do
        r <- readRecord "test/records/alone-record-test.gr"
        let r' = r { aTermSize = E.fromList (replicate 1000 Nothing) }
            t = runTest (runGameGeneral aloneInARoom) r'
        last (snd t) `shouldBe` TException CannotGetDisplaySize
