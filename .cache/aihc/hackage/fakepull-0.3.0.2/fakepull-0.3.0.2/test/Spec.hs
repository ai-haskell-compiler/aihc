{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE ScopedTypeVariables #-}

import           Test.Hspec
import           Test.Hspec.QuickCheck

import qualified Test.Pull.Fake.IO     as IO
import qualified Test.Pull.Fake.Pure   as Pure


main :: IO ()
main = hspec $ do
  let runner pull = go
       where
        go =
          pull >>= \case
            Nothing -> return []
            Just payload -> (payload :) <$> go

  describe "Test.Pull.Fake.Pure" $
    describe "pull" $
      prop "pulls from the stack" $ \(payloads :: [Int]) -> do
        let action = runner Pure.pull
            (pulledThings, leftPayloads) = Pure.runPullM action payloads
        pulledThings `shouldBe` payloads
        leftPayloads `shouldBe` []

  describe "Test.Pull.Fake.IO" $
    describe "pull" $
      prop "pulls from the stack" $ \(payloads :: [Int]) -> do
        stack <- IO.newFakeStream payloads
        pulledThings <- runner (IO.pull stack)
        pulledThings `shouldBe` payloads
        leftPayloads <- IO.getFakeStreamContents stack
        leftPayloads `shouldBe` []
