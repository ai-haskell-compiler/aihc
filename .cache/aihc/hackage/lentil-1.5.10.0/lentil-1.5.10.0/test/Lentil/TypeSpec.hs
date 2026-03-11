module Lentil.TypeSpec where

import           Test.Hspec

import           Lentil.Types

-- File tests

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "prettyFP" $ do
    it "eliminates ./ from beginning of fp, if present" $
      prettyFP "./file" `shouldBe` "file"
    it "doesn't change fp if ./ is not there at begin-of-fp" $
      prettyFP "file" `shouldBe` "file"

