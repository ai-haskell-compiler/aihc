module Test.Hspec.SmallCheck.TypesSpec (spec) where

import           Test.Hspec

import           Test.Hspec.SmallCheck.Types

spec :: Spec
spec = do
  describe "parseResult" $ do
    let r = Failure Nothing (ExpectedActual "" "23" "42")
    it "parses result" $ do
      parseResult (show r) `shouldBe` ("", Just r)

    context "with prefix" $ do
      it "includes prefix" $ do
        let
          prefix = "some prefix"
          input = prefix ++ show r
        parseResult input `shouldBe` (prefix, Just r)

    context "on parse error" $ do
      it "returns input verbatim" $ do
        let input = init (show r)
        parseResult input `shouldBe` (input, Nothing)

  describe "concatPrefix" $ do
    context "when given two empty strings" $ do
      it "returns Nothing" $ do
        concatPrefix "" "" `shouldBe` Nothing

    context "with first string empty" $ do
      it "returns second" $ do
        concatPrefix "foo" "" `shouldBe` Just "foo"

    context "with second string empty" $ do
      it "returns first" $ do
        concatPrefix "" "foo" `shouldBe` Just "foo"

    context "with two strings" $ do
      it "concatenates with newline" $ do
        concatPrefix "foo" "bar" `shouldBe` Just "foo\nbar"
