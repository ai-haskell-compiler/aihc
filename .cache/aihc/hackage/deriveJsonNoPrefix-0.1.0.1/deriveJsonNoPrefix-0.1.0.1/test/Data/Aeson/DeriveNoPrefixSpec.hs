{-# LANGUAGE OverloadedStrings #-}

module Data.Aeson.DeriveNoPrefixSpec where


import qualified Data.Aeson as Json
import qualified Data.HashMap.Strict as HM
import           Data.Text ()
import           Test.Hspec

import           Data.Aeson.DeriveNoPrefixSpecLib


main :: IO ()
main = hspec $ do
  let jsonVal = Json.Object $
        HM.fromList
        [ ("id",  Json.String "the id")
        , ("max", Json.Number 456)
        , ("min", Json.Number 123)
        ]
      record = SomeRecord "the id" 456 123

  describe "ToJSON instance derived by deriveJsonNoTypeNamePrefix" $
    it "the encoded JSON's keys are same as record labels without their type name prefix." $
      Json.toJSON record `shouldBe` jsonVal

  describe "FromJSON instance derived by deriveJsonNoTypeNamePrefix" $
    it "decodes JSONs whose keys are same as record labels without their type name prefix." $
      Json.fromJSON jsonVal `shouldBe` Json.Success record
