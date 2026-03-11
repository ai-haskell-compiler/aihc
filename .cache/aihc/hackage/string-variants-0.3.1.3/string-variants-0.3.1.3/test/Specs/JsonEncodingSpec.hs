{-# LANGUAGE CPP #-}

module Specs.JsonEncodingSpec (spec) where

import Data.Aeson
import Data.StringVariants.NonEmptyText
import Data.StringVariants.NonEmptyText.Internal
import Data.StringVariants.NullableNonEmptyText
import GHC.Generics
import Prelude
import Test.Hspec

data Person = Person {name :: NonEmptyText 6, catchphrase :: NullableNonEmptyText 15}
  deriving stock (Eq, Generic, Show)
  deriving anyclass (FromJSON, ToJSON)

spec :: Spec
spec = describe "FromJSON instances" $ do
  describe "NonEmptyText" $ do
    it "rejects strings that are too long" $
      assertParseFailure $
        object ["name" .= replicate 7 'a', "catchphrase" .= String "Yabba-Dabba Do!"]
    it "rejects missing properties" $
      assertParseFailure $
        object ["catchphrase" .= String "Yabba-Dabba Do!"]
    it "rejects null properties" $
      assertParseFailure $
        object ["name" .= Null, "catchphrase" .= String "Yabba-Dabba Do!"]
    it "rejects empty strings" $
      assertParseFailure $
        object ["name" .= String "", "catchphrase" .= String "Yabba-Dabba Do!"]
    it "rejects whitespace strings" $
      assertParseFailure $
        object ["name" .= String " ", "catchphrase" .= String "Yabba-Dabba Do!"]
    it "strips whitespace" $
      object ["name" .= String " Daniel ", "catchphrase" .= String "Yabba-Dabba Do!"]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText $ Just $ NonEmptyText "Yabba-Dabba Do!")
  describe "NullableNonEmptyText" $ do
    it "rejects strings that are too long" $
      assertParseFailure $
        object ["name" .= String "Daniel", "catchphrase" .= replicate 16 'a']
#if MIN_VERSION_aeson(2,2,0)
    it "accepts missing properties" $
      object ["name" .= String "Daniel"]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText Nothing)
    it "accepts null properties" $
      object ["name" .= String "Daniel", "catchphrase" .= Null]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText Nothing)
#endif
    it "accepts empty strings" $
      object ["name" .= String "Daniel", "catchphrase" .= String ""]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText Nothing)
    it "accepts whitespace strings" $
      object ["name" .= String "Daniel", "catchphrase" .= String " "]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText Nothing)
    it "strips whitespace" $
      object ["name" .= String "Daniel", "catchphrase" .= String " Yabba-Dabba Do! "]
        `shouldParseAs`
          Person (NonEmptyText "Daniel") (NullableNonEmptyText $ Just $ NonEmptyText "Yabba-Dabba Do!")
  where
    assertParseFailure :: HasCallStack => Value -> IO ()
    assertParseFailure val =
      decode @Person (encode val) `shouldBe` Nothing

    shouldParseAs :: HasCallStack => Value -> Person -> IO ()
    shouldParseAs val person =
      decode (encode val) `shouldBe` Just person
