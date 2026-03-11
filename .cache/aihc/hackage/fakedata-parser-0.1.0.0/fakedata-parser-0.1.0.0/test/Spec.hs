{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Data.Attoparsec.Text as P
import Data.Text
import Fakedata.Parser
import Test.Hspec

main :: IO ()
main =
  hspec $ do
    describe "parseLiteral" $ do
      it "parses resolved text" $ do
        let result = P.parseOnly parseLiteralText "hello world"
        result `shouldBe` (Right $ Literal "hello world")
      it "parses resolved text and hash" $ do
        let result = P.parseOnly parseLiteralText "hello world#"
        result `shouldBe` (Right $ Literal "hello world")
      it "parses resolved text and ?" $ do
        let result = P.parseOnly parseLiteralText "hello wo?rld?#"
        result `shouldBe` (Right $ Literal "hello wo")
    describe "parseHash" $ do
      it "works with - #####" $ do
        let result = P.parseOnly parseHash "#####"
        result `shouldBe` (Right [Hash 5])
      it "works with - ###{hello}" $ do
        let result = P.parseOnly parseHash "###{hello}"
        result `shouldBe` (Right [Hash 2, Resolve "hello"])
      it "works with - #{hello}" $ do
        let result = P.parseOnly parseHash "#{hello}"
        result `shouldBe` (Right [Resolve "hello"])
    describe "parseFakedata" $ do
      it "works with - hello world" $ do
        let result = P.parseOnly parseFakedata "hello world"
        result `shouldBe` (Right $ (Literal "hello world") : [])
      it "works with - hello world ###" $ do
        let result = P.parseOnly parseFakedata "hello world ###"
        result `shouldBe` (Right $ (Literal "hello world ") : [Hash 3])
      it "works with - hello world ### ??" $ do
        let result = P.parseOnly parseFakedata "hello world ### ??"
        result `shouldBe`
          (Right $ (Literal "hello world ") : [Hash 3, Literal " ", Ques 2])
      it "works with - ###" $ do
        let result = P.parseOnly parseFakedata "###"
        result `shouldBe` (Right $ (Hash 3) : [])
      it "works with - ???" $ do
        let result = P.parseOnly parseFakedata "???"
        result `shouldBe` (Right $ (Ques 3) : [])
      it "works with - ???hello" $ do
        let result = P.parseOnly parseFakedata "???hello"
        result `shouldBe` (Right $ (Ques 3) : [Literal "hello"])
      it "works with - ???hello #{hello} hi #{world}" $ do
        let result = P.parseOnly parseFakedata "???hello #{hello} hi #{world}"
        result `shouldBe`
          (Right $
           (Ques 3) :
           [Literal "hello ", Resolve "hello", Literal " hi ", Resolve "world"])
    describe "unresolved text in address" $ do
      it "works with - PO Box ####" $ do
        let result = P.parseOnly parseFakedata "PO Box ####"
        result `shouldBe` (Right $ (Literal "PO Box ") : [Hash 4])
      it
        "works with - #{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}" $ do
        let result =
              P.parseOnly
                parseFakedata
                "#{secondary_address} #{street_address}, #{city}, #{state_abbr} #{zip_code}"
        result `shouldBe`
          (Right $
           (Resolve "secondary_address") :
           [ Literal " "
           , Resolve "street_address"
           , Literal ", "
           , Resolve "city"
           , Literal ", "
           , Resolve "state_abbr"
           , Literal " "
           , Resolve "zip_code"
           ])
      it "works with - #{Name.last_name}#{city_suffix}" $ do
        let result = P.parseOnly parseFakedata "#{Name.last_name}#{city_suffix}"
        result `shouldBe` (Right $ (Resolve "Name.last_name") : [Resolve "city_suffix"])
      it "works with - 831##" $ do
        let result = P.parseOnly parseFakedata "831##"
        result `shouldBe` (Right $ (Literal "831") : [Hash 2])
      it "works with - #####-####" $ do
        let result = P.parseOnly parseFakedata "#####-####"
        result `shouldBe` (Right $ (Hash 5) : [Literal "-", Hash 4])
