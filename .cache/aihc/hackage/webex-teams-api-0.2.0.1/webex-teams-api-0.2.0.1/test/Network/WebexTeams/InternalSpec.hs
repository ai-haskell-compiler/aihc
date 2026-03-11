{-# LANGUAGE OverloadedStrings #-}

module Network.WebexTeams.InternalSpec where

import           Data.Attoparsec.ByteString
import           Data.ByteString             (pack)
import           Data.Either                 (isLeft)
import           Network.URI                 (URIAuth (..))

import           Test.Hspec

import           Network.WebexTeams.Internal


spec :: Spec
spec = do
    describe "Parser elements" $ do
        it "decodes DQUOTE" $ do
            parseOnly dQuote "\"" `shouldBe` Right 0x22

        it "decodes quoted-pair" $ do
            parseOnly quotedPair "\\\\" `shouldBe` Right 0x5c
            parseOnly quotedPair "\\\"" `shouldBe` Right 0x22
            parseOnly quotedPair "no quoted pair" `shouldSatisfy` isLeft

        it "decodes quoted-string of '\"next\"' as 'next'" $ do
            parseOnly quotedString "\"next\"" `shouldBe` Right "next"

        it "decodes rel parameter as a token" $ do
            parseOnly token "rel" `shouldBe` Right "rel"

        it "decodes rel=\"next\" as LinkParam and its value" $ do
            parseOnly param "; rel=\"next\"" `shouldBe` Right (Rel, "next")

        it "decodes rel=next as LinkParam and its value" $ do
            parseOnly param "; rel=next" `shouldBe` Right (Rel, "next")

        it "decodes entire link header value" $ do
            parseOnly linkHeader "<https://api.ciscospark.com/v1/people?link=to&next=page>; rel=\"next\""
                `shouldBe` Right (LinkHeader "https://api.ciscospark.com/v1/people?link=to&next=page" [ (Rel, "next") ])

    describe "HTTP Link header detector" $ do
        it "returns empty list from empty input" $ do
            extractNextUrl [] `shouldBe` []

        it "extracts a URL from a header value when the header has rel=next param" $ do
            extractNextUrl ["<https://api.ciscospark.com/v1/people?link=to&next=page>; rel=\"next\""]
                `shouldBe` ["https://api.ciscospark.com/v1/people?link=to&next=page"]

        it "ignores header which has no rel=next param" $ do
            extractNextUrl ["<http://hosta>; norel=next", "<http://hostb>; rel=next"] `shouldBe` ["http://hostb"]

        it "ignores invalid header" $ do
            extractNextUrl ["invalid header", "<http://valid>; rel=next"] `shouldBe` ["http://valid"]

    describe "HTTP Link Header validator" $ do
        it "validates URL if it has same schema and URI Authority" $ do
            validateUrl "https:" (URIAuth "" "api.ciscospark.com" "")
                        "https://api.ciscospark.com/v1/people?link=to&next=page"
                        `shouldBe` Just "https://api.ciscospark.com/v1/people?link=to&next=page"

        it "checks port number part too for validation" $ do
            validateUrl "http:" (URIAuth "" "localhost" ":3000")
                        "http://localhost:3000/v1/people?link=to&next=page"
                        `shouldBe` Just "http://localhost:3000/v1/people?link=to&next=page"

        it "ignores path part of URL for validation" $ do
            validateUrl "https:" (URIAuth "" "api.ciscospark.com" "") "https://api.ciscospark.com/abcdef"
                        `shouldBe` Just "https://api.ciscospark.com/abcdef"

        it "invalidates URL if scheme is different" $ do
            validateUrl "https:" (URIAuth "" "api.ciscospark.com" "") "http://api.ciscospark.com/abcdef"
                        `shouldBe` Nothing

        it "invalidates URL if host name is different" $ do
            validateUrl "https:" (URIAuth "" "api.ciscospark.com" "") "http://host.example.com/abcdef"
                        `shouldBe` Nothing

        it "invalidates URL if port is different" $ do
            validateUrl "https:" (URIAuth "" "localhost" ":3000") "http://localhost:8888/abcdef"
                        `shouldBe` Nothing
