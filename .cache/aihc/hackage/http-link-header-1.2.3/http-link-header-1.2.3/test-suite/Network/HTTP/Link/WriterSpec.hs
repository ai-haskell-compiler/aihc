{-# LANGUAGE OverloadedStrings, UnicodeSyntax #-}

module Network.HTTP.Link.WriterSpec where

import           Test.Hspec
import           Data.Maybe (fromJust)
import           Network.HTTP.Link (lnk)
import           Network.HTTP.Link.Types
import           Network.HTTP.Link.Writer
import           Network.URI (URI)

spec ∷ Spec
spec = do
  describe "writeLinkHeader" $ do
    let l u r = fromJust $ lnk u r :: Link URI

    it "writes a single link" $ do
      writeLinkHeader [l "http://example.com" [(Rel, "next")]]
        `shouldBe` "<http://example.com>; rel=\"next\""

    it "writes params with quote escaping" $ do
      writeLinkHeader [l "http://example.com" [(Rel, "some \"weirdness\"")]]
        `shouldBe` "<http://example.com>; rel=\"some %22weirdness%22\""

    it "writes multiple parameters" $ do
      writeLinkHeader [l "http://example.com" [(Rel, "next"), (Title, "hello world")]]
        `shouldBe` "<http://example.com>; rel=\"next\"; title=\"hello world\""

    it "writes custom params" $ do
      writeLinkHeader [l "http://example.com" [(Rel, "next"), (Other "thing", "http://example.com/foo"), (Rev, "license")]]
        `shouldBe` "<http://example.com>; rel=\"next\"; thing=\"http://example.com/foo\"; rev=\"license\""

    it "writes multiple links" $ do
      writeLinkHeader [ l "http://example.com" [(Rel, "next"), (Title, "hello world")]
                      , l "https://hello.world" [(Rev, "license")] ]
        `shouldBe` "<http://example.com>; rel=\"next\"; title=\"hello world\", <https://hello.world>; rev=\"license\""
