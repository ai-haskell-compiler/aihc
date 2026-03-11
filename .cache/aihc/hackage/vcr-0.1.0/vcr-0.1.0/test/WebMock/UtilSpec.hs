{-# LANGUAGE OverloadedStrings #-}
module WebMock.UtilSpec (spec) where

import Imports

import Test.Hspec
import Test.Mockery.Directory

import Data.ByteString.Lazy.Char8 qualified as L
import Data.ByteString.Lazy.Internal (defaultChunkSize)
import Network.HTTP.Client

import WebMock.Util

spec :: Spec
spec = around_ inTempDirectory do
  describe "requestBodyToByteString" do
    describe "with RequestBodyLBS" do
      it "converts it to a ByteString" do
        requestBodyToByteString (RequestBodyLBS "foo") `shouldReturn` "foo"

    describe "with RequestBodyBS" do
      it "converts it to a ByteString" do
        requestBodyToByteString (RequestBodyBS "foo") `shouldReturn` "foo"

    describe "with RequestBodyBuilder" do
      it "converts it to a ByteString" do
        requestBodyToByteString (RequestBodyBuilder 3 "foo") `shouldReturn` "foo"

      context "with wrong length" do
        it "throws an exception" do
          requestBodyToByteString (RequestBodyBuilder 5 "foo") `shouldThrow` anyException

    let file = "test.txt"
    let expected = L.fromChunks $ replicate defaultChunkSize $ "foobar"
    before_ (L.writeFile file expected) do
      context "with RequestBodyStream" do
        it "converts it to a ByteString" do
          body@(RequestBodyStream _ _) <- streamFile file
          requestBodyToByteString body `shouldReturn` expected

        context "with wrong length" do
          it "throws an exception" do
            RequestBodyStream n body <- streamFile file
            requestBodyToByteString (RequestBodyStream (pred n) body) `shouldThrow` anyException

      context "with RequestBodyStreamChunked" do
        it "converts it to a ByteString" do
          (RequestBodyStream _ body) <- streamFile file
          requestBodyToByteString (RequestBodyStreamChunked body) `shouldReturn` expected
