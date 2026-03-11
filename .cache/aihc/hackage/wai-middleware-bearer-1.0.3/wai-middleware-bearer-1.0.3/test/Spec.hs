{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.Wai.Middleware.BearerTokenAuth
import Test.Hspec
import Test.Hspec.Wai
import Network.Wai
import Network.HTTP.Types.Status (status200)

myApp :: Application
myApp req rsp = rsp $ responseLBS status200 [] "Hello World"

secureApp :: Application
secureApp = tokenListAuth ["abc", "123"] myApp

main :: IO ()
main = hspec $ do
  with (return secureApp) $ do
      describe "GET /" $ do
        it "accepts valid token" $ do
          let validHeader1 = ("Authorization", "Bearer abc")
          request "GET" "/" [validHeader1] "" `shouldRespondWith`
            200
          let validHeader2 = ("Authorization", "Bearer 123")
          request "GET" "/" [validHeader2] "" `shouldRespondWith`
            200
        it "rejects invalid token" $ do
          let invalidHeader = ("Authorization", "Bearer abcd")
          request "GET" "/" [invalidHeader] "" `shouldRespondWith`
            401
        it "rejects missing token" $ do
          get "/" `shouldRespondWith` 401

