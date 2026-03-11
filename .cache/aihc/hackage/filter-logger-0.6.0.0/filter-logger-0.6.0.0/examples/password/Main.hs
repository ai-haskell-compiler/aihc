{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Data.Aeson
import           GHC.Generics
import           Network.Wai.Middleware.FilterLogger
import           Web.Scotty

data LoginRequest = LoginRequest {
    username :: String
  , password :: String
  } deriving (Generic)

instance FromJSON LoginRequest where
instance ToJSON LoginRequest where

instance LogShowable LoginRequest where
  logShow = logShowJSON

instance LogFilterable LoginRequest where
  prep = logFilterJSON

instance Loggable LoginRequest where

{-

  Sending a POST request to localhost:3000
  with a body like:
  {
    "username": "test-username",
    "password": "myPassw0rd123"
  }
  will result in a log message like:
  11/Jul/2017:21:48:20 -0400
  200 - OK
  0.03ms
  {
      "username": "test-username",
      "password": "*****"
  }

-}

main :: IO ()
main = scotty 3000 $ do
  middleware filterPasswords
  post "/" $ text "SUCCESS"

filterPasswords =
  mkDefaultFilterLogger hidePasswords
  where hidePasswords r@LoginRequest{..} = Just r {password = "*****"}
