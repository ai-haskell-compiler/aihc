{-# LANGUAGE OverloadedStrings #-}

module Network.Api.Types where

import Data.Text (Text)
import Data.Aeson ((.:), Value(..), FromJSON(..), (.:?))
import Control.Applicative (empty, (<*>))
import Data.Functor ((<$>))

data Error = Error
    { ecode :: Int
    , emessage :: Text
    } deriving (Eq,Ord,Show)

instance FromJSON Error where
    parseJSON (Object v) = Error <$> v .: "code" <*> v .: "message"
    parseJSON _ = empty

data Warning = Warning
    { wcode :: Int
    , wmessage :: Text
    } deriving (Eq,Ord,Show)

instance FromJSON Warning where
    parseJSON (Object v) = Warning <$> v .: "code" <*> v .: "message"
    parseJSON _ = empty

data TLStatus
    = Success
    | Failure
    deriving (Show,Eq,Ord)

instance FromJSON TLStatus where
    parseJSON (String v) =
        if (v == "success")
            then return Success
            else return Failure
    parseJSON _ = empty

data TLResponse = TLResponse
    { status :: TLStatus
    , warnings :: Maybe [Warning]
    , errors :: Maybe [Error]
    } deriving (Eq,Ord,Show)

instance FromJSON TLResponse where
    parseJSON (Object resp) =
        TLResponse <$> resp .: "status" <*> resp .:? "warnings" <*>
        resp .:? "errors"
    parseJSON _ = empty