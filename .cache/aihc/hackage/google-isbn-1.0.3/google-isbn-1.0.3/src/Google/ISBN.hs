{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE LambdaCase #-}

-- |
--
-- Basic utility to search an ISBN using the Google Books webservice
--
-- use: https://developers.google.com/books/docs/v1/using
--

module Google.ISBN (
  ISBN (..)
  , GoogleISBN (..)
  , Book (..)
  , googleISBN
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text as T
import           GHC.Generics
import           Network.HTTP.Simple

newtype ISBN = ISBN T.Text

instance Show ISBN where
  show (ISBN i) = "isbn:" <> T.unpack i

data GoogleISBN = GoogleISBN {
  googleISBNTotalItems :: Integer
  , googleISBNItems :: [Book]
} deriving (Show, Generic)

data Book = Book {
  bookTitle :: T.Text
  , bookSubtitle :: Maybe T.Text
  , bookPublisher :: Maybe T.Text
  , bookDescription :: T.Text
  , bookPublishedDate :: T.Text
  , bookLanguage :: T.Text
  , bookAuthors :: Maybe [T.Text]
  } deriving (Show, Generic)

instance FromJSON Book where
  parseJSON (Object v) = (v .: "volumeInfo") >>= \b -> Book
    <$> b .: "title"
    <*> b .:? "subtitle"
    <*> b .:? "publisher"
    <*> b .: "description"
    <*> b .: "publishedDate"
    <*> b .: "language"
    <*> b .: "authors"
  parseJSON invalid = typeMismatch "Book" invalid

instance FromJSON GoogleISBN where
  parseJSON (Object v) = GoogleISBN
    <$> v .: "totalItems"
    <*> v .: "items"
  parseJSON invalid = typeMismatch "APIResult" invalid

urlApi :: String
urlApi = "https://www.googleapis.com/books/v1/"

googleISBN :: ISBN -> IO (Maybe GoogleISBN)
googleISBN isbn = parseRequest (urlApi <> "volumes?q=" <> show isbn)
                  >>= (\req ->
                         (\r -> fromJSON r :: Result GoogleISBN) <$> ((\r -> getResponseBody r::Value) <$> httpJSON req) >>= \case
                          Success x -> return $ Just x
                          Error _ -> return Nothing)
