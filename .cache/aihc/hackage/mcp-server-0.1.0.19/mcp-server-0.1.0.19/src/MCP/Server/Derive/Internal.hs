{-# LANGUAGE OverloadedStrings #-}

module MCP.Server.Derive.Internal
  ( parseText
  , parseInt
  , parseInteger
  , parseDouble
  , parseFloat
  , parseBool
  ) where

import           Data.Text (Text)
import qualified Data.Text as T
import           Text.Read (readMaybe)

parseText :: Text -> Either Text Text
parseText = Right

parseInt :: Text -> Either Text Int
parseInt t = case readMaybe (T.unpack t) of
  Just v  -> Right v
  Nothing -> Left $ "Failed to parse Int from: " <> t

parseInteger :: Text -> Either Text Integer
parseInteger t = case readMaybe (T.unpack t) of
  Just v  -> Right v
  Nothing -> Left $ "Failed to parse Integer from: " <> t

parseDouble :: Text -> Either Text Double
parseDouble t = case readMaybe (T.unpack t) of
  Just v  -> Right v
  Nothing -> Left $ "Failed to parse Double from: " <> t

parseFloat :: Text -> Either Text Float
parseFloat t = case readMaybe (T.unpack t) of
  Just v  -> Right v
  Nothing -> Left $ "Failed to parse Float from: " <> t

parseBool :: Text -> Either Text Bool
parseBool t = case T.toLower t of
  "true"  -> Right True
  "false" -> Right False
  _       -> Left $ "Failed to parse Bool from: " <> t
