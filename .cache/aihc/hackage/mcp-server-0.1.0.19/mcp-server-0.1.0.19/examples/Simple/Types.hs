{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

data SimpleTool
    = GetValue { key :: Text }
    | SetValue { key :: Text, value :: Text }
    deriving (Show, Eq)

simpleDescriptions :: [(String, String)]
simpleDescriptions =
    [ ("GetValue", "Retrieve the value associated with the given key")
    , ("SetValue", "Set the value for the given key")
    , ("key", "The key to look up or set")
    , ("value", "The value to store")
    ]
