{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Data.Text (Text)

-- High-level data type definitions from SPEC.md

data MyPrompt
    = Recipe { idea :: Text }
    | Shopping { description :: Text }
    deriving (Show, Eq)

data MyResource
    = ProductCategories
    | SaleItems
    | HeadlineBannerAd
    deriving (Show, Eq)

data MyTool
    = SearchForProduct { q :: Text, category :: Maybe Text }
    | AddToCart { sku :: Text }
    | Checkout
    | ComplexTool { field1 :: Text, field2 :: Text, field3 :: Maybe Text, field4 :: Text, field5 :: Maybe Text }
    deriving (Show, Eq)
