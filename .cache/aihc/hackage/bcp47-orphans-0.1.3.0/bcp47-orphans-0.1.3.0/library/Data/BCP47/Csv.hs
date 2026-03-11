{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Csv () where

import Data.BCP47 (BCP47, fromText, toText)
import Data.Csv (FromField (..), ToField (..))
import Data.Text (unpack)

instance ToField BCP47 where
  toField = toField . toText

instance FromField BCP47 where
  parseField bytes = do
    text <- parseField bytes
    either (fail . unpack) pure $ fromText text
