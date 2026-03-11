{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.HttpApiData ()
where

import Data.BCP47 (BCP47, fromText, toText)
import Web.HttpApiData (FromHttpApiData (..), ToHttpApiData (..))

instance ToHttpApiData BCP47 where
  toUrlPiece = toText

instance FromHttpApiData BCP47 where
  parseUrlPiece = fromText
