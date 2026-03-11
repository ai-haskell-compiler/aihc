-- | Pagination
module Pinecone.Pagination
    ( -- * Types
      Pagination(..)
    ) where

import Pinecone.Prelude

-- | Pagination
data Pagination = Pagination
    { next :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)
