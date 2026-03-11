-- | Rerank
module Pinecone.Rerank
    ( -- * Main types
      RerankResults(..)
    , _RerankResults
    , Documents(..)

      -- * Other types
    , Document(..)
    , Usage(..)

      -- * Servant
    , API
    ) where

import Pinecone.Metadata (Scalar)
import Pinecone.Prelude
import Pinecone.Vectors (Record)

-- | Rerank documents for the given query
data RerankResults = RerankResults
    { model :: Text
    , query :: Text
    , documents :: Vector Record
    , top_n :: Maybe Natural
    , return_documents :: Bool
    , parameters :: Map Text Scalar
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON)

data RerankResults_ = RerankResults_
    { model :: Text
    , query :: Text
    , documents :: Vector Record
    , top_n :: Maybe Natural
    , return_documents :: Bool
    , rank_fields :: Vector Text
    , parameters :: Map Text Scalar
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance ToJSON RerankResults where
    toJSON RerankResults{..} = toJSON RerankResults_{..}
      where
        rank_fields = [ "text" ]

-- | Default `RerankResults`
_RerankResults :: RerankResults
_RerankResults = RerankResults
    { top_n = Nothing
    }

-- | The result of a reranking request.
data Documents = Documents
    { model :: Text
    , data_ :: Vector Document
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)

instance FromJSON Documents where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Documents where
    toJSON = genericToJSON aesonOptions

-- | A ranked document with a relevance score and an index position.
data Document = Document
    { index :: Natural
    , score :: Double
    , document :: Maybe Record
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Usage statistics for the model inference.
data Usage = Usage
    { rerank_units :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
    "rerank" :> ReqBody '[JSON] RerankResults :> Post '[JSON] Documents
