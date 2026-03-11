-- | Embed
module Pinecone.Embed
    ( -- * Main types
      GenerateVectors(..)
    , _GenerateVectors
    , Embeddings(..)

      -- * Other types
    , Input(..)
    , VectorType(..)
    , Embedding(..)
    , Usage(..)

      -- * Servant
    , API
    ) where

import Pinecone.Metadata (Scalar)
import Pinecone.Prelude

-- | Generate embeddings for inputs
data GenerateVectors = GenerateVectors
    { model :: Text
    , inputs :: Vector Text
    , parameters :: Map Text Scalar
    } deriving stock (Eq, Generic, Show)

instance FromJSON GenerateVectors where
    parseJSON value = do
        GenerateVectors_{..} <- parseJSON value

        return GenerateVectors{ inputs = fmap text inputs, ..}

instance ToJSON GenerateVectors where
    toJSON GenerateVectors{..} =
        toJSON GenerateVectors_{ inputs = fmap Input inputs, ..}

data GenerateVectors_ = GenerateVectors_
    { model :: Text
    , inputs :: Vector Input
    , parameters :: Map Text Scalar
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `GenerateVectors`
_GenerateVectors :: GenerateVectors
_GenerateVectors = GenerateVectors{ }

-- | Embeddings generated for the input.
data Embeddings = Embeddings
    { model :: Text
    , vector_type :: VectorType
    , data_ :: Vector Embedding
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)

instance FromJSON Embeddings where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Embeddings where
    toJSON = genericToJSON aesonOptions

-- | An embedding generated for an input
data Embedding
    = EmbeddingDense
        { values :: Vector Double
        }
    | EmbeddingSparse
        { sparse_values :: Vector Double
        , sparse_indices :: Vector Natural
        , sparse_tokens :: Vector Text
        }
    deriving stock (Eq, Generic, Show)

embeddingOptions :: Options
embeddingOptions = aesonOptions
    { sumEncoding =
        TaggedObject{ tagFieldName = "vector_type", contentsFieldName = "" }

    , constructorTagModifier = stripPrefix "Embedding"

    , tagSingleConstructors = True
    }

instance FromJSON Embedding where
    parseJSON = genericParseJSON embeddingOptions

instance ToJSON Embedding where
    toJSON = genericToJSON embeddingOptions

-- | Input to generate embedding for
data Input = Input
    { text :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The index vector type
data VectorType = Dense | Sparse
    deriving stock (Eq, Generic, Show)

instance FromJSON VectorType where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON VectorType where
    toJSON = genericToJSON aesonOptions

-- | Usage statistics for the model inference.
data Usage = Usage
    { total_tokens :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
        "embed"
    :>  ReqBody '[JSON] GenerateVectors
    :>  Post '[JSON] Embeddings
