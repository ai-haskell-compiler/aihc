-- | Indexes
module Pinecone.Indexes
    ( -- * Main types
      Index(..)
    , Host(..)
    , IndexModels(..)
    , IndexModel(..)
    , CreateIndex(..)
    , _CreateIndex
    , CreateIndexWithEmbedding(..)
    , _CreateIndexWithEmbedding
    , ConfigureIndex(..)
    , _ConfigureIndex
    , GetIndexStats(..)
    , _GetIndexStats
    , IndexStats(..)

      -- * Other types
    , Metric(..)
    , Spec(..)
    , Pod(..)
    , PodType(..)
    , Prefix(..)
    , Suffix(..)
    , MetadataConfig(..)
    , Serverless(..)
    , Cloud(..)
    , Status(..)
    , State(..)
    , DeletionProtection(..)
    , EmbedRequest(..)
    , EmbedResponse(..)
    , Contents(..)

      -- * API
    , ControlAPI
    , DataAPI
    ) where

import Pinecone.Embed (VectorType)
import Pinecone.Metadata (Filter)
import Pinecone.Prelude

import qualified Data.Text as Text

-- | The name of the index
newtype Index = Index{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The host for the index
newtype Host = Host{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The list of indexes that exist in the project
data IndexModels = IndexModels
    { indexes :: Vector IndexModel
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The `IndexModel` describes the configuration and status of a Pinecone
-- index
data IndexModel = IndexModel
    { name :: Index
    , metric :: Metric
    , host :: Host
    , spec :: Spec
    , status :: Status
    , vector_type :: VectorType
    , dimension :: Maybe Natural
    , deletion_protection :: Maybe DeletionProtection
    , tags :: Maybe (Map Text Text)
    , embed :: Maybe EmbedResponse
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The desired configuration for the index
data CreateIndex = CreateIndex
    { name :: Index
    , spec :: Spec
    , dimension :: Maybe Natural
    , metric :: Maybe Metric
    , deletion_protection :: Maybe DeletionProtection
    , tags :: Maybe (Map Text Text)
    , vector_type :: Maybe VectorType
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateIndex`
_CreateIndex :: CreateIndex
_CreateIndex = CreateIndex
    { dimension = Nothing
    , metric = Nothing
    , deletion_protection = Nothing
    , tags = Nothing
    , vector_type = Nothing
    }

-- | The desired configuration for the index and associated embedding model
data CreateIndexWithEmbedding = CreateIndexWithEmbedding
    { name :: Index
    , cloud :: Cloud
    , region :: Text
    , embed :: EmbedRequest
    , deletion_protection :: Maybe DeletionProtection
    , tags :: Maybe (Map Text Text)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateIndexWithEmbedding`
_CreateIndexWithEmbedding :: CreateIndexWithEmbedding
_CreateIndexWithEmbedding = CreateIndexWithEmbedding
    { deletion_protection = Nothing
    , tags = Nothing
    }

-- | The desired pod size and replica configuration for the index
data ConfigureIndex = ConfigureIndex
    { spec :: Maybe Spec
    , deletion_protection :: Maybe DeletionProtection
    , tags :: Maybe (Map Text Text)
    , embed :: Maybe EmbedRequest
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `ConfigureIndex`
_ConfigureIndex :: ConfigureIndex
_ConfigureIndex = ConfigureIndex
    { spec = Nothing
    , deletion_protection = Nothing
    , tags = Nothing
    , embed = Nothing
    }

-- | Request body for @\/describe_index_stats@
data GetIndexStats = GetIndexStats
    { filter :: Maybe Filter
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `GetIndexStats`
_GetIndexStats :: GetIndexStats
_GetIndexStats = GetIndexStats
    { filter = Nothing
    }

-- | Response body for @\/describe_index_stats@
data IndexStats = IndexStats
    { namespaces :: Map Text Contents
    , dimension :: Natural
    , indexFullness :: Double
    , totalVectorCount :: Natural
    , metric :: Metric
    , vectorType :: VectorType
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The distance metric to be used for similarity search
data Metric = Cosine | Euclidean | DotProduct
    deriving stock (Eq, Generic, Show)

instance FromJSON Metric where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Metric where
    toJSON = genericToJSON aesonOptions

-- | `Spec` object
data Spec = Spec
    { pod :: Maybe Pod
    , serverless :: Maybe Serverless
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration needed to deploy a pod-based index.
data Pod = Pod
    { environment :: Text
    , pod_type :: PodType
    , replicas :: Maybe Natural
    , shards :: Maybe Natural
    , pods :: Maybe Natural
    , metadata_config :: Maybe MetadataConfig
    , source_collection :: Maybe Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The type of pod to use
data PodType = PodType
    { prefix :: Prefix
    , suffix :: Suffix
    } deriving stock (Eq, Generic, Show)

instance ToJSON PodType where
    toJSON PodType{..} = do
        toJSON (Text.concat [ prefixText, ".", suffixText ])
      where
        prefixText = case toJSON prefix of
            String text -> text
            _ -> ""

        suffixText = case toJSON suffix of
            String text -> text
            _ -> ""

instance FromJSON PodType where
    parseJSON value = do
        text <- parseJSON value

        case Text.splitOn "." text of
            [ prefixText, suffixText ] -> do
                prefix <- parseJSON (String prefixText)
                suffix <- parseJSON (String suffixText)
                return PodType{..}
            _ -> do
                fail "Pod type must be of the form PREFIX.SUFFIX"

-- | The first component of a pod type
data Prefix = S1 | P1 | P2
    deriving stock (Eq, Generic, Show)

instance FromJSON Prefix where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Prefix where
    toJSON = genericToJSON aesonOptions

-- | The second component of a pod type
data Suffix = X1 | X2 | X4 | X8
    deriving stock (Eq, Generic, Show)

instance FromJSON Suffix where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Suffix where
    toJSON = genericToJSON aesonOptions

-- | Configuration for the behavior of Pinecone's internal metadata index
data MetadataConfig = MetadataConfig
    { indexed :: Maybe (Vector Text)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Configuration needed to deploy a serverless index
data Serverless = Serverless
    { cloud :: Cloud
    , region :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The public cloud where you would like your index hosted
data Cloud = GCP | AWS | Azure
    deriving stock (Eq, Generic, Show)

instance FromJSON Cloud where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON Cloud where
    toJSON = genericToJSON aesonOptions

-- | Index status
data Status = Status
    { ready :: Bool
    , state :: State
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Index state
data State
    = Initializing
    | InitializationFailed
    | ScalingUp
    | ScalingDown
    | ScalingUpPodSize
    | ScalingDownPodSize
    | Terminating
    | Ready
    deriving stock (Eq, Generic, Show)

instance FromJSON State where
    parseJSON = genericParseJSON aesonOptions{ constructorTagModifier = id }

instance ToJSON State where
    toJSON = genericToJSON aesonOptions{ constructorTagModifier = id }

-- | Whether deletion protection is enabled/disabled for the index.
data DeletionProtection = Disabled | Enabled
    deriving stock (Eq, Generic, Show)

instance FromJSON DeletionProtection where
    parseJSON = genericParseJSON aesonOptions

instance ToJSON DeletionProtection where
    toJSON = genericToJSON aesonOptions

-- | Specify the integrated inference embedding configuration for the index
data EmbedRequest = EmbedRequest
    { model :: Text
    , metric :: Maybe Metric
    , read_parameters :: Maybe (Map Text Value)
    , write_parameters :: Maybe (Map Text Value)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON)

data EmbedRequest_ = EmbedRequest_
    { model :: Text
    , field_map :: Map Text Text
    , metric :: Maybe Metric
    , read_parameters :: Maybe (Map Text Value)
    , write_parameters :: Maybe (Map Text Value)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

instance ToJSON EmbedRequest where
    toJSON EmbedRequest{..} = toJSON EmbedRequest_{..}
      where
        field_map = [ ("text", "text") ]

-- | The embedding model and document fields mapped to embedding inputs
data EmbedResponse = EmbedResponse
    { model :: Text
    , metric :: Maybe Metric
    , dimension :: Maybe Natural
    , vector_type :: Maybe VectorType
    , field_map :: Maybe (Map Text Text)
    , read_parameters :: Maybe (Map Text Value)
    , write_parameters :: Maybe (Map Text Value)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | A summary of the contents of a namespace
data Contents = Contents
    { vectorCount :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Control API
type ControlAPI =
        "indexes"
    :>  (     Get '[JSON] IndexModels

        :<|>  (   ReqBody '[JSON] CreateIndex
              :>  Post '[JSON] IndexModel
              )

        :<|>  (   "create-for-model"
              :>  ReqBody '[JSON] CreateIndexWithEmbedding
              :>  PostCreated '[JSON] IndexModel
              )

        :<|>  (   Capture "index_name" Index
              :>  Get '[JSON] IndexModel
              )

        :<|>  (   Capture "index_name" Index
              :>  DeleteAccepted '[JSON] NoContent
              )

        :<|>  (   Capture "index_name" Index
              :>  ReqBody '[JSON] ConfigureIndex
              :>  Patch '[JSON] IndexModel
              )
        )

-- | Data API
type DataAPI =
        "describe_index_stats"
    :>  ReqBody '[JSON] GetIndexStats
    :>  Post '[JSON] IndexStats
