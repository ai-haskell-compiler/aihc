-- | Backups
module Pinecone.Backups
    ( -- * Main types
      Collection(..)
    , Collections(..)
    , CreateCollection(..)
    , _CreateCollection
    , CollectionModel(..)

     -- * Other types
    , Status(..)

      -- * Servant
    , API
    ) where

import Pinecone.Indexes (Index)
import Pinecone.Prelude

-- | The name of the collection
newtype Collection = Collection{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | The list of collections that exist in the project.
data Collections = Collections
    { collections :: Vector CollectionModel
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The configuration needed to create a Pinecone collection.
data CreateCollection = CreateCollection
    { name :: Text
    , source :: Index
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `CreateCollection`
_CreateCollection :: CreateCollection
_CreateCollection = CreateCollection{ }

-- | The CollectionModel describes the configuration and status of a Pinecone collection.
data CollectionModel = CollectionModel
    { name :: Collection
    , status :: Status
    , environment :: Text
    , size :: Maybe Natural
    , dimension :: Maybe Natural
    , vector_count :: Maybe Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | The status of the operation.
data Status
    = Initializing
    | Ready
    | Terminating
    deriving stock (Eq, Generic, Show)

instance FromJSON Status where
    parseJSON = genericParseJSON aesonOptions{ constructorTagModifier = id }

instance ToJSON Status where
    toJSON = genericToJSON aesonOptions{ constructorTagModifier = id }

-- | Servant API
type API =
        "collections"
    :>  (     Get '[JSON] Collections

        :<|>      ReqBody '[JSON] CreateCollection
              :>  Post '[JSON] CollectionModel

        :<|>      Capture "collection_name" Collection
              :>  Get '[JSON] CollectionModel

        :<|>      Capture "collection_name" Collection
              :>  DeleteAccepted '[JSON] NoContent
        )
