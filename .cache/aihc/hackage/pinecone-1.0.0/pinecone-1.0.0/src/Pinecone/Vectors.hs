-- | Vectors
module Pinecone.Vectors
    ( -- * Main types
      Namespace(..)
    , UpsertVectors(..)
    , _UpsertVectors
    , UpsertStats(..)
    , Vectors(..)
    , UpdateVector(..)
    , _UpdateVector
    , DeleteVectors(..)
    , _DeleteVectors
    , VectorIDs(..)
    , Record(..)
    , _Record

      -- * Other types
    , VectorObject(..)
    , SparseValues(..)
    , Usage(..)

      -- * Servant
    , API
    ) where

import Pinecone.Metadata (Filter, Scalar)
import Pinecone.Pagination (Pagination)
import Pinecone.Prelude
import Prelude hiding (id)

import qualified Data.Map as Map

-- | The namespace of a record
newtype Namespace = Namespace{ text :: Text }
    deriving newtype (Eq, FromJSON, IsString, Show, ToHttpApiData, ToJSON)

-- | Request body for @\/vectors\/upsert@
data UpsertVectors = UpsertVectors
    { vectors :: Vector VectorObject
    , namespace :: Maybe Namespace
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `UpsertVectors`
_UpsertVectors :: UpsertVectors
_UpsertVectors = UpsertVectors
    { namespace = Nothing
    }

-- | Response body for @\/vectors\/upsert@
data UpsertStats = UpsertStats
    { upsertedCount :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Response body for @\/vectors\/fetch@
data Vectors = Vectors
    { vectors :: Map Text VectorObject
    , namespace :: Namespace
    , usage :: Maybe Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Request body for @\/vectors\/update@
data UpdateVector = UpdateVector
    { id :: Text
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , setMetadata :: Maybe (Map Text Scalar)
    , namespace :: Maybe Namespace
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `UpdateVector`
_UpdateVector :: UpdateVector
_UpdateVector = UpdateVector
    { values = Nothing
    , sparseValues = Nothing
    , setMetadata = Nothing
    , namespace = Nothing
    }

-- | Request body for @\/vectors\/delete@
data DeleteVectors = DeleteVectors
    { ids :: Maybe (Vector Text)
    , deleteAll :: Maybe Bool
    , namespace :: Maybe Namespace
    , filter :: Maybe Filter
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Default `DeleteVectors`
_DeleteVectors :: DeleteVectors
_DeleteVectors = DeleteVectors
    { deleteAll = Nothing
    , namespace = Nothing
    , filter = Nothing
    }

-- | Response body for @\/vectors\/list@
data VectorIDs = VectorIDs
    { vectors :: Vector Text
    , pagination :: Maybe Pagination
    , namespace :: Namespace
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)

instance FromJSON VectorIDs where
    parseJSON value = do
        VectorIDs_{..} <- parseJSON value

        return VectorIDs{ vectors = do VectorID{ id } <- vectors; return id, ..}

data VectorIDs_ = VectorIDs_
    { vectors :: Vector VectorID
    , pagination :: Maybe Pagination
    , namespace :: Namespace
    , usage :: Usage
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Record to upsert
data Record = Record
    { id :: Text
    , text :: Text
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)

instance FromJSON Record where
    parseJSON value = do
        m <- parseJSON value

        text <- case Map.lookup "text" m of
            Nothing -> do
                fail "Missing text field"
            Just v -> do
                parseJSON v

        id <- case Map.lookup "_id" m of
            Nothing -> do
                fail "Missing id field"
            Just v -> do
                parseJSON v

        metadata <- fmap Just (traverse parseJSON (Map.delete "_id" (Map.delete "text" m)))

        return Record{..}

instance ToJSON Record where
    toJSON Record{..} = toJSON (Map.union reserved nonReserved)
      where
        reserved =
            [ ("_id", toJSON id)
            , ("text", toJSON text)
            ]

        nonReserved = case metadata of
            Nothing -> Map.empty
            Just m -> Map.map toJSON m

-- | Default `Record`
_Record :: Record
_Record = Record
    { metadata = Nothing
    }

-- | A vector
data VectorObject = VectorObject
    { id :: Text
    , values :: Maybe (Vector Double)
    , sparseValues :: Maybe SparseValues
    , metadata :: Maybe (Map Text Scalar)
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Vector sparse data
data SparseValues = SparseValues
    { indices :: Vector Natural
    , values :: Vector Double
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Vector ID
data VectorID = VectorID
    { id :: Text
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Usage
data Usage = Usage
    { readUnits :: Natural
    } deriving stock (Eq, Generic, Show)
      deriving anyclass (FromJSON, ToJSON)

-- | Servant API
type API =
          (   "vectors"
          :>  (     (   "upsert"
                    :>  ReqBody '[JSON] UpsertVectors
                    :>  Post '[JSON] UpsertStats
                    )

              :<|>  (   "fetch"
                    :>  QueryParams "ids" Text
                    :>  QueryParam "namespace" Namespace
                    :>  Get '[JSON] Vectors
                    )

              :<|>  (   "update"
                    :>  ReqBody '[JSON] UpdateVector
                    :>  Post '[JSON] NoContent
                    )

              :<|>  (   "delete"
                    :>  ReqBody '[JSON] DeleteVectors
                    :>  Post '[JSON] NoContent
                    )

              :<|>  (   "list"
                    :>  QueryParam "prefix" Text
                    :>  QueryParam "limit" Natural
                    :>  QueryParam "paginationToken" Text
                    :>  QueryParam "namespace" Namespace
                    :>  Get '[JSON] VectorIDs
                    )
              )
          )
    :<|>  (   "records"
          :>  "namespaces"
          :>  Capture "namespace" Namespace
          :>  "upsert"
          :>  ReqBody '[JSON] Record
          :>  PostCreated '[JSON] NoContent
          )
