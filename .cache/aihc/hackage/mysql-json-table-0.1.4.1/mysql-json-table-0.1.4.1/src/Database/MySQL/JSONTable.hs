----------------------------------------------------------------------------------------------------

-- | Interface to use a MySQL table with a very specific format, where each row
--   consists of a row identifier - used for lookups - and a JSON-encoded value.
--
-- +----------------------------+--------------------+
-- |             id             |        data        |
-- +============================+====================+
-- | Row identifier (type 'Id') | JSON-encoded value |
-- +----------------------------+--------------------+
--
module Database.MySQL.JSONTable
  ( -- * JSON tables
    -- ** Types
    Id
  , Row (..)
  , JSONTable (..)
    -- * Connections
  , SQL.ConnectInfo (..)
  , SQL.defaultConnectInfo
  , SQL.Connection
  , withSQL
    -- ** Table operations
  , createTable
  , deleteTable
    -- ** Row operations
  , insert
  , lookup
  , adjust
  , delete
  , replace
    -- ** Streaming
  , sourceRows
    -- * Id tables
    -- ** Types
  , IdTable (..)
    -- ** Table operations
  , createIdTable
  , deleteIdTable
    -- ** Row operations
  , insertId
  , lookupId
  , adjustId
  , alterId
  , deleteId
  , replaceId
  , moveId
    -- ** Streaming
  , sourceIds
    ) where

import Prelude hiding (lookup)
import Data.Word
import Data.String (fromString)
import Data.Char (toUpper)
import Text.Read (readEither)
import Data.Maybe (listToMaybe)
import Data.Typeable (Typeable)
import Data.Proxy
import Control.Applicative (liftA2)
import Control.Monad (forM_, when, unless)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Catch (MonadMask, bracket)
#if MIN_VERSION_bytestring(0,11,0)
import Data.ByteString qualified as ByteString
#else
import Data.ByteString.Lazy qualified as LazyByteString
#endif
import Data.ByteString.Char8 qualified as CByteString
import Database.MySQL.Simple qualified as SQL
import Database.MySQL.Simple.QueryResults qualified as SQL
import Database.MySQL.Base qualified as SQLBase
import Data.Aeson (FromJSON, ToJSON, FromJSONKey, ToJSONKey)
import Data.Aeson qualified as JSON
import Data.Conduit (ConduitT)
import Conduit (ResourceT)
import Data.Conduit qualified as Conduit
import Web.HttpApiData (FromHttpApiData, ToHttpApiData)
import Data.Hashable (Hashable)

-- | Open a connection to a MySQL server and apply a function to it.
--   The connection is closed both when the function completes or throws an exception.
withSQL :: (MonadMask m, MonadIO m) => SQL.ConnectInfo -> (SQL.Connection -> m a) -> m a
withSQL sql = bracket (liftIO $ SQL.connect sql) (liftIO . SQL.close)

-- | Row identifier used for table lookups.
--   The type parameter indicates the type of data
--   stored in the table.
newtype Id a = Id { fromId :: Word64 }
  deriving ( Eq, Ord, Show, ToJSON, FromJSON, FromJSONKey, ToJSONKey
           , FromHttpApiData, ToHttpApiData, Hashable )

instance SQL.FromField (Id a) where
  fromField = ([SQLBase.LongLong], fmap Id . readEither . CByteString.unpack)

instance Typeable a => SQL.Result (Id a)

instance SQL.ToField (Id a) where
  toField = fromString . show . fromId

instance SQL.Param (Id a)

-- | A single row.
data Row a = Row
  { -- | Row identifier.
    rowId :: Id a
    -- | Row data.
  , rowData :: a
    } deriving (Eq, Show)

instance FromJSON a => FromJSON (Row a) where
  parseJSON = JSON.withObject "Row" $ \o ->
    liftA2 Row (o JSON..: "id") (o JSON..: "data")

instance ToJSON a => ToJSON (Row a) where
  toJSON (Row i x) = JSON.object ["id" JSON..= i, "data" JSON..= x]

-- | A MySQL table with two columns:
--
-- +----------------------------+-----------+
-- |             id             |    data   |
-- +============================+===========+
-- | Row identifier (type 'Id') | JSON data |
-- +----------------------------+-----------+
--
-- The type parameter indicates the type of data
--  stored in the table.
data JSONTable a = JSONTable
  { -- | Table name.
    tableName :: String
    }

tableSpecs :: String
tableSpecs = concat
  [ "("
  , "id BIGINT UNSIGNED NOT NULL PRIMARY KEY AUTO_INCREMENT"
  , ", "
  , "data JSON NOT NULL"
  , ") ENGINE=InnoDB"
    ]

-- | Create a new JSON table in a MySQL database.
createTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table already exists.
  -> String -- ^ Table name.
  -> IO (JSONTable a)
createTable conn failIfExists name = do
  let ifNotExists = if failIfExists then " " else " IF NOT EXISTS "
      query = "CREATE TABLE" ++ ifNotExists ++ "`" ++ name ++ "` " ++ tableSpecs
  _ <- SQL.execute conn (fromString query) ()
  pure $ JSONTable
    { tableName = name
      }

-- | Delete a JSON table from a MySQL database, together with all of its content.
deleteTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table doesn't exist.
  -> JSONTable a 
  -> IO ()
deleteTable conn failIfNotExist table = do
  let ifExists = if failIfNotExist then " " else " IF EXISTS "
      query = "DROP TABLE" ++ ifExists ++ "`" ++ tableName table ++ "`"
  _ <- SQL.execute conn (fromString query) ()
  pure ()

-- | JSON serialization helper.
newtype AsJSON a = AsJSON { asJSON :: a }

instance FromJSON a => SQL.FromField (AsJSON a) where
  fromField = ([SQLBase.Json], fmap AsJSON . JSON.eitherDecodeStrict)

instance ToJSON a => SQL.ToField (AsJSON a) where
#if MIN_VERSION_bytestring(0,11,0)
  toField = ByteString.toStrict . JSON.encode . asJSON
#else
  toField = LazyByteString.toStrict . JSON.encode . asJSON
#endif

instance (Typeable a, FromJSON a) => SQL.Result (AsJSON a)
instance ToJSON a => SQL.Param (AsJSON a)

-- | Insert a new row into a table.
--
--   /Warning:/ It is recommended not to call 'insert' with the same 'SQL.Connection'
--   argument from multiple threads. The 'Id's returned might get mixed up.
--   If you need to call 'insert' from multiple threads, use a different
--   'SQL.Connection' on each thread.
insert
  :: ToJSON a
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table to insert the new row.
  -> a -- ^ Data for the new row.
  -> IO (Id a) -- ^ Identifier of the new row.
insert conn table x = do
  let query = "INSERT INTO `" ++ tableName table ++ "` (data) VALUES (?)"
  _ <- SQL.execute conn (fromString query) $ SQL.Only $ AsJSON x
  Id <$> SQL.insertID conn

-- | Lookup a row in a table.
lookup
  :: (Typeable a, FromJSON a)
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table for lookup.
  -> Id a -- ^ Identifier to use for the table lookup.
  -> IO (Maybe a)
lookup conn table i = do
  let query = "SELECT data FROM `" ++ tableName table ++ "` WHERE id=?"
  fmap (asJSON . SQL.fromOnly) . listToMaybe <$> SQL.query conn (fromString query) (SQL.Only i)

-- | Update a row by applying the supplied function. If the row doesn't exist,
--   it does nothing.
adjust
  :: (Typeable a, FromJSON a, ToJSON a)
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a
  -> (a -> IO a) -- ^ Update function.
  -> Id a
  -> IO ()
adjust conn table f i = SQL.withTransaction conn $ do
  let query1 = "SELECT data FROM `" ++ tableName table ++ "` WHERE id=? FOR SHARE"
  mr <- listToMaybe <$> SQL.query conn (fromString query1) (SQL.Only i)
  forM_ mr $ \(SQL.Only (AsJSON x)) -> do
    y <- f x
    let query2 = "UPDATE `" ++ tableName table ++ "` SET data=? WHERE id=?"
    _ <- SQL.execute conn (fromString query2) (AsJSON y,i)
    pure ()

-- | Replace the current value of a row. It does nothing if the row doesn't exist.
replace
  :: ToJSON a
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a
  -> Id a -- ^ Row identifier.
  -> a -- ^ New value.
  -> IO ()
replace conn table i x = do
  let query = "UPDATE `" ++ tableName table ++ "` SET data=? WHERE id=?"
  _ <- SQL.execute conn (fromString query) (AsJSON x,i)
  pure ()

-- | Delete a row from a table. It does nothing if the row doesn't exist.
delete
  :: SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table to delete the row from.
  -> Id a -- ^ Identifier of the row to delete.
  -> IO ()
delete conn table i = do
  let query = "DELETE FROM `" ++ tableName table ++ "` WHERE id=?"
  _ <- SQL.execute conn (fromString query) $ SQL.Only i
  pure ()

-- | Stream all rows using a conduit.
sourceRows
  :: (Typeable a, FromJSON a)
  => SQL.Connection -- ^ MySQL database connection.
  -> JSONTable a -- ^ Table to stream rows from.
  -> ConduitT i (Row a) (ResourceT IO) ()
sourceRows conn table = do
  let query = "SELECT * FROM `" ++ tableName table ++ "`"
  liftIO $ SQLBase.query conn $ fromString query
  Conduit.bracketP (SQLBase.useResult conn) SQLBase.freeResult $ \result -> do
    fields <- liftIO $ do
      ncols <- SQLBase.fieldCount $ Right result
      when (ncols == 0) $ fail "Query error: Result has no columns."
      SQLBase.fetchFields result
    let loop = do
          row <- liftIO $ SQLBase.fetchRow result
          unless (null row) $ do
            let (i,AsJSON x) = SQL.convertResults fields row
            Conduit.yield $ Row i x
            loop
    loop

-- | Lookup key in an 'Id' table.
newtype Key key = Key key deriving (SQL.FromField, SQL.ToField)

instance (Typeable key, SQL.FromField key) => SQL.Result (Key key)
instance SQL.ToField key => SQL.Param (Key key)

-- | Table that stores a map from keys to row identifiers from
--   some 'JSONTable'. It has the following shape:
--
-- +------------------------+----------------------------+
-- |          key           |            id              |
-- +========================+============================+
-- | User-provided key type | Row identifier (type 'Id') |
-- +------------------------+----------------------------+
--
data IdTable key a = IdTable
  { -- | Table name.
    idTableName :: String
    }

typeToSpec :: SQLBase.Type -> String
typeToSpec SQLBase.Tiny = "TINYINT"
typeToSpec SQLBase.Short = "SMALLINT"
typeToSpec SQLBase.Int24 = "MEDIUMINT"
typeToSpec SQLBase.Long = "INT"
typeToSpec SQLBase.LongLong = "BIGINT"
typeToSpec SQLBase.NewDate = "DATE"
typeToSpec SQLBase.NewDecimal = "DECIMAL"
typeToSpec SQLBase.VarChar = "VARCHAR(255)"
typeToSpec t = fmap toUpper $ show t

idTableSpecs :: forall proxy key . SQL.FromField key => proxy key -> String
idTableSpecs _ = concat
  [ "("
  , "`key` " ++ typeToSpec (head $ fst (SQL.fromField @key)) ++ " NOT NULL PRIMARY KEY"
  , ", "
  , "id BIGINT UNSIGNED NOT NULL"
  , ") ENGINE=InnoDB"
    ]

-- | Create a new Id table in a MySQL database.
--
--   The type of the @key@ column will be set to the first type listed in
--   'SQL.fromField'.
createIdTable
  :: forall key a
   . SQL.FromField key
  => SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table already exists.
  -> String -- ^ Table name.
  -> IO (IdTable key a)
createIdTable conn failIfExists name = do
  let ifNotExists = if failIfExists then " " else " IF NOT EXISTS "
      query = "CREATE TABLE" ++ ifNotExists ++ "`" ++ name ++ "` " ++ idTableSpecs (Proxy @key)
  _ <- SQL.execute conn (fromString query) ()
  pure $ IdTable
    { idTableName = name
      }

-- | Delete an Id table from a MySQL database, together with all of its content.
deleteIdTable
  :: SQL.Connection -- ^ MySQL database connection.
  -> Bool -- ^ Fail if table doesn't exist.
  -> IdTable key a 
  -> IO ()
deleteIdTable conn failIfNotExist itable = do
  let ifExists = if failIfNotExist then " " else " IF EXISTS "
      query = "DROP TABLE" ++ ifExists ++ "`" ++ idTableName itable ++ "`"
  _ <- SQL.execute conn (fromString query) ()
  pure ()

-- | Insert a new Id into an Id table.
insertId
  :: SQL.ToField key
  => SQL.Connection
  -> IdTable key a
  -> key
  -> Id a
  -> IO ()
insertId conn itable k i = do
  let query = "INSERT INTO `" ++ idTableName itable ++ "` (`key`,id) VALUES (?,?)"
  _ <- SQL.execute conn (fromString query) (Key k, i)
  pure ()

-- | Id table lookup.
lookupId
  :: (SQL.ToField key, Typeable a)
  => SQL.Connection
  -> IdTable key a
  -> key
  -> IO (Maybe (Id a))
lookupId conn itable k = do
  let query = "SELECT id FROM `" ++ idTableName itable ++ "` WHERE `key`=?"
  fmap SQL.fromOnly . listToMaybe <$> SQL.query conn (fromString query) (SQL.Only $ Key k)

-- | Update an 'Id' by applying the supplied function. If the key is not found,
--   it does nothing.
adjustId
  :: (SQL.ToField key, Typeable a)
  => SQL.Connection -- ^ MySQL database connection.
  -> IdTable key a
  -> (Id a -> IO (Id a)) -- ^ Update function.
  -> key
  -> IO ()
adjustId conn itable f k = SQL.withTransaction conn $ do
  let query1 = "SELECT id FROM `" ++ idTableName itable ++ "` WHERE `key`=? FOR SHARE"
  mr <- listToMaybe <$> SQL.query conn (fromString query1) (SQL.Only $ Key k)
  forM_ mr $ \(SQL.Only i) -> do
    j <- f i
    let query2 = "UPDATE `" ++ idTableName itable ++ "` SET id=? WHERE `key`=?"
    _ <- SQL.execute conn (fromString query2) (j,Key k)
    pure ()

-- | Alter an 'Id' by applying the supplied function, either inserting it, removing
--   it, or updating it.
alterId
  :: (SQL.ToField key, Typeable a)
  => SQL.Connection -- ^ MySQL database connection.
  -> IdTable key a
  -> (Maybe (Id a) -> IO (Maybe (Id a))) -- ^ Update function.
  -> key
  -> IO ()
alterId conn itable f k = SQL.withTransaction conn $ do
  let query1 = "SELECT id FROM `" ++ idTableName itable ++ "` WHERE `key`=? FOR SHARE"
  mi <- fmap SQL.fromOnly . listToMaybe <$> SQL.query conn (fromString query1) (SQL.Only $ Key k)
  case mi of
    Nothing -> do
      mj <- f mi
      case mj of
        Nothing -> pure ()
        Just j -> do
          let query2 = "INSERT INTO `" ++ idTableName itable ++ "` (`key`,id) VALUES (?,?)"
          _ <- SQL.execute conn (fromString query2) (Key k,j)
          pure ()
    _ -> do
      mj <- f mi
      case mj of
        Nothing -> do
          let query2 = "DELETE FROM `" ++ idTableName itable ++ "` WHERE `key`=?"
          _ <- SQL.execute conn (fromString query2) $ SQL.Only $ Key k
          pure ()
        Just j -> do
          let query2 = "UPDATE `" ++ idTableName itable ++ "` SET id=? WHERE `key`=?"
          _ <- SQL.execute conn (fromString query2) (j,Key k)
          pure ()

-- | Delete an Id from and Id table. It does nothing if the key is not found.
deleteId
  :: SQL.ToField key
  => SQL.Connection
  -> IdTable key a
  -> key
  -> IO ()
deleteId conn itable k = do
  let query = "DELETE FROM `" ++ idTableName itable ++ "` WHERE `key`=?"
  _ <- SQL.execute conn (fromString query) $ SQL.Only $ Key k
  pure ()

-- | Replace the 'Id' associated to the given key. It does nothing if the key
--   isn't found.
replaceId
  :: SQL.ToField key
  => SQL.Connection
  -> IdTable key a
  -> key
  -> Id a
  -> IO ()
replaceId conn itable k i = do
  let query = "UPDATE `" ++ idTableName itable ++ "` SET id=? WHERE `key`=?"
  _ <- SQL.execute conn (fromString query) (i,Key k)
  pure ()

-- | Move an Id from one key to another. This fails if the original key
--   doesn't exist or the target key already exists.
moveId
  :: (SQL.ToField key, Typeable a)
  => SQL.Connection
  -> IdTable key a
  -> key -- ^ Original key
  -> key -- ^ New key
  -> IO ()
moveId conn itable k k' = do
  mi <- lookupId conn itable k
  case mi of
    Just i -> SQL.withTransaction conn $ do
      deleteId conn itable k
      insertId conn itable k' i
    Nothing -> fail $ "Key not found: " ++ show (SQL.toField k) ++ "."

-- | Stream all ids using a conduit.
sourceIds
  :: (Typeable key, SQL.FromField key, Typeable a)
  => SQL.Connection -- ^ MySQL database connection.
  -> IdTable key a -- ^ Table to stream ids from.
  -> ConduitT i (key, Id a) (ResourceT IO) ()
sourceIds conn itable = do
  let query = "SELECT * FROM `" ++ idTableName itable ++ "`"
  liftIO $ SQLBase.query conn $ fromString query
  Conduit.bracketP (SQLBase.useResult conn) SQLBase.freeResult $ \result -> do
    fields <- liftIO $ do
      ncols <- SQLBase.fieldCount $ Right result
      when (ncols == 0) $ fail "Query error: Result has no columns."
      SQLBase.fetchFields result
    let loop = do
          row <- liftIO $ SQLBase.fetchRow result
          unless (null row) $ do
            let (Key k,i) = SQL.convertResults fields row
            Conduit.yield (k,i)
            loop
    loop
