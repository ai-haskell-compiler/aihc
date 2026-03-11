{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving  #-}

module Database.CQL.Protocol.Request
    ( Request           (..)
    , pack
    , encodeRequest
    , getOpCode

      -- ** Options
    , Options           (..)
    , encodeOptions

      -- ** Startup
    , Startup           (..)
    , encodeStartup

      -- ** Auth Response
    , AuthResponse      (..)
    , encodeAuthResponse

      -- ** Register
    , Register          (..)
    , EventType         (..)
    , encodeRegister
    , encodeEventType

      -- ** Query
    , Query             (..)
    , QueryParams       (..)
    , SerialConsistency (..)
    , encodeQuery
    , encodeQueryParams

      -- ** Batch
    , Batch             (..)
    , BatchQuery        (..)
    , BatchType         (..)
    , encodeBatch
    , encodeBatchType
    , encodeBatchQuery

      -- ** Prepare
    , Prepare           (..)
    , encodePrepare

      -- ** Execute
    , Execute           (..)
    , encodeExecute
    ) where

import Control.Applicative
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Foldable (traverse_)
import Data.Int
import Data.Text (Text)
import Data.Maybe (isJust)
import Data.Monoid
import Data.Serialize hiding (decode, encode)
import Data.Word
import Database.CQL.Protocol.Tuple
import Database.CQL.Protocol.Codec
import Database.CQL.Protocol.Types
import Database.CQL.Protocol.Header
import Prelude

import qualified Data.ByteString.Lazy as LB

------------------------------------------------------------------------------
-- Request

-- | The type corresponding to the protocol request frame.
--
-- The type parameter 'k' denotes the kind of request. It is present to allow
-- distinguishing read operations from write operations. Use 'R' for read,
-- 'W' for write and 'S' for schema related operations.
--
-- 'a' represents the argument type and 'b' the return type of this request.
data Request k a b
    = RqStartup  !Startup
    | RqOptions  !Options
    | RqRegister !Register
    | RqBatch    !Batch
    | RqAuthResp !AuthResponse
    | RqPrepare  !(Prepare k a b)
    | RqQuery    !(Query k a b)
    | RqExecute  !(Execute k a b)
    deriving Show

encodeRequest :: Tuple a => Version -> Putter (Request k a b)
encodeRequest _ (RqStartup  r) = encodeStartup r
encodeRequest _ (RqOptions  r) = encodeOptions r
encodeRequest _ (RqRegister r) = encodeRegister r
encodeRequest v (RqBatch    r) = encodeBatch v r
encodeRequest _ (RqAuthResp r) = encodeAuthResponse r
encodeRequest _ (RqPrepare  r) = encodePrepare r
encodeRequest v (RqQuery    r) = encodeQuery v r
encodeRequest v (RqExecute  r) = encodeExecute v r

-- | Serialise the given request, optionally using compression.
-- The result is either an error description in case of failure or a binary
-- protocol frame, including 'Header', 'Length' and body.
pack :: Tuple a
     => Version       -- ^ protocol version, which determines the encoding
     -> Compression   -- ^ compression to use
     -> Bool          -- ^ enable/disable tracing
     -> StreamId      -- ^ the stream Id to use
     -> Request k a b -- ^ the actual request to serialise
     -> Either String ByteString
pack v c t i r = do
    body <- runCompression c (runPutLazy $ encodeRequest v r)
    let len = Length . fromIntegral $ LB.length body
    return . runPutLazy $ do
        encodeHeader v RqHeader mkFlags i (getOpCode r) len
        putLazyByteString body
  where
    runCompression f x = maybe compressError return (shrink f $ x)
    compressError      = Left "pack: compression failure"

    mkFlags = (if t then tracing else mempty)
        <> (if algorithm c /= None then compress else mempty)

-- | Get the protocol 'OpCode' corresponding to the given 'Request'.
getOpCode :: Request k a b -> OpCode
getOpCode (RqQuery _)    = OcQuery
getOpCode (RqExecute _)  = OcExecute
getOpCode (RqPrepare _)  = OcPrepare
getOpCode (RqBatch _)    = OcBatch
getOpCode (RqRegister _) = OcRegister
getOpCode (RqOptions _)  = OcOptions
getOpCode (RqStartup _)  = OcStartup
getOpCode (RqAuthResp _) = OcAuthResponse

------------------------------------------------------------------------------
-- STARTUP

-- | A startup request which is used when initialising a connection to the
-- server. It specifies the CQL version to use and optionally the
-- compression algorithm.
data Startup = Startup !CqlVersion !CompressionAlgorithm deriving Show

encodeStartup :: Putter Startup
encodeStartup (Startup v c) =
    encodeMap $ ("CQL_VERSION", mapVersion v) : mapCompression c
  where
    mapVersion :: CqlVersion -> Text
    mapVersion Cqlv300        = "3.0.0"
    mapVersion (CqlVersion s) = s

    mapCompression :: CompressionAlgorithm -> [(Text, Text)]
    mapCompression Snappy = [("COMPRESSION", "snappy")]
    mapCompression LZ4    = [("COMPRESSION", "lz4")]
    mapCompression None   = []

------------------------------------------------------------------------------
-- AUTH_RESPONSE

-- | A request send in response to a previous authentication challenge.
newtype AuthResponse = AuthResponse LB.ByteString deriving Show

encodeAuthResponse :: Putter AuthResponse
encodeAuthResponse (AuthResponse b) = encodeBytes b

------------------------------------------------------------------------------
-- OPTIONS

-- | An options request, send prior to 'Startup' to request the server's
-- startup options.
data Options = Options deriving Show

encodeOptions :: Putter Options
encodeOptions _ = return ()

------------------------------------------------------------------------------
-- QUERY

-- | A CQL query (select, insert, etc.).
data Query k a b = Query !(QueryString k a b) !(QueryParams a) deriving Show

encodeQuery :: Tuple a => Version -> Putter (Query k a b)
encodeQuery v (Query (QueryString s) p) =
    encodeLongString s >> encodeQueryParams v p

------------------------------------------------------------------------------
-- EXECUTE

-- | Executes a prepared query.
data Execute k a b = Execute !(QueryId k a b) !(QueryParams a) deriving Show

encodeExecute :: Tuple a => Version -> Putter (Execute k a b)
encodeExecute v (Execute (QueryId q) p) =
    encodeShortBytes q >> encodeQueryParams v p

------------------------------------------------------------------------------
-- PREPARE

-- | Prepare a query for later execution (cf. 'Execute').
newtype Prepare k a b = Prepare (QueryString k a b) deriving Show

encodePrepare :: Putter (Prepare k a b)
encodePrepare (Prepare (QueryString p)) = encodeLongString p

------------------------------------------------------------------------------
-- REGISTER

-- | Register's the connection this request is made through, to receive
-- server events.
newtype Register = Register [EventType] deriving Show

encodeRegister :: Putter Register
encodeRegister (Register t) = do
    encodeShort (fromIntegral (length t))
    mapM_ encodeEventType t

-- | Event types to register.
data EventType
    = TopologyChangeEvent -- ^ events related to change in the cluster topology
    | StatusChangeEvent   -- ^ events related to change of node status
    | SchemaChangeEvent   -- ^ events related to schema change
    deriving Show

encodeEventType :: Putter EventType
encodeEventType TopologyChangeEvent = encodeString "TOPOLOGY_CHANGE"
encodeEventType StatusChangeEvent   = encodeString "STATUS_CHANGE"
encodeEventType SchemaChangeEvent   = encodeString "SCHEMA_CHANGE"

------------------------------------------------------------------------------
-- BATCH

-- | Allows executing a list of queries (prepared or not) as a batch.
data Batch = Batch
    { batchType              :: !BatchType
    , batchQuery             :: [BatchQuery]
    , batchConsistency       :: !Consistency
    , batchSerialConsistency :: Maybe SerialConsistency
    } deriving Show

data BatchType
    = BatchLogged   -- ^ default, uses a batch log for atomic application
    | BatchUnLogged -- ^ skip the batch log
    | BatchCounter  -- ^ used for batched counter updates
    deriving (Show)

encodeBatch :: Version -> Putter Batch
encodeBatch v (Batch t q c s) = do
    encodeBatchType t
    encodeShort (fromIntegral (length q))
    mapM_ (encodeBatchQuery v) q
    encodeConsistency c
    put batchFlags
    traverse_ encodeConsistency (mapCons <$> s)
  where
    batchFlags :: Word8
    batchFlags = if isJust s then 0x10 else 0x0

encodeBatchType :: Putter BatchType
encodeBatchType BatchLogged   = putWord8 0
encodeBatchType BatchUnLogged = putWord8 1
encodeBatchType BatchCounter  = putWord8 2

-- | A GADT to unify queries and prepared queries both of which can be used
-- in batch requests.
data BatchQuery where
    BatchQuery :: (Show a, Tuple a, Tuple b)
               => !(QueryString W a b)
               -> !a
               -> BatchQuery

    BatchPrepared :: (Show a, Tuple a, Tuple b)
                  => !(QueryId W a b)
                  -> !a
                  -> BatchQuery

deriving instance Show BatchQuery

encodeBatchQuery :: Version -> Putter BatchQuery
encodeBatchQuery n (BatchQuery (QueryString q) v) = do
    putWord8 0
    encodeLongString q
    store n v
encodeBatchQuery n (BatchPrepared (QueryId i) v)  = do
    putWord8 1
    encodeShortBytes i
    store n v

------------------------------------------------------------------------------
-- Query Parameters

-- | Query parameters.
data QueryParams a = QueryParams
    { consistency :: !Consistency
        -- ^ (Regular) consistency level to use.
    , skipMetaData :: !Bool
        -- ^ Whether to omit the metadata in the 'Response'
        -- of the query. This is an optimisation only relevant for
        -- use with prepared queries, for which the metadata obtained
        -- from the 'PreparedResult' may be reused.
    , values :: a
        -- ^ The bound parameters of the query.
    , pageSize :: Maybe Int32
        -- ^ The desired maximum result set size.
    , queryPagingState :: Maybe PagingState
        -- ^ The current paging state that determines the "offset"
        -- of the results to return for a read query.
    , serialConsistency :: Maybe SerialConsistency
        -- ^ Serial consistency level to use for conditional updates
        -- (aka "lightweight transactions"). Irrelevant for any other queries.
    , enableTracing :: Maybe Bool
        -- ^ Whether tracing should be enabled for the query, in which case the
        -- 'Response' will carry a 'traceId'.
    } deriving Show

-- | Consistency level for the serial phase of conditional updates
-- (aka "lightweight transactions").
--
-- See: <https://docs.datastax.com/en/cassandra/latest/cassandra/dml/dmlConfigSerialConsistency.html SerialConsistency>
data SerialConsistency
    = SerialConsistency
        -- ^ Default. Quorum-based linearizable consistency.
    | LocalSerialConsistency
        -- ^ Like 'SerialConsistency' except confined to a single (logical)
        -- data center.
    deriving Show

encodeQueryParams :: forall a. Tuple a => Version -> Putter (QueryParams a)
encodeQueryParams v p = do
    encodeConsistency (consistency p)
    put queryFlags
    store v (values p)
    traverse_ encodeInt         (pageSize p)
    traverse_ encodePagingState (queryPagingState p)
    traverse_ encodeConsistency (mapCons <$> serialConsistency p)
  where
    queryFlags :: Word8
    queryFlags =
            (if hasValues                    then 0x01 else 0x0)
        .|. (if skipMetaData p               then 0x02 else 0x0)
        .|. (if isJust (pageSize p)          then 0x04 else 0x0)
        .|. (if isJust (queryPagingState p)  then 0x08 else 0x0)
        .|. (if isJust (serialConsistency p) then 0x10 else 0x0)

    hasValues = untag (count :: Tagged a Int) /= 0

mapCons :: SerialConsistency -> Consistency
mapCons SerialConsistency      = Serial
mapCons LocalSerialConsistency = LocalSerial
