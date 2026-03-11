{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Database.CQL.Protocol.Response
    ( Response            (..)
    , warnings
    , traceId
    , unpack

      -- ** Ready
    , Ready               (..)
    , decodeReady

      -- ** Authenticate
    , Authenticate        (..)
    , AuthChallenge       (..)
    , AuthSuccess         (..)
    , decodeAuthenticate
    , decodeAuthChallenge
    , decodeAuthSuccess

      -- ** Result
    , Result              (..)
    , MetaData            (..)
    , ColumnSpec          (..)
    , decodeResult
    , decodeMetaData

      -- ** Supported
    , Supported           (..)
    , decodeSupported

      -- ** Event
    , Event               (..)
    , TopologyChange      (..)
    , SchemaChange        (..)
    , StatusChange        (..)
    , Change              (..)
    , decodeSchemaChange
    , decodeChange
    , decodeEvent
    , decodeTopologyChange
    , decodeStatusChange

      -- ** Error
    , Error               (..)
    , WriteType           (..)
    , decodeError
    , decodeWriteType
    ) where

import Control.Applicative
import Control.Exception (Exception)
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.Int
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Serialize hiding (Result)
import Data.Typeable
import Data.UUID (UUID)
import Data.Word
import Database.CQL.Protocol.Tuple
import Database.CQL.Protocol.Codec
import Database.CQL.Protocol.Types
import Database.CQL.Protocol.Header
import Network.Socket (SockAddr)
import Prelude

import qualified Data.ByteString.Lazy as LB

------------------------------------------------------------------------------
-- Response

-- | The type corresponding to the protocol response frame.
--
-- The type parameter 'k' denotes the kind of response. It is present to allow
-- distinguishing read operations from write operations. Use 'R' for read,
-- 'W' for write and 'S' for schema related operations.
--
-- 'a' represents the argument type and 'b' the return type of this
-- response.
data Response k a b
    = RsError         (Maybe UUID) [Text] !Error
    | RsReady         (Maybe UUID) [Text] !Ready
    | RsAuthenticate  (Maybe UUID) [Text] !Authenticate
    | RsAuthChallenge (Maybe UUID) [Text] !AuthChallenge
    | RsAuthSuccess   (Maybe UUID) [Text] !AuthSuccess
    | RsSupported     (Maybe UUID) [Text] !Supported
    | RsResult        (Maybe UUID) [Text] !(Result k a b)
    | RsEvent         (Maybe UUID) [Text] !Event
    deriving Show

-- | Get server warnings from response if any.
warnings :: Response k a b -> [Text]
warnings (RsError         _ w _) = w
warnings (RsReady         _ w _) = w
warnings (RsAuthenticate  _ w _) = w
warnings (RsAuthChallenge _ w _) = w
warnings (RsAuthSuccess   _ w _) = w
warnings (RsSupported     _ w _) = w
warnings (RsResult        _ w _) = w
warnings (RsEvent         _ w _) = w

-- | Get server trace ID from response if any.
traceId :: Response k a b -> Maybe UUID
traceId (RsError         x _ _) = x
traceId (RsReady         x _ _) = x
traceId (RsAuthenticate  x _ _) = x
traceId (RsAuthChallenge x _ _) = x
traceId (RsAuthSuccess   x _ _) = x
traceId (RsSupported     x _ _) = x
traceId (RsResult        x _ _) = x
traceId (RsEvent         x _ _) = x

-- | Deserialise a 'Response' from the given 'ByteString'.
unpack :: (Tuple a, Tuple b)
       => Compression
       -> Header
       -> LB.ByteString
       -> Either String (Response k a b)
unpack c h b = do
    let f = flags h
    let v = version h
    x <- if compress `isSet` f then deflate c b else return b
    flip runGetLazy x $ do
        t <- if tracing `isSet` f then Just <$> decodeUUID else return Nothing
        w <- if warning `isSet` f then decodeList else return []
        message v t w (opCode h)
  where
    message _ t w OcError         = RsError         t w <$> decodeError
    message _ t w OcReady         = RsReady         t w <$> decodeReady
    message _ t w OcAuthenticate  = RsAuthenticate  t w <$> decodeAuthenticate
    message _ t w OcSupported     = RsSupported     t w <$> decodeSupported
    message v t w OcResult        = RsResult        t w <$> decodeResult v
    message v t w OcEvent         = RsEvent         t w <$> decodeEvent v
    message _ t w OcAuthChallenge = RsAuthChallenge t w <$> decodeAuthChallenge
    message _ t w OcAuthSuccess   = RsAuthSuccess   t w <$> decodeAuthSuccess
    message _ _ _ other           = fail $ "decode-response: unknown: " ++ show other

    deflate f x  = maybe deflateError return (expand f x)
    deflateError = Left "unpack: decompression failure"

------------------------------------------------------------------------------
-- AUTHENTICATE

-- | The server requires authentication.
newtype Authenticate = Authenticate Text deriving Show

decodeAuthenticate :: Get Authenticate
decodeAuthenticate = Authenticate <$> decodeString

------------------------------------------------------------------------------
-- AUTH_CHALLENGE

-- | A server-send authentication challenge.
newtype AuthChallenge = AuthChallenge (Maybe LB.ByteString) deriving Show

decodeAuthChallenge :: Get AuthChallenge
decodeAuthChallenge = AuthChallenge <$> decodeBytes

------------------------------------------------------------------------------
-- AUTH_SUCCESS

-- | Indicates the success of an authentication phase.
newtype AuthSuccess = AuthSuccess (Maybe LB.ByteString) deriving Show

decodeAuthSuccess :: Get AuthSuccess
decodeAuthSuccess = AuthSuccess <$> decodeBytes

------------------------------------------------------------------------------
-- READY

-- | The server is ready to process queries. Response of a 'Startup'
-- request.
data Ready = Ready deriving Show

decodeReady :: Get Ready
decodeReady = return Ready

------------------------------------------------------------------------------
-- SUPPORTED

-- | The startup options supported by the server. Response of an 'Options'
-- request.
data Supported = Supported [CompressionAlgorithm] [CqlVersion] deriving Show

decodeSupported :: Get Supported
decodeSupported = do
    opt <- decodeMultiMap
    cmp <- mapM toCompression . fromMaybe [] $ lookup "COMPRESSION" opt
    let v = map toVersion . fromMaybe [] $ lookup "CQL_VERSION" opt
    return $ Supported cmp v
  where
    toCompression "snappy" = return Snappy
    toCompression "lz4"    = return LZ4
    toCompression other    = fail $
        "decode-supported: unknown compression: " ++ show other

    toVersion "3.0.0" = Cqlv300
    toVersion other   = CqlVersion other

------------------------------------------------------------------------------
-- RESULT

-- | Query response.
data Result k a b
    = VoidResult
    | RowsResult         !MetaData [b]
    | SetKeyspaceResult  !Keyspace
    | PreparedResult     !(QueryId k a b) !MetaData !MetaData
    | SchemaChangeResult !SchemaChange
    deriving (Show)

-- | Part of a @RowsResult@. Describes the result set.
data MetaData = MetaData
    { columnCount       :: !Int32
    , pagingState       :: Maybe PagingState
    , columnSpecs       :: [ColumnSpec]
    , primaryKeyIndices :: [Int32]
    } deriving (Show)

-- | The column specification. Part of 'MetaData' unless 'skipMetaData' in
-- 'QueryParams' was True.
data ColumnSpec = ColumnSpec
    { keyspace   :: !Keyspace
    , table      :: !Table
    , columnName :: !Text
    , columnType :: !ColumnType
    } deriving (Show)

decodeResult :: forall k a b. (Tuple a, Tuple b) => Version -> Get (Result k a b)
decodeResult v = decodeInt >>= go
  where
    go 0x1 = return VoidResult
    go 0x2 = do
        m <- decodeMetaData
        n <- decodeInt
        let c = untag (count :: Tagged b Int)
        unless (c == -1 || columnCount m == fromIntegral c) $
            fail $ "column count: "
                ++ show (columnCount m)
                ++ " =/= "
                ++ show c
        let typecheck = untag (check :: Tagged b ([ColumnType] -> [ColumnType]))
        let ctypes    = map columnType (columnSpecs m)
        let expected  = typecheck ctypes
        let message   = "expected: " ++ show expected ++ ", but got " ++ show ctypes
        unless (null expected) $
            fail $ "column-type error: " ++ message
        RowsResult m <$> replicateM (fromIntegral n) (tuple v ctypes)
    go 0x3 = SetKeyspaceResult <$> decodeKeyspace
    go 0x4 = if v == V4
                then PreparedResult <$> decodeQueryId <*> decodePreparedV4 <*> decodeMetaData
                else PreparedResult <$> decodeQueryId <*> decodeMetaData <*> decodeMetaData
    go 0x5 = SchemaChangeResult <$> decodeSchemaChange v
    go int = fail $ "decode-result: unknown: " ++ show int

decodeMetaData :: Get MetaData
decodeMetaData = do
    f <- decodeInt
    n <- decodeInt
    p <- if hasMorePages f then decodePagingState else return Nothing
    if hasNoMetaData f
        then return $ MetaData n p [] []
        else MetaData n p <$> decodeSpecs n (hasGlobalSpec f) <*> pure []
  where
    hasGlobalSpec f = f `testBit` 0
    hasMorePages  f = f `testBit` 1
    hasNoMetaData f = f `testBit` 2

    decodeSpecs n True = do
        k <- decodeKeyspace
        t <- decodeTable
        replicateM (fromIntegral n) $ ColumnSpec k t
            <$> decodeString
            <*> decodeColumnType

    decodeSpecs n False =
        replicateM (fromIntegral n) $ ColumnSpec
            <$> decodeKeyspace
            <*> decodeTable
            <*> decodeString
            <*> decodeColumnType

decodePreparedV4 :: Get MetaData
decodePreparedV4 = do
    f <- decodeInt
    n <- decodeInt
    pkCount <- decodeInt
    pkis <- replicateM (fromIntegral pkCount) decodeShort
    specs <- decodeSpecs n (hasGlobalSpec f)
    return $ MetaData n Nothing specs (fromIntegral <$> pkis)
  where
    hasGlobalSpec f = f `testBit` 0

    decodeSpecs n True = do
        k <- decodeKeyspace
        t <- decodeTable
        replicateM (fromIntegral n) $ ColumnSpec k t
            <$> decodeString
            <*> decodeColumnType

    decodeSpecs n False =
        replicateM (fromIntegral n) $ ColumnSpec
            <$> decodeKeyspace
            <*> decodeTable
            <*> decodeString
            <*> decodeColumnType

------------------------------------------------------------------------------
-- SCHEMA_CHANGE

data SchemaChange
    = SchemaCreated !Change
    | SchemaUpdated !Change
    | SchemaDropped !Change
    deriving Show

data Change
    = KeyspaceChange  !Keyspace
    | TableChange     !Keyspace !Table
    | TypeChange      !Keyspace !Text
    | FunctionChange  !Keyspace !Text ![Text]
    | AggregateChange !Keyspace !Text ![Text]
    deriving Show

decodeSchemaChange :: Version -> Get SchemaChange
decodeSchemaChange v = decodeString >>= fromString
  where
    fromString "CREATED" = SchemaCreated <$> decodeChange v
    fromString "UPDATED" = SchemaUpdated <$> decodeChange v
    fromString "DROPPED" = SchemaDropped <$> decodeChange v
    fromString other     = fail $ "decode-schema-change: unknown: " ++ show other

decodeChange :: Version -> Get Change
decodeChange V4 = decodeString >>= fromString
  where
    fromString "KEYSPACE"  = KeyspaceChange  <$> decodeKeyspace
    fromString "TABLE"     = TableChange     <$> decodeKeyspace <*> decodeTable
    fromString "TYPE"      = TypeChange      <$> decodeKeyspace <*> decodeString
    fromString "FUNCTION"  = FunctionChange  <$> decodeKeyspace <*> decodeString <*> decodeList
    fromString "AGGREGATE" = AggregateChange <$> decodeKeyspace <*> decodeString <*> decodeList
    fromString other      = fail $ "decode-change V4: unknown: " ++ show other
decodeChange V3 = decodeString >>= fromString
  where
    fromString "KEYSPACE" = KeyspaceChange <$> decodeKeyspace
    fromString "TABLE"    = TableChange    <$> decodeKeyspace <*> decodeTable
    fromString "TYPE"     = TypeChange     <$> decodeKeyspace <*> decodeString
    fromString other      = fail $ "decode-change V3: unknown: " ++ show other

------------------------------------------------------------------------------
-- EVENT

-- | Messages send by the server without request, if the connection has
-- been 'Register'ed to receive such events.
data Event
    = TopologyEvent !TopologyChange !SockAddr
    | StatusEvent   !StatusChange   !SockAddr
    | SchemaEvent   !SchemaChange
    deriving Show

data TopologyChange = NewNode | RemovedNode deriving (Eq, Ord, Show)
data StatusChange   = Up | Down deriving (Eq, Ord, Show)

decodeEvent :: Version -> Get Event
decodeEvent v = decodeString >>= decodeByType
  where
    decodeByType "TOPOLOGY_CHANGE" = TopologyEvent <$> decodeTopologyChange <*> decodeSockAddr
    decodeByType "STATUS_CHANGE"   = StatusEvent   <$> decodeStatusChange <*> decodeSockAddr
    decodeByType "SCHEMA_CHANGE"   = SchemaEvent   <$> decodeSchemaChange v
    decodeByType other             = fail $ "decode-event: unknown: " ++ show other

decodeTopologyChange :: Get TopologyChange
decodeTopologyChange = decodeString >>= fromString
  where
    fromString "NEW_NODE"     = return NewNode
    fromString "REMOVED_NODE" = return RemovedNode
    fromString other          = fail $
        "decode-topology: unknown: "  ++ show other

decodeStatusChange :: Get StatusChange
decodeStatusChange = decodeString >>= fromString
  where
    fromString "UP"   = return Up
    fromString "DOWN" = return Down
    fromString other  = fail $
        "decode-status-change: unknown: " ++ show other

-----------------------------------------------------------------------------
-- ERROR

-- | Error response.
data Error
    = AlreadyExists   !Text !Keyspace !Table
    | BadCredentials  !Text
    | ConfigError     !Text
    | FunctionFailure !Text !Keyspace !Text ![Text]
    | Invalid         !Text
    | IsBootstrapping !Text
    | Overloaded      !Text
    | ProtocolError   !Text
    | ServerError     !Text
    | SyntaxError     !Text
    | TruncateError   !Text
    | Unauthorized    !Text
    | Unprepared      !Text !ByteString
    | Unavailable
        { unavailMessage     :: !Text
        , unavailConsistency :: !Consistency
        , unavailNumRequired :: !Int32
        , unavailNumAlive    :: !Int32
        }
    | ReadFailure
        { rFailureMessage     :: !Text
        , rFailureConsistency :: !Consistency
        , rFailureNumAck      :: !Int32
        , rFailureNumRequired :: !Int32
        , rFailureNumFailures :: !Int32
        , rFailureDataPresent :: !Bool
        }

    | ReadTimeout
        { rTimeoutMessage     :: !Text
        , rTimeoutConsistency :: !Consistency
        , rTimeoutNumAck      :: !Int32
        , rTimeoutNumRequired :: !Int32
        , rTimeoutDataPresent :: !Bool
        }
    | WriteFailure
        { wFailureMessage     :: !Text
        , wFailureConsistency :: !Consistency
        , wFailureNumAck      :: !Int32
        , wFailureNumRequired :: !Int32
        , wFailureNumFailures :: !Int32
        , wFailureWriteType   :: !WriteType
        }
    | WriteTimeout
        { wTimeoutMessage     :: !Text
        , wTimeoutConsistency :: !Consistency
        , wTimeoutNumAck      :: !Int32
        , wTimeoutNumRequired :: !Int32
        , wTimeoutWriteType   :: !WriteType
        }

    deriving (Eq, Show, Typeable)

instance Exception Error

data WriteType
    = WriteSimple
    | WriteBatch
    | WriteBatchLog
    | WriteUnloggedBatch
    | WriteCounter
    | WriteCas
    | WriteView
        -- ^ Since 'V4'.
    | WriteCdc
        -- ^ Since 'V4'.
    deriving (Eq, Show)

decodeError :: Get Error
decodeError = do
    code <- decodeInt
    msg  <- decodeString
    toError code msg
  where
    toError :: Int32 -> Text -> Get Error
    toError 0x0000 m = return $ ServerError m
    toError 0x000A m = return $ ProtocolError m
    toError 0x0100 m = return $ BadCredentials m
    toError 0x1000 m = Unavailable m <$> decodeConsistency <*> decodeInt <*> decodeInt
    toError 0x1001 m = return $ Overloaded m
    toError 0x1002 m = return $ IsBootstrapping m
    toError 0x1003 m = return $ TruncateError m
    toError 0x1100 m = WriteTimeout m
        <$> decodeConsistency
        <*> decodeInt
        <*> decodeInt
        <*> decodeWriteType
    toError 0x1200 m = ReadTimeout m
        <$> decodeConsistency
        <*> decodeInt
        <*> decodeInt
        <*> (bool <$> decodeByte)
    toError 0x1300 m = ReadFailure m
        <$> decodeConsistency
        <*> decodeInt
        <*> decodeInt
        <*> decodeInt
        <*> (bool <$> decodeByte)
    toError 0x1400 m = FunctionFailure m
        <$> decodeKeyspace
        <*> decodeString
        <*> decodeList
    toError 0x1500 m = WriteFailure m
        <$> decodeConsistency
        <*> decodeInt
        <*> decodeInt
        <*> decodeInt
        <*> decodeWriteType
    toError 0x2000 m = return $ SyntaxError m
    toError 0x2100 m = return $ Unauthorized m
    toError 0x2200 m = return $ Invalid m
    toError 0x2300 m = return $ ConfigError m
    toError 0x2400 m = AlreadyExists m <$> decodeKeyspace <*> decodeTable
    toError 0x2500 m = Unprepared m <$> decodeShortBytes
    toError code _   = fail $ "decode-error: unknown: " ++ show code

    bool :: Word8 -> Bool
    bool 0 = False
    bool _ = True

decodeWriteType :: Get WriteType
decodeWriteType = decodeString >>= fromString
  where
    fromString "SIMPLE"          = return WriteSimple
    fromString "BATCH"           = return WriteBatch
    fromString "BATCH_LOG"       = return WriteBatchLog
    fromString "UNLOGGED_BATCH"  = return WriteUnloggedBatch
    fromString "COUNTER"         = return WriteCounter
    fromString "CAS"             = return WriteCas
    fromString "VIEW"            = return WriteView
    fromString "CDC"             = return WriteCdc
    fromString unknown           = fail $
        "decode: unknown write-type: " ++ show unknown
