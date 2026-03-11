module Database.CQL.Protocol.Types where

import Data.ByteString (ByteString)
import Data.Text (Text, pack, unpack)
import Data.Decimal
import Data.Int
import Data.IP
import Data.String
import Data.UUID (UUID)

import qualified Data.ByteString.Lazy as LB
import qualified Data.List            as List
import qualified Data.Set             as Set
import qualified Data.Map.Strict      as Map
import qualified Data.Text.Lazy       as LT

newtype Keyspace = Keyspace
    { unKeyspace :: Text } deriving (Eq, Show)

newtype Table = Table
    { unTable :: Text } deriving (Eq, Show)

-- | Opaque token passed to the server to continue result paging.
newtype PagingState = PagingState
    { unPagingState :: LB.ByteString } deriving (Eq, Show)

-- | ID representing a prepared query.
newtype QueryId k a b = QueryId
    { unQueryId :: ByteString } deriving (Eq, Show)

newtype QueryString k a b = QueryString
    { unQueryString :: LT.Text } deriving (Eq, Show)

instance IsString (QueryString k a b) where
    fromString = QueryString . LT.pack

-- | CQL binary protocol version.
data Version
    = V3 -- ^ version 3
    | V4 -- ^ version 4
    deriving (Eq, Ord, Show)

-- | The CQL version (not the binary protocol version).
data CqlVersion
    = Cqlv300
    | CqlVersion !Text
    deriving (Eq, Show)

data CompressionAlgorithm
    = Snappy
    | LZ4
    | None
    deriving (Eq, Show)

data Compression = Compression
    { algorithm :: !CompressionAlgorithm
    , shrink    :: LB.ByteString -> Maybe LB.ByteString
    , expand    :: LB.ByteString -> Maybe LB.ByteString
    }

instance Show Compression where
    show = show . algorithm

noCompression :: Compression
noCompression = Compression None Just Just

-- | Consistency level.
--
-- See: <https://docs.datastax.com/en/cassandra/latest/cassandra/dml/dmlConfigConsistency.html Consistency>
data Consistency
    = Any
    | One
    | LocalOne
    | Two
    | Three
    | Quorum
    | LocalQuorum
    | All
    | EachQuorum  -- ^ Only for write queries.
    | Serial      -- ^ Only for read queries.
    | LocalSerial -- ^ Only for read queries.
    deriving (Eq, Show)

-- | An opcode is a tag to distinguish protocol frame bodies.
data OpCode
    = OcError
    | OcStartup
    | OcReady
    | OcAuthenticate
    | OcOptions
    | OcSupported
    | OcQuery
    | OcResult
    | OcPrepare
    | OcExecute
    | OcRegister
    | OcEvent
    | OcBatch
    | OcAuthChallenge
    | OcAuthResponse
    | OcAuthSuccess
    deriving (Eq, Show)

-- | The type of a single CQL column.
data ColumnType
    = CustomColumn !Text
    | AsciiColumn
    | BigIntColumn
    | BlobColumn
    | BooleanColumn
    | CounterColumn
    | DecimalColumn
    | DoubleColumn
    | FloatColumn
    | IntColumn
    | TextColumn
    | TimestampColumn
    | UuidColumn
    | VarCharColumn
    | VarIntColumn
    | TimeUuidColumn
    | InetColumn
    | MaybeColumn !ColumnType
    | ListColumn  !ColumnType
    | SetColumn   !ColumnType
    | MapColumn   !ColumnType !ColumnType
    | TupleColumn [ColumnType]
    | UdtColumn   !Text [(Text, ColumnType)]
    | DateColumn
    | TimeColumn
    | SmallIntColumn
    | TinyIntColumn
    deriving (Eq)

instance Show ColumnType where
    show (CustomColumn a)  = unpack a
    show AsciiColumn       = "ascii"
    show BigIntColumn      = "bigint"
    show BlobColumn        = "blob"
    show BooleanColumn     = "boolean"
    show CounterColumn     = "counter"
    show DecimalColumn     = "decimal"
    show DoubleColumn      = "double"
    show FloatColumn       = "float"
    show IntColumn         = "int"
    show TextColumn        = "text"
    show TimestampColumn   = "timestamp"
    show UuidColumn        = "uuid"
    show VarCharColumn     = "varchar"
    show VarIntColumn      = "varint"
    show TimeUuidColumn    = "timeuuid"
    show InetColumn        = "inet"
    show DateColumn        = "date"
    show TimeColumn        = "time"
    show SmallIntColumn    = "smallint"
    show TinyIntColumn     = "tinyint"
    show (MaybeColumn a)   = show a ++ "?"
    show (ListColumn a)    = showString "list<" . shows a . showString ">" $ ""
    show (SetColumn a)     = showString "set<" . shows a . showString ">" $ ""
    show (MapColumn a b)   = showString "map<"
                           . shows a
                           . showString ", "
                           . shows b
                           . showString ">"
                           $ ""
    show (TupleColumn a)   = showString "tuple<"
                           . showString (List.intercalate ", " (map show a))
                           . showString ">"
                           $ ""
    show (UdtColumn t f)   = showString (unpack t)
                           . showString "<"
                           . shows (List.intercalate ", " (map show f))
                           . showString ">"
                           $ ""

newtype Ascii    = Ascii    { fromAscii    :: Text          } deriving (Eq, Ord, Show)
newtype Blob     = Blob     { fromBlob     :: LB.ByteString } deriving (Eq, Ord, Show)
newtype Counter  = Counter  { fromCounter  :: Int64         } deriving (Eq, Ord, Show)
newtype TimeUuid = TimeUuid { fromTimeUuid :: UUID          } deriving (Eq, Ord, Show)
newtype Set a    = Set      { fromSet      :: [a]           } deriving Show
newtype Map a b  = Map      { fromMap      :: [(a, b)]      } deriving Show

instance IsString Ascii where
    fromString = Ascii . pack

instance (Eq a, Ord a) => Eq (Set a) where
    a == b = Set.fromList (fromSet a) == Set.fromList (fromSet b)

instance (Eq k, Eq v, Ord k) => Eq (Map k v) where
    a == b = Map.fromList (fromMap a) == Map.fromList (fromMap b)

-- | A CQL value. The various constructors correspond to CQL data-types for
-- individual columns in Cassandra.
data Value
    = CqlCustom    !LB.ByteString
    | CqlBoolean   !Bool
    | CqlInt       !Int32
    | CqlBigInt    !Int64
    | CqlVarInt    !Integer
    | CqlFloat     !Float
    | CqlDecimal   !Decimal
    | CqlDouble    !Double
    | CqlText      !Text
    | CqlInet      !IP
    | CqlUuid      !UUID
    | CqlTimestamp !Int64
    | CqlAscii     !Text
    | CqlBlob      !LB.ByteString
    | CqlCounter   !Int64
    | CqlTimeUuid  !UUID
    | CqlMaybe     (Maybe Value)
    | CqlList      [Value]
    | CqlSet       [Value]
    | CqlMap       [(Value, Value)]
    | CqlTuple     [Value]
    | CqlUdt       [(Text, Value)]
    | CqlDate      !Int32
    | CqlTime      !Int64
    | CqlSmallInt  !Int16
    | CqlTinyInt   !Int8
    deriving (Eq, Show)

-- | Tag some value with a phantom type.
newtype Tagged a b = Tagged { untag :: b }

retag :: Tagged a c -> Tagged b c
retag = Tagged . untag

-- | Type tag for read queries, i.e. 'QueryString R a b'.
data R
-- | Type tag for write queries, i.e. 'QueryString W a b'.
data W
-- | Type tag for schema queries, i.e. 'QueryString S a b'.
data S

