-- | The CQL native protocol is a binary frame-based protocol where
-- each frame has a 'Header', a 'Length' and a body. The protocol
-- distinguishes 'Request's and 'Response's.
--
-- Some usage examples:
--
-- __Constructing and Serialising a Request__
--
-- @
-- let q = QueryString "select peer from system.peers where data_center = ? and rack = ?"
--     p = QueryParams One False ("uk-1", "r-0") Nothing Nothing Nothing
--     r = RqQuery (Query q p :: Query R (Text, Text) (Identity IP))
--     i = mkStreamId 0
-- in pack V3 noCompression False i r
-- @
--
-- __Deserialising a Response__
--
-- @
-- -- assuming 'bh' contains the raw header byte string and 'bb' the raw
-- -- body byte string.
-- case header V3 bh of
--     Left  e -> ...
--     Right h -> unpack noCompression h bb
-- @
--
-- __A generic query processing function__
--
-- @
-- query :: (Tuple a, Tuple b) => Version -> Socket -> QueryString k a b -> QueryParams a -> IO (Response k a b)
-- query v s q p = do
--     let i = mkStreamId 0
--     sendToServer s (pack v noCompression False i (RqQuery (Query q p)))
--     b <- recv (if v == V3 then 9 else 8) s
--     h <- either (throwIO . MyException) return (header v b)
--     when (headerType h == RqHeader) $
--         throwIO UnexpectedRequestHeader
--     let len = lengthRepr (bodyLength h)
--     x <- recv (fromIntegral len) s
--     case unpack noCompression h x of
--         Left e              -> throwIO $ AnotherException e
--         Right (RsError _ e) -> throwIO e
--         Right response      -> return response
-- @
--
module Database.CQL.Protocol
    ( -- * Cql type-class
      Cql (..)

      -- * Basic type definitions
    , module Database.CQL.Protocol.Types

      -- * Header
    , Header     (..)
    , HeaderType (..)
    , header

      -- ** Length
    , Length (..)

      -- ** StreamId
    , StreamId
    , mkStreamId
    , fromStreamId

      -- ** Flags
    , Flags
    , compress
    , tracing
    , isSet

      -- * Request
    , Request (..)
    , getOpCode
    , pack

      -- ** Options
    , Options (..)

      -- ** Startup
    , Startup (..)

      -- ** Auth Response
    , AuthResponse (..)

      -- ** Register
    , Register  (..)
    , EventType (..)

      -- ** Query
    , Query             (..)
    , QueryParams       (..)
    , SerialConsistency (..)

      -- ** Batch
    , Batch      (..)
    , BatchQuery (..)
    , BatchType  (..)

      -- ** Prepare
    , Prepare (..)

      -- ** Execute
    , Execute (..)

      -- * Response
    , Response (..)
    , warnings
    , traceId
    , unpack

      -- ** Ready
    , Ready (..)

      -- ** Authenticate
    , Authenticate  (..)
    , AuthChallenge (..)
    , AuthSuccess   (..)

      -- ** Result
    , Result     (..)
    , MetaData   (..)
    , ColumnSpec (..)

      -- ** Supported
    , Supported  (..)

      -- ** Event
    , Event          (..)
    , TopologyChange (..)
    , SchemaChange   (..)
    , StatusChange   (..)
    , Change         (..)

      -- ** Error
    , Error     (..)
    , WriteType (..)

      -- * Row, Tuple and Record
    , module Database.CQL.Protocol.Tuple
    , module Database.CQL.Protocol.Record
    ) where

import Database.CQL.Protocol.Class
import Database.CQL.Protocol.Header
import Database.CQL.Protocol.Record
import Database.CQL.Protocol.Request
import Database.CQL.Protocol.Response
import Database.CQL.Protocol.Tuple
import Database.CQL.Protocol.Types
