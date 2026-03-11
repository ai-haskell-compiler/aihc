-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Database.CQL.IO.Exception where

import Control.Exception (SomeAsyncException (..))
import Control.Monad.Catch
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import Data.Typeable
import Data.UUID
import Database.CQL.IO.Cluster.Host
import Database.CQL.IO.Connection.Settings
import Database.CQL.Protocol

import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as Lazy

-----------------------------------------------------------------------------
-- ResponseError

-- | The server responded with an 'Error'.
--
-- Most of these errors are either not retryable or only safe to retry
-- for idempotent queries. For more details of which errors may be safely
-- retried under which circumstances, see also the documentation of the
-- <https://docs.datastax.com/en/developer/java-driver/latest/manual/retries/ Java driver>.
data ResponseError = ResponseError
    { reHost  :: !Host
    , reTrace :: !(Maybe UUID)
    , reWarn  :: ![Text]
    , reCause :: !Error
    } deriving (Show, Typeable)

instance Exception ResponseError

toResponseError :: HostResponse k a b -> Maybe ResponseError
toResponseError (HostResponse h (RsError t w c)) = Just (ResponseError h t w c)
toResponseError _                                = Nothing

fromResponseError :: ResponseError -> HostResponse k a b
fromResponseError (ResponseError h t w c) = HostResponse h (RsError t w c)

-----------------------------------------------------------------------------
-- HostError

-- | An error during host selection prior to query execution.
--
-- These errors are always safe to retry but may indicate an overload
-- situation and thus suggest a review of the client and cluster
-- configuration (number of hosts, pool sizes, connections per host,
-- streams per connection, ...).
data HostError
    = NoHostAvailable
        -- ^ There is currently not a single host available to the
        -- client according to the configured 'Policy'.
    | HostsBusy
        -- ^ All streams on all connections are currently in use.
    deriving Typeable

instance Exception HostError

instance Show HostError where
    show NoHostAvailable = "cql-io: no host available"
    show HostsBusy       = "cql-io: hosts busy"

-----------------------------------------------------------------------------
-- ConnectionError

-- | An error while establishing or using a connection to send a
-- request or receive a response.
data ConnectionError
    = ConnectionClosed !InetAddr
        -- ^ The connection was suddenly closed.
        -- Retries are only safe for idempotent queries.
    | ConnectTimeout   !InetAddr
        -- ^ A timeout occurred while establishing a connection.
        -- See also 'setConnectTimeout'. Retries are always safe.
    | ResponseTimeout  !InetAddr
        -- ^ A timeout occurred while waiting for a response.
        -- See also 'setResponseTimeout'. Retries are only
        -- safe for idempotent queries.
    deriving Typeable

instance Exception ConnectionError

instance Show ConnectionError where
    show (ConnectionClosed i) = "cql-io: connection closed: " ++ show i
    show (ConnectTimeout   i) = "cql-io: connect timeout: " ++ show i
    show (ResponseTimeout  i) = "cql-io: response timeout: " ++ show i

-----------------------------------------------------------------------------
-- ProtocolError

-- | A protocol error indicates a problem related to the client-server
-- communication protocol. The cause may either be misconfiguration
-- on the client or server, or an implementation bug. In the latter case
-- it should be reported. In either case these errors are not recoverable
-- and should never be retried.
data ProtocolError where
    -- | The client received an unexpected response for a request.
    -- This indicates a problem with the communication protocol
    -- and should be reported.
    UnexpectedResponse :: Host -> Response k a b -> ProtocolError
    -- | The client received an unexpected query ID in an 'Unprepared'
    -- server response upon executing a prepared query. This indicates
    -- a problem with the communication protocol and should be reported.
    UnexpectedQueryId :: QueryId k a b -> ProtocolError
    -- | The client tried to use a compression algorithm that
    -- is not supported by the server. The first argument is the offending
    -- algorithm and the second argument the list of supported algorithms
    -- as reported by the server. This indicates a client or server-side
    -- configuration error.
    UnsupportedCompression :: CompressionAlgorithm -> [CompressionAlgorithm] -> ProtocolError
    -- | An error occurred during the serialisation of a request.
    -- This indicates a problem with the wire protocol and should
    -- be reported.
    SerialiseError :: String -> ProtocolError
    -- | An error occurred during parsing of a response. This indicates
    -- a problem with the wire protocol and should be reported.
    ParseError :: String -> ProtocolError

deriving instance Typeable ProtocolError
instance Exception ProtocolError

instance Show ProtocolError where
    show e = showString "cql-io: protocol error: " . case e of
        ParseError x ->
            showString "parse error: " . showString x
        SerialiseError x ->
            showString "serialise error: " . showString x
        UnsupportedCompression x cc ->
            showString "unsupported compression: " . shows x .
            showString ", expected one of " . shows cc
        UnexpectedQueryId i ->
            showString "unexpected query ID: " . shows i
        UnexpectedResponse h r -> showString "unexpected response: " .
            shows h . showString ": " . shows (f r)
        $ ""
      where
        f :: Response k a b -> Response k a NoShow
        f (RsError         a b c) = RsError a b c
        f (RsReady         a b c) = RsReady a b c
        f (RsAuthenticate  a b c) = RsAuthenticate a b c
        f (RsAuthChallenge a b c) = RsAuthChallenge a b c
        f (RsAuthSuccess   a b c) = RsAuthSuccess a b c
        f (RsSupported     a b c) = RsSupported a b c
        f (RsResult        a b c) = RsResult a b (g c)
        f (RsEvent         a b c) = RsEvent a b c

        g :: Result k a b -> Result k a NoShow
        g VoidResult                       = VoidResult
        g (RowsResult              a  b  ) = RowsResult a (map (const NoShow) b)
        g (SetKeyspaceResult       a     ) = SetKeyspaceResult a
        g (SchemaChangeResult      a     ) = SchemaChangeResult a
        g (PreparedResult (QueryId a) b c) = PreparedResult (QueryId a) b c

-- | Placeholder for parts of a 'Response' that are not 'Show'able.
data NoShow = NoShow deriving Show

-----------------------------------------------------------------------------
-- HashCollision

-- | An unexpected hash collision occurred for a prepared query string.
-- This indicates a problem with the implementation of prepared queries
-- and should be reported.
data HashCollision = HashCollision !Lazy.Text !Lazy.Text
    deriving Typeable

instance Exception HashCollision

instance Show HashCollision where
    show (HashCollision a b) = showString "cql-io: hash collision: "
                             . shows a
                             . showString " "
                             . shows b
                             $ ""

-----------------------------------------------------------------------------
-- AuthenticationError

-- | An error occurred during the authentication phase while
-- initialising a new connection. This indicates a configuration
-- error or a faulty 'Authenticator'.
data AuthenticationError
    = AuthenticationRequired !AuthMechanism
        -- ^ The server demanded authentication but none was provided
        -- by the client.
    | UnexpectedAuthenticationChallenge !AuthMechanism !AuthChallenge
        -- ^ The server presented an additional authentication challenge
        -- that the configured 'Authenticator' did not respond to.

instance Exception AuthenticationError

instance Show AuthenticationError where
    show (AuthenticationRequired a)
        = showString "cql-io: authentication required: "
        . shows a
        $ ""

    show (UnexpectedAuthenticationChallenge n c)
        = showString "cql-io: unexpected authentication challenge: '"
        . shows c
        . showString "' using mechanism '"
        . shows n
        . showString "'"
        $ ""

-----------------------------------------------------------------------------
-- Utilities

-- | Recover from all (synchronous) exceptions raised by a
-- computation with a fixed value.
recover :: forall m a. MonadCatch m => m a -> a -> m a
recover io val = try io >>= either fallback return
  where
    fallback :: SomeException -> m a
    fallback e = case fromException e of
        Just (SomeAsyncException _) -> throwM e
        Nothing                     -> return val
{-# INLINE recover #-}

-- | Ignore all (synchronous) exceptions raised by a
-- computation that produces no result, i.e. is only run for
-- its (side-)effects.
ignore :: MonadCatch m => m () -> m ()
ignore io = recover io ()
{-# INLINE ignore #-}

-- | Try a computation on a non-empty list of values, recovering
-- from (synchronous) exceptions for all but the last value.
tryAll :: forall m a b. MonadCatch m => NonEmpty a -> (a -> m b) -> m b
tryAll (a :| []) f = f a
tryAll (a :| aa) f = try (f a) >>= either next return
  where
    next :: SomeException -> m b
    next e = case fromException e of
        Just (SomeAsyncException _) -> throwM e
        Nothing                     -> tryAll (NonEmpty.fromList aa) f

