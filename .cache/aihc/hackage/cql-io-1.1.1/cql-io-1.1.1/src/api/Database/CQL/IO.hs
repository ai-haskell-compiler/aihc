-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

-- | This driver operates on some state which must be initialised prior to
-- executing client operations and terminated eventually. The library uses
-- <http://hackage.haskell.org/package/tinylog tinylog> for its logging
-- output and expects a 'Logger'.
--
-- For example (here using the @OverloadedStrings@ extension) :
--
-- @
-- > import Data.Text (Text)
-- > import Data.Functor.Identity
-- > import Database.CQL.IO as Client
-- > import qualified System.Logger as Logger
-- >
-- > g <- Logger.new Logger.defSettings
-- > c <- Client.init g defSettings
-- > let q = "SELECT cql_version from system.local" :: QueryString R () (Identity Text)
-- > let p = defQueryParams One ()
-- > runClient c (query q p)
-- [Identity "3.4.4"]
-- > shutdown c
-- @
--

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE LambdaCase    #-}

module Database.CQL.IO
    ( -- * Client Settings
      Settings
    , S.defSettings
    , addContact
    , setCompression
    , setConnectTimeout
    , setContacts
    , setIdleTimeout
    , setKeyspace
    , setMaxConnections
    , setMaxStreams
    , setMaxTimeouts
    , setPolicy
    , setPoolStripes
    , setPortNumber
    , PrepareStrategy (..)
    , setPrepareStrategy
    , setProtocolVersion
    , setResponseTimeout
    , setSendTimeout
    , setRetrySettings
    , setMaxRecvBuffer
    , setSSLContext

      -- ** Logging
    , Logger (..)
    , LogLevel (..)
    , setLogger
    , nullLogger
    , stdoutLogger

      -- ** Authentication
    , setAuthentication
    , Authenticator (..)
    , AuthContext
    , ConnId
    , authConnId
    , authHost
    , AuthMechanism (..)
    , AuthUser      (..)
    , AuthPass      (..)
    , passwordAuthenticator

      -- ** Retry Settings
    , RetrySettings
    , noRetry
      -- *** Default
    , defRetrySettings
    , defRetryPolicy
    , defRetryHandlers
      -- *** Eager
    , eagerRetrySettings
    , eagerRetryPolicy
    , eagerRetryHandlers
      -- *** Configuration
    , setRetryPolicy
    , setRetryHandlers
    , adjustConsistency
    , adjustSendTimeout
    , adjustResponseTimeout

      -- ** Load-balancing
    , Policy (..)
    , random
    , roundRobin

      -- *** Hosts
    , Host
    , HostEvent (..)
    , InetAddr  (..)
    , hostAddr
    , dataCentre
    , rack

      -- * Client Monad
    , Client
    , MonadClient (..)
    , ClientState
    , DebugInfo   (..)
    , init
    , runClient
    , shutdown
    , debugInfo

      -- * Queries
      -- $queries
    , R, W, S
    , QueryParams       (..)
    , defQueryParams
    , Consistency       (..)
    , SerialConsistency (..)
    , Identity          (..)

      -- ** Basic Queries
    , QueryString (..)
    , query
    , query1
    , write
    , schema

      -- ** Prepared Queries
    , PrepQuery
    , prepared
    , queryString

      -- ** Paging
    , Page (..)
    , emptyPage
    , paginate

      -- ** Lightweight Transactions
    , Row
    , fromRow
    , trans

      -- ** Batch Queries
    , BatchM
    , addQuery
    , addPrepQuery
    , setType
    , setConsistency
    , setSerialConsistency
    , batch

      -- ** Retries
    , retry
    , once

      -- ** Low-Level Queries
      -- $low-level-queries
    , RunQ (..)
    , HostResponse (..)
    , request
    , getResult

      -- * Exceptions
    , ProtocolError       (..)
    , HostError           (..)
    , ConnectionError     (..)
    , ResponseError       (..)
    , AuthenticationError (..)
    , HashCollision       (..)
    ) where

import Control.Applicative
import Data.Functor.Identity
import Data.Maybe (isJust, listToMaybe)
import Database.CQL.Protocol
import Database.CQL.IO.Batch hiding (batch)
import Database.CQL.IO.Client
import Database.CQL.IO.Cluster.Host
import Database.CQL.IO.Cluster.Policies
import Database.CQL.IO.Connection.Settings as C
import Database.CQL.IO.Exception
import Database.CQL.IO.Log
import Database.CQL.IO.PrepQuery
import Database.CQL.IO.Settings as S
import Prelude hiding (init)

import qualified Database.CQL.IO.Batch as B

-- $queries
--
-- Queries are defined either as 'QueryString's or 'PrepQuery's.
-- Both types carry three phantom type parameters used to describe
-- the query, input and output types, respectively, as follows:
--
--   * @__k__@ is one of 'R'ead, 'W'rite or 'S'chema.
--   * @__a__@ is the tuple type for the input, i.e. for the
--     parameters bound by positional (@?@) or named (@:foo@) placeholders.
--   * @__b__@ is the tuple type for the outputs, i.e. for the
--     columns selected in a query.
--
-- Thereby every type used in an input or output tuple must be an instance
-- of the 'Cql' typeclass. It is the responsibility of user code
-- that the type ascription of a query matches the order, number and types of
-- the parameters. For example:
--
-- @
-- myQuery :: QueryString R (Identity UUID) (Text, Int, Maybe UTCTime)
-- myQuery = "select name, age, birthday from user where id = ?"
-- @
--
-- In this example, the query is declared as a 'R'ead with a single
-- input (id) and three outputs (name, age and birthday).
--
-- Note that a single input or output type needs to be wrapped
-- in the 'Identity' newtype, for which there is a `Cql` instance,
-- in order to avoid overlapping instances.
--
-- It is a common strategy to use additional @newtype@s with derived
-- @Cql@ instances for additional type safety, e.g.
--
-- @
-- newtype UserId = UserId UUID deriving (Eq, Show, Cql)
-- @
--
-- The input and output tuples can further be automatically
-- converted from and to records via the 'Database.CQL.Protocol.Record'
-- typeclass, whose instances can be generated via @TemplateHaskell@,
-- if desired.
--
-- __Note on null values__
--
-- In principle, any column in Cassandra is /nullable/, i.e. may be
-- be set to @null@ as a result of row operations. It is therefore
-- important that any output type of a query that may be null
-- is wrapped in the 'Maybe' type constructor.
-- It is a common pitfall that a column is assumed to never contain
-- null values, when in fact partial updates or deletions on a row,
-- including via the use of TTLs, may result in null values and thus
-- runtime errors when processing the responses.

-- $low-level-queries
--
-- /Note/: Use of the these functions may require additional imports from
-- @Database.CQL.Protocol@ or its submodules in order to construct
-- 'Request's and evaluate 'Response's.

-- | A type which can be run as a query.
class RunQ q where
    runQ :: (MonadClient m, Tuple a, Tuple b)
         => q k a b
         -> QueryParams a
         -> m (HostResponse k a b)

instance RunQ QueryString where
    runQ q p = request (RqQuery (Query q p))

instance RunQ PrepQuery where
    runQ q = liftClient . execute q

-- | Run a CQL read-only query returning a list of results.
query :: (MonadClient m, Tuple a, Tuple b, RunQ q) => q R a b -> QueryParams a -> m [b]
query q p = do
    r <- runQ q p
    getResult r >>= \case
        RowsResult _ b -> return b
        _              -> unexpected r

-- | Run a CQL read-only query returning a single result.
query1 :: (MonadClient m, Tuple a, Tuple b, RunQ q) => q R a b -> QueryParams a -> m (Maybe b)
query1 q p = listToMaybe <$> query q p

-- | Run a CQL write-only query (e.g. insert\/update\/delete),
-- returning no result.
--
-- /Note: If the write operation is conditional, i.e. is in fact a "lightweight
-- transaction" returning a result, 'trans' must be used instead./
write :: (MonadClient m, Tuple a, RunQ q) => q W a () -> QueryParams a -> m ()
write q p = do
    r <- runQ q p
    getResult r >>= \case
        VoidResult -> return ()
        _          -> unexpected r

-- | Run a CQL conditional write query (e.g. insert\/update\/delete) as a
-- "lightweight transaction", returning the result 'Row's describing the
-- outcome.
trans :: (MonadClient m, Tuple a, RunQ q) => q W a Row -> QueryParams a -> m [Row]
trans q p = do
    r <- runQ q p
    getResult r >>= \case
        RowsResult _ b -> return b
        _              -> unexpected r

-- | Run a CQL schema query, returning 'SchemaChange' information, if any.
schema :: (MonadClient m, Tuple a, RunQ q) => q S a () -> QueryParams a -> m (Maybe SchemaChange)
schema q p = do
    r <- runQ q p
    getResult r >>= \case
        SchemaChangeResult s -> return $ Just s
        VoidResult           -> return Nothing
        _                    -> unexpected r

-- | Run a batch query against a Cassandra node.
batch :: MonadClient m => BatchM () -> m ()
batch = liftClient . B.batch

-- | Return value of 'paginate'. Contains the actual result values as well
-- as an indication of whether there is more data available and the actual
-- action to fetch the next page.
data Page a = Page
    { hasMore  :: !Bool
    , result   :: [a]
    , nextPage :: Client (Page a)
    } deriving (Functor)

-- | A page with an empty result list.
emptyPage :: Page a
emptyPage = Page False [] (return emptyPage)

-- | Run a CQL read-only query against a Cassandra node.
--
-- This function is like 'query', but limits the result size to 10000
-- (default) unless there is an explicit size restriction given in
-- 'QueryParams'. The returned 'Page' can be used to continue the query.
--
-- Please note that -- as of Cassandra 2.1.0 -- if your requested page size
-- is equal to the result size, 'hasMore' might be true and a subsequent
-- 'nextPage' will return an empty list in 'result'.
paginate :: (MonadClient m, Tuple a, Tuple b, RunQ q) => q R a b -> QueryParams a -> m (Page b)
paginate q p = do
    let p' = p { pageSize = pageSize p <|> Just 10000 }
    r <- runQ q p'
    getResult r >>= \case
        RowsResult m b ->
            if isJust (pagingState m) then
                return $ Page True b (paginate q p' { queryPagingState = pagingState m })
            else
                return $ Page False b (return emptyPage)
        _ -> unexpected r

