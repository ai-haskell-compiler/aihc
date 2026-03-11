{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module: Network.Wai.Session.PostgreSQL
-- Copyright: (C) 2015, Hans-Christian Esperer
-- License: BSD3
-- Maintainer: Hans-Christian Esperer <hc@hcesperer.org>
-- Stability: experimental
-- Portability: portable
--
-- Simple PostgreSQL backed wai-session backend. This module allows you to store
-- session data of wai-sessions in a PostgreSQL database. Two tables are kept, one
-- to store the session's metadata and one to store key,value pairs for each session.
-- All keys and values are stored as bytea values in the postgresql database using
-- haskell's cereal module to serialize and deserialize them.
--
-- Please note that the module does not let you configure the names of the database
-- tables. It is recommended to use this module with its own database schema.
module Network.Wai.Session.PostgreSQL
    ( dbStore
    , clearSession
    , defaultSettings
    , fromSimpleConnection
    , purgeOldSessions
    , purger
    , ratherSecureGen
    , SimpleConnection
    , StoreSettings (..)
    , WithPostgreSQLConn (..)
    ) where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.Int (Int64)
import Data.Pool (Pool, withResource)
import Data.Serialize (encode, decode, Serialize)
import Data.String (fromString)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Database.PostgreSQL.Simple
import Network.Wai (Request, requestHeaders)
import Network.Wai.Session
import Numeric (showHex)
import System.Entropy (getEntropy)
import Web.Cookie (parseCookies)

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE

-- |These settings control how the session store is behaving
data StoreSettings = StoreSettings {
    -- |The number of seconds a session is valid
    -- Seconds are counted since the session is last accessed (read or written),
    -- not since it was created.
      storeSettingsSessionTimeout :: Int64
    -- |A random session key generator. The session ID should provide
    -- sufficient entropy, and must not be predictable. It is recommended
    -- to use a cryptographically secure random number generator.
    , storeSettingsKeyGen :: IO B.ByteString
    -- |Whether to create the database table if it does not exist upon
    -- creating the session store. If set to false, the database table
    -- must exist or be created by some other means.
    , storeSettingsCreateTable :: Bool
    -- |A function that is called by to log events such as session
    -- purges or the table creation.
    , storeSettingsLog :: String -> IO ()
    -- |The number of microseconds to sleep between two runs of the
    -- old session purge worker.
    , storeSettingsPurgeInterval :: Int
    }

instance Default StoreSettings where
    def = defaultSettings

-- |By default, you pass a postgresql connection to the session store
-- when creating it. The passed connection will have to stay open
-- for the (possibly very long) existence of the session and it should
-- not be used for any other purpose during that time.
-- You can implement an instance of this class for a connection pool
-- instead, so that the session manager will not require a permanent
-- open PostgreSQL connection.
class WithPostgreSQLConn a where
    -- |Call the function (Connection -> IO b) with a valid and open
    -- PostgreSQL connection.
    withPostgreSQLConn :: a -> (Connection -> IO b) -> IO b


-- |Prepare a simple postgresql connection for use by the postgresql
-- session store. This basically wraps the connection along with a mutex
-- to ensure transactions work correctly. Connections used this way must
-- not be used anywhere else for the duration of the session store!
-- It is recommended to use a connection pool instead. To use a connection
-- pool, you simply need to implement the WithPostgreSQLConn type class.
fromSimpleConnection :: Connection -> IO SimpleConnection
fromSimpleConnection connection = do
    mvar <- newMVar ()
    return $ SimpleConnection (mvar, connection)

-- |A simple PostgreSQL connection stored together with a mutex that
-- prevents from running more than one postgresql transaction at
-- the same time. It is recommended to use a connection pool
-- instead for larger sites.
newtype SimpleConnection = SimpleConnection (MVar (), Connection)

instance WithPostgreSQLConn SimpleConnection where
    withPostgreSQLConn (SimpleConnection (mvar, conn)) =
        bracket (takeMVar mvar >> return conn) (\_ -> putMVar mvar ())

instance WithPostgreSQLConn (Pool Connection) where
    withPostgreSQLConn = withResource

qryCreateTable1 :: Query
qryCreateTable1         = "CREATE TABLE IF NOT EXISTS wai_pg_sessions (id bigserial NOT NULL, session_key character varying NOT NULL, session_created_at bigint NOT NULL, session_last_access bigint NOT NULL, session_invalidate_key boolean NOT NULL DEFAULT false, CONSTRAINT wai_pg_sessions_pkey PRIMARY KEY (id), CONSTRAINT wai_pg_sessions_session_key UNIQUE (session_key)) WITH ( OIDS=FALSE );"

qryCreateIndex1 :: Query
qryCreateIndex1         = "CREATE INDEX IF NOT EXISTS idx_session_last_access ON public.wai_pg_sessions USING btree (session_last_access);"

qryCreateTable2 :: Query
qryCreateTable2         = "CREATE TABLE IF NOT EXISTS wai_pg_session_data ( id bigserial NOT NULL, wai_pg_session bigint, key bytea, value bytea, CONSTRAINT wai_pg_session_data_pkey PRIMARY KEY (id), CONSTRAINT wai_pg_session_data_wai_pg_session_fkey FOREIGN KEY (wai_pg_session) REFERENCES wai_pg_sessions (id) MATCH SIMPLE ON UPDATE RESTRICT ON DELETE CASCADE, CONSTRAINT wai_pg_session_data_wai_pg_session_key_key UNIQUE (wai_pg_session, key) ) WITH (OIDS=FALSE);"

qryCreateSession :: Query
qryCreateSession        = "INSERT INTO wai_pg_sessions (session_key, session_created_at, session_last_access) VALUES (?,?,?) RETURNING id"

qryCreateOrUpdateSessionEntry :: Query
qryCreateOrUpdateSessionEntry = "INSERT INTO wai_pg_session_data (wai_pg_session,key,value) VALUES (?,?,?) ON CONFLICT (wai_pg_session,key) DO UPDATE SET value=EXCLUDED.value"

qryLookupSession       :: Query
qryLookupSession        = "SELECT id FROM wai_pg_sessions WHERE session_key=? AND session_last_access>=?"

qryLookupSession'      :: Query
qryLookupSession'       = "UPDATE wai_pg_sessions SET session_last_access=? WHERE id=?"

qryLookupSession''     :: Query
qryLookupSession''      = "SELECT value FROM wai_pg_session_data WHERE wai_pg_session=? AND key=?"

qryPurgeOldSessions    :: Query
qryPurgeOldSessions     = "DELETE FROM wai_pg_sessions WHERE session_last_access<?"

qryCheckNewKey         :: Query
qryCheckNewKey          = "SELECT session_invalidate_key FROM wai_pg_sessions WHERE session_key=?"

qryInvalidateSess1     :: Query
qryInvalidateSess1      = "UPDATE wai_pg_sessions SET session_invalidate_key=TRUE WHERE session_key=?"

qryInvalidateSess2     :: Query
qryInvalidateSess2      = "DELETE FROM wai_pg_session_data WHERE wai_pg_session=(SELECT id FROM wai_pg_sessions WHERE session_key=?)"

qryUpdateKey           :: Query
qryUpdateKey            = "UPDATE wai_pg_sessions SET session_key=?,session_invalidate_key=FALSE WHERE session_key=?"

-- |Create a new postgresql backed wai session store.
dbStore :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> IO (SessionStore m k v)
dbStore pool stos = do
    when (storeSettingsCreateTable stos) $
        withPostgreSQLConn pool $ \ conn -> do
            void $ execute_ conn qryCreateTable1
            void $ execute_ conn qryCreateTable2
            void $ execute_ conn qryCreateIndex1
            storeSettingsLog stos "Created tables."
    return $ dbStore' pool stos

-- |Delete expired sessions from the database.
purgeOldSessions :: WithPostgreSQLConn a => a -> StoreSettings -> IO Int64
purgeOldSessions pool stos = do
    curtime <- round <$> liftIO getPOSIXTime
    count <- withPostgreSQLConn pool $ \ conn ->
        execute conn qryPurgeOldSessions (Only (curtime - storeSettingsSessionTimeout stos))
    storeSettingsLog stos $ "Purged " ++ show count ++ " session(s)."
    return count

-- |Run a thread using forkIO that runs periodically to
-- purge old sessions.
purger :: WithPostgreSQLConn a => a -> StoreSettings -> IO ThreadId
purger pool stos = forkIO . forever . unerror $ do
    purgeOldSessions pool stos
    threadDelay $ storeSettingsPurgeInterval stos

-- |Create default settings using a session timeout of
-- one hour, a cryptographically secure session id generator
-- using 24 bytes of entropy and putStrLn to log events
-- to stdout.
defaultSettings :: StoreSettings
defaultSettings = StoreSettings
    { storeSettingsSessionTimeout=3600
    , storeSettingsKeyGen=ratherSecureGen 24
    , storeSettingsCreateTable=True
    , storeSettingsLog=putStrLn
    , storeSettingsPurgeInterval=600000000
    }

-- |Generate a session ID with n bytes of entropy
ratherSecureGen :: Int -> IO B.ByteString
ratherSecureGen n = TE.encodeUtf8 . prettyPrint <$> getEntropy n

prettyPrint :: B.ByteString -> T.Text
prettyPrint = T.pack . concatMap (`showHex` "") . B.unpack

dbStore' :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> SessionStore m k v
dbStore' pool stos Nothing = do
    newKey <- storeSettingsKeyGen stos
    curtime <- liftIO getPOSIXTime
    sessionPgId <- withPostgreSQLConn pool $ \ conn -> do
        [Only res] <- query conn qryCreateSession (newKey, round curtime :: Int64, round curtime :: Int64) :: IO [Only Int64]
        return (res :: Int64)
    backend pool stos newKey sessionPgId
dbStore' pool stos (Just key) = do
    curtime <- round <$> liftIO getPOSIXTime
    res <- withPostgreSQLConn pool $ \ conn ->
        query conn qryLookupSession (key, curtime - storeSettingsSessionTimeout stos) :: IO [Only Int64]
    case res of
        [Only sessionPgId]  -> backend pool stos key sessionPgId
        _                   -> dbStore' pool stos Nothing

-- |This function can be called to invalidate a session and enforce creating
-- a new one with a new session ID. It should be called *before* any calls
-- to sessionStore are made. It needs to be passed a request and the cookie
-- name explicitly due to the limited nature of the Network.Wai.Session
-- interface.
-- Sessions should be cleared when a login is performed, to prevent certain
-- kinds of session hijacking attacks.
clearSession :: (WithPostgreSQLConn a) => a -> B.ByteString -> Request -> IO ()
clearSession pool cookieName req = do
    let map         = [] :: [(k, v)]
        map'        = "" -- encode map
        cookies     = parseCookies <$> lookup (fromString "Cookie") (requestHeaders req)
        Just key    = lookup cookieName =<< cookies
    withPostgreSQLConn pool $ \ conn ->
        withTransaction conn $ do
            void $ execute conn qryInvalidateSess1 (Only key)
            void $ execute conn qryInvalidateSess2 (Only key)

backend :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> StoreSettings -> B.ByteString -> Int64 -> IO (Session m k v, IO B.ByteString)
backend pool stos key sessionPgId =
    return ( (
        reader pool key sessionPgId
      , writer pool key sessionPgId ), withPostgreSQLConn pool $ \conn -> do
        -- Update session access time
        curtime <- liftIO getPOSIXTime
        void $ execute conn qryLookupSession' (round curtime :: Int64, sessionPgId)

        [Only shouldNewKey] <- query conn qryCheckNewKey (Only key)
        if shouldNewKey then do
            newKey' <- storeSettingsKeyGen stos
            execute conn qryUpdateKey (newKey', key)
            return newKey'
        else
            return key
      )


reader :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> Int64 -> k -> m (Maybe v)
reader pool key sessionPgId k = do
    res <- liftIO $ withPostgreSQLConn pool $ \conn -> do
        query conn qryLookupSession'' (sessionPgId, Binary $ encode k)
    case res of
        [Only value]    -> case decode (fromBinary value) of
            Right value'    -> return $ Just value'
            Left error      -> return Nothing
        []              -> return Nothing

writer :: (WithPostgreSQLConn a, Serialize k, Eq k, Serialize v, MonadIO m) => a -> B.ByteString -> Int64 -> k -> v -> m ()
writer pool key sessionPgId k v = do
    let k' = Binary $ encode k
        v' = Binary $ encode v
    liftIO $ withPostgreSQLConn pool $ \conn ->
        void $ execute conn qryCreateOrUpdateSessionEntry (sessionPgId, k', v')

ignoreSqlError :: SqlError -> IO ()
ignoreSqlError _ = return ()

unerror :: IO a -> IO ()
unerror action = void action `catch` ignoreSqlError
