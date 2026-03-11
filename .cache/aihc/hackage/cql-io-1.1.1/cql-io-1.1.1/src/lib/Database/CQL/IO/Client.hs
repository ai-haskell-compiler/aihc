-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TupleSections              #-}
{-# LANGUAGE ViewPatterns               #-}

module Database.CQL.IO.Client
    ( Client
    , MonadClient (..)
    , ClientState
    , DebugInfo (..)
    , ControlState (..)
    , runClient
    , init
    , shutdown
    , request
    , requestN
    , request1
    , execute
    , executeWithPrepare
    , prepare
    , retry
    , once
    , debugInfo
    , preparedQueries
    , withPrepareStrategy
    , getResult
    , unexpected
    , C.defQueryParams
    ) where

import Control.Applicative
import Control.Concurrent (threadDelay, forkIO)
import Control.Concurrent.Async (async, wait)
import Control.Concurrent.STM (STM, atomically)
import Control.Concurrent.STM.TVar
import Control.Exception (IOException, SomeAsyncException (..))
import Control.Lens (makeLenses, (^.), set, over, view)
import Control.Monad (when, unless)
import Control.Monad.Catch
import Control.Monad.IO.Class
import Control.Monad.IO.Unlift
import Control.Monad.Reader (ReaderT (..), runReaderT, MonadReader, ask)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Retry (capDelay, exponentialBackoff, rsIterNumber)
import Control.Retry (recovering)
import Data.Foldable (for_, foldrM)
import Data.List (find)
import Data.Map.Strict (Map)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Semigroup
import Data.Text.Encoding (encodeUtf8)
import Data.Word
import Database.CQL.IO.Cluster.Host
import Database.CQL.IO.Cluster.Policies
import Database.CQL.IO.Connection (Connection, host, Raw)
import Database.CQL.IO.Connection.Settings
import Database.CQL.IO.Exception
import Database.CQL.IO.Jobs
import Database.CQL.IO.Log
import Database.CQL.IO.Pool (Pool)
import Database.CQL.IO.PrepQuery (PrepQuery, PreparedQueries)
import Database.CQL.IO.Settings
import Database.CQL.IO.Signal
import Database.CQL.IO.Timeouts (TimeoutManager)
import Database.CQL.Protocol hiding (Map)
import OpenSSL.Session (SomeSSLException)
import Prelude hiding (init)

import qualified Control.Monad.Reader              as Reader
import qualified Control.Monad.State.Strict        as S
import qualified Control.Monad.State.Lazy          as LS
import qualified Data.List.NonEmpty                as NE
import qualified Data.Map.Strict                   as Map
import qualified Database.CQL.IO.Cluster.Discovery as Disco
import qualified Database.CQL.IO.Connection        as C
import qualified Database.CQL.IO.Pool              as Pool
import qualified Database.CQL.IO.PrepQuery         as PQ
import qualified Database.CQL.IO.Timeouts          as TM
import qualified Database.CQL.Protocol             as Cql

data ControlState
    = Connected
    | Reconnecting
    | Disconnected
    deriving (Eq, Ord, Show)

data Control = Control
    { _state      :: !ControlState
    , _connection :: !Connection
    }

data Context = Context
    { _settings :: !Settings
    , _timeouts :: !TimeoutManager
    , _sigMonit :: !(Signal HostEvent)
    }

-- | Opaque client state/environment.
data ClientState = ClientState
    { _context     :: !Context
    , _policy      :: !Policy
    , _prepQueries :: !PreparedQueries
    , _control     :: !(TVar Control)
    , _hostmap     :: !(TVar (Map Host Pool))
    , _jobs        :: !(Jobs InetAddr)
    }

makeLenses ''Control
makeLenses ''Context
makeLenses ''ClientState

-- | The Client monad.
--
-- A simple reader monad on `IO` around some internal state. Prior to executing
-- this monad via 'runClient', its state must be initialised through
-- 'Database.CQL.IO.Client.init' and after finishing operation it should be
-- terminated with 'shutdown'.
--
-- To lift 'Client' actions into another monad, see 'MonadClient'.
newtype Client a = Client
    { client :: ReaderT ClientState IO a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadIO
               , MonadUnliftIO
               , MonadThrow
               , MonadCatch
               , MonadMask
               , MonadReader ClientState
               )

-- | Monads in which 'Client' actions may be embedded.
class (MonadIO m, MonadThrow m) => MonadClient m
  where
    -- | Lift a computation from the 'Client' monad.
    liftClient :: Client a -> m a
    -- | Execute an action with a modified 'ClientState'.
    localState :: (ClientState -> ClientState) -> m a -> m a

instance MonadClient Client where
    liftClient = id
    localState = Reader.local

instance MonadClient m => MonadClient (ReaderT r m) where
    liftClient     = lift . liftClient
    localState f m = ReaderT (localState f . runReaderT m)

instance MonadClient m => MonadClient (S.StateT s m) where
    liftClient     = lift . liftClient
    localState f m = S.StateT (localState f . S.runStateT m)

instance MonadClient m => MonadClient (LS.StateT s m) where
    liftClient     = lift . liftClient
    localState f m = LS.StateT (localState f . LS.runStateT m)

instance MonadClient m => MonadClient (ExceptT e m) where
    liftClient     = lift . liftClient
    localState f m = ExceptT $ localState f (runExceptT m)

-----------------------------------------------------------------------------
-- API

-- | Execute the client monad.
runClient :: MonadIO m => ClientState -> Client a -> m a
runClient p a = liftIO $ runReaderT (client a) p

-- | Use given 'RetrySettings' during execution of some client action.
retry :: MonadClient m => RetrySettings -> m a -> m a
retry r = localState (set (context.settings.retrySettings) r)

-- | Execute a client action once, without retries, i.e.
--
-- @once action = retry noRetry action@.
--
-- Primarily for use in applications where global 'RetrySettings'
-- are configured and need to be selectively disabled for individual
-- queries.
once :: MonadClient m => m a -> m a
once = retry noRetry

-- | Change the default 'PrepareStrategy' for the given client action.
withPrepareStrategy :: MonadClient m => PrepareStrategy -> m a -> m a
withPrepareStrategy s = localState (set (context.settings.prepStrategy) s)

-- | Send a 'Request' to the server and return a 'Response'.
--
-- This function will first ask the clients load-balancing 'Policy' for
-- some host and use its connection pool to acquire a connection for
-- request transmission.
--
-- If all available hosts are busy (i.e. their connection pools are fully
-- utilised), the function will block until a connection becomes available
-- or the maximum wait-queue length has been reached.
--
-- The request is retried according to the configured 'RetrySettings'.
request :: (MonadClient m, Tuple a, Tuple b) => Request k a b -> m (HostResponse k a b)
request a = liftClient $ do
    n <- liftIO . hostCount =<< view policy
    withRetries (requestN n) a

-- | Send a request to a host chosen by the configured host policy.
--
-- Tries up to @max(1,n)@ hosts. If no host can execute the request,
-- a 'HostError' is thrown. Specifically:
--
--   * If no host is available from the 'Policy', 'NoHostAvailable' is thrown.
--   * If no host can execute the request, e.g. because all streams
--     on all connections are occupied, 'HostsBusy' is thrown.
requestN :: (Tuple b, Tuple a)
    => Word
    -> Request k a b
    -> ClientState
    -> Client (HostResponse k a b)
requestN !n a s = liftIO (select (s^.policy)) >>= \case
    Nothing -> replaceControl >> throwM NoHostAvailable
    Just  h -> tryRequest1 h a s >>= \case
        Just hr -> return hr
        Nothing -> if n > 1
            then requestN (n - 1) a s
            else throwM HostsBusy

-- | Send a 'Request' to a specific 'Host'.
--
-- If the request cannot be executed on the given host, e.g.
-- because all connections are occupied, 'HostsBusy' is thrown.
request1 :: (Tuple a, Tuple b)
    => Host
    -> Request k a b
    -> ClientState
    -> Client (HostResponse k a b)
request1 h r s = do
    rs <- tryRequest1 h r s
    maybe (throwM HostsBusy) return rs

-- | Try to send a 'Request' to a specific 'Host'.
--
-- If the request cannot be executed on the given host, e.g.
-- because all connections are occupied, 'Nothing' is returned.
tryRequest1 :: (Tuple a, Tuple b)
    => Host
    -> Request k a b
    -> ClientState
    -> Client (Maybe (HostResponse k a b))
tryRequest1 h a s = do
    pool <- Map.lookup h <$> readTVarIO' (s^.hostmap)
    case pool of
        Just p -> do
            result <- Pool.with p exec `catches` handlers
            for_ result $ \(HostResponse _ r) ->
                for_ (Cql.warnings r) $ \w ->
                    logWarn' $ "Server warning: " <> byteString (encodeUtf8 w)
            return result
        Nothing -> do
            logError' $ "No pool for host: " <> string8 (show h)
            p' <- mkPool (s^.context) h
            atomically' $ modifyTVar' (s^.hostmap) (Map.alter (maybe (Just p') Just) h)
            tryRequest1 h a s
  where
    exec c = do
        r <- C.request c a
        return $ HostResponse h r

    handlers =
        [ Handler $ \(e :: ConnectionError)  -> onConnectionError e
        , Handler $ \(e :: IOException)      -> onConnectionError e
        , Handler $ \(e :: SomeSSLException) -> onConnectionError e
        ]

    onConnectionError exc = do
        e <- ask
        logWarn' (string8 (show exc))
        -- Tell the policy that the host is down until monitoring confirms
        -- it is still up, which will be signalled by a subsequent 'HostUp'
        -- event.
        liftIO $ ignore $ onEvent (e^.policy) (HostDown (h^.hostAddr))
        runJob_ (e^.jobs) (h^.hostAddr) $
            runClient e $ monitor (Ms 0) (Ms 30000) h
        -- Any connection error may indicate a problem with the
        -- control connection, if it uses the same host.
        ch <- fmap (view (connection.host)) . readTVarIO' =<< view control
        when (h == ch) $ do
            ok <- checkControl
            unless ok replaceControl
        throwM exc

------------------------------------------------------------------------------
-- Prepared queries

-- | Execute the given request. If an 'Unprepared' error is returned, this
-- function will automatically try to re-prepare the query and re-execute
-- the original request using the same host which was used for re-preparation.
executeWithPrepare :: (Tuple b, Tuple a)
    => Maybe Host
    -> Request k a b
    -> Client (HostResponse k a b)
executeWithPrepare mh rq
    | Just h <- mh = exec (request1 h)
    | otherwise    = do
        p <- view policy
        n <- liftIO $ hostCount p
        exec (requestN n)
  where
    exec action = do
        r <- withRetries action rq
        case hrResponse r of
            RsError _ _ (Unprepared _ i) -> do
                pq <- preparedQueries
                qs <- atomically' (PQ.lookupQueryString (QueryId i) pq)
                case qs of
                    Nothing -> throwM $ UnexpectedQueryId (QueryId i)
                    Just  s -> do
                        (h, _) <- prepare (Just LazyPrepare) (s :: Raw QueryString)
                        executeWithPrepare (Just h) rq
            _ -> return r

-- | Prepare the given query according to the given 'PrepareStrategy',
-- returning the resulting 'QueryId' and 'Host' which was used for
-- preparation.
prepare :: (Tuple b, Tuple a) => Maybe PrepareStrategy -> QueryString k a b -> Client (Host, QueryId k a b)
prepare (Just LazyPrepare) qs = do
    s <- ask
    n <- liftIO $ hostCount (s^.policy)
    r <- withRetries (requestN n) (RqPrepare (Prepare qs))
    getPreparedQueryId r

prepare (Just EagerPrepare) qs = view policy
    >>= liftIO . current
    >>= mapM (action (RqPrepare (Prepare qs)))
    >>= first
  where
    action rq h = withRetries (request1 h) rq >>= getPreparedQueryId

    first (x:_) = return x
    first []    = replaceControl >> throwM NoHostAvailable

prepare Nothing qs = do
    ps <- view (context.settings.prepStrategy)
    prepare (Just ps) qs

-- | Execute a prepared query (transparently re-preparing if necessary).
execute :: (Tuple b, Tuple a) => PrepQuery k a b -> QueryParams a -> Client (HostResponse k a b)
execute q p = do
    pq <- view prepQueries
    maybe (new pq) (exec Nothing) =<< atomically' (PQ.lookupQueryId q pq)
  where
    exec h i = executeWithPrepare h (RqExecute (Execute i p))
    new pq = do
        (h, i) <- prepare (Just LazyPrepare) (PQ.queryString q)
        atomically' (PQ.insert q i pq)
        exec (Just h) i

prepareAllQueries :: Host -> Client ()
prepareAllQueries h = do
    pq <- view prepQueries
    qs <- atomically' $ PQ.queryStrings pq
    for_ qs $ \q ->
        let qry = QueryString q :: Raw QueryString in
        withRetries (request1 h) (RqPrepare (Prepare qry))

------------------------------------------------------------------------------
-- Debug info

data DebugInfo = DebugInfo
    { policyInfo :: String
        -- ^ Host 'Policy' string representation.
    , jobInfo :: [InetAddr]
        -- ^ Hosts with running background jobs (e.g. monitoring of hosts
        -- currently considered down).
    , hostInfo :: [Host]
        -- ^ All known hosts.
    , controlInfo :: (Host, ControlState)
        -- ^ Control connection information.
    }

instance Show DebugInfo where
    show dbg = showString "running jobs: "
             . shows (jobInfo dbg)
             . showString "\nknown hosts: "
             . shows (hostInfo dbg)
             . showString "\npolicy info: "
             . shows (policyInfo dbg)
             . showString "\ncontrol host: "
             . shows (controlInfo dbg)
             $ ""

debugInfo :: MonadClient m => m DebugInfo
debugInfo = liftClient $ do
    hosts <- Map.keys <$> (readTVarIO' =<< view hostmap)
    pols  <- liftIO . display =<< view policy
    jbs   <- listJobKeys =<< view jobs
    ctrl  <- (\(Control s c) -> (c^.host, s)) <$> (readTVarIO' =<< view control)
    return $ DebugInfo pols jbs hosts ctrl

preparedQueries :: Client PreparedQueries
preparedQueries = view prepQueries

-----------------------------------------------------------------------------
-- Initialisation

-- | Initialise client state with the given 'Settings' using the provided
-- 'Logger' for all it's logging output.
init :: MonadIO m => Settings -> m ClientState
init s = liftIO $ do
    tom <- TM.create (Ms 250)
    ctx <- Context s tom <$> signal
    bracketOnError (mkContact ctx) C.close $ \con -> do
        pol <- s^.policyMaker
        cst <- ClientState ctx
                <$> pure pol
                <*> PQ.new
                <*> newTVarIO (Control Connected con)
                <*> newTVarIO Map.empty
                <*> newJobs
        ctx^.sigMonit |-> onEvent pol
        runClient cst (setupControl con)
        return cst

-- | Try to establish a connection to one of the initial contacts.
mkContact :: Context -> IO Connection
mkContact (Context s t _) = tryAll (s^.contacts) mkConnection
  where
    mkConnection h = do
        as <- C.resolve h (s^.portnumber)
        NE.fromList as `tryAll` doConnect

    doConnect a = do
        logDebug (s^.logger) $ "Connecting to " <> string8 (show a)
        c <- C.connect (s^.connSettings) t (s^.protoVersion) (s^.logger) (Host a "" "")
        return c

discoverPeers :: MonadIO m => Context -> Connection -> m [Host]
discoverPeers ctx c = liftIO $ do
    let p = ctx^.settings.portnumber
    map (peer2Host p . asRecord) <$> C.query c One Disco.peers ()

mkPool :: MonadIO m => Context -> Host -> m Pool
mkPool ctx h = liftIO $ do
    let s = ctx^.settings
    let m = s^.connSettings.maxStreams
    Pool.create (connOpen s) connClose (ctx^.settings.logger) (s^.poolSettings) m
  where
    lgr = ctx^.settings.logger

    connOpen s = do
        c <- C.connect (s^.connSettings) (ctx^.timeouts) (s^.protoVersion) lgr h
        logDebug lgr $ "Connection established: " <> string8 (show c)
        return c

    connClose c = do
        C.close c
        logDebug lgr $ "Connection closed: " <> string8 (show c)

-----------------------------------------------------------------------------
-- Termination

-- | Terminate client state, i.e. end all running background checks and
-- shutdown all connection pools. Once this is entered, the client
-- will eventually be shut down, though an asynchronous exception can
-- interrupt the wait for that to occur.
shutdown :: MonadIO m => ClientState -> m ()
shutdown s = liftIO $ asyncShutdown >>= wait
  where
    asyncShutdown = async $ do
        TM.destroy (s^.context.timeouts) True
        cancelJobs (s^.jobs)
        ignore $ C.close . view connection =<< readTVarIO (s^.control)
        mapM_ Pool.destroy . Map.elems =<< readTVarIO (s^.hostmap)

-----------------------------------------------------------------------------
-- Monitoring

-- | @monitor initialDelay maxDelay host@ tries to establish a connection
-- to @host@ after @initialDelay@. If the connection attempt fails, it is
-- retried with exponentially increasing delays, up to a maximum delay of
-- @maxDelay@. When a connection attempt suceeds, a 'HostUp' event is
-- signalled.
--
-- The function returns when one of the following conditions is met:
--
--   1. The connection attempt suceeds.
--   2. The host is no longer found to be in the client's known host map.
--
-- I.e. as long as the host is still known to the client and is unreachable, the
-- connection attempts continue. Both @initialDelay@ and @maxDelay@ are bounded
-- by a limit of 5 minutes.
monitor :: Milliseconds -> Milliseconds -> Host -> Client ()
monitor initial maxDelay h = do
    liftIO $ threadDelay (toMicros initial)
    logInfo' $ "Monitoring: " <> string8 (show h)
    hostCheck 0
  where
    hostCheck :: Int -> Client ()
    hostCheck !n = do
        hosts <- liftIO . readTVarIO =<< view hostmap
        when (Map.member h hosts) $ do
            isUp <- C.canConnect h
            if isUp then do
                sig <- view (context.sigMonit)
                liftIO $ sig $$ (HostUp (h^.hostAddr))
                logInfo' $ "Reachable: " <> string8 (show h)
            else do
                logInfo' $ "Unreachable: " <> string8 (show h)
                liftIO $ threadDelay (2^n * minDelay)
                hostCheck (min (n + 1) maxExp)

    -- Bounded to 5min
    toMicros :: Milliseconds -> Int
    toMicros (Ms s) = min (s * 1000) (5 * 60 * 1000000)

    minDelay :: Int
    minDelay = 50000 -- 50ms

    maxExp :: Int
    maxExp = let steps = fromIntegral (toMicros maxDelay `div` minDelay) :: Double
              in floor (logBase 2 steps)

-----------------------------------------------------------------------------
-- Exception handling

-- [Note: Error responses]
-- Cassandra error responses are locally thrown as 'ResponseError's to achieve
-- a unified handling of retries in the context of a single retry policy,
-- together with other recoverable (i.e. retryable) exceptions. However, this
-- is just an internal technicality for handling retries - generally error
-- responses must not escape this function as exceptions. Deciding if and when
-- to actually throw a 'ResponseError' upon inspection of the 'HostResponse'
-- must be left to the caller.
withRetries
    :: (Tuple a, Tuple b)
    => (Request k a b -> ClientState -> Client (HostResponse k a b))
    -> Request k a b
    -> Client (HostResponse k a b)
withRetries fn a = do
    s <- ask
    let how = s^.context.settings.retrySettings.retryPolicy
    let what = s^.context.settings.retrySettings.retryHandlers
    r <- try $ recovering how what $ \i -> do
        hr <- if rsIterNumber i == 0
                 then fn a s
                 else fn (newRequest s) (adjust s)
        -- [Note: Error responses]
        maybe (return hr) throwM (toResponseError hr)
    return $ either fromResponseError id r
  where
    adjust s =
        let Ms x = s^.context.settings.retrySettings.sendTimeoutChange
            Ms y = s^.context.settings.retrySettings.recvTimeoutChange
        in over (context.settings.connSettings.sendTimeout)     (Ms . (+ x) . ms)
         . over (context.settings.connSettings.responseTimeout) (Ms . (+ y) . ms)
         $ s

    newRequest s =
        case s^.context.settings.retrySettings.reducedConsistency of
            Nothing -> a
            Just  c ->
                case a of
                    RqQuery   (Query   q p) -> RqQuery (Query q p { consistency = c })
                    RqExecute (Execute q p) -> RqExecute (Execute q p { consistency = c })
                    RqBatch b               -> RqBatch b { batchConsistency = c }
                    _                       -> a

------------------------------------------------------------------------------
-- Control connection handling
--
-- The control connection is dedicated to maintaining the client's
-- view of the cluster topology. There is a single control connection in a
-- client's 'ClientState' at any particular time.

-- | Setup and install the given connection as the new control
-- connection, replacing the current one.
setupControl :: Connection -> Client ()
setupControl c = do
    env <- ask
    pol <- view policy
    ctx <- view context
    l <- updateHost (c^.host) . listToMaybe <$> C.query c One Disco.local ()
    r <- discoverPeers ctx c
    (up, down) <- mkHostMap ctx pol (l:r)
    m <- view hostmap
    let h = Map.union up down
    atomically' $ writeTVar m h
    liftIO $ setup pol (Map.keys up) (Map.keys down)
    C.register c C.allEventTypes (runClient env . onCqlEvent)
    logInfo' $ "Known hosts: " <> string8 (show (Map.keys h))
    j <- view jobs
    for_ (Map.keys down) $ \d ->
        runJob j (d^.hostAddr) $
            runClient env $ monitor (Ms 1000) (Ms 60000) d
    ctl <- view control
    let c' = set C.host l c
    atomically' $ writeTVar ctl (Control Connected c')
    logInfo' $ "New control connection: " <> string8 (show c')

-- | Initialise connection pools for the given hosts, checking for
-- acceptability with the host policy and separating them by reachability.
mkHostMap :: Context -> Policy -> [Host] -> Client (Map Host Pool, Map Host Pool)
mkHostMap c p = liftIO . foldrM checkHost (Map.empty, Map.empty)
  where
    checkHost h (up, down) = do
        okay <- acceptable p h
        if okay then do
            isUp <- C.canConnect h
            if isUp then do
                up' <- Map.insert h <$> mkPool c h <*> pure up
                return (up', down)
            else do
                down' <- Map.insert h <$> mkPool c h <*> pure down
                return (up, down')
        else
            return (up, down)

-- | Check if the control connection is healthy.
checkControl :: Client Bool
checkControl = do
    cc <- view connection <$> (readTVarIO' =<< view control)
    rs <- liftIO $ C.requestRaw cc (RqOptions Options)
    return $ case rs of
        RsSupported {} -> True
        _              -> False
  `recover`
    False

-- | Asynchronously replace the control connection.
--
-- Invariants:
--
--   1) When the control connection is in state 'Reconnecting' there
--      is a thread running that attempts to establish a new control
--      connection.
--
--   2) There is only one thread performing a reconnect at a time.
--
-- To that end, the 'ControlState' acts as a mutex that is acquired
-- in state 'Reconnecting' and must eventually be released by either
-- a successful reconnect with state 'Connected' or a (fatal) failure
-- with state 'Disconnected'. In the latter case, further failing
-- requests may trigger another recovery attempt of the control
-- connection.
replaceControl :: Client ()
replaceControl = do
    e <- ask
    let l = e^.context.settings.logger
    liftIO $ mask $ \restore -> do
        cc <- setReconnecting e
        for_ cc $ \c -> forkIO $
            restore $ do
                ignore (C.close c)
                reconnect e l
              `catchAll` \ex -> do
                logError l $ "Control connection reconnect aborted: " <> string8 (show ex)
                atomically $ modifyTVar' (e^.control) (set state Disconnected)
  where
    setReconnecting e = atomically $ do
        ctrl <- readTVar (e^.control)
        if ctrl^.state /= Reconnecting
            then do
                writeTVar (e^.control) (set state Reconnecting ctrl)
                return $ Just (ctrl^.connection)
            else
                return Nothing

    reconnect e l = recovering adInf (onExc l) $ \_ -> do
        hosts <- NE.nonEmpty . Map.keys <$> readTVarIO (e^.hostmap)
        case hosts of
            Just hs -> hs `tryAll` (runClient e . renewControl)
                `catch` \x -> case fromException x of
                    Just (SomeAsyncException _) -> throwM x
                    Nothing                     -> do
                        logError l "All known hosts unreachable."
                        runClient e rebootControl
            Nothing -> do
                logError l "No known hosts."
                runClient e rebootControl

    adInf = capDelay 5000000 (exponentialBackoff 5000)

    onExc l =
        [ const $ Handler $ \(_ :: SomeAsyncException) -> return False
        , const $ Handler $ \(e :: SomeException)      -> do
            logError l $ "Replacement of control connection failed with: "
                <> string8 (show e)
                <> ". Retrying ..."
            return True
        ]

-- | Create a new connection to a known host and set it up
-- as the new control connection.
renewControl :: Host -> Client ()
renewControl h = do
    ctx <- view context
    logInfo' "Renewing control connection with known host ..."
    let s = ctx^.settings
    bracketOnError
        (C.connect (s^.connSettings) (ctx^.timeouts) (s^.protoVersion) (s^.logger) h)
        (liftIO . C.close)
        setupControl

-- | Create a new connection to one of the initial contacts
-- and set it up as the new control connection.
rebootControl :: Client ()
rebootControl = do
    e <- ask
    logInfo' "Renewing control connection with initial contacts ..."
    bracketOnError
        (liftIO (mkContact (e^.context)))
        (liftIO . C.close)
        setupControl

-----------------------------------------------------------------------------
-- Event handling

onCqlEvent :: Event -> Client ()
onCqlEvent x = do
    logInfo' $ "Event: " <> string8 (show x)
    pol <- view policy
    prt <- view (context.settings.portnumber)
    case x of
        StatusEvent Down (sock2inet prt -> a) ->
            liftIO $ onEvent pol (HostDown a)
        TopologyEvent RemovedNode (sock2inet prt -> a) -> do
            hmap <- view hostmap
            atomically' $
                modifyTVar' hmap (Map.filterWithKey (\h _ -> h^.hostAddr /= a))
            liftIO $ onEvent pol (HostGone a)
        StatusEvent Up (sock2inet prt -> a) -> do
            s <- ask
            startMonitor s a
        TopologyEvent NewNode (sock2inet prt -> a) -> do
            s <- ask
            let ctx  = s^.context
            let hmap = s^.hostmap
            ctrl <- readTVarIO' (s^.control)
            let c = ctrl^.connection
            peers <- liftIO $ discoverPeers ctx c `recover` []
            let h = fromMaybe (Host a "" "") $ find ((a == ) . view hostAddr) peers
            okay <- liftIO $ acceptable pol h
            when okay $ do
                p <- mkPool ctx h
                atomically' $ modifyTVar' hmap (Map.alter (maybe (Just p) Just) h)
                liftIO $ onEvent pol (HostNew h)
                tryRunJob_ (s^.jobs) a $ runClient s (prepareAllQueries h)
        SchemaEvent _ -> return ()
  where
    startMonitor s a = do
        hmp <- readTVarIO' (s^.hostmap)
        case find ((a ==) . view hostAddr) (Map.keys hmp) of
            Just h -> tryRunJob_ (s^.jobs) a $ runClient s $ do
                monitor (Ms 3000) (Ms 60000) h
                prepareAllQueries h
            Nothing -> return ()

-----------------------------------------------------------------------------
-- Utilities

-- | Get the 'Result' out of a 'HostResponse'. If the response is an 'RsError',
-- a 'ResponseError' is thrown. If the response is neither
-- 'RsResult' nor 'RsError', an 'UnexpectedResponse' is thrown.
getResult :: MonadThrow m => HostResponse k a b -> m (Result k a b)
getResult (HostResponse _ (RsResult _ _ r)) = return r
getResult (HostResponse h (RsError  t w e)) = throwM (ResponseError h t w e)
getResult hr                                = unexpected hr
{-# INLINE getResult #-}

getPreparedQueryId :: MonadThrow m => HostResponse k a b -> m (Host, QueryId k a b)
getPreparedQueryId hr = getResult hr >>= \case
    PreparedResult i _ _ -> return (hrHost hr, i)
    _                    -> unexpected hr
{-# INLINE getPreparedQueryId #-}

unexpected :: MonadThrow m => HostResponse k a b -> m c
unexpected (HostResponse h r) = throwM $ UnexpectedResponse h r

atomically' :: STM a -> Client a
atomically' = liftIO . atomically

readTVarIO' :: TVar a -> Client a
readTVarIO' = liftIO . readTVarIO

logInfo' :: Builder -> Client ()
logInfo' m = do
    l <- view (context.settings.logger)
    liftIO $ logInfo l m
{-# INLINE logInfo' #-}

logWarn' :: Builder -> Client ()
logWarn' m = do
    l <- view (context.settings.logger)
    liftIO $ logWarn l m
{-# INLINE logWarn' #-}

logError' :: Builder -> Client ()
logError' m = do
    l <- view (context.settings.logger)
    liftIO $ logError l m
{-# INLINE logError' #-}

