--------------------------------------------------------------------------------
-- Logstash client for Haskell                                                --
--------------------------------------------------------------------------------
-- This source code is licensed under the MIT license found in the LICENSE    --
-- file in the root directory of this source tree.                            --
--------------------------------------------------------------------------------

-- | This module implements a logstash client for the tcp input plugin:
-- https://www.elastic.co/guide/en/logstash/7.10/plugins-inputs-tcp.html
module Logstash.TCP (
    LogstashTcpConfig(..),
    logstashTcp,
    logstashTcpPool,
    logstashTls,
    logstashTlsPool
) where 

--------------------------------------------------------------------------------

import Control.Monad.IO.Class

import Data.Acquire
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Default.Class
import Data.Time
import Data.Pool

import Network.Socket
import Network.Socket.ByteString (sendAll)
import Network.TLS

import Logstash.Connection

--------------------------------------------------------------------------------

-- | Represents configurations for Logstash TCP inputs.
data LogstashTcpConfig = LogstashTcpConfig {
    -- | The hostname of the server to connect to.
    logstashTcpHost :: String,
    -- | The port of the server to connect to.
    logstashTcpPort :: Int
} deriving (Eq, Show)

instance Default LogstashTcpConfig where 
    def = LogstashTcpConfig{
        logstashTcpHost = "127.0.0.1",
        logstashTcpPort = 5000
    }

-- | `connectTCP` @config@ establishes a TCP socket connection to the server
-- configured by @config@.
connectTcpSocket 
    :: MonadIO m 
    => LogstashTcpConfig 
    -> m Socket
connectTcpSocket LogstashTcpConfig{..} = liftIO $ withSocketsDo $ do
    -- initialise a TCP socket for the given host and port
    let hints = defaultHints{ addrSocketType = Stream }
    addr <- head <$> getAddrInfo 
        (Just hints) (Just logstashTcpHost) (Just $ show logstashTcpPort)
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    connect sock $ addrAddress addr

    -- return the socket
    pure sock

-- | `createTcpConnection` @config@ establishes a `LogstashConnection` via
-- TCP to the server configured by @config@.
createTcpConnection 
    :: MonadIO m 
    => LogstashTcpConfig 
    -> m LogstashConnection
createTcpConnection cfg = do 
    -- establish a TCP connection
    sock <- connectTcpSocket cfg

    -- return the Logstash connection
    pure $ LogstashConnection{
        writeData = sendAll sock . BSL.toStrict,
        closeConnection = close sock
    }

-- | `logstashTcp` @config@ produces an `Acquire` for establishing
-- `LogstashConnection` values for the given TCP @config@.
logstashTcp 
    :: LogstashTcpConfig 
    -> Acquire LogstashConnection
logstashTcp cfg = mkAcquire (createTcpConnection cfg) closeConnection 

-- | `logstashTcpPool` @config stripes ttl resources@ produces a `Pool`
-- of `LogstashConnection` values for the given TCP @config@. The other
-- parameters are passed on directly to `createPool`.
logstashTcpPool 
    :: LogstashTcpConfig
    -> Int 
    -> NominalDiffTime
    -> Int 
    -> IO LogstashPool
logstashTcpPool cfg = createPool (createTcpConnection cfg) closeConnection

--------------------------------------------------------------------------------

-- | `createTlsConnection` @config params@ establishes a `LogstashConnection` via
-- TLS to the server configured by @config@ and the TLS configuration given by
-- @params@.
createTlsConnection 
    :: MonadIO m 
    => LogstashTcpConfig 
    -> ClientParams
    -> m LogstashConnection
createTlsConnection cfg params = do 
    -- establish a TCP connection
    sock <- connectTcpSocket cfg

    -- establish a TLS connection on top
    ctx <- contextNew sock params
    handshake ctx

    -- return the Logstash connection
    pure $ LogstashConnection{
        writeData = sendData ctx,
        closeConnection = do 
            bye ctx 
            close sock
    }

-- | `logstashTls` @config params@ produces an `Acquire` for establishing
-- `LogstashConnection` values for the given TCP @config@ and TLS @params@.
logstashTls 
    :: LogstashTcpConfig 
    -> ClientParams 
    -> Acquire LogstashConnection 
logstashTls cfg params = 
    mkAcquire (createTlsConnection cfg params) closeConnection 

-- | `logstashTlsPool` @config params stripes ttl resources@ produces a 
-- `Pool` of `LogstashConnection` values for the given TCP @config@ and TLS
-- @params@. The other parameters are passed on directly to `createPool`.
logstashTlsPool
    :: LogstashTcpConfig 
    -> ClientParams 
    -> Int 
    -> NominalDiffTime
    -> Int
    -> IO LogstashPool
logstashTlsPool cfg params = 
    createPool (createTlsConnection cfg params) closeConnection 

--------------------------------------------------------------------------------
