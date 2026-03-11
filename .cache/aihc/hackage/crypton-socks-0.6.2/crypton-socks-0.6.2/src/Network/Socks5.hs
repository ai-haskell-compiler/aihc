{- |
Module      : Network.Socks5
License     : BSD-style
Copyright   : (c) 2010-2019 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

This is an implementation of the SOCKS Protocol Version 5 as defined in
[RFC 1928](https://datatracker.ietf.org/doc/html/rfc1928).

In Wikipedia's words:

  SOCKS is an Internet protocol that exchanges network packets between a client
  and server through a proxy server. SOCKS5 optionally provides authentication
  so only authorized users may access a server. Practically, a SOCKS server
  proxies TCP connections to an arbitrary IP address, and provides a means for
  UDP packets to be forwarded. A SOCKS server accepts incoming client connection
  on TCP port 1080.

BIND and UDP ASSOCIATE messages are not implemented. However the main usage of
SOCKS is implemented.
-}

module Network.Socks5
  ( -- * Types
    SocksAddress (..)
  , SocksHostAddress (..)
  , SocksReply (..)
  , SocksError (..)
    -- * Configuration
  , SocksConf (..)
  , socksHost
  , defaultSocksConf
  , defaultSocksConfFromSockAddr
    -- * Methods
  , socksConnectWithSocket
  , socksConnect
    -- * Variants
  , socksConnectName
  ) where

import           Control.Exception ( bracketOnError )
import           Control.Monad ( when )
import           Data.ByteString.Char8  ( pack )
import           Network.Socket
                   ( close, Socket, SocketType(..), Family(..), socket, connect
                   , PortNumber, defaultProtocol
                   )
import           Network.Socks5.Command ( Connect (..), establish, rpc_ )
import           Network.Socks5.Conf
                   ( SocksConf (..), defaultSocksConf
                   , defaultSocksConfFromSockAddr, socksHost
                   )
import           Network.Socks5.Types
                   ( SocksAddress (..), SocksError (..), SocksHostAddress (..)
                   , SocksMethod (..), SocksReply (..)
                   )

-- | Connect a user-specified new socket on the SOCKS server to a destination.
--
-- The specified socket needs to be connected to the SOCKS server already.
--
-- |socket|-----sockServer----->|server|----destAddr----->|destination|
--
socksConnectWithSocket ::
     Socket       -- ^ The socket to use.
  -> SocksConf    -- ^ The SOCKS configuration for the server.
  -> SocksAddress -- ^ The SOCKS address to connect to.
  -> IO (SocksHostAddress, PortNumber)
socksConnectWithSocket sock serverConf destAddr = do
  r <- establish (socksVersion serverConf) sock [SocksMethodNone]
  when (r == SocksMethodNotAcceptable) $
    error "cannot connect with no socks method of authentication"
  rpc_ sock (Connect destAddr)

-- | Connect a new socket to a SOCKS server and connect the stream on the
-- server side to the specified SOCKS address.
socksConnect ::
     SocksConf
     -- ^ The SOCKS configuration for the server.
  -> SocksAddress
     -- ^ The SOCKS address to connect to.
  -> IO (Socket, (SocksHostAddress, PortNumber))
socksConnect serverConf destAddr =
  bracketOnError (socket AF_INET Stream defaultProtocol) close $ \sock -> do
    connect sock (socksServer serverConf)
    ret <- socksConnectWithSocket sock serverConf destAddr
    return (sock, ret)

-- | Connect a new socket to the SOCKS server, and connect the stream to a
-- fully-qualified domain name (FQDN) resolved on the server side.
socksConnectName ::
     Socket
     -- ^ The socket to use. The socket must *not* be connected already.
  -> SocksConf
     -- ^ The SOCKS configuration for the server.
  -> String
     -- ^ Destination FQDN. Should comprise only ASCII characters, otherwise
     -- unexpected behaviour will ensure. For FQDN including other Unicode code
     -- points, Punycode encoding should be used.
  -> PortNumber
     -- ^ The port number to use.
  -> IO ()
socksConnectName sock sockConf destination port = do
  connect sock (socksServer sockConf)
  (_, _) <- socksConnectWithSocket sock sockConf addr
  return ()
 where
  addr = SocksAddress (SocksAddrDomainName $ pack destination) port
