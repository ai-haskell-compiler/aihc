{-# LANGUAGE OverloadedStrings #-}

{- |
Module      : Main
License     : BSD-style
Copyright   : (c) 2010-2019 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown

An example application.
-}

module Main
  ( main
  ) where

import           Data.ByteString.Char8 ( pack )
import           Network.BSD ( getHostByName, hostAddresses )
import           Network.Socket
                   ( Family (..), SockAddr (..), SocketType (..), close
                   , defaultProtocol
                   )
import qualified Network.Socket as Network
import           Network.Socket.ByteString ( recv, sendAll )
import           Network.Socks5
                   ( SocksAddress (..), SocksHostAddress (..), defaultSocksConf
                   , socksConnect, socksConnectName
                   )
import           System.Environment ( getArgs )

main :: IO ()
main = do
  args <- getArgs
  let serverName = "localhost"
      serverPort = 1080
      destinationName = case args of
        []    -> "www.google.com"
        (x:_) -> x
  -- A SOCKS server is expected to be running on localhost port 1080.
  he <- getHostByName serverName
  case hostAddresses he of
    [] -> putStrLn "Error: expected a host address."
    (ha:_) -> do
      let socksServerAddr = SockAddrInet serverPort ha
      example1 socksServerAddr destinationName
      example2 socksServerAddr destinationName
 where
  -- Connect to @destName on port 80 through the SOCKS server. www.google.com
  -- gets resolved on the client here and then the sockaddr is passed to
  -- socksConnectAddr.
  example1 socksServerAddr destName = do
    (socket, _) <- socksConnect
      (defaultSocksConf socksServerAddr)
      (SocksAddress (SocksAddrDomainName $ pack destName) 80)
    sendAll socket "GET / HTTP/1.0\r\n\r\n"
    recv socket 4096 >>= print
    close socket

  -- Connect to @destName on port 80 through the SOCKS server. The server is
  -- doing the resolution itself.
  example2 socksServerAddr destName = do
    socket <- Network.socket AF_INET Stream defaultProtocol
    socksConnectName socket (defaultSocksConf socksServerAddr) destName 80
    sendAll socket "GET / HTTP/1.0\r\n\r\n"
    recv socket 4096 >>= print
    close socket
