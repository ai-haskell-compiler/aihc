{-|

Module      : Netcode.IO
Description : High-level bindings to the netcode.io library.
Copyright   : (c) Pavel Krajcevski, 2020
License     : BSD-3
Maintainer  : krajcevski@gmail.com
Stability   : experimental
Portability : Portable

This module contains the high-level bindings on top of the module
"Bindings.Netcode.IO". These provide a cleaner interface to the
<https://github.com/networkprotocol/netcode.io netcode.io> C library and are
the recommended interface for application developers.

These bindings have some limitations. Namely, they are not as performant as
the "close to the metal" bindings provided in "Bindings.Netcode.IO". In the
event that you need more performance, that module is available for use.

The general architecture of a @netcode.io@ application is outlined in the C
library
<https://github.com/networkprotocol/netcode.io/blob/master/STANDARD.md#architecture documentation>. The jist is that we need three main entities:

 * A server to connect to
 * An authentication server to dispatch connection tokens
 * A client that wants to connect to a server

In this case, the client will request a connection token from the authentication
server. The authentication server will know a-priori what the available servers
are for the client to connect to. These servers (and other information) are
stored in an encrypted 'ConnectToken', based on the 64-bit unique client ID. The
authentication server will send a 'ConnectToken' to a client when that client
looks for servers to connect to.

Once a connection between server and client is established, they may exchange
packets of information. These packets are sent over UDP and therefore are not
guaranteed to arrive in order or even arrive at all. In order to create a
reliable information channel between client and server, it is recommended to
use this library in conjunction with the
<https://github.com/networkprotocol/reliable.io reliable.io> library.
-}

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO (
    -- * Initialization
      initialize
    , terminate

    -- * Addresses
    , Address
    , AddressMode(..)
    , addressMode
    , addressPort
    , addressValues
    , constructAddress
    , parseAddress
    , addressToString
    , addressEqual

    -- * Common callbacks
    , SendPacketOverride
    , ReceivePacketOverride

    -- * Packets
    , module Netcode.IO.Packet
    , module Netcode.IO.Client
    , module Netcode.IO.Server

    -- * Utilities
    , sleep

    , LogLevel(..), logLevel
) where
--------------------------------------------------------------------------------

import Control.Monad         (unless)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Foreign.C.Types       (CDouble(..))
import GHC.Generics          (Generic)

import Bindings.Netcode.IO
import Netcode.IO.Address
import Netcode.IO.Callbacks
import Netcode.IO.Client
import Netcode.IO.Packet
import Netcode.IO.Server

--------------------------------------------------------------------------------

-- | Initializes the @netcode.io@ library runtime. This should be called before
-- any additional functions in this library. Throws an
-- t'Control.Exception.IOException' on failure.
initialize :: IO ()
initialize = do
    result <- c'netcode_init
    unless (result == c'NETCODE_OK) $
      fail $ "Failed to initialize netcode.io. Result: " <> show result
    return ()

-- | Terminates the @netcode.io@ library runtime. This should be called only
-- after all other library functions terminate.
terminate :: IO ()
terminate = c'netcode_term

-- | Specifies the logging behavior of @netcode.io@. Note, this logging behavior
-- is called from C calls to @printf@ and therefore might interfere with the
-- Haskell runtime (such as 'putStrLn').
data LogLevel = LogLevel'None
              | LogLevel'Info
              | LogLevel'Error
              | LogLevel'Debug
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

-- | Set the @netcode.io@ 'LogLevel'. The default is 'LogLevel'None'.
logLevel :: LogLevel -> IO ()
logLevel LogLevel'None  = c'netcode_log_level c'NETCODE_LOG_LEVEL_NONE
logLevel LogLevel'Info  = c'netcode_log_level c'NETCODE_LOG_LEVEL_INFO
logLevel LogLevel'Error = c'netcode_log_level c'NETCODE_LOG_LEVEL_ERROR
logLevel LogLevel'Debug = c'netcode_log_level c'NETCODE_LOG_LEVEL_DEBUG

-- | Sleep the current thread. This is usually only used in example programs.
-- It's probably safer to use the built-in 'Control.Concurrent.threadDelay'.
sleep :: Double -> IO ()
sleep = c'netcode_sleep . CDouble
