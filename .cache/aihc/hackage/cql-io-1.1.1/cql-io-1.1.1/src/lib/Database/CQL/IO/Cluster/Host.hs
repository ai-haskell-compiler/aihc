-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.CQL.IO.Cluster.Host where

import Control.Lens (Lens')
import Database.CQL.Protocol (Response (..))
import Database.CQL.IO.Cluster.Discovery
import Data.IP
import Data.Text (Text, unpack)
import Network.Socket (SockAddr (..), PortNumber)

-- | A Cassandra host known to the client.
data Host = Host
    { _hostAddr   :: !InetAddr
    , _dataCentre :: !Text
    , _rack       :: !Text
    }

instance Eq Host where
    a == b = _hostAddr a == _hostAddr b

instance Ord Host where
    compare a b = compare (_hostAddr a) (_hostAddr b)

peer2Host :: PortNumber -> Peer -> Host
peer2Host i p = Host (ip2inet i (peerRPC p)) (peerDC p) (peerRack p)

updateHost :: Host -> Maybe (Text, Text) -> Host
updateHost h (Just (dc, rk)) = h { _dataCentre = dc, _rack = rk }
updateHost h Nothing         = h

-- | A response that is known to originate from a specific 'Host'.
data HostResponse k a b = HostResponse
    { hrHost     :: !Host
    , hrResponse :: !(Response k a b)
    } deriving (Show)

-- | This event will be passed to a 'Policy' to inform it about
-- cluster changes.
data HostEvent
    = HostNew  !Host     -- ^ a new host has been added to the cluster
    | HostGone !InetAddr -- ^ a host has been removed from the cluster
    | HostUp   !InetAddr -- ^ a host has been started
    | HostDown !InetAddr -- ^ a host has been stopped

-- | The IP address and port number of a host.
hostAddr :: Lens' Host InetAddr
hostAddr f ~(Host a c r) = fmap (\x -> Host x c r) (f a)
{-# INLINE hostAddr #-}

-- | The data centre name (may be an empty string).
dataCentre :: Lens' Host Text
dataCentre f ~(Host a c r) = fmap (\x -> Host a x r) (f c)
{-# INLINE dataCentre #-}

-- | The rack name (may be an empty string).
rack :: Lens' Host Text
rack f ~(Host a c r) = fmap (\x -> Host a c x) (f r)
{-# INLINE rack #-}

instance Show Host where
    show h = showString (unpack (_dataCentre h))
           . showString ":"
           . showString (unpack (_rack h))
           . showString ":"
           . shows (_hostAddr h)
           $ ""

-----------------------------------------------------------------------------
-- InetAddr

newtype InetAddr = InetAddr { sockAddr :: SockAddr }
    deriving (Eq, Ord)

instance Show InetAddr where
    show (InetAddr (SockAddrInet p a)) =
        let i = fromIntegral p :: Int in
        shows (fromHostAddress a) . showString ":" . shows i $ ""
    show (InetAddr (SockAddrInet6 p _ a _)) =
        let i = fromIntegral p :: Int in
        shows (fromHostAddress6 a) . showString ":" . shows i $ ""
    show (InetAddr (SockAddrUnix unix)) = unix
#if MIN_VERSION_network(2,6,1) && !MIN_VERSION_network(3,0,0)
    show (InetAddr (SockAddrCan int32)) = show int32
#endif

-- | Map a 'SockAddr' into an 'InetAddr', using the given port number.
sock2inet :: PortNumber -> SockAddr -> InetAddr
sock2inet i (SockAddrInet _ a)      = InetAddr (SockAddrInet i a)
sock2inet i (SockAddrInet6 _ f a b) = InetAddr (SockAddrInet6 i f a b)
sock2inet _ unix                    = InetAddr unix

-- | Map an 'IP' into an 'InetAddr', using the given port number.
ip2inet :: PortNumber -> IP -> InetAddr
ip2inet p (IPv4 a) = InetAddr $ SockAddrInet p (toHostAddress a)
ip2inet p (IPv6 a) = InetAddr $ SockAddrInet6 p 0 (toHostAddress6 a) 0

