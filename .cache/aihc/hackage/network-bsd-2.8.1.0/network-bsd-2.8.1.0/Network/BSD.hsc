{-# LANGUAGE CPP, NondecreasingIndentation, DeriveDataTypeable, BangPatterns #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Network.BSD
-- Copyright   :  (c) The University of Glasgow 2001
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Maintainer  :  libraries@haskell.org
-- Stability   :  experimental
-- Portability :  non-portable
--
-- The "Network.BSD" module defines Haskell bindings to network
-- programming functionality (mostly [network database operations](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/netdb.h.html))
-- provided by BSD Unix derivatives.
--
-- __NOTE__: Some of the types are reexported from "Network.Socket" in order to make the @network-bsd@ API self-contained.
--
-- == Windows compatibility
--
-- The following functions are not exported by "Network.BSD" on the
-- Windows platform:
--
-- * 'getHostEntries', 'setHostEntry', 'getHostEntry', 'endHostEntry'
-- * 'getServiceEntries', 'getServiceEntry', 'setServiceEntry', 'endServiceEntry'
-- * 'getProtocolEntries', 'setProtocolEntry', 'getProtocolEntry', 'endProtocolEntry'
-- * 'getNetworkByName', 'getNetworkByAddr', 'getNetworkEntries',
--   'setNetworkEntry', 'getNetworkEntry', 'endNetworkEntry'
--
-----------------------------------------------------------------------------

#include "HsNet.h"
##include "HsNetDef.h"

module Network.BSD
    (
    -- * Host names and network addresses
      N.HostName
    , N.HostAddress
      -- NB: We're explicit here to reduce the risk of inadvertently leaking through new constructors w/o reflecting this in @network-bsd@'s API version
    , N.Family(AF_UNSPEC, AF_UNIX, AF_INET, AF_INET6, AF_IMPLINK, AF_PUP, AF_CHAOS, AF_NS, AF_NBS, AF_ECMA, AF_DATAKIT, AF_CCITT, AF_SNA, AF_DECnet, AF_DLI, AF_LAT, AF_HYLINK, AF_APPLETALK, AF_ROUTE, AF_NETBIOS, AF_NIT, AF_802, AF_ISO, AF_OSI, AF_NETMAN, AF_X25, AF_AX25, AF_OSINET, AF_GOSSIP, AF_IPX, Pseudo_AF_XTP, AF_CTF, AF_WAN, AF_SDL, AF_NETWARE, AF_NDD, AF_INTF, AF_COIP, AF_CNT, Pseudo_AF_RTIP, Pseudo_AF_PIP, AF_SIP, AF_ISDN, Pseudo_AF_KEY, AF_NATM, AF_ARP, Pseudo_AF_HDRCMPLT, AF_ENCAP, AF_LINK, AF_RAW, AF_RIF, AF_NETROM, AF_BRIDGE, AF_ATMPVC, AF_ROSE, AF_NETBEUI, AF_SECURITY, AF_PACKET, AF_ASH, AF_ECONET, AF_ATMSVC, AF_IRDA, AF_PPPOX, AF_WANPIPE, AF_BLUETOOTH, AF_CAN)
    , getHostName

    , HostEntry(..)
    , getHostByName
    , getHostByAddr
    , hostAddress

#if defined(HAVE_GETHOSTENT) && !defined(mingw32_HOST_OS)
    , getHostEntries

    -- ** Low level functionality
    , setHostEntry
    , getHostEntry
    , endHostEntry
#endif

    -- * Service names
    , ServiceEntry(..)
    , N.ServiceName
    , N.PortNumber
    , getServiceByName
    , getServiceByPort
    , getServicePortNumber

#if !defined(mingw32_HOST_OS)
    , getServiceEntries

    -- ** Low level functionality
    , getServiceEntry
    , setServiceEntry
    , endServiceEntry
#endif

    -- * Protocol names
    , ProtocolName
    , N.ProtocolNumber
    , ProtocolEntry(..)
    , getProtocolByName
    , getProtocolByNumber
    , getProtocolNumber
    , N.defaultProtocol

#if !defined(mingw32_HOST_OS)
    , getProtocolEntries
    -- ** Low level functionality
    , setProtocolEntry
    , getProtocolEntry
    , endProtocolEntry
#endif

    -- * Network names
    , NetworkName
    , NetworkAddr
    , NetworkEntry(..)

#if !defined(mingw32_HOST_OS)
    , getNetworkByName
    , getNetworkByAddr
    , getNetworkEntries
    -- ** Low level functionality
    , setNetworkEntry
    , getNetworkEntry
    , endNetworkEntry
#endif

    -- * Interface names
    , N.ifNameToIndex

    ) where

import qualified Network.Socket as N

import Control.Concurrent (MVar, newMVar, withMVar)
import qualified Control.Exception as E
import Foreign.C.String (CString, peekCString, withCString)
#if defined(mingw32_HOST_OS)
import Foreign.C.Types ( CShort )
#endif
import Foreign.C.Types ( CInt(..), CULong(..), CSize(..) )
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (allocaArray0, peekArray0)
import Foreign.Marshal.Utils (with, fromBool)
import Data.Typeable
import System.IO.Error (ioeSetErrorString, mkIOError)
import System.IO.Unsafe (unsafePerformIO)

import GHC.IO.Exception

import Control.DeepSeq (NFData(rnf))
import Control.Monad (liftM)

import Network.Socket.Internal (throwSocketErrorIfMinus1_)

-- ---------------------------------------------------------------------------
-- Basic Types

type ProtocolName = String

-- ---------------------------------------------------------------------------
-- Service Database Access

-- Calling getServiceByName for a given service and protocol returns
-- the systems service entry.  This should be used to find the port
-- numbers for standard protocols such as SMTP and FTP.  The remaining
-- three functions should be used for browsing the service database
-- sequentially.

-- Calling setServiceEntry with True indicates that the service
-- database should be left open between calls to getServiceEntry.  To
-- close the database a call to endServiceEntry is required.  This
-- database file is usually stored in the file /etc/services.

-- | Representation of the POSIX @servent@ structure defined in [<netdb.h>](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/netdb.h.html).
data ServiceEntry  =
  ServiceEntry  {
     serviceName     :: N.ServiceName,    -- ^ Official service name
     serviceAliases  :: [N.ServiceName],  -- ^ aliases
     servicePort     :: N.PortNumber,     -- ^ Port Number
     serviceProtocol :: ProtocolName      -- ^ Protocol to use
  } deriving (Show, Typeable)

-- | @since 2.8.1.0
instance NFData ServiceEntry where
   -- TODO: PortNumber is a newtype over Word16; add NFData instance to `network`
   rnf (ServiceEntry n a !_pn pr) = rnf (n,a,pr)

instance Storable ServiceEntry where
   sizeOf    _ = #const sizeof(struct servent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        s_name    <- (#peek struct servent, s_name) p >>= peekCString
        s_aliases <- (#peek struct servent, s_aliases) p
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString
        s_port    <- (#peek struct servent, s_port) p
        s_proto   <- (#peek struct servent, s_proto) p >>= peekCString
        return (ServiceEntry {
                        serviceName     = s_name,
                        serviceAliases  = s_aliases,
#if defined(mingw32_HOST_OS)
                        servicePort     = (fromIntegral (s_port :: CShort)),
#else
                           -- s_port is already in network byte order, but it
                           -- might be the wrong size.
                        servicePort     = (fromIntegral (s_port :: CInt)),
#endif
                        serviceProtocol = s_proto
                })

   poke = throwUnsupportedOperationPoke "ServiceEntry"


-- | Get service by name.
getServiceByName :: N.ServiceName         -- Service Name
                 -> ProtocolName        -- Protocol Name
                 -> IO ServiceEntry     -- Service Entry
getServiceByName name proto = withLock $ do
 withCString name  $ \ cstr_name  -> do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "Network.BSD.getServiceByName" "no such service entry"
   $ c_getservbyname cstr_name cstr_proto
 >>= peek

foreign import CALLCONV unsafe "getservbyname"
  c_getservbyname :: CString -> CString -> IO (Ptr ServiceEntry)

-- | Get the service given a 'N.PortNumber' and 'ProtocolName'.
getServiceByPort :: N.PortNumber -> ProtocolName -> IO ServiceEntry
getServiceByPort port proto = withLock $ do
 withCString proto $ \ cstr_proto -> do
 throwNoSuchThingIfNull "Network.BSD.getServiceByPort" "no such service entry"
   $ c_getservbyport (fromIntegral port) cstr_proto
 >>= peek

foreign import CALLCONV unsafe "getservbyport"
  c_getservbyport :: CInt -> CString -> IO (Ptr ServiceEntry)

-- | Get the 'N.PortNumber' corresponding to the 'N.ServiceName'.
getServicePortNumber :: N.ServiceName -> IO N.PortNumber
getServicePortNumber name = do
    (ServiceEntry _ _ port _) <- getServiceByName name "tcp"
    return port

#if !defined(mingw32_HOST_OS)

-- | @getservent(3)@.
getServiceEntry :: IO ServiceEntry
getServiceEntry = withLock getServiceEntry'

getServiceEntry' :: IO ServiceEntry
getServiceEntry' = do
  throwNoSuchThingIfNull "Network.BSD.getServiceEntry" "no such service entry"
    $ c_getservent
  >>= peek

foreign import ccall unsafe "getservent" c_getservent :: IO (Ptr ServiceEntry)

-- | @setservent(3)@.
setServiceEntry :: Bool -> IO ()
setServiceEntry = withLock . setServiceEntry'

setServiceEntry' :: Bool -> IO ()
setServiceEntry' flg = c_setservent (fromBool flg)

foreign import ccall unsafe  "setservent" c_setservent :: CInt -> IO ()

-- | @endservent(3)@.
endServiceEntry :: IO ()
endServiceEntry = withLock $ c_endservent

foreign import ccall unsafe  "endservent" c_endservent :: IO ()

-- | Retrieve list of all 'ServiceEntry' via @getservent(3)@.
getServiceEntries :: Bool -> IO [ServiceEntry]
getServiceEntries stayOpen = withLock $ do
  setServiceEntry' stayOpen
  getEntries getServiceEntry' c_endservent
#endif

-- ---------------------------------------------------------------------------
-- Protocol Entries

-- The following relate directly to the corresponding UNIX C
-- calls for returning the protocol entries. The protocol entry is
-- represented by the Haskell type ProtocolEntry.

-- As for setServiceEntry above, calling setProtocolEntry.
-- determines whether or not the protocol database file, usually
-- @/etc/protocols@, is to be kept open between calls of
-- getProtocolEntry. Similarly,

-- | Representation of the POSIX @protoent@ structure defined in [<netdb.h>](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/netdb.h.html).
data ProtocolEntry =
  ProtocolEntry  {
     protoName    :: ProtocolName,      -- ^ Official name
     protoAliases :: [ProtocolName],    -- ^ aliases
     protoNumber  :: N.ProtocolNumber   -- ^ Protocol number
  } deriving (Read, Show, Typeable)

-- | @since 2.8.1.0
instance NFData ProtocolEntry where
   -- NB: deepseq-1.3 didn't have `NFData CInt` yet; but we don't need it
   rnf (ProtocolEntry na a !_nu) = rnf (na,a)

instance Storable ProtocolEntry where
   sizeOf    _ = #const sizeof(struct protoent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        p_name    <- (#peek struct protoent, p_name) p >>= peekCString
        p_aliases <- (#peek struct protoent, p_aliases) p
                           >>= peekArray0 nullPtr
                           >>= mapM peekCString
#if defined(mingw32_HOST_OS)
         -- With WinSock, the protocol number is only a short;
         -- hoist it in as such, but represent it on the Haskell side
         -- as a CInt.
        p_proto_short  <- (#peek struct protoent, p_proto) p
        let p_proto = fromIntegral (p_proto_short :: CShort)
#else
        p_proto        <- (#peek struct protoent, p_proto) p
#endif
        return (ProtocolEntry {
                        protoName    = p_name,
                        protoAliases = p_aliases,
                        protoNumber  = p_proto
                })

   poke = throwUnsupportedOperationPoke "ProtocolEntry"


-- | @getprotobyname(3)@.
getProtocolByName :: ProtocolName -> IO ProtocolEntry
getProtocolByName name = withLock $ do
 withCString name $ \ name_cstr -> do
 throwNoSuchThingIfNull "Network.BSD.getProtocolByName" ("no such protocol name: " ++ name)
   $ c_getprotobyname name_cstr
 >>= peek

foreign import  CALLCONV unsafe  "getprotobyname"
   c_getprotobyname :: CString -> IO (Ptr ProtocolEntry)

-- | @getprotobynumber(3)@.
getProtocolByNumber :: N.ProtocolNumber -> IO ProtocolEntry
getProtocolByNumber num = withLock $ do
 throwNoSuchThingIfNull "Network.BSD.getProtocolByNumber" ("no such protocol number: " ++ show num)
   $ c_getprotobynumber (fromIntegral num)
 >>= peek

foreign import CALLCONV unsafe  "getprotobynumber"
   c_getprotobynumber :: CInt -> IO (Ptr ProtocolEntry)

-- | @getprotobyname(3)@.
getProtocolNumber :: ProtocolName -> IO N.ProtocolNumber
getProtocolNumber proto = do
 (ProtocolEntry _ _ num) <- getProtocolByName proto
 return num

#if !defined(mingw32_HOST_OS)
-- | @getprotoent(3)@.
getProtocolEntry :: IO ProtocolEntry    -- Next Protocol Entry from DB
getProtocolEntry = withLock getProtocolEntry'

getProtocolEntry' :: IO ProtocolEntry
getProtocolEntry' = do
  ent <- throwNoSuchThingIfNull "Network.BSD.getProtocolEntry" "no such protocol entry"
           $ c_getprotoent
  peek ent

foreign import ccall unsafe  "getprotoent" c_getprotoent :: IO (Ptr ProtocolEntry)

-- | @setprotoent(3)@.
setProtocolEntry :: Bool -> IO ()       -- Keep DB Open ?
setProtocolEntry = withLock . setProtocolEntry'

setProtocolEntry' :: Bool -> IO ()       -- Keep DB Open ?
setProtocolEntry' flg = c_setprotoent (fromBool flg)

foreign import ccall unsafe "setprotoent" c_setprotoent :: CInt -> IO ()

-- | @endprotoent(3)@.
endProtocolEntry :: IO ()
endProtocolEntry = withLock $ c_endprotoent

foreign import ccall unsafe "endprotoent" c_endprotoent :: IO ()

-- | Retrieve list of all 'ProtocolEntry' via @getprotoent(3)@.
getProtocolEntries :: Bool -> IO [ProtocolEntry]
getProtocolEntries stayOpen = withLock $ do
  setProtocolEntry' stayOpen
  getEntries getProtocolEntry' c_endprotoent

#endif

-- ---------------------------------------------------------------------------
-- Host lookups

-- | Representation of the POSIX @hostent@ structure defined in [<netdb.h>](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/netdb.h.html).
data HostEntry =
  HostEntry  {
     hostName      :: N.HostName,         -- ^ Official name of the host
     hostAliases   :: [N.HostName],       -- ^ Alternative names of the host
     hostFamily    :: N.Family,           -- ^ Address type (currently @AF_INET@)
     hostAddresses :: [N.HostAddress]     -- ^ Set of network addresses for the host
  } deriving (Read, Show, Typeable)

-- | @since 2.8.1.0
instance NFData HostEntry where
   -- TODO: NFData N.Family
   rnf (HostEntry n al !_f ad) = rnf (n,al,ad)

instance Storable HostEntry where
   sizeOf    _ = #const sizeof(struct hostent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        h_name       <- (#peek struct hostent, h_name) p >>= peekCString
        h_aliases    <- (#peek struct hostent, h_aliases) p
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        h_addrtype   <- (#peek struct hostent, h_addrtype) p
        -- h_length       <- (#peek struct hostent, h_length) p
        h_addr_list  <- (#peek struct hostent, h_addr_list) p
                                >>= peekArray0 nullPtr
                                >>= mapM peek
        return (HostEntry {
                        hostName       = h_name,
                        hostAliases    = h_aliases,
#if defined(mingw32_HOST_OS)
                        hostFamily     = N.unpackFamily (fromIntegral (h_addrtype :: CShort)),
#else
                        hostFamily     = N.unpackFamily h_addrtype,
#endif
                        hostAddresses  = h_addr_list
                })

   poke = throwUnsupportedOperationPoke "HostEntry"


-- convenience function:
-- | Convenience function extracting one address in a 'HostEntry'.
-- Returns 'error' if 'HostEntry' contains no addresses.
hostAddress :: HostEntry -> N.HostAddress
hostAddress (HostEntry nm _ _ ls) =
 case ls of
   []    -> error $ "Network.BSD.hostAddress: empty network address list for " ++ nm
   (x:_) -> x

-- getHostByName must use the same lock as the *hostent functions
-- may cause problems if called concurrently.

-- | Resolve a 'N.HostName' to IPv4 address.
getHostByName :: N.HostName -> IO HostEntry
getHostByName name = withLock $ do
  withCString name $ \ name_cstr -> do
   ent <- throwNoSuchThingIfNull "Network.BSD.getHostByName" "no such host entry"
                $ c_gethostbyname name_cstr
   peek ent

foreign import CALLCONV safe "gethostbyname"
   c_gethostbyname :: CString -> IO (Ptr HostEntry)


-- The locking of gethostbyaddr is similar to gethostbyname.
-- | Get a 'HostEntry' corresponding to the given address and family.
-- Note that only IPv4 is currently supported.
getHostByAddr :: N.Family -> N.HostAddress -> IO HostEntry
getHostByAddr family addr = do
 with addr $ \ ptr_addr -> withLock $ do
 throwNoSuchThingIfNull "Network.BSD.getHostByAddr" "no such host entry"
   $ c_gethostbyaddr ptr_addr (fromIntegral (sizeOf addr)) (N.packFamily family)
 >>= peek

foreign import CALLCONV safe "gethostbyaddr"
   c_gethostbyaddr :: Ptr N.HostAddress -> CInt -> CInt -> IO (Ptr HostEntry)

#if defined(HAVE_GETHOSTENT) && !defined(mingw32_HOST_OS)
-- | @gethostent(3)@.
getHostEntry :: IO HostEntry
getHostEntry = withLock getHostEntry

getHostEntry' :: IO HostEntry
getHostEntry' = do
  throwNoSuchThingIfNull "Network.BSD.getHostEntry" "unable to retrieve host entry"
    $ c_gethostent
  >>= peek

foreign import ccall unsafe "gethostent" c_gethostent :: IO (Ptr HostEntry)

-- | @sethostent(3)@.
setHostEntry :: Bool -> IO ()
setHostEntry = withLock . setHostEntry'

setHostEntry' :: Bool -> IO ()
setHostEntry' flg = c_sethostent (fromBool flg)

foreign import ccall unsafe "sethostent" c_sethostent :: CInt -> IO ()

-- | @endhostent(3)@.
endHostEntry :: IO ()
endHostEntry = withLock $ c_endhostent

foreign import ccall unsafe "endhostent" c_endhostent :: IO ()

-- | Retrieve list of all 'HostEntry' via @gethostent(3)@.
getHostEntries :: Bool -> IO [HostEntry]
getHostEntries stayOpen = withLock $ do
  setHostEntry' stayOpen
  getEntries getHostEntry' c_endhostent

#endif

-- ---------------------------------------------------------------------------
-- Accessing network information

-- Same set of access functions as for accessing host,protocol and
-- service system info, this time for the types of networks supported.

-- network addresses are represented in host byte order.
type NetworkAddr = CULong

type NetworkName = String

-- | Representation of the POSIX @netent@ structure defined in [<netdb.h>](http://pubs.opengroup.org/onlinepubs/9699919799/basedefs/netdb.h.html).
data NetworkEntry =
  NetworkEntry {
     networkName        :: NetworkName,   -- ^ Official network name
     networkAliases     :: [NetworkName], -- ^ aliases
     networkFamily      :: N.Family,      -- ^ Network address type
     networkAddress     :: NetworkAddr    -- ^ Network number
   } deriving (Read, Show, Typeable)

-- | @since 2.8.1.0
instance NFData NetworkEntry where
   -- NB: We avoid relying on the `NFData CULong` instance which isn't available in deepseq-1.3 yet
   rnf (NetworkEntry n al !_f !_ad) = rnf (n,al)

instance Storable NetworkEntry where
   sizeOf    _ = #const sizeof(struct hostent)
   alignment _ = alignment (undefined :: CInt) -- ???

   peek p = do
        n_name         <- (#peek struct netent, n_name) p >>= peekCString
        n_aliases      <- (#peek struct netent, n_aliases) p
                                >>= peekArray0 nullPtr
                                >>= mapM peekCString
        n_addrtype     <- (#peek struct netent, n_addrtype) p
        n_net          <- (#peek struct netent, n_net) p
        return (NetworkEntry {
                        networkName      = n_name,
                        networkAliases   = n_aliases,
                        networkFamily    = N.unpackFamily (fromIntegral (n_addrtype :: CInt)),
                        networkAddress   = n_net
                })

   poke = throwUnsupportedOperationPoke "NetworkEntry"


#if !defined(mingw32_HOST_OS)
-- | @getnetbyname(3)@.
getNetworkByName :: NetworkName -> IO NetworkEntry
getNetworkByName name = withLock $ do
 withCString name $ \ name_cstr -> do
  throwNoSuchThingIfNull "Network.BSD.getNetworkByName" "no such network entry"
    $ c_getnetbyname name_cstr
  >>= peek

foreign import ccall unsafe "getnetbyname"
   c_getnetbyname  :: CString -> IO (Ptr NetworkEntry)

-- | @getnetbyaddr(3)@.
getNetworkByAddr :: NetworkAddr -> N.Family -> IO NetworkEntry
getNetworkByAddr addr family = withLock $ do
 throwNoSuchThingIfNull "Network.BSD.getNetworkByAddr" "no such network entry"
   $ c_getnetbyaddr addr (N.packFamily family)
 >>= peek

foreign import ccall unsafe "getnetbyaddr"
   c_getnetbyaddr  :: NetworkAddr -> CInt -> IO (Ptr NetworkEntry)

-- | @getnetent(3)@.
getNetworkEntry :: IO NetworkEntry
getNetworkEntry = withLock getNetworkEntry'

getNetworkEntry' :: IO NetworkEntry
getNetworkEntry' = do
  throwNoSuchThingIfNull "Network.BSD.getNetworkEntry" "no more network entries"
    $ c_getnetent
  >>= peek

foreign import ccall unsafe "getnetent" c_getnetent :: IO (Ptr NetworkEntry)

-- | Open the network name database. The parameter specifies
-- whether a connection is maintained open between various
-- networkEntry calls
--
-- @setnetent(3)@.
setNetworkEntry :: Bool -> IO ()
setNetworkEntry = withLock . setNetworkEntry'

setNetworkEntry' :: Bool -> IO ()
setNetworkEntry' flg = c_setnetent (fromBool flg)

foreign import ccall unsafe "setnetent" c_setnetent :: CInt -> IO ()

-- | Close the connection to the network name database.
--
-- @endnetent(3)@.
endNetworkEntry :: IO ()
endNetworkEntry = withLock $ c_endnetent

foreign import ccall unsafe "endnetent" c_endnetent :: IO ()

-- | Get the list of network entries via @getnetent(3)@.
getNetworkEntries :: Bool -> IO [NetworkEntry]
getNetworkEntries stayOpen = withLock $ do
  setNetworkEntry' stayOpen
  getEntries getNetworkEntry' c_endnetent
#endif

-- Mutex for name service lockdown

{-# NOINLINE lock #-}
lock :: MVar ()
lock = unsafePerformIO $ N.withSocketsDo $ newMVar ()

withLock :: IO a -> IO a
withLock act = withMVar lock (\_ -> act)

-- ---------------------------------------------------------------------------
-- Miscellaneous Functions

-- | Calling 'getHostName' returns the standard host name for the current
-- processor, as set at boot time.
--
-- @gethostname(2)@.
getHostName :: IO N.HostName
getHostName = do
  let size = 256
  allocaArray0 size $ \ cstr -> do
    throwSocketErrorIfMinus1_ "Network.BSD.getHostName" $ c_gethostname cstr (fromIntegral size)
    peekCString cstr

foreign import CALLCONV unsafe "gethostname"
   c_gethostname :: CString -> CSize -> IO CInt

-- Helper function used by the exported functions that provides a
-- Haskellised view of the enumerator functions:

getEntries :: IO a  -- read
           -> IO () -- at end
           -> IO [a]
getEntries getOne atEnd = loop
  where
    loop = do
      vv <- E.catch (liftM Just getOne)
            (\ e -> let _types = e :: IOException in return Nothing)
      case vv of
        Nothing -> return []
        Just v  -> loop >>= \ vs -> atEnd >> return (v:vs)


throwNoSuchThingIfNull :: String -> String -> IO (Ptr a) -> IO (Ptr a)
throwNoSuchThingIfNull loc desc act = do
  ptr <- act
  if (ptr == nullPtr)
   then ioError (ioeSetErrorString (mkIOError NoSuchThing loc Nothing Nothing) desc)
   else return ptr

throwUnsupportedOperationPoke :: String -> Ptr a -> a -> IO ()
throwUnsupportedOperationPoke typ _ _ =
  ioError $ ioeSetErrorString ioe "Operation not implemented"
  where
    ioe = mkIOError UnsupportedOperation
                    ("Network.BSD: instance Storable " ++ typ ++ ": poke")
                    Nothing
                    Nothing
