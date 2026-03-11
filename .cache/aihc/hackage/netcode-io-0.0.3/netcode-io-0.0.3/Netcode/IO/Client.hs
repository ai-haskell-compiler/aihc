{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO.Client (
    -- * Clients

    -- ** Client-specific callbacks
      ClientStateChangeCallback

    -- ** Client configs
    , ClientConfig
    , defaultClientConfig
    , setClientStateChangeCallback, clearClientStateChangeCallback
    , setClientSendReceiveOverrides, clearClientSendReceiveOverrides

    -- ** Client objects
    , Client
    , createClient
    , destroyClient
    , generateClientID
    , connectClient
    , disconnectClient
    , updateClient
    , sendPacketFromClient
    , receivePacketFromServer
    , nextClientPacketSequence
    , getClientPort
    , withClientServerAddress

    -- ** Client state
    , ClientState(..)
    , getClientState
    , isClientDisconnected

    -- ** Connect Tokens
    , ConnectToken
    , maximumServersPerConnect
    , maximumUserDataSize
    , privateKeySize
    , generateConnectToken
) where

--------------------------------------------------------------------------------

import Control.Monad         (when)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word16, Word64)
import Foreign.C.String      (withCString)
import Foreign.C.Types       (CDouble(..), CInt)
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    ( ForeignPtr, newForeignPtr_, mallocForeignPtrBytes
                             , withForeignPtr
                             )
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (allocaArray, pokeArray)
import Foreign.Ptr           ( Ptr, nullPtr, castPtr
                             , FunPtr, nullFunPtr, freeHaskellFunPtr
                             )
import Foreign.Storable      (peek, poke, sizeOf, pokeElemOff)
import GHC.Generics          (Generic)

import Bindings.Netcode.IO
import Netcode.IO.Address
import Netcode.IO.Callbacks
import Netcode.IO.Packet

--------------------------------------------------------------------------------

freeNullFunPtr :: FunPtr a -> IO ()
freeNullFunPtr x
    | x == nullFunPtr = return ()
    | otherwise       = freeHaskellFunPtr x

--------------------------------------------------------------------------------

-- | The possible connection states of a 'Client'. The default state is
-- 'ClientState'Disconnected'.
data ClientState
    = ClientState'ConnectTokenExpired
    | ClientState'InvalidConnectToken
    | ClientState'ConnectionTimedOut
    | ClientState'ConnectionResponseTimedOut
    | ClientState'ConnectionRequestTimedOut
    | ClientState'ConnectionDenied
    | ClientState'Disconnected
    | ClientState'SendingConnectionRequest
    | ClientState'SendingConnectionResponse
    | ClientState'Connected
    deriving (Eq, Ord, Show, Enum, Bounded, Data, Typeable, Generic)

_rawClientState :: ClientState -> CInt
_rawClientState ClientState'ConnectTokenExpired        = c'NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED
_rawClientState ClientState'InvalidConnectToken        = c'NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN
_rawClientState ClientState'ConnectionTimedOut         = c'NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT
_rawClientState ClientState'ConnectionResponseTimedOut = c'NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT
_rawClientState ClientState'ConnectionRequestTimedOut  = c'NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT
_rawClientState ClientState'ConnectionDenied           = c'NETCODE_CLIENT_STATE_CONNECTION_DENIED
_rawClientState ClientState'Disconnected               = c'NETCODE_CLIENT_STATE_DISCONNECTED
_rawClientState ClientState'SendingConnectionRequest   = c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST
_rawClientState ClientState'SendingConnectionResponse  = c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE
_rawClientState ClientState'Connected                  = c'NETCODE_CLIENT_STATE_CONNECTED

typedClientState :: CInt -> ClientState
typedClientState raw
  | raw == c'NETCODE_CLIENT_STATE_CONNECT_TOKEN_EXPIRED         = ClientState'ConnectTokenExpired
  | raw == c'NETCODE_CLIENT_STATE_INVALID_CONNECT_TOKEN         = ClientState'InvalidConnectToken
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_TIMED_OUT          = ClientState'ConnectionTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_RESPONSE_TIMED_OUT = ClientState'ConnectionResponseTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_REQUEST_TIMED_OUT  = ClientState'ConnectionRequestTimedOut
  | raw == c'NETCODE_CLIENT_STATE_CONNECTION_DENIED             = ClientState'ConnectionDenied
  | raw == c'NETCODE_CLIENT_STATE_DISCONNECTED                  = ClientState'Disconnected
  | raw == c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_REQUEST    = ClientState'SendingConnectionRequest
  | raw == c'NETCODE_CLIENT_STATE_SENDING_CONNECTION_RESPONSE   = ClientState'SendingConnectionResponse
  | raw == c'NETCODE_CLIENT_STATE_CONNECTED                     = ClientState'Connected
  | otherwise = error "Unrecognized client state value"

-- | A client object. This is an opaque type meant to be used in conjunction
-- with this library.
--
-- A 'Client' is generally meant to connect to one of potentially many servers
-- through a 'ConnectToken'. The main loop of the application that manages the
-- lifetime of the client is expected to maintain a running timer with a
-- resolution of at least seconds. This main loop is also expected to call
-- 'updateClient' on a regular basis to allow the library to process incoming
-- packets and send outgoing packets.
data Client = Client 
  { clientPtr :: Ptr C'netcode_client_t
  , clientCallbacks :: ClientCallbacks
  } deriving (Show)

data ClientCallbacks = ClientCallbacks
  { clientStateChange           :: C'state_change_callback_t
  , clientSendPacketOverride    :: C'send_packet_override_t
  , clientReceivePacketOverride :: C'receive_packet_override_t
  } deriving (Show)

defaultClientCallbacks :: ClientCallbacks
defaultClientCallbacks = ClientCallbacks nullFunPtr nullFunPtr nullFunPtr

-- | A 'ClientConfig' is a type that specifies the behavior of a 'Client'.
-- Client configs are pretty spartan: the only options available at this time
-- are setting callbacks.
newtype ClientConfig = ClientConfig
  (   Ptr C'netcode_client_config_t
   -> ClientCallbacks
   -> IO (Ptr C'netcode_client_config_t, ClientCallbacks)
  )

-- | A 'ClientConfig' with no callback overrides.
defaultClientConfig :: ClientConfig
defaultClientConfig = ClientConfig $ \clientConfig cbs -> do
    c'netcode_default_client_config clientConfig
    return (clientConfig, cbs)

-- | A client-specific callback that gets invoked each time the underlying
-- state of the client changes.
type ClientStateChangeCallback
   = ClientState   -- ^ Old state
  -> ClientState   -- ^ New state
  -> IO ()

mkClientStateChangeCallback :: ClientStateChangeCallback
                            -> IO C'state_change_callback_t
mkClientStateChangeCallback cb = mk'state_change_callback_t $ \_ oldSt newSt ->
    cb (typedClientState oldSt) (typedClientState newSt)

-- | Creates a config that removes the existing 'ClientStateChangeCallback' and
-- instead uses the given callback.
setClientStateChangeCallback :: ClientStateChangeCallback
                             -> ClientConfig -> ClientConfig
setClientStateChangeCallback cb (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientStateChange callbacks
        fPtr <- mkClientStateChangeCallback cb
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_client_config_t'state_change_callback = fPtr }
        return (configPtr, callbacks { clientStateChange = fPtr })

-- | Clears the 'ClientStateChangeCallback' for the given config.
clearClientStateChangeCallback :: ClientConfig -> ClientConfig
clearClientStateChangeCallback (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientStateChange callbacks
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_client_config_t'state_change_callback = nullFunPtr }
        return (configPtr, callbacks { clientStateChange = nullFunPtr })

-- | Removes the existing send and receive overrides for the given config, if
-- set, and instead uses the ones given.
setClientSendReceiveOverrides :: SendPacketOverride
                              -> ReceivePacketOverride
                              -> ClientConfig -> ClientConfig
setClientSendReceiveOverrides sendFn recvFn (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientSendPacketOverride callbacks
        freeNullFunPtr $ clientReceivePacketOverride callbacks
        config <- peek configPtr
        sendOverride <- mkSendPacketOverride sendFn
        recvOverride <- mkReceivePacketOverride recvFn
        poke configPtr $ config
            { c'netcode_client_config_t'send_packet_override = sendOverride
            , c'netcode_client_config_t'receive_packet_override = recvOverride
            , c'netcode_client_config_t'override_send_and_receive = 1
            }
        let newcbs = callbacks
              { clientSendPacketOverride = sendOverride
              , clientReceivePacketOverride = recvOverride
              }
        return (configPtr, newcbs)

-- | Changes the config to use the default send and receive packet functions.
clearClientSendReceiveOverrides :: ClientConfig -> ClientConfig
clearClientSendReceiveOverrides (ClientConfig mkConfig) =
    ClientConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ clientSendPacketOverride callbacks
        freeNullFunPtr $ clientReceivePacketOverride callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_client_config_t'send_packet_override = nullFunPtr
            , c'netcode_client_config_t'receive_packet_override = nullFunPtr
            , c'netcode_client_config_t'override_send_and_receive = 0
            }
        let newcbs = callbacks
              { clientSendPacketOverride = nullFunPtr
              , clientReceivePacketOverride = nullFunPtr
              }
        return (configPtr, newcbs)

-- | Creates a client at the given address using the provided config. Throws an
-- IOException on failure.
--
-- Note, the address used here can be either formatted as an IPv4 address or an
-- IPv6 address, similar to the arguments passed to 'parseAddress'. In the
-- common case, you will likely want to use INADDR_ANY to bind to the
-- underlying socket, which is represented by the address "0.0.0.0"
--
-- The time passed to this create function should be a measurement in seconds,
-- such that when connecting in the future using 'updateClient', the same
-- resolution timer is being passed. That allows the library to properly
-- timeout in cases where connections are taking too long to establish.
createClient :: String -> ClientConfig -> Double -> IO Client
createClient s (ClientConfig mkConfig) time = alloca $ \clientConfig -> do
    (config, callbacks) <- mkConfig clientConfig defaultClientCallbacks
    ptr <- withCString s $ \cs ->
        c'netcode_client_create cs config (CDouble time)
    when (ptr == nullPtr) $ fail "Failed to create client!"
    return (Client ptr callbacks)

-- | Destroys the client and frees all of the Haskell-side function pointers
-- that were registered as callbacks.
destroyClient :: Client -> IO ()
destroyClient (Client c cbs) = do
    c'netcode_client_destroy c
    freeNullFunPtr $ clientStateChange cbs
    freeNullFunPtr $ clientSendPacketOverride cbs
    freeNullFunPtr $ clientReceivePacketOverride cbs

-- | Generates a random 64-bit client ID to be used with 'generateConnectToken'
generateClientID :: IO Word64
generateClientID = alloca $ \idPtr -> do
    c'netcode_random_bytes (castPtr idPtr) $
        fromIntegral $ sizeOf (undefined :: Word64)
    peek idPtr

-- | Begin the process to connect the client to a server stored in the given
-- 'ConnectToken'. This does not connect the client immediately, but rather
-- resets the client object and sets the state to
-- 'ClientState'SendingConnectionRequest'. The client will attempt to connect
-- on the next call to 'updateClient'.
connectClient :: Client -> ConnectToken -> IO ()
connectClient (Client c _) (ConnectToken ctPtr) =
    withForeignPtr ctPtr (c'netcode_client_connect c)

-- | Disconnects the client from anything it might be connected to.
disconnectClient :: Client -> IO ()
disconnectClient (Client c _) = c'netcode_client_disconnect c

-- | Main processing call for clients with the current time in seconds (in the
-- same domain as the time passed to 'createClient'). This flushes packet
-- queues at the appropriate rate and updates connection statuses among other
-- things. It is expected to be called in the main loop of the application.
updateClient :: Client -> Double -> IO ()
updateClient (Client c _) = c'netcode_client_update c . CDouble

-- | Returns the current state of the 'Client'.
getClientState :: Client -> IO ClientState
getClientState (Client c _) = typedClientState <$> c'netcode_client_state c

-- | Returns true if the 'Client' is in a state considered to be disconnected,
-- as opposed to connected or connecting.
isClientDisconnected :: Client -> IO Bool
isClientDisconnected (Client c _) =
    (<= c'NETCODE_CLIENT_STATE_DISCONNECTED) <$> c'netcode_client_state c

-- | Returns the sequence number of the next packet that the 'Client' will
-- send.
nextClientPacketSequence :: Client -> IO Word64
nextClientPacketSequence (Client c _) = c'netcode_client_next_packet_sequence c

-- | Enqueues a packet to be sent during the next call to 'updateClient'.
sendPacketFromClient :: Client -> Int -> Ptr Word8 -> IO ()
sendPacketFromClient (Client c _) pktSz pktMem = do
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
    when (pktSz > c'NETCODE_MAX_PACKET_SIZE) $ putStrLn $
        "WARNING: Sending packet that's too large: " <> show pktSz
    c'netcode_client_send_packet c pktMem pktSize

-- | Dequeues a received packet from the t'Netcode.IO.Server'. This function
-- returns a @Just@ until the queue is empty, upon which it will return
-- @Nothing@.
receivePacketFromServer :: Client -> IO (Maybe Packet)
receivePacketFromServer (Client c _) =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_client_receive_packet c pktSzPtr sequenceNumPtr
        let finalizer = c'netcode_client_free_packet c (castPtr packetMem)
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                       <*> (fromIntegral <$> peek pktSzPtr)
                       <*> newForeignPtr packetMem finalizer

-- | Returns the port assigned to this 'Client'.
getClientPort :: Client -> IO Word16
getClientPort (Client c _) = c'netcode_client_get_port c

-- | Performs an action with the address of the server to which the given
-- 'Client' is connected to. This is meant to minimize the chances that the
-- 'Address' value will be used in a manner that outlives the given 'Client'.
-- Callers should avoid storing the 'Address' value or returning it as a result
-- of this function.
--
-- In the event that the client is not connected to a server, the address
-- passed to the action will be @0.0.0.0@.
withClientServerAddress :: Client -> (Address -> IO a) -> IO a
withClientServerAddress (Client c _) fn = do
    aptr <- c'netcode_client_server_address c
    if aptr == nullPtr
        then parseAddress "0.0.0.0" >>= fn
        else Address <$> newForeignPtr_ aptr >>= fn

-- | A 'ConnectToken' represents an encrypted set of data fields that describe
-- both the client requesting to make a connection and the available servers to
-- which that connection can be made. It is generated solely via the 
newtype ConnectToken = ConnectToken (ForeignPtr Word8)

-- | Gives the maximum size, in bytes, of user data stored in a 'ConnectToken'.
maximumUserDataSize :: Num a => a
maximumUserDataSize = c'NETCODE_USER_DATA_BYTES

-- | Returns the maximum number of servers that can be stored in a
-- 'ConnectToken'.
maximumServersPerConnect :: Num a => a
maximumServersPerConnect = c'NETCODE_MAX_SERVERS_PER_CONNECT

-- | Returns the number of bytes expected in the private key used to generate a
-- 'ConnectToken'
privateKeySize :: Num a => a
privateKeySize = c'NETCODE_KEY_BYTES

-- | Creates a connect token for the given client (by clientID) with the list
-- of associated addresses. User data may be at most 'maximumUserDataSize'
-- values, otherwise is truncated or zero-padded to fill. The list of public
-- and internal servers must not be empty and may contain at most
-- 'maximumServersPerConnect' values, otherwise is truncated. Throws an
-- IOException on failure.
generateConnectToken :: [(String, String)] -- ^ Public and internal servers
                     -> Int                -- ^ Token expiration in seconds
                     -> Int                -- ^ Token timeout in seconds
                     -> Word64             -- ^ Unique Client ID
                     -> Word64             -- ^ Protocol ID
                     -> [Word8]            -- ^ Private key
                     -> [Word8]            -- ^ User data
                     -> IO ConnectToken
generateConnectToken [] _ _ _ _ _ _ = fail "Connect token server list is empty."
generateConnectToken addrs expiry timeout clientID protocolID privateKey userData =
    allocaArray (length addrs) $ \externalAddrs ->
    allocaArray (length addrs) $ \internalAddrs ->
        let checkServers =
                when (length addrs > maximumServersPerConnect) $ putStrLn $
                "Warning: Too many servers passed to connect token: " <> show (length addrs)

            writeAddrsAndGo i ((s1, s2) : rest) = withCString s1 $ \cs1 -> do
                pokeElemOff externalAddrs i cs1
                withCString s2 $ \cs2 -> do
                    pokeElemOff internalAddrs i cs2
                    writeAddrsAndGo (i + 1) rest
            writeAddrsAndGo _ [] =  -- go....
                allocaArray c'NETCODE_USER_DATA_BYTES $ \userDataBytes ->
                allocaArray c'NETCODE_KEY_BYTES $ \privateKeyBytes -> do
                    connectTokenPtr <- mallocForeignPtrBytes c'NETCODE_CONNECT_TOKEN_BYTES
                    pokeArray privateKeyBytes (take c'NETCODE_KEY_BYTES       $ privateKey <> repeat 0)
                    pokeArray userDataBytes   (take c'NETCODE_USER_DATA_BYTES $ userData   <> repeat 0)
                    result <- withForeignPtr connectTokenPtr $ \connectTokenBytes ->
                        c'netcode_generate_connect_token (fromIntegral $ length addrs)
                                                         externalAddrs
                                                         internalAddrs
                                                         (fromIntegral expiry)
                                                         (fromIntegral timeout)
                                                         clientID
                                                         protocolID
                                                         privateKeyBytes
                                                         userDataBytes
                                                         connectTokenBytes
                    when (result == c'NETCODE_ERROR) $ fail "Error generating connect token"
                    return $ ConnectToken connectTokenPtr
          in checkServers >> writeAddrsAndGo 0 addrs
    