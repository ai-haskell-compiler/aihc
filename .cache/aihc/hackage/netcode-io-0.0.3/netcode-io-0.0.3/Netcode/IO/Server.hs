module Netcode.IO.Server (
    -- * Servers

    -- ** Server-specific callbacks
      ServerConnectDisconnectCallback

    -- ** Server configs
    , ServerConfig
    , defaultServerConfig
    , setProtocolID
    , setPrivateKey
    , setServerConnectDisconnectCallback, clearServerConnectDisconnectCallback
    , setServerSendReceiveOverrides, clearServerSendReceiveOverrides

    -- ** Server objects
    , Server
    , createServer
    , destroyServer
    , startServer
    , maxNumClients
    , stopServer
    , updateServer
    , clientConnectedAtIndex
    , clientIdAtIndex
    , withClientAddressAtIndex
    , withClientUserDataAtIndex
    , clientUserDataAtIndex
    , maxClientsForServer
    , numConnectedClients
    , isServerRunning
    , isServerFull
    , getServerPort
    , sendPacketFromServer
    , broadcastPacketFromServer
    , disconnectClientFromServer
    , disconnectAllClientsFromServer
    , receivePacketFromClient
    , nextServerPacketSequence
) where

--------------------------------------------------------------------------------

import Control.Applicative   (liftA2)
import Control.Monad         (when, forM_)
import Data.Word             (Word8, Word16, Word64)
import Foreign.C.String      (withCString)
import Foreign.C.Types       (CDouble(..))
import Foreign.Concurrent    (newForeignPtr)
import Foreign.ForeignPtr    (newForeignPtr_)
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           ( Ptr, nullPtr, castPtr
                             , FunPtr, nullFunPtr, freeHaskellFunPtr
                             )
import Foreign.Storable      (peek, poke)

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

-- | A server object. This is an opaque type meant to be used in conjunction
-- with this library.
--
-- A server is generally meant to represent and endpoint for one or more
-- t'Network.IO.Client's to connect to. The server application is similar to
-- that of the client in that it is expected to have a running timer with a
-- resolution of at least seconds. The main loop of the server application is
-- meant to call 'updateServer' to allow the library to process incoming
-- packets and send outgoing packets to the clients.
data Server = Server 
    { serverPtr :: Ptr C'netcode_server_t
    , serverCallbacks :: ServerCallbacks
    } deriving (Show)

data ServerCallbacks = ServerCallbacks
    { serverConnectDisconnect     :: C'connect_disconnect_callback_t
    , serverSendPacketOverride    :: C'send_packet_override_t
    , serverReceivePacketOverride :: C'receive_packet_override_t
    } deriving (Show)

defaultServerCallbacks :: ServerCallbacks
defaultServerCallbacks = ServerCallbacks nullFunPtr nullFunPtr nullFunPtr

-- | A 'ServerConfig' is a type that specifies the behavior of a 'Server' and
-- contains associated metadata.
--
-- At a minimum, the connection protocol ID and the private key for the
-- application should be set for each server (via 'setProtocolID' and
-- 'setPrivateKey', respectively).
newtype ServerConfig = ServerConfig
    (   Ptr C'netcode_server_config_t
        -> ServerCallbacks
        -> IO (Ptr C'netcode_server_config_t, ServerCallbacks)
    )

-- | The default 'ServerConfig' contains no callbacks or overrides, and
-- contains empty values for the required fields needed to properly have
-- a server respond to a connecting t'Netcode.IO.Client'.
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig $ \serverConfig cbs -> do
    c'netcode_default_server_config serverConfig
    return (serverConfig, cbs)

-- | Sets the connection protocol ID used by this 'Server'. This is a unique
-- ID that must match the protocol ID used in
-- 'Netcode.IO.generateConnectToken'
setProtocolID :: Word64 -> ServerConfig -> ServerConfig
setProtocolID protocolID (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig cbs' -> do
        (configPtr, cbs) <- mkServerPtr serverConfig cbs'
        config <- peek configPtr
        poke configPtr $
            config { c'netcode_server_config_t'protocol_id = protocolID }
        return (configPtr, cbs)

-- | Sets the private key used by this 'Server'. This key must match the
-- private key used in 'Netcode.IO.generateConnectToken'
setPrivateKey :: [Word8] -> ServerConfig -> ServerConfig
setPrivateKey key (ServerConfig mkServerPtr) =
    ServerConfig $ \serverConfig cbs' -> do
        (configPtr, cbs) <- mkServerPtr serverConfig cbs'
        config <- peek configPtr
        poke configPtr $    
            config {
                c'netcode_server_config_t'private_key = 
                    take c'NETCODE_KEY_BYTES (key <> repeat 0)
            }
        return (configPtr, cbs)

-- | A server-specific callback that gets invoked each time a client either
-- connects to, or disconnects from, the server.
type ServerConnectDisconnectCallback
   = Int  -- ^ Client index connected
  -> Bool -- ^ True if the client connected
  -> IO ()

mkServerConnectDisconnectCallback :: ServerConnectDisconnectCallback
                                  -> IO C'connect_disconnect_callback_t
mkServerConnectDisconnectCallback cb = mk'connect_disconnect_callback_t $
    \_ clientIdx connected -> cb (fromIntegral clientIdx) (connected /= 0)

-- | Replaces the existing 'ServerConnectDisconnectCallback' with the given one
-- and frees any associated memory that may be allocated for the the existing
-- callback.
setServerConnectDisconnectCallback :: ServerConnectDisconnectCallback
                                    -> ServerConfig -> ServerConfig
setServerConnectDisconnectCallback cb (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverConnectDisconnect callbacks
        fPtr <- mkServerConnectDisconnectCallback cb
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'connect_disconnect_callback = fPtr
            }
        return (configPtr, callbacks { serverConnectDisconnect = fPtr })

-- | Removes the existing 'ServerConnectDisconnectCallback' and frees any
-- associated memory that may be allocated for the the existing callback.
clearServerConnectDisconnectCallback :: ServerConfig -> ServerConfig
clearServerConnectDisconnectCallback (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverConnectDisconnect callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'connect_disconnect_callback = nullFunPtr
            }
        return (configPtr, callbacks { serverConnectDisconnect = nullFunPtr })

-- | Removes the existing send and receive overrides for the given config, if
-- set, and instead uses the ones given.
setServerSendReceiveOverrides :: SendPacketOverride
                            -> ReceivePacketOverride
                            -> ServerConfig -> ServerConfig
setServerSendReceiveOverrides sendFn recvFn (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverSendPacketOverride callbacks
        freeNullFunPtr $ serverReceivePacketOverride callbacks
        config <- peek configPtr
        sendOverride <- mkSendPacketOverride sendFn
        recvOverride <- mkReceivePacketOverride recvFn
        poke configPtr $ config
            { c'netcode_server_config_t'send_packet_override = sendOverride
            , c'netcode_server_config_t'receive_packet_override = recvOverride
            , c'netcode_server_config_t'override_send_and_receive = 1
            }
        let newcbs = callbacks
                { serverSendPacketOverride = sendOverride
                , serverReceivePacketOverride = recvOverride
                }
        return (configPtr, newcbs)

-- | Changes the config to use the default send and receive packet functions.
clearServerSendReceiveOverrides :: ServerConfig -> ServerConfig
clearServerSendReceiveOverrides (ServerConfig mkConfig) =
    ServerConfig $ \configPtr' callbacks' -> do
        (configPtr, callbacks) <- mkConfig configPtr' callbacks'
        freeNullFunPtr $ serverSendPacketOverride callbacks
        freeNullFunPtr $ serverReceivePacketOverride callbacks
        config <- peek configPtr
        poke configPtr $ config
            { c'netcode_server_config_t'send_packet_override = nullFunPtr
            , c'netcode_server_config_t'receive_packet_override = nullFunPtr
            , c'netcode_server_config_t'override_send_and_receive = 0
            }
        let newcbs = callbacks
                { serverSendPacketOverride = nullFunPtr
                , serverReceivePacketOverride = nullFunPtr
                }
        return (configPtr, newcbs)

-- | Creates a server at the given address using the provided config. Throws an
-- IOException on failure.
--
-- Note, the address used here can be either formatted as an IPv4 address or an
-- IPv6 address, similar to the arguments passed to 'parseAddress'. In the
-- common case, you will likely want to use INADDR_ANY to bind to the
-- underlying socket, which is represented by the address "0.0.0.0"
--
-- The time passed to this create function should be a measurement in seconds,
-- such that when connecting in the future using 'updateServer', the same
-- resolution timer is being passed. That allows the library to properly
-- timeout in cases where connections are taking too long to establish.
createServer :: String -> ServerConfig -> Double -> IO Server
createServer s (ServerConfig mkConfig) time = alloca $ \serverConfig -> do
    (config, cbs) <- mkConfig serverConfig defaultServerCallbacks
    ptr <- withCString s (\cs -> c'netcode_server_create cs config (CDouble time))
    when (ptr == nullPtr) $ fail "Failed to create server!"
    return (Server ptr cbs)

-- | Starts the server and specifies the maximum number of clients that can
-- connect. Emits a warning when the maximum number of clients is more than
-- 'maxNumClients'.
startServer :: Server -> Int -> IO ()
startServer (Server s _) n = do
    when (n > maxNumClients) $ putStrLn $ concat [
          "Warning: Can only start server with at most "
        , show (maxNumClients :: Int), " clients. Requested: ", show n
        ]
    c'netcode_server_start s (min maxNumClients $ fromIntegral n)

-- | Returns the maximum number of clients that a server can support.
maxNumClients :: Num a => a
maxNumClients = c'NETCODE_MAX_CLIENTS

-- | Stops the server.
stopServer :: Server -> IO ()
stopServer (Server s _) = c'netcode_server_stop s

-- | Destroys the server object and frees the associated Haskell-side callbacks
-- and overrides
destroyServer :: Server -> IO ()
destroyServer (Server s cbs) = do
    c'netcode_server_destroy s
    freeNullFunPtr $ serverConnectDisconnect cbs
    freeNullFunPtr $ serverSendPacketOverride cbs
    freeNullFunPtr $ serverReceivePacketOverride cbs

-- | Main processing call for a 'Server' with the current time in seconds (in
-- the same domain as the time passed to 'createServer'). This flushes packet
-- queues at the appropriate rate and updates connection statuses among other
-- things. It is expected to be called in the main loop of the application.
updateServer :: Server -> Double -> IO ()
updateServer (Server s _) = c'netcode_server_update s . CDouble

-- | Returns @True@ if the client at the given client index is connected to
-- the server. Returns @False@ if not connected, if the server is not running,
-- or if the client index is out of bounds.
clientConnectedAtIndex :: Server -> Int -> IO Bool
clientConnectedAtIndex (Server s _) =
    fmap (/= 0) . c'netcode_server_client_connected s . fromIntegral

-- | Returns the client ID of the client at the given client index. Returns @0@
-- if not connected, the server is not running, or if the client index is out
-- of bounds.
clientIdAtIndex :: Server -> Int -> IO Word64
clientIdAtIndex (Server s _) = c'netcode_server_client_id s . fromIntegral

-- | Performs an action with the address of the client to which the given
-- 'Server' is connected to. This is meant to minimize the chances that the
-- 'Address' value will be used in a manner that outlives the given 'Server' or
-- the connection lifetime of the client. Callers should avoid storing the
-- 'Address' value or returning it as a result of this function.
--
-- In the event that the client index is out of bounds, or the client is not
-- connected at that slot, the address passed to the action will be @0.0.0.0@.
withClientAddressAtIndex :: Server -> Int -> (Address -> IO a) -> IO a
withClientAddressAtIndex (Server s _) cidx fn = do
    aptr <- c'netcode_server_client_address s (fromIntegral cidx)
    if aptr == nullPtr
        then parseAddress "0.0.0.0" >>= fn
        else Address <$> newForeignPtr_ aptr >>= fn

-- | Performs an action with the user data of the client to which the given
-- 'Server' is connected to. This is meant to minimize the chances that the
-- memory buffer will be used in a manner that outlives the given 'Server' or
-- the connection lifetime of the client. Callers should avoid storing the
-- @Ptr@ value or returning it as a result of this function.
--
-- In the event that the client index is out of bounds, or the client is not
-- connected at that slot, the given action will receive @nullPtr@.
withClientUserDataAtIndex :: Server -> Int -> (Ptr () -> IO a) -> IO a
withClientUserDataAtIndex (Server s _) cidx fn =
    c'netcode_server_client_user_data s (fromIntegral cidx) >>= fn

-- | Returns the user data for the client connected at the given client index.
--
-- In the event that the client index is out of bounds, or the client is not
-- connected at that slot, the result will be the empty list.
clientUserDataAtIndex :: Server -> Int -> IO [Word8]
clientUserDataAtIndex s i = withClientUserDataAtIndex s i $ \ptr ->
    if ptr == nullPtr
        then return []
        else peekArray c'NETCODE_USER_DATA_BYTES (castPtr ptr)

-- | Returns the maximum number of clients that can connect to this server, or
-- zero if the server has not been started yet (via a call to 'startServer').
maxClientsForServer :: Server -> IO Int
maxClientsForServer (Server s _) =
    fromIntegral <$> c'netcode_server_max_clients s

-- | Returns the number of currently connected clients.
numConnectedClients :: Server -> IO Int
numConnectedClients (Server s _) =
    fromIntegral <$> c'netcode_server_num_connected_clients s

-- | Returns True if the server has been started, and is ready to accept
-- incoming connections from clients.
isServerRunning :: Server -> IO Bool
isServerRunning (Server s _) = (/= 0) <$> c'netcode_server_running s

-- | Returns true if the number of connected clients matches the maximum number
-- of possibly connected clients.
isServerFull :: Server -> IO Bool
isServerFull (Server s _) =
    liftA2 (==) (c'netcode_server_num_connected_clients s)
                (c'netcode_server_max_clients s)

-- | Returns the port assigned to the server's IP address.
getServerPort :: Server -> IO Word16
getServerPort (Server s _) = c'netcode_server_get_port s

-- | Disconnects the client at the given index from the server.
disconnectClientFromServer :: Server -> Int -> IO ()
disconnectClientFromServer (Server s _) =
    c'netcode_server_disconnect_client s . fromIntegral

-- | Disconnects all clients from the server.
disconnectAllClientsFromServer :: Server -> IO ()
disconnectAllClientsFromServer (Server s _) =
    c'netcode_server_disconnect_all_clients s

-- | Returns the next sequence number of a packet destined for the client at
-- the given client index.
nextServerPacketSequence :: Server -> Int -> IO Word64
nextServerPacketSequence (Server s _) =
    c'netcode_server_next_packet_sequence s . fromIntegral

-- | Enqueues a packet to be sent to the client at the given index during the
-- next call to 'updateServer'.
sendPacketFromServer :: Server
                     -> Int        -- ^ Client index
                     -> Int        -- ^ Size in bytes of packet data
                     -> Ptr Word8  -- ^ Packet data buffer
                     -> IO ()
sendPacketFromServer (Server s _) clientIdx pktSz pktMem = do
    let pktSize = min c'NETCODE_MAX_PACKET_SIZE (fromIntegral pktSz)
    when (pktSz > c'NETCODE_MAX_PACKET_SIZE) $ putStrLn $
            "WARNING: Sending packet that's too large: " <> show pktSz
    c'netcode_server_send_packet s (fromIntegral clientIdx) pktMem pktSize

-- | Enqueues a packet with the given size and data to all connected clients.
broadcastPacketFromServer :: Server -> Int -> Ptr Word8 -> IO ()
broadcastPacketFromServer s pktSz pktMem = do
    numClients <- numConnectedClients s
    forM_ [0..(numClients - 1)] $ \i -> sendPacketFromServer s i pktSz pktMem

-- | Dequeues a received packet from the t'Netcode.IO.Client' at the given
-- client index. This function returns a @Just@ until the queue is empty, upon
-- which it will return a @Nothing@.
receivePacketFromClient :: Server -> Int -> IO (Maybe Packet)
receivePacketFromClient (Server s _) clientIdx =
    alloca $ \sequenceNumPtr ->
    alloca $ \pktSzPtr -> do
        packetMem <- c'netcode_server_receive_packet s (fromIntegral clientIdx) pktSzPtr sequenceNumPtr
        if packetMem == nullPtr
            then return Nothing
            else fmap Just $
                Packet <$> peek sequenceNumPtr
                        <*> (fromIntegral <$> peek pktSzPtr)
                        <*> newForeignPtr packetMem (c'netcode_server_free_packet s (castPtr packetMem))