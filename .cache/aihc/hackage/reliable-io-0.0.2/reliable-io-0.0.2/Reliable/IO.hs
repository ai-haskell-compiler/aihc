{-|

Module      : Reliable.IO
Description : High-level bindings to the reliable.io library.
Copyright   : (c) Pavel Krajcevski, 2020
License     : BSD-3
Maintainer  : krajcevski@gmail.com
Stability   : experimental
Portability : Portable

This module contains the high-level bindings on top of the module
"Bindings.Netcode.IO". These provide a cleaner interface to the
<https://github.com/networkprotocol/reliable.io reliable.io> C library and are
the recommended interface for application developers.

These bindings have some limitations. Namely, they are not as performant as
the "close to the metal" bindings provided in "Bindings.Reliable.IO". In the
event that you need more performance, that module is available for use.

This library is intended to be used with a way to send and receive fixed size
packets over an unreliable channel. If such an interface exists, then, assuming
that the two parties are in constant communication, this library will do the
following for you:

    1. Break a packet down into a sequence of fixed size fragments to match
       your data channel size.
    2. Determine whether or not a sent packet has been acked by the receiver.
    3. Reassemble a packet once all fragments have been received.

With this in mind, the singular datatype provided by this library is an
'Endpoint'. Each endpoint requires the following:

    * How to send packet fragments ('TransmitPacketFunction')
    * What to do with reassembled packets ('ProcessPacketFunction')

Once you have an 'Endpoint', the two main operations that you would do with it
are to send a (possibly very large) packet ('sendPacket'), and provide it with
(possibly just one) packet fragments ('receivePacket'). On top of this library,
if a user would like to create a fully-reliable data channel (a la TCP), it
is that user's responsibility to identify when a packet has been dropped or has
arrived out of order to resend the appropriate packet.
-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
module Reliable.IO (
  -- * Initialization
    initialize
  , terminate

  -- * Utilities
  , LogLevel(..)
  , logLevel

  -- * Endpoint Configuration
  , EndpointConfig
  , defaultConfig
  , setName
  , setMaxPacketSize
  , setPacketFragmentationLimit
  , setPacketFragmentSize
  , setMaxNumFragments
  , setAckBufferSize
  , setSentPacketsBufferSize
  , setReceivedPacketsBufferSize
  , setFragmentReassemblyBufferSize
  , setRTTSmoothingFactor
  , setPacketLossSmoothingFactor
  , setBandwidthSmoothingFactor
  , PacketType(..)
  , setPacketType

  -- * Callbacks
  , TransmitPacketFunction
  , ProcessPacketFunction
  
  -- * Endpoints
  , Endpoint
  , createEndpoint
  , destroyEndpoint
  , withEndpoint
  , nextPacketSequence
  , sendPacket
  , receivePacket
  , getAcks
  , clearAcks
  , reset
  , update

  -- * Analytics
  , roundTripTime
  , packetLoss

  , BandwidthMeasurements(..)
  , bandwidth

  , Counter(..)
  , getCounter

) where 

-------------------------------------------------------------------------------

import Control.Monad         (when, unless)
import Data.Bool             (bool)
import Data.Data             (Data)
import Data.Typeable         (Typeable)
import Data.Word             (Word8, Word16, Word64)
import GHC.Generics          (Generic)
import GHC.Ptr               (Ptr)
import Foreign.C.String      (withCStringLen)
import Foreign.C.Types       (CInt, CDouble(..), CFloat(..))
import Foreign.Marshal.Alloc (alloca)
import Foreign.Marshal.Array (peekArray)
import Foreign.Ptr           (freeHaskellFunPtr)
import Foreign.Storable      (poke, Storable(..))

import Bindings.Reliable.IO

-------------------------------------------------------------------------------

-- | Initializes the @reliable.io@ library runtime. This should be called before
-- any additional functions in this library. Throws an
-- t'Control.Exception.IOException' on failure.
initialize :: IO ()
initialize = do
    result <- c'reliable_init
    unless (result == c'RELIABLE_OK) $
      fail $ "Failed to initialize reliable.io. Result: " <> show result
    return ()

-- | Terminates the @reliable.io@ library runtime. This should be called only
-- after all other library functions terminate.
terminate :: IO ()
terminate = c'reliable_term

-- | Specifies the logging behavior of @reliable.io@. Note, this logging behavior
-- is called from C calls to @printf@ and therefore might interfere with the
-- Haskell runtime (such as 'putStrLn').
data LogLevel = LogLevel'None
              | LogLevel'Info
              | LogLevel'Error
              | LogLevel'Debug
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

-- | Set the @reliable.io@ 'LogLevel'. The default is 'LogLevel'None'.
logLevel :: LogLevel -> IO ()
logLevel LogLevel'None  = c'reliable_log_level c'RELIABLE_LOG_LEVEL_NONE
logLevel LogLevel'Info  = c'reliable_log_level c'RELIABLE_LOG_LEVEL_INFO
logLevel LogLevel'Error = c'reliable_log_level c'RELIABLE_LOG_LEVEL_ERROR
logLevel LogLevel'Debug = c'reliable_log_level c'RELIABLE_LOG_LEVEL_DEBUG

-- | An 'EndpointConfig' is a write-only opaque datatype that is used to define
-- the settings for creating an 'Endpoint'.
newtype EndpointConfig = EndpointConfig { 
    generateConfig :: Ptr C'reliable_config_t -> IO ()
    }

-- | The default 'EndpointConfig'. This uses sensible defaults for the library
-- (as opposed to being zero-initialized, for example).
defaultConfig :: EndpointConfig
defaultConfig = EndpointConfig c'reliable_default_config

-- | Sets the name of the endpoint. This is usually not relevant, except when
-- setting the log level to be more than 'LogLevel'None'.
setName :: String -> EndpointConfig -> EndpointConfig
setName s (EndpointConfig fn) = EndpointConfig $ \cfgPtr -> do
    fn cfgPtr
    when (length s >= 256) $ fail "Endpoint config name too long"
    withCStringLen s $ \(csPtr, csLen) -> do
        config <- peek cfgPtr
        cs <- peekArray csLen csPtr
        poke cfgPtr $ config { c'reliable_config_t'name = (cs <> [0]) }

-- Helper function to convert transforms on C structs into transforms on
-- Haskell datatypes.
setConfig :: (C'reliable_config_t -> C'reliable_config_t)
          -> EndpointConfig -> EndpointConfig
setConfig fn (EndpointConfig mkCfg) = EndpointConfig $ \cfgPtr ->
    mkCfg cfgPtr >> peek cfgPtr >>= poke cfgPtr . fn

-- Calls 'setConfig' with an Int value
setConfigIntVal :: (CInt -> C'reliable_config_t -> C'reliable_config_t)
                -> Int -> EndpointConfig -> EndpointConfig
setConfigIntVal fn x = setConfig $ fn (fromIntegral x)

-- Calls 'setConfig' with a Float value
setConfigFloatVal :: (CFloat -> C'reliable_config_t -> C'reliable_config_t)
                  -> Float -> EndpointConfig -> EndpointConfig
setConfigFloatVal fn x = setConfig $ fn (CFloat x)

-- | Sets the maximum packet size for the endpoint. This will allow the API to
-- know when to throw an error when the packet being sent is too big. The
-- packet size is purely application specific, but may be useful for making
-- sure that your data sizes don't grow too large during development. The
-- default value for this is 16KB.
setMaxPacketSize :: Int -> EndpointConfig -> EndpointConfig
setMaxPacketSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'max_packet_size = sz }

-- | Sets the fragmentation limit for this endpoint. The fragmentation limit is
-- the size in bytes for a packet where it will be split into multiple
-- fragments. This need not be @maxPacketSize / maxNumFragments@, but that is
-- usually a sensible choice. The default value is 1KB.
setPacketFragmentationLimit :: Int -> EndpointConfig -> EndpointConfig
setPacketFragmentationLimit = setConfigIntVal $ \l config ->
    config { c'reliable_config_t'fragment_above = l }

-- | Sets the fragment size for this endpoint. The fragment size determines the
-- size in bytes of each fragment. This need not be the same as the
-- fragmentation limit, although that is certainly a sensible choice. The
-- default for this value is 1KB.
setPacketFragmentSize :: Int -> EndpointConfig -> EndpointConfig
setPacketFragmentSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'fragment_size = sz }

-- | Sets the number of fragments per packet in this endpoint. This is only to
-- make sure that the endpoint has enough buffer space provisioned for incoming
-- packets. Default for this value is 16, and the maximum value is 256.
setMaxNumFragments :: Int -> EndpointConfig -> EndpointConfig
setMaxNumFragments = setConfigIntVal $ \n config ->
    config { c'reliable_config_t'max_fragments = n }

-- | Sets the number of packets for which to store received sequence numbers.
-- The default value is 256.
setAckBufferSize :: Int -> EndpointConfig -> EndpointConfig
setAckBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'ack_buffer_size = sz }

-- | Sets the maximum number of packets for which to store sent packet info.
-- This number reflects the largest number of packets we expect to be in flight
-- at any given time, in order to properly ack them upon receipt of some other
-- endpoint's packets. Also useful for properly computing bandwidth of the
-- endpoint. Default value is 256.
setSentPacketsBufferSize :: Int -> EndpointConfig -> EndpointConfig
setSentPacketsBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'sent_packets_buffer_size = sz }

-- | Sets the maximum number of packets for which to store received packet
-- info. Useful for properly acking packets and for accurately computing
-- bandwidth of the endpoint. Default value is 256.
setReceivedPacketsBufferSize :: Int -> EndpointConfig -> EndpointConfig
setReceivedPacketsBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'received_packets_buffer_size = sz }

-- | Sets the maximum number of in flight packet fragments that we can store
-- in order to properly recreate the packets upon receipt. This buffer is used
-- to process out of order and dropped packets, as fragments from a packet may
-- not arrive contiguously. Default value is 64.
setFragmentReassemblyBufferSize :: Int -> EndpointConfig -> EndpointConfig
setFragmentReassemblyBufferSize = setConfigIntVal $ \sz config ->
    config { c'reliable_config_t'fragment_reassembly_buffer_size = sz }

-- | Sets the round trip time smoothing factor. This is purely for diagnostic
-- purposes when determining what the round trip time is for this endpoint.
-- Smaller numbers will vary the RTT measurement more slowly. Default value is
-- 0.0025f.
setRTTSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setRTTSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'rtt_smoothing_factor = factor }

-- | Sets the packet loss smoothing factor. This is purely for diagnostic
-- purposes when determining what the packet loss rate is for this endpoint.
-- Smaller numbers will vary the packet loss measurement more slowly.
-- Default value is 0.1f.
setPacketLossSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setPacketLossSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'packet_loss_smoothing_factor = factor }

-- | Sets the bandwidth smoothing factor. This is purely for diagnostic
-- purposes when determining what the bandwidth is from this endpoint. Smaller
-- numbers will vary the bandwidth measurement more slowly. Default value is
-- 0.1f.
setBandwidthSmoothingFactor :: Float -> EndpointConfig -> EndpointConfig
setBandwidthSmoothingFactor = setConfigFloatVal $ \factor config ->
    config { c'reliable_config_t'bandwidth_smoothing_factor = factor }

-- | Endpoints support two packet types, either IPV4 or IPV6.
data PacketType = PacketType'IPV4 | PacketType'IPV6
    deriving (Eq, Ord, Show, Read, Bounded, Enum, Generic, Data, Typeable)

-- | Sets the packet type for this endpoint, which determines the header size
-- that the library needs to allocate in order to properly keep track of the
-- packets.
setPacketType :: PacketType -> EndpointConfig -> EndpointConfig
setPacketType PacketType'IPV4 = setConfig $ \config ->
    config { c'reliable_config_t'packet_header_size = 28 }
setPacketType PacketType'IPV6 = setConfig $ \config ->
    config { c'reliable_config_t'packet_header_size = 48 }
    
-- Utility structure to know what to free when destroying endpoints.
data EndpointCallbacks = EndpointCallbacks
    { _endpointCallbacksXmit :: C'transmit_packet_function_t
    , _endpointCallbacksRecv :: C'process_packet_function_t
    }

-- | An 'Endpoint' is the main datatype of the reliable.io library. Two
-- endpoints (usually, but not exclusively) on separate hosts represent a
-- connection over an unreliable network, such as the UDP protocol over the
-- internet. The function of an endpoint is to provide a way to administer
-- traffic to the corresponding receiver. It is not responsible for performing
-- the actual sending and receiving of data.
--
-- Endpoints provide two main services:
--
--   1. Breaking down a large packet into fragments, each of which is a
--      predetermined size.
--   2. Assembling a sequence of fragments from a corresponding endpoint.
--   3. Notifying the user when a packet has been received (ack'd) by the
--      corresponding endpoint.
--
-- Packets to be disassembled into fragments and transmitted are passed to the
-- endpoint via the 'sendPacket' function. Fragments that are received from the
-- corresponding endpoint and should be reassmbled are passed to the endpoint
-- via the 'receivePacket' function. These functions only queue the data for
-- processing, but the actual processing of packets only takes place during a
-- call to 'update'.
--
-- Additionally, each 'Endpoint' keeps track of the metrics associated with it,
-- providing the user with ways to measure the round trip time for each packet,
-- the bandwidth of the connection, and a measurement of the packet loss.
data Endpoint = Endpoint
    { _endpointPtr :: Ptr C'reliable_endpoint_t
    , _endpointCallbacks :: EndpointCallbacks
    }

-- | Function used by an 'Endpoint' to send packet fragments over the
-- unreliable data channel. One use case would be to have the given data sent
-- to a UDP socket.
type TransmitPacketFunction
     = Word16     -- ^ Sequence number of the packet being sent
    -> Ptr Word8  -- ^ Pointer to memory containing the packet data
    -> Int        -- ^ Size of the data in bytes
    -> IO ()

-- Utility function for converting TransmitPacketFunctions to the C callback
-- type.
mkTransmitPacketFunction :: TransmitPacketFunction
                         -> IO C'transmit_packet_function_t
mkTransmitPacketFunction fn = mk'transmit_packet_function_t $
    \_ _ seqNo ptr -> fn seqNo ptr . fromIntegral

-- | A user function supplied to an 'Endpoint' that handles reassembled packets
-- once they've been received.
type ProcessPacketFunction
     = Word16     -- ^ Sequence number of the packet received.
    -> Ptr Word8  -- ^ Pointer to the memory containing the packet data
    -> Int        -- ^ Size of the data in bytes.
    -> IO Bool    -- ^ Returns true if the packet was successfully processed

-- Utility function for converting ProcessPacketFunctions to the C callback type.
mkProcessPacketFunction :: ProcessPacketFunction -> IO C'process_packet_function_t
mkProcessPacketFunction fn = mk'process_packet_function_t $
    \_ _ seqNo ptr -> fmap (bool 0 1) . fn seqNo ptr . fromIntegral

-- | Creates an 'Endpoint'. The two main callbacks required for each endpoint:
--
--   1. A 'TransmitPacketFunction' that is able to send packet fragments to a
--      corresponding 'Endpoint'
--   2. A 'ProcessPacketFunction' that administers the reassmbled packet from a
--      collection of fragments.
--
-- The 'Double' parameter corresponds to the time (in seconds) at which the
-- endpoint is created. This time value is needed to be in the same domain to
-- subsequent calls to 'update' in order to properly calculate metrics.
createEndpoint :: EndpointConfig
               -> Double
               -> TransmitPacketFunction
               -> ProcessPacketFunction
               -> IO Endpoint
createEndpoint cfg t xmitFn recvFn = alloca $ \ptr -> do
    generateConfig cfg ptr
    config <- peek ptr
    xmitCFn <- mkTransmitPacketFunction xmitFn
    recvCFn <- mkProcessPacketFunction recvFn
    poke ptr $ config {
        c'reliable_config_t'transmit_packet_function = xmitCFn,
        c'reliable_config_t'process_packet_function = recvCFn
    }
    endpoint <- c'reliable_endpoint_create ptr (CDouble t)
    return $ Endpoint endpoint (EndpointCallbacks xmitCFn recvCFn)

-- | Destroys an 'Endpoint' and any associated callbacks.
destroyEndpoint :: Endpoint -> IO ()
destroyEndpoint (Endpoint ptr cbs) = do
    c'reliable_endpoint_destroy ptr
    freeHaskellFunPtr $ _endpointCallbacksXmit cbs
    freeHaskellFunPtr $ _endpointCallbacksRecv cbs
    return ()

-- | Convenience function that follows the
-- <https://wiki.haskell.org/Bracket_pattern Bracket pattern> for encapsulating
-- the resource management associated with interfacing with an 'Endpoint'.
withEndpoint :: EndpointConfig
             -> Double
             -> TransmitPacketFunction
             -> ProcessPacketFunction
             -> (Endpoint -> IO a)
             -> IO a
withEndpoint cfg t xmit recv fn = do
    endpoint <- createEndpoint cfg t xmit recv
    result <- fn endpoint
    destroyEndpoint endpoint
    return result

-- | Returns the sequence number of the next packet that will be sent from this
-- 'Endpoint'.
nextPacketSequence :: Endpoint -> IO Word16
nextPacketSequence (Endpoint ptr _) =
    c'reliable_endpoint_next_packet_sequence ptr

-- | @sendPacket e p sz@ will send a packet from 'Endpoint' @e@ with @sz@ bytes
-- whose data resides in the memory pointed to by @p@. If @sz@ is larger than
-- the fragment limit, the packet will be split into multiple fragments. Each
-- fragment will then be sent via the 'TransmitPacketFunction' passed to
-- 'createEndpoint'. Note, this function does not actually send the packet, and
-- rather queues it for sending during the next call to 'update'.
sendPacket :: Endpoint -> Ptr Word8 -> Int -> IO ()
sendPacket (Endpoint epPtr _) pktPtr =
    c'reliable_endpoint_send_packet epPtr pktPtr . fromIntegral

-- | @receivePacket e p sz@ will add a packet fragment to 'Endpoint' @e@ with
-- @sz@ bytes whose data resides in the memory pointed to by @p@. Once all of
-- the fragments of a given packet have been received via this function, the
-- 'Endpoint' will pass the reassembled packet to the 'ProcessPacketFunction'
-- passed to 'createEndpoint'. Note, this function does not actually reassemble
-- the packet, and rather queues it for processing during the next call to
-- 'update'.
receivePacket :: Endpoint -> Ptr Word8 -> Int -> IO ()
receivePacket (Endpoint epPtr _) pktPtr =
    c'reliable_endpoint_receive_packet epPtr pktPtr . fromIntegral

-- | Returns the list of sequence numbers for the most recently ack'd packets
-- that have been sent from this 'Endpoint'.
getAcks :: Endpoint -> IO [Word16]
getAcks (Endpoint epPtr _) = alloca $ \numAcksPtr -> do
    acksPtr <- c'reliable_endpoint_get_acks epPtr numAcksPtr
    numAcks <- peek numAcksPtr
    peekArray (fromIntegral numAcks) acksPtr

-- | Clears the list of sequence numbers for the most recently ack'd packets.
clearAcks :: Endpoint -> IO ()
clearAcks (Endpoint ptr _) = c'reliable_endpoint_clear_acks ptr

-- | Resets the endpoint, including all metrics about network traffic and any
-- information about ack'd packets.
reset :: Endpoint -> IO ()
reset (Endpoint ptr _) = c'reliable_endpoint_reset ptr

-- | Performs the work of updating the endpoint. This sends packets,
-- reassembles packets, and identifies any acks received from the corresponding
-- 'Endpoint'. The time passed to this function must be measured in seconds and
-- correspond to the same time domain as 'createEndpoint'.
update :: Endpoint -> Double -> IO ()
update (Endpoint ptr _) = c'reliable_endpoint_update ptr . CDouble

-- | Returns the measured round trip time for packets sent from this
-- 'Endpoint'.
roundTripTime :: Endpoint -> IO Float
roundTripTime (Endpoint ptr _) = do
    (CFloat f) <- c'reliable_endpoint_rtt ptr
    return f

-- | Returns the measured packet loss for packets sent from this 'Endpoint'.
packetLoss :: Endpoint -> IO Float
packetLoss (Endpoint ptr _) = do
    (CFloat f) <- c'reliable_endpoint_packet_loss ptr
    return f

-- | Bandwidth measurements taken for each 'Endpoint'.
data BandwidthMeasurements = BandwidthMeasurements
    { bandwidthSentKbps :: Float
    , bandwidthReceivedKbps :: Float
    , bandwidthAckedKbps :: Float
    } deriving (Show, Read, Generic)

-- | Returns the measured bandwidth for data on this 'Endpoint'.
bandwidth :: Endpoint -> IO BandwidthMeasurements
bandwidth (Endpoint ptr _) =
    alloca $ \sentPtr ->
    alloca $ \receivedPtr ->
    alloca $ \ackedPtr -> do
        c'reliable_endpoint_bandwidth ptr sentPtr receivedPtr ackedPtr
        (CFloat sent) <- peek sentPtr
        (CFloat received) <- peek receivedPtr
        (CFloat acked) <- peek ackedPtr
        return $ BandwidthMeasurements sent received acked

-- | Counters for metrics that are collected for each 'Endpoint'. These are
-- reset upon calling 'reset' for the given 'Endpoint'.
data Counter
    = Counter'NumPacketsSent
    | Counter'NumPacketsReceived
    | Counter'NumPacketsAcked
    | Counter'NumPacketsStale
    | Counter'NumPacketsInvalid
    | Counter'NumPacketsTooLargeToSend
    | Counter'NumPacketsTooLargeToReceive
    | Counter'NumFragmentsSent
    | Counter'NumFragmentsReceived
    | Counter'NumFragmentsInvalid
    deriving (Eq, Ord, Show, Read, Enum, Bounded, Generic, Data, Typeable)

-- | Returns the counter value associated with the 'Counter' for the given
-- 'Endpoint'.
getCounter :: Endpoint -> Counter -> IO Word64
getCounter (Endpoint ptr _) ctr =
    let ctrIdx = case ctr of
            Counter'NumPacketsSent              -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_SENT
            Counter'NumPacketsReceived          -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_RECEIVED
            Counter'NumPacketsAcked             -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_ACKED
            Counter'NumPacketsStale             -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_STALE
            Counter'NumPacketsInvalid           -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_INVALID
            Counter'NumPacketsTooLargeToSend    -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_SEND
            Counter'NumPacketsTooLargeToReceive -> c'RELIABLE_ENDPOINT_COUNTER_NUM_PACKETS_TOO_LARGE_TO_RECEIVE
            Counter'NumFragmentsSent            -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_SENT
            Counter'NumFragmentsReceived        -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_RECEIVED
            Counter'NumFragmentsInvalid         -> c'RELIABLE_ENDPOINT_COUNTER_NUM_FRAGMENTS_INVALID
     in c'reliable_endpoint_counters ptr >>= flip peekElemOff ctrIdx
