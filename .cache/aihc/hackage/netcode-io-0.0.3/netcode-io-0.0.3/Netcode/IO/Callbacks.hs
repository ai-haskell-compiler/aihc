module Netcode.IO.Callbacks where

-------------------------------------------------------------------------------
import Data.Word             (Word8)
import Foreign.C.Types       (CInt)
import Foreign.ForeignPtr    (newForeignPtr_)
import Foreign.Ptr           (Ptr)

import Bindings.Netcode.IO
import Netcode.IO.Address

-------------------------------------------------------------------------------

-- | Override that sends a packet to the given address. This can be used by
-- both clients and servers. This is invoked after @netcode.io@ processes and
-- encrypts the packet.
type SendPacketOverride
     = Address    -- ^ 'Address' to send the packet to
    -> Ptr Word8  -- ^ A pointer to the memory that holds the packet data
    -> CInt       -- ^ The size of the packet
    -> IO ()

mkSendPacketOverride :: SendPacketOverride -> IO C'send_packet_override_t
mkSendPacketOverride sendFn =
    mk'send_packet_override_t $ \_ aptr pkt pktSize -> do
        addr <- Address <$> newForeignPtr_ aptr
        sendFn addr pkt pktSize

-- | Override that receives a packet from the given address. This can be used
-- by both clients and servers. This is invoked before @netcode.io@ processes
-- and decrypts the packet data.
--
-- Implementations of this callback are meant to fill the memory at the given
-- pointer with the data from a packet received from the 'Address'. The maximum
-- size of the buffer pointed to is also passed and the implementation is
-- expected to return the actual size of the packet. In the event that there is
-- no packet (or equivalently, no packet data), then the implementation should
-- return zero.
type ReceivePacketOverride
     = Address    -- ^ Address from which to receive a packet
    -> Ptr Word8  -- ^ Pointer to the buffer where to write packet data
    -> CInt       -- ^ Maximum size of destination buffer in bytes
    -> IO CInt    -- ^ Return value: should be size of packet data

mkReceivePacketOverride :: ReceivePacketOverride
                        -> IO C'receive_packet_override_t
mkReceivePacketOverride recvFn =
    mk'receive_packet_override_t $ \_ aptr pkt pktSize -> do
        addr <- Address <$> newForeignPtr_ aptr
        recvFn addr pkt pktSize
    