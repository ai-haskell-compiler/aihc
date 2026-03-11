{-# LANGUAGE DeriveGeneric #-}
module Netcode.IO.Packet where

--------------------------------------------------------------------------------

import Data.Word (Word8, Word64)
import Foreign.ForeignPtr (ForeignPtr)
import GHC.Generics (Generic)

import Bindings.Netcode.IO

--------------------------------------------------------------------------------

-- | A packet is a basic unit of data that is transferred between client and
-- server. Sequence numbers indicate the order in which the packets were sent,
-- and this library contains no guarantee that they will be received in a
-- monotonically increasing order.
data Packet = Packet {
    -- | The sequence number for this packet.
    packetSequenceNumber :: Word64,
    -- | The size, in bytes, of the data stored at 'packetDataPtr'
    packetSize :: Int,
    -- | A pointer to the bytes that are contained in this packet. This can be
    -- cast to any pointer type for the purposes of deseralizing, but this
    -- pointer must outlive the amount of time that this library has been
    -- initialized.
    packetDataPtr :: ForeignPtr Word8
} deriving (Show, Generic)

-- | The maximum size, in bytes, of a packet. In other words, this is the
-- maximum value that 'packetSize' can take.
maximumPacketSize :: Num a => a
maximumPacketSize = c'NETCODE_MAX_PACKET_SIZE
