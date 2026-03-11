module Test.Data.Radius.IsoBase (
  isoAttribute',
  isoPacket,
  ) where

import Data.ByteString (ByteString)
import Data.Serialize.Get (Get, runGet)
import Data.Serialize.Put (Put, runPut)

import Data.Radius.Packet (Packet)
import Data.Radius.Attribute (Attribute')
import qualified Data.Radius.StreamGet as Get
import qualified Data.Radius.StreamPut as Put


isoAttribute' :: Eq a
              => Get (Attribute' a)
              -> (a -> ByteString -> Put)
              -> Attribute' a -> Bool
isoAttribute' vGet vPut a =
  runGet (Get.attribute' vGet) (runPut $ Put.attribute' vPut a)
  ==
  Right a

isoPacket :: Eq a
          => Get (Attribute' a)
          -> (a -> ByteString -> Put)
          -> Packet [Attribute' a] -> Bool
isoPacket vGet vPut p =
  runGet (Get.upacket vGet) (runPut $ Put.upacket vPut p)
  ==
  Right p
