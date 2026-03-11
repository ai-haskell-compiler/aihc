module Data.Radius.StreamPut.Base (
  upacket, packet,

  header, attribute', vendorID, simpleVendorAttribute,

  code, bin128,

  atText, atString, atInteger, atIpV4,
  ) where

import Data.Word (Word8, Word32)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Serialize.Put
  (Put, putWord8, putWord16be, putWord32be,
   putByteString, runPut)

import Data.Radius.Scalar
  (Bin128, fromBin128, AtText (..), AtString (..), AtInteger (..), AtIpV4 (..))
import Data.Radius.Packet (Code, Header, Packet, codeToWord)
import qualified Data.Radius.Packet as Data
import Data.Radius.Attribute (NumberAbstract (..), Attribute' (..))
import qualified Data.Radius.Attribute as Attribute


code :: Code -> Put
code c = putWord8 $ codeToWord c

pktId :: Word8 -> Put
pktId = putWord8

bin128 :: Bin128 -> Put
bin128 = putByteString . fromBin128

header :: Header -> Put
header h = do
  code  $ Data.code h
  pktId $ Data.pktId h
  putWord16be $ Data.pktLength h
  bin128 $ Data.authenticator h

packet :: (a -> Put) -> Packet a -> Put
packet putAttrs pkt = do
  header   $ Data.header pkt
  putAttrs $ Data.attributes pkt

radiusNumber :: Attribute.Number -> Put
radiusNumber = putWord8 . Attribute.toWord

vendorID :: Word32 -> Put
vendorID = putWord32be

simpleVendorAttribute :: Word8
                      -> ByteString
                      -> Put
simpleVendorAttribute n bs = do
  putWord8 n
  putWord8 $ fromIntegral (BS.length bs) + 2
  putByteString bs

vendorAttribute :: (a -> ByteString -> Put)
                -> a -> ByteString -> Put
vendorAttribute = id

attribute' :: (a -> ByteString -> Put)
           -> (Attribute' a) -> Put
attribute' vp (Attribute' an bs) = do
  case an of
    Standard n -> do
      radiusNumber n
      putWord8 $ fromIntegral (BS.length bs) + 2
      putByteString bs
    Vendors nn  -> do
      radiusNumber Attribute.VendorSpecific

      let bsn = runPut $ vendorAttribute vp nn bs
      putWord8 . fromIntegral $ BS.length bsn + 2  {- sizeof(number)            1
                                                    + sizeof(attribute length)  1 -}
      putByteString bsn

upacket :: (a -> ByteString -> Put)
        -> Packet [Attribute' a] -> Put
upacket vp = packet $ mapM_ $ attribute' vp


atText :: AtText -> Put
atText (AtText t) = putByteString . Text.encodeUtf8 $ Text.pack t

atString :: AtString -> Put
atString (AtString s) = putByteString s

atInteger :: AtInteger -> Put
atInteger (AtInteger i) = putWord32be i

atIpV4 :: AtIpV4 -> Put
atIpV4 (AtIpV4 ip) = putWord32be ip
