module Data.Radius.StreamGet.Base (
  upacket, packet,

  header, attribute', vendorID, simpleVendorAttribute,

  code, bin128,

  atText, atString, atInteger, atIpV4,

  eof,
  ) where

import Control.Applicative ((<$>), pure, (<*>), (<*), (<|>), many)
import Control.Monad (guard)
import Data.ByteString (ByteString)
import Data.Word (Word8, Word32)
import qualified Data.Text as Text
import qualified Data.Text.Encoding as Text
import Data.Serialize.Get
  (Get, getWord8, getWord16be, getWord32be,
   getBytes, isEmpty, runGet)

import Data.Radius.Scalar
  (Bin128, mayBin128, AtText (..), AtString (..), AtInteger (..), AtIpV4 (..))
import Data.Radius.Packet
  (Code, Header (Header), Packet (Packet), codeFromWord)
import qualified Data.Radius.Packet as Data
import Data.Radius.Attribute (NumberAbstract (..), Attribute' (..))
import qualified Data.Radius.Attribute as Attribute


code :: Get Code
code = codeFromWord <$> getWord8

pktId :: Get Word8
pktId = getWord8

bin128 :: Get Bin128
bin128 =
  maybe
  (fail "Illegal state: Bin128")
  pure
  . mayBin128 =<< getBytes 16

header :: Get Header
header =
  Header
  <$> code
  <*> pktId
  <*> getWord16be
  <*> bin128

eof :: Get ()
eof = guard =<< isEmpty

packet :: Get a -> Get (Packet a)
packet getAttrs = do
  h   <-  header
  let alen = fromIntegral (Data.pktLength h) - 20 {- sizeof(code) + sizeof(pktId) + sizeof(pkgLength) + sizeof(authenticator) -}
  guard (alen >= 0) <|> fail ("Parse error of header: Packet: invalid length: " ++ show alen)
  bs  <-  getBytes alen
  either
    (fail . ("Parse error of attributes: Packet: " ++))
    (pure . Packet h)
    $ runGet (getAttrs <* eof) bs

radiusNumber :: Get Attribute.Number
radiusNumber = Attribute.fromWord <$> getWord8

vendorID :: Get Word32
vendorID = getWord32be

simpleVendorAttribute :: Get (Word8, ByteString)
simpleVendorAttribute = do
  n    <-  getWord8
  len  <-  getWord8
  bs   <-  getBytes $ fromIntegral len - 2 {- sizeof(number) + sizeof(attribute length) -}
  pure $ (n, bs)

-- {26, length, vendorID(45137), サブ属性番号, サブ属性 length, サブ属性値}

attribute' :: Get (Attribute' v) -> Get (Attribute' v)
attribute' va = do
  n    <- radiusNumber
  len  <- getWord8
  bs   <- getBytes $ fromIntegral len - 2 {- sizeof(number) + sizeof(attribute length) -}
  case n of
    Attribute.VendorSpecific ->
      either (fail . ("Parse error of Vendor-Specific attribute: " ++)) pure
      $ runGet va bs
    _                     ->
      pure $ Attribute' (Standard n) bs

upacket :: Get (Attribute' v) -> Get (Packet [Attribute' v])
upacket va = packet $ many $ attribute' va


atText :: Int -> Get AtText
atText len
  | 0 <= len && len <= 253  =  either
                               (fail . ("Get.atText: fail to decode UTF8: " ++) . show)
                               (pure . AtText . Text.unpack)
                               =<< Text.decodeUtf8' <$> getBytes len
  | len > 253               =  fail $ "Get.atText: Too long: " ++ show len
  | otherwise               =  fail $ "Get.atText: Positive length required: " ++ show len

atString :: Int -> Get AtString
atString len
  | 0 <= len && len <= 253  =  AtString <$> getBytes len
  | len > 253               =  fail $ "Get.atString: Too long: " ++ show len
  | otherwise               =  fail $ "Get.atString: Positive length required: " ++ show len

atInteger :: Get AtInteger
atInteger = AtInteger <$> getWord32be

atIpV4 :: Get AtIpV4
atIpV4 = AtIpV4 <$> getWord32be
