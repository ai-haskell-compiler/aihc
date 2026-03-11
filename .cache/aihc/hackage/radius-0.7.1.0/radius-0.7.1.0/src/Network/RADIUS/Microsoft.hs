{-# LANGUAGE RecordWildCards, OverloadedStrings #-}
{-|
Module      : Network.RADIUS.Microsoft
Description : Microsoft specific RADIUS Attributes
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

This module provides encoding for some of the Microsoft specific attributes, particularly those
needed for MSCHAPv2.

-}
module Network.RADIUS.Microsoft (encodeMPPESendKeyAttribute,
                                 encodeMPPERecvKeyAttribute,
                                 encodeMPPEEncryptionPolicyAttribute,
                                 encodeMPPEEncryptionTypesAttribute,
                                 encodePrimaryDNSServer,
                                 encodeSecondaryDNSServer) where

import Prelude hiding (zipWith)
import Crypto.Hash.Algorithms    (MD5)
import Crypto.Hash               (Digest, hash)
import Data.Binary.Put           (Put, putByteString, putWord8, putWord16be, runPut, putWord32be)
import Data.Bits                 ((.|.), xor)
import Data.ByteArray            (convert)
import Data.ByteString           (ByteString, pack, zipWith)
import Data.ByteString.Internal  (w2c)
import Data.IP                   (IPv4)
import Data.Word                 (Word8, Word16, Word32)
import Network.RADIUS.Encoding   (putAttribute)
import Network.RADIUS.Types

import qualified Data.ByteString.Lazy.Char8 as LB
import qualified Data.ByteString.Char8      as B

-- | Wraps the given encoded vendor specific attribute data into a PacketAttribute with
-- Microsoft SMI Network Management Enterprise Code
vendorSpecificAttribute :: LB.ByteString -> PacketAttribute
vendorSpecificAttribute = VendorSpecificAttribute 311

-- | Encode the MS-MPPE-Send-Key RADIUS attribute as per [RFC2548]
encodeMPPESendKeyAttribute :: Word16    -- ^ 16 bit random salt
                           -> ByteString -- ^ MPPE send key
                           -> ByteString -- ^ Password
                           -> ByteString -- ^ Authenticator in Access-Request message
                           -> PacketAttribute
encodeMPPESendKeyAttribute salt key secret authenticator =
    vendorSpecificAttribute . runPut $ encodeMPPEKeyAttribute 16 salt key secret authenticator

-- | Encode the MS-MPPE-Recv-Key RADIUS attribute as per [RFC2548]
encodeMPPERecvKeyAttribute :: Word16     -- ^ 16 bit random salt
                           -> ByteString -- ^ MPPE recv key
                           -> ByteString -- ^ Password
                           -> ByteString -- ^ Authenticator in Access-Request message
                           -> PacketAttribute
encodeMPPERecvKeyAttribute salt key secret authenticator =
    vendorSpecificAttribute . runPut $ encodeMPPEKeyAttribute 17 salt key secret authenticator

encodeMPPEKeyAttribute :: Word8
                       -> Word16
                       -> ByteString
                       -> ByteString
                       -> ByteString
                       -> Put
encodeMPPEKeyAttribute vendorType salt key secret authenticator = do
  putWord8 vendorType
  let salt'      = LB.toStrict . runPut . putWord16be $ salt .|. 0x8000 -- MSB in salt must be set
      keyLength  = w2c . fromIntegral $ B.length key
      str        = B.cons keyLength key
      n          = B.length str `mod` 16
      m          = if n == 0 then 0 else 16 - n
      str'       = str <> B.replicate m '\NUL'
      (_,result) = foldl encrypt ((authenticator <> salt'), B.empty) $ partition 16 str'
      vendorLen  = fromIntegral $ 4 + B.length result
  putWord8 vendorLen
  putByteString salt'
  putByteString result
      where md5                  = convert . (hash :: ByteString -> Digest MD5)
            partition n          = partition' [] n
            partition' acc _ ""  = reverse acc
            partition' acc n str =
                let (x, xs) = B.splitAt n str
                in partition' (x:acc) n xs
            encrypt (bytes, acc) chunk =
                let c = pack $ zipWith xor chunk (md5 $ secret <> bytes)
                in (c, acc <> c)

-- | Encode MS-MPPE-Encryption-Policy as per [RFC2548]
encodeMPPEEncryptionPolicyAttribute :: Word32         -- ^ Policy value
                                    -> PacketAttribute
encodeMPPEEncryptionPolicyAttribute policy =
    vendorSpecificAttribute . runPut $ do
      putWord8 7 -- for MS-MPPE-Encryption-Policy.
      putWord8 6 -- fixed length
      putWord32be policy

-- | Encode MS-MPPE-Encryption-Types as per [RFC2548]
encodeMPPEEncryptionTypesAttribute :: Word32
                                   -> PacketAttribute -- ^ Encryption types value (see RFC)
encodeMPPEEncryptionTypesAttribute types =
    vendorSpecificAttribute . runPut $ do
      putWord8 8 -- for MS-MPPE-Encryption-Types.
      putWord8 6 -- fixed length
      putWord32be types

-- | Encode MS-DNS-Primary-DNA-Server as per [RFC2548]
encodePrimaryDNSServer :: IPv4
                       -> PacketAttribute
encodePrimaryDNSServer =
    vendorSpecificAttribute . runPut . putAttribute 28

-- | Encode MS-DNS-Secondary-DNA-Server as per [RFC2548]
encodeSecondaryDNSServer :: IPv4
                       -> PacketAttribute
encodeSecondaryDNSServer =
    vendorSpecificAttribute . runPut . putAttribute 29
