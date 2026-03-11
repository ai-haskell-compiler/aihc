{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Network.RADIUS.Encoding
Description : Provides on the wire de/coding of RADIUS packets as per RFC 3748
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

This module provides Binary instances for the RADIUS Packet type and attributes. So you basically decode a (lazy) ByteString and get a RADIUS Packet back or you can encode a RADIUS Packet to a ByteString, which you can then send on the wire as is, etc. Simple as that.

-}

module Network.RADIUS.Encoding where

import Control.Monad               (when)
import Data.Binary                 (Binary(..), decode, encode)
import Data.Binary.Put             (Put, putLazyByteString, putWord8, putWord16be, putWord32be, runPut)
import Data.Binary.Get             (Get,
                                    getLazyByteString,
                                    getWord8,
                                    getWord16be,
                                    getWord32be,
                                    isEmpty)
import Data.ByteArray              (convert)
import Data.ByteString.Lazy.Char8  (ByteString, append)
import Data.IP                     (IPv4, IPv6)
import Data.Int                    (Int64)
import Data.Word                   (Word8)
import Crypto.Hash.Algorithms      (MD5)
import Crypto.Hash                 (Digest, hashlazy)
import Network.RADIUS.Types

import qualified Data.ByteString.Lazy.Char8 as B

-- | Self explanatory. It can be useful when reading a RADIUS packet from a socket for example,
-- so one can retrieve the packet header (containing the packet length) first and then use that
-- to figure out how much data is left to read
radiusHeaderSize :: Num a => a
radiusHeaderSize = 20

-- | Fixed authenticator length as per RFC 2865
authenticatorLength :: Int64
authenticatorLength = 16

instance Binary Packet where
    put Packet{ _getHeader = Header{..} , .. } = do
      let iD         = fromIntegral $ _getPacketId
          authLen    = B.length _getPacketAuthenticator
          attributes = encodeAttributes _getPacketAttributes
          attrsLen   = fromIntegral $ B.length $ attributes
      when (authLen /= authenticatorLength) $
           error $ "RADIUS.Encoding: Invalid Authenticator length " ++ show authLen
      put _getPacketType
      putWord8 iD
      putWord16be $ attrsLen + radiusHeaderSize
      putLazyByteString _getPacketAuthenticator
      putLazyByteString attributes
    get = do
      header <- decodeHeader
      decodePacket header

-- | Allows decoding of a RADIUS header in the Get Monad
decodeHeader :: Get Header
decodeHeader = do
  packetType    <- get
  iD            <- getWord8
  packetLength  <- getWord16be
  authenticator <- getLazyByteString authenticatorLength
  return Header { _getPacketType          = packetType,
                  _getPacketId            = iD,
                  _getPacketLength        = packetLength,
                  _getPacketAuthenticator = authenticator }

-- | Given an already decoded header, this function can be used to decode the complete packet
-- from the available data
decodePacket :: Header -> Get Packet
decodePacket header@Header{..} = do
  attributes <- decodeAttributes []
  return Packet { _getHeader           = header,
                  _getPacketAttributes = attributes }

sign :: ByteString -> ByteString -> ByteString
sign packet secret =
    let authenticator = hashMD5 $ packet `append` secret
        prologue      = B.take 4 packet -- size of type, id, length
        attributes    = B.drop radiusHeaderSize packet
    in prologue `append` authenticator `append` attributes

hashMD5 :: ByteString -> ByteString
hashMD5 = B.fromStrict . convert . (hashlazy :: ByteString -> Digest MD5)

instance Binary PacketType where
    put = putWord8 . fromIntegral . fromEnum
    get = getWord8 >>= return . toEnum . fromIntegral

-- | Used internally to encode a list of RADIUS attributes. You probably don't need this.
encodeAttributes :: [PacketAttribute] -> ByteString
encodeAttributes = B.concat . fmap encode

-- | Used internally to decode a list of RADIUS attributes. You probably don't need this.
decodeAttributes :: [PacketAttribute] -> Get [PacketAttribute]
decodeAttributes acc = do
  done <- isEmpty
  if done
    then return . reverse $ acc
    else do
      attribute <- get
      decodeAttributes $ attribute : acc

instance Binary IPv4
instance Binary IPv6

instance Binary PacketAttribute where
    put (AcctSessionIdAttribute str)            = putAttributeStr  44 str
    put (AcctMultiSessionIdAttribute str)       = putAttributeStr  50 str
    put (UserNameAttribute str)                 = putAttributeStr   1 str
    put (UserPasswordAttribute str)             = putAttributeStr   2 str
    put (FramedIPv6Route str)                   = putAttributeStr  99 str
    put (FramedIPv6Pool str)                    = putAttributeStr 100 str
    put (FilterIdAttribute str)                 = putAttributeStr  11 str
    put (ReplyMessageAttribute str)             = putAttributeStr  18 str
    put (CallbackNumberAttribute str)           = putAttributeStr  19 str
    put (CallbackIdAttribute str)               = putAttributeStr  20 str
    put (FramedRouteAttribute str)              = putAttributeStr  22 str
    put (StateAttribute str)                    = putAttributeStr  24 str
    put (ClassAttribute str)                    = putAttributeStr  25 str
    put (CalledStationIdAttribute str)          = putAttributeStr  30 str
    put (CallingStationIdAttribute str)         = putAttributeStr  31 str
    put (NASIdentifierAttribute str)            = putAttributeStr  32 str
    put (ProxyStateAttribute str)               = putAttributeStr  33 str
    put (LoginLATServiceAttribute str)          = putAttributeStr  34 str
    put (LoginLATNodeAttribute str)             = putAttributeStr  35 str
    put (LoginLATGroupAttribute str)            = putAttributeStr  36 str
    put (FramedAppleTalkZoneAttribute str)      = putAttributeStr  39 str
    put (CHAPChallengeAttribute str)            = putAttributeStr  60 str
    put (LoginLATPortAttribute str)             = putAttributeStr  63 str
    put (ARAPPasswordAttribute str)             = putAttributeStr  70 str
    put (ARAPFeaturesAttribute str)             = putAttributeStr  71 str
    put (ARAPSecurityDataAttribute str)         = putAttributeStr  74 str
    put (ConnectInfoAttribute str)              = putAttributeStr  77 str
    put (ConfigurationTokenAttribute str)       = putAttributeStr  78 str
    put (EAPMessageAttribute str)               = putAttributeStr  79 str
    put (MessageAuthenticatorAttribute str)     = putAttributeStr  80 str
    put (ARAPChallengeResponseAttribute str)    = putAttributeStr  84 str
    put (NASPortIdAttribute str)                = putAttributeStr  87 str
    put (FramedPoolAttribute str)               = putAttributeStr  88 str
    put (AcctStatusTypeAttribute value)         = putAttribute     40 value
    put (AcctDelayTimeAttribute value)          = putAttribute     41 value
    put (AcctInputOctetsAttribute value)        = putAttribute     42 value
    put (AcctOutputOctetsAttribute value)       = putAttribute     43 value
    put (AcctAuthenticAttribute value)          = putAttribute     45 value
    put (AcctSessionTimeAttribute value)        = putAttribute     46 value
    put (AcctInputPacketsAttribute value)       = putAttribute     47 value
    put (AcctOutputPacketsAttribute value)      = putAttribute     48 value
    put (AcctTerminateCauseAttribute value)     = putAttribute     49 value
    put (AcctLinkCountAttribute value)          = putAttribute     51 value
    put (NASPortAttribute value)                = putAttribute      5 value
    put (FramedMTUAttribute value)              = putAttribute     12 value
    put (LoginTCPPortAttribute value)           = putAttribute     16 value
    put (FramedIPXNetworkAttribute value)       = putAttribute     23 value
    put (SessionTimeoutAttribute value)         = putAttribute     27 value
    put (IdleTimeoutAttribute value)            = putAttribute     28 value
    put (FramedAppleTalkLinkAttribute value)    = putAttribute     37 value
    put (FramedAppleTalkNetworkAttribute value) = putAttribute     38 value
    put (PortLimitAttribute value)              = putAttribute     62 value
    put (NASIPAddress value)                    = putAttribute      4 value
    put (NASIPv6Address value)                  = putAttribute     95 value
    put (ServiceTypeAttribute value)            = putAttribute      6 value
    put (FramedProtocolAttribute value)         = putAttribute      7 value
    put (FramedIPAddressAttribute value)        = putAttribute      8 value
    put (FramedIPNetmaskAttribute value)        = putAttribute      9 value
    put (FramedRoutingAttribute value)          = putAttribute     10 value
    put (FramedCompressionAttribute value)      = putAttribute     13 value
    put (FramedInterfaceIdAttribute value)      = putAttribute     96 value
    put (LoginIPHostAttribute value)            = putAttribute     14 value
    put (LoginIPv6HostAttribute value)          = putAttribute     98 value
    put (LoginServiceAttribute value)           = putAttribute     15 value
    put (TerminationActionAttribute value)      = putAttribute     29 value
    put (NASPortTypeAttribute value)            = putAttribute     61 value
    put (AccountInputGigawordsAttribute value)  = putAttribute     52 value
    put (AccountOutputGigawordsAttribute value) = putAttribute     53 value
    put (EventTimeStampAttribute value)         = putAttribute     55 value
    put (ARAPZoneAccessAttribute value)         = putAttribute     72 value
    put (ARAPSecurityAttribute value)           = putAttribute     73 value
    put (PasswordRetryAttribute value)          = putAttribute     75 value
    put (PromptAttribute value)                 = putAttribute     76 value
    put (AcctInterimIntervalAttribute value)    = putAttribute     85 value
    put (DNSServerIPv6AddressAttribute value)   = putAttribute    169 value
    put (UnknownAttribute attributeType value)  = putAttribute attributeType value
    put (FramedIPv6Prefix prefixLength prefix)    = putAttributeM  97 $ do
      putWord8 0  -- reserved
      putWord8 prefixLength
      putLazyByteString $ encode prefix
    put (DelegatedIPv6Prefix prefixLength prefix) = putAttributeM 123 $ do
      putWord8 0 -- reserved
      putWord8 prefixLength
      putLazyByteString $ encode prefix
    put (VendorSpecificAttribute vendorId str)    = putAttributeM  26 $ do
      when (B.null str) $ error "Empty String not allowed in Vendor-Specific Attribute "
      putWord32be vendorId
      putLazyByteString str
    put (CHAPPassword identity str)               = putAttributeM   3 $ do
      when (B.length str /= 16) $ error $ "Invalid RADIUS CHAP Password length " ++ show str
      putWord8 identity
      putLazyByteString str

    get = do
      code <- getWord8
      getAttribute code

-- | For internal use
putAttributeStr :: Word8 -> ByteString -> Put
putAttributeStr code str = do
    putWord8 code -- Attribute type
    putWord8 $ (fromIntegral $ B.length $ str) + 2 -- attr length + code + len octets
    putLazyByteString str

-- | For internal use
putAttribute :: (Binary a) => Word8 -> a -> Put
putAttribute code = putAttributeStr code . encode

putAttributeM :: Word8 -> Put -> Put
putAttributeM code = putAttributeStr code . runPut

getAttribute :: Word8 -> Get PacketAttribute
getAttribute  44 = getAttributeStr >>= return . AcctSessionIdAttribute
getAttribute  50 = getAttributeStr >>= return . AcctMultiSessionIdAttribute
getAttribute 40 = getAttributeValue >>= return . AcctStatusTypeAttribute
getAttribute 41 = getAttributeValue >>= return . AcctDelayTimeAttribute
getAttribute 42 = getAttributeValue >>= return . AcctInputOctetsAttribute
getAttribute 43 = getAttributeValue >>= return . AcctOutputOctetsAttribute
getAttribute 45 = getAttributeValue >>= return . AcctAuthenticAttribute
getAttribute 46 = getAttributeValue >>= return . AcctSessionTimeAttribute
getAttribute 47 = getAttributeValue >>= return . AcctInputPacketsAttribute
getAttribute 48 = getAttributeValue >>= return . AcctOutputPacketsAttribute
getAttribute 49 = getAttributeValue >>= return . AcctTerminateCauseAttribute
getAttribute 51 = getAttributeValue >>= return . AcctLinkCountAttribute
getAttribute   1 = getAttributeStr >>= return . UserNameAttribute
getAttribute   2 = getAttributeStr >>= return . UserPasswordAttribute
getAttribute  99 = getAttributeStr >>= return . FramedIPv6Route
getAttribute 100 = getAttributeStr >>= return . FramedIPv6Pool
getAttribute  11 = getAttributeStr >>= return . FilterIdAttribute
getAttribute  18 = getAttributeStr >>= return . ReplyMessageAttribute
getAttribute  19 = getAttributeStr >>= return . CallbackNumberAttribute
getAttribute  20 = getAttributeStr >>= return . CallbackIdAttribute
getAttribute  22 = getAttributeStr >>= return . FramedRouteAttribute
getAttribute  24 = getAttributeStr >>= return . StateAttribute
getAttribute  25 = getAttributeStr >>= return . ClassAttribute
getAttribute  30 = getAttributeStr >>= return . CalledStationIdAttribute
getAttribute  31 = getAttributeStr >>= return . CallingStationIdAttribute
getAttribute  32 = getAttributeStr >>= return . NASIdentifierAttribute
getAttribute  33 = getAttributeStr >>= return . ProxyStateAttribute
getAttribute  34 = getAttributeStr >>= return . LoginLATServiceAttribute
getAttribute  35 = getAttributeStr >>= return . LoginLATNodeAttribute
getAttribute  36 = getAttributeStr >>= return . LoginLATGroupAttribute
getAttribute  39 = getAttributeStr >>= return . FramedAppleTalkZoneAttribute
getAttribute  60 = getAttributeStr >>= return . CHAPChallengeAttribute
getAttribute  63 = getAttributeStr >>= return . LoginLATPortAttribute
getAttribute  70 = getAttributeStr >>= return . ARAPPasswordAttribute
getAttribute  71 = getAttributeStr >>= return . ARAPFeaturesAttribute
getAttribute  74 = getAttributeStr >>= return . ARAPSecurityDataAttribute
getAttribute  77 = getAttributeStr >>= return . ConnectInfoAttribute
getAttribute  78 = getAttributeStr >>= return . ConfigurationTokenAttribute
getAttribute  79 = getAttributeStr >>= return . EAPMessageAttribute
getAttribute  80 = getAttributeStr >>= return . MessageAuthenticatorAttribute
getAttribute  84 = getAttributeStr >>= return . ARAPChallengeResponseAttribute
getAttribute  87 = getAttributeStr >>= return . NASPortIdAttribute
getAttribute  88 = getAttributeStr >>= return . FramedPoolAttribute
getAttribute 5  = getAttributeValue >>= return . NASPortAttribute
getAttribute 12 = getAttributeValue >>= return . FramedMTUAttribute
getAttribute 16 = getAttributeValue >>= return . LoginTCPPortAttribute
getAttribute 23 = getAttributeValue >>= return . FramedIPXNetworkAttribute
getAttribute 27 = getAttributeValue >>= return . SessionTimeoutAttribute
getAttribute 28 = getAttributeValue >>= return . IdleTimeoutAttribute
getAttribute 37 = getAttributeValue >>= return . FramedAppleTalkLinkAttribute
getAttribute 38 = getAttributeValue >>= return . FramedAppleTalkNetworkAttribute
getAttribute 62 = getAttributeValue >>= return . PortLimitAttribute
getAttribute 4  = getAttributeValue >>= return . NASIPAddress
getAttribute 95 = getAttributeValue >>= return . NASIPv6Address
getAttribute 6  = getAttributeValue >>= return . ServiceTypeAttribute
getAttribute 7  = getAttributeValue >>= return . FramedProtocolAttribute
getAttribute 8  = getAttributeValue >>= return . FramedIPAddressAttribute
getAttribute 9  = getAttributeValue >>= return . FramedIPNetmaskAttribute
getAttribute 10 = getAttributeValue >>= return . FramedRoutingAttribute
getAttribute 13 = getAttributeValue >>= return . FramedCompressionAttribute
getAttribute 96 = getAttributeValue >>= return . FramedInterfaceIdAttribute
getAttribute 14 = getAttributeValue >>= return . LoginIPHostAttribute
getAttribute 98 = getAttributeValue >>= return . LoginIPv6HostAttribute
getAttribute 15 = getAttributeValue >>= return . LoginServiceAttribute
getAttribute 29 = getAttributeValue >>= return . TerminationActionAttribute
getAttribute 61 = getAttributeValue >>= return . NASPortTypeAttribute
getAttribute 52 = getAttributeValue >>= return . AccountInputGigawordsAttribute
getAttribute 53 = getAttributeValue >>= return . AccountOutputGigawordsAttribute
getAttribute 55 = getAttributeValue >>= return . EventTimeStampAttribute
getAttribute 72 = getAttributeValue >>= return . ARAPZoneAccessAttribute
getAttribute 73 = getAttributeValue >>= return . ARAPSecurityAttribute
getAttribute 75 = getAttributeValue >>= return . PasswordRetryAttribute
getAttribute 76 = getAttributeValue >>= return . PromptAttribute
getAttribute 85 = getAttributeValue >>= return . AcctInterimIntervalAttribute
getAttribute 169 = getAttributeValue >>= return . DNSServerIPv6AddressAttribute
getAttribute 97 = do
  attrLen      <- getWord8
  _reserved    <- getWord8
  prefixLength <- getWord8
  when (attrLen < 4 || attrLen > 20) $
    error $ show attrLen ++ " is an invalid RADIUS Framed IPv6 Prefix attribute length"
  let available = fromIntegral $ attrLen - 4
  prefix <- getFromBytes available 20
  return $ FramedIPv6Prefix prefixLength prefix
getAttribute 26 = do
  attrLen  <- getWord8
  iD       <- get
  attrData <- getLazyByteString . fromIntegral $ attrLen - 6
  return $ VendorSpecificAttribute iD attrData
getAttribute 3 = do
  attrLen  <- getWord8
  when (attrLen /= 19) $ error $ "Invalid RADIUS CHAP Password length " ++ show attrLen
  identity <- getWord8
  attrData <- getLazyByteString 16 -- CHAP response is always 16 octets
  return $ CHAPPassword identity attrData
getAttribute 123 = do
  attrLen      <- getWord8
  when (attrLen < 4 || attrLen > 20) $
    error $ show attrLen ++
              " is an invalid RADIUS Delegated IPv6 Prefix attribute length"
  _reserved    <- getWord8
  prefixLength <- getWord8
  let available = fromIntegral $ attrLen - 4
  prefix <- getFromBytes available 20
  return $ DelegatedIPv6Prefix prefixLength prefix
getAttribute n  = getAttributeStr >>= return . UnknownAttribute n

-- | For internal use.
getFromBytes :: Binary a => Int64 -> Int64 -> Get a
getFromBytes len size = do
  let padBytes    = size - len
      pad         = B.replicate padBytes '\0'
  str <- getLazyByteString len
  return . decode $ B.append str pad

getAttributeStr :: Get ByteString
getAttributeStr = getWord8 >>= getLazyByteString . fromIntegral . (subtract 2) -- minus type + len

-- | For internal use.
getAttributeValue :: (Binary a) => Get a
getAttributeValue = getWord8 >> get

instance Binary ServiceType where
    put = putEnum
    get = getEnum

instance Binary FramedProtocol where
    put = putEnum
    get = getEnum

instance Binary FramedRouting where
    put = putEnum
    get = getEnum

instance Binary FramedCompression where
    put = putEnum
    get = getEnum

instance Binary LoginService where
    put = putEnum
    get = getEnum

instance Binary TerminationAction where
    put = putEnum
    get = getEnum

instance Binary NASPortType where
    put = putEnum
    get = getEnum

instance Binary ARAPZoneAccess where
    put = putEnum
    get = getEnum

instance Binary StatusType where
    put = putEnum
    get = getEnum

instance Binary Authentic where
    put = putEnum
    get = getEnum

instance Binary TerminateCause where
    put = putEnum
    get = getEnum

-- | For internal use.
putEnum :: (Enum a) => a -> Put
putEnum = putWord32be . fromIntegral . fromEnum

-- | For internal use.
getEnum :: (Enum a) => Get a
getEnum = getWord32be >>= return . toEnum . fromIntegral
