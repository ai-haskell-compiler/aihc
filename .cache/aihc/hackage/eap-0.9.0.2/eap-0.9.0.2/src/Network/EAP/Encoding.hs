{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-|
Module      : Network.EAP.Encoding
Description : Provides on the wire de/coding of EAP packets as per RFC 3748
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

This module provides Binary instances for the EAP Packet type and the embedded messages it
encapsulates. So you decode a (lazy) bytestring and get an EAP Packet back or you can
encode an EAP packet to ByteString you can send on the wire as is. Simple as that.

-}
module Network.EAP.Encoding () where

import Control.Monad                   (when)
import Data.Binary                     (Binary(..), encode)
import Data.Binary.Get                 (Get,
                                        getRemainingLazyByteString,
                                        getLazyByteString,
                                        getWord8,
                                        getWord16be,
                                        runGet)
import Data.Binary.Put                 (Put, putLazyByteString, putWord8, putWord16be, runPut)
import Data.Word                       (Word16)
import Network.EAP.Types

import qualified Data.ByteString.Lazy.Char8 as LB

instance Binary Packet where
    put Packet{..} = do
      let packetData   = runPut $ putMessage getPacketType getPacketMessage
          packetLength = fromIntegral $ 4 + LB.length packetData -- EAP header length is 4 bytes
      put getPacketType
      putWord8 getPacketId
      putWord16be packetLength
      putLazyByteString packetData

    get = do
      packetType   <- get
      packetId     <- getWord8
      packetLength <- getWord16be
      let dataLength = packetLength - 4
      packetMessage <- getMessage packetType $ fromIntegral dataLength
      return $ Packet packetType packetId packetMessage

-- | Given the length of data to decode, decode an EAP message in the Get Monad. Used internally
-- so you probably don't need to use this.
getMessage :: PacketType -> Word16 -> Get (Maybe Message)
getMessage _ 0 = return Nothing
getMessage packetType dataLength = do
  packetData <- getLazyByteString $ fromIntegral dataLength
  return . Just $ runGet (decodeMessage packetType) packetData

-- | Given Maybe an EAP message to encode, do so in the Put Monad. Used internally. This is
-- necessary cause if one was to put the Maybe value directly on the wire, it would include the
-- coding for the Maybe type itself, as opposed to empty or *just* the data which is what we want
putMessage :: PacketType -> Maybe Message -> Put
putMessage _ Nothing             = return ()
putMessage packetType (Just msg) = encodeMessage packetType msg

encodeMessage :: PacketType -> Message -> Put
encodeMessage _ (IdentityMessage str)     = putWord8 1 >> putLazyByteString str
encodeMessage _ (NotificationMessage str) = putWord8 2 >> putLazyByteString str
encodeMessage _ (NakMessage value)        = putWord8 3 >> putWord8 value
encodeMessage _ MD5ChallengeMessage{..}   = do
  putWord8 4 -- MD5 Challenge
  putWord8 . fromIntegral . LB.length $ getMD5ChallengeValue
  putLazyByteString getMD5ChallengeValue
  putLazyByteString getMD5ChallengeName
encodeMessage _ (OTPMessage str)                  = putWord8 5 >> putLazyByteString str
encodeMessage _ (GenericTokenCardMessage str)     = putWord8 6 >> putLazyByteString str
encodeMessage packetType (MSCHAPv2Message op iD _len msgData)
  | op == MSCHAPv2Success || op == MSCHAPv2Failure, packetType == ResponsePacket = do
    putWord8 26 -- MSCHAPv2
    putWord8 . fromIntegral . fromEnum $ op
  | otherwise = do
    putWord8 26 -- MSCHAPv2
    putWord8 . fromIntegral . fromEnum $ op
    putWord8 iD
    let bytes  = encode msgData
        msgLen = fromIntegral $ 4 + LB.length bytes
    putWord16be msgLen
    putLazyByteString bytes

decodeMessage :: PacketType -> Get Message
decodeMessage packetType = do
      messageType <- getWord8
      getMessage' messageType
        where getMessage' 1  = getRemainingLazyByteString >>= return . IdentityMessage
              getMessage' 2  = getRemainingLazyByteString >>= return . NotificationMessage
              getMessage' 3  = getWord8 >>= return . NakMessage
              getMessage' 4  = do
                valueSize <- getWord8
                value     <- getLazyByteString $ fromIntegral valueSize
                name      <- getRemainingLazyByteString
                return $ MD5ChallengeMessage value name
              getMessage' 5  = getRemainingLazyByteString >>= return . OTPMessage
              getMessage' 6  = getRemainingLazyByteString >>= return . GenericTokenCardMessage
              getMessage' 26 = do
                op  <- getWord8 >>= return . toEnum . fromIntegral
                if packetType == ResponsePacket && (op == MSCHAPv2Success || op == MSCHAPv2Failure)
                   then return $ MSCHAPv2Message op 0 0 MSCHAPv2NoData
                   else do
                     iD  <- getWord8
                     len <- getWord16be
                     let dataLen = fromIntegral $ len - 4
                     when (dataLen < 0) $ error $ "Invalid MSCHAPv2 data length: " ++ show dataLen
                     bytes <- getLazyByteString dataLen
                     return $ MSCHAPv2Message op iD len $ runGet (decodeData op) bytes
              getMessage' n = fail $ "Invalid EAP Message Type " ++ show n

instance Binary PacketType where
    put = putWord8 . fromIntegral . fromEnum
    get = getWord8 >>= return . toEnum . fromIntegral

instance Binary MSCHAPv2Data where
    put MSCHAPv2ChallengeData{..}
      | msCHAPv2ChallengeLen == 16 = do
        putWord8 . fromIntegral $ msCHAPv2ChallengeLen
        putLazyByteString getMSCHAPv2Challenge
        putLazyByteString getMSCHAPv2ChallengeName
      | otherwise = error $ "Illegal MSCHAPv2 challenge length " ++ (show msCHAPv2ChallengeLen)
      where msCHAPv2ChallengeLen = LB.length getMSCHAPv2Challenge
    put MSCHAPv2ResponseData{..} = do
      putWord8 49 -- fixed length as per RFC
      put getMSCHAPv2ResponseData
      put getMSCHAPv2ResponseName
    put MSCHAPv2SuccessRequestData{..} =
      putLazyByteString getMSCHAPv2SuccessRequestMessage
    put MSCHAPv2FailureRequestData{..} =
      putLazyByteString getMSCHAPv2FailureRequestMessage
    put MSCHAPv2ChangePasswordData{..} =
      if LB.length getMSCHAPv2EncryptedPassword /= 516 then
          error $ "Invalid MSCHAPv2 encrypted password length "
                    ++ (show $ LB.length getMSCHAPv2EncryptedPassword)
      else if LB.length getMSCHAPv2EncryptedHash /= 16 then
          error $ "Invalid MSCHAPv2 encrypted hash length "
                    ++ (show $ LB.length getMSCHAPv2EncryptedHash)
      else if LB.length getMSCHAPv2PeerChallenge /= 16 then
          error $ "Invalid MSCHAPv2 peer challenge length "
                    ++ (show $ LB.length getMSCHAPv2PeerChallenge)
      else if LB.length getMSCHAPv2NTResponse /= 24 then
          error $ "Invalid MSCHAPv2 NT Response length "
                    ++ (show $ LB.length getMSCHAPv2NTResponse)
      else do
        putLazyByteString getMSCHAPv2EncryptedPassword
        putLazyByteString getMSCHAPv2EncryptedHash
        putLazyByteString getMSCHAPv2PeerChallenge
        putLazyByteString $ LB.replicate 8 '\NUL'
        putLazyByteString getMSCHAPv2NTResponse
        putWord16be 0 -- flags unused
    put MSCHAPv2NoData = return ()
    get = error "Impossible to decode MSCHAPv2 message data for an unknown opcode. Use decodeData"

instance Binary MSCHAPv2ResponseDataField where
    put MSCHAPv2ResponseDataField{..}
      | dataLength == 49 = do
      putLazyByteString getMSCHAPv2ResponsePeerChallenge
      putLazyByteString getMSCHAPv2ResponseNTResponse
      | otherwise = error $ "Invalid MSCHAPv2 Response Data length: " ++ show dataLength
        where dataLength = LB.length getMSCHAPv2ResponsePeerChallenge +
                           LB.length getMSCHAPv2ResponseNTResponse +
                           9 -- reserved 8 bytes + flags octet
    get = error $ "Do not use get on MSCHAPv2ResponseDataField directly. Use decodeData instead"

-- | Used internally to decode MSCHAPv2Data since it is not possible to do so in the Get Monad
-- cause the data itself is meaningless without the opcode of the enclosing packet. You normally
-- wouldn't need this.
decodeData :: MSCHAPv2OpCode -> Get MSCHAPv2Data
decodeData MSCHAPv2Challenge = do
  valueLen  <- getWord8
  when (valueLen /= 16) $ error $ "Invalid MSCHAPv2 Challenge length " ++ show valueLen
  challenge <- getLazyByteString 16
  name      <- getRemainingLazyByteString
  return $ MSCHAPv2ChallengeData challenge name
decodeData MSCHAPv2Response = do
  valueLen     <- getWord8
  when (valueLen /= 49) $ error $ "Invalid MSCHAPv2 Response value length " ++ show valueLen
  challenge    <- getLazyByteString 16
  _reserved    <- getLazyByteString 8
  ntResponse   <- getLazyByteString 24
  _flagsUnused <- getWord8
  let msCHAPv2ResponseData = MSCHAPv2ResponseDataField challenge ntResponse
  name         <- getRemainingLazyByteString
  return $ MSCHAPv2ResponseData msCHAPv2ResponseData name
decodeData MSCHAPv2Success = do
  msg <- getRemainingLazyByteString
  return $ MSCHAPv2SuccessRequestData msg
decodeData MSCHAPv2Failure = do
  msg <- getRemainingLazyByteString
  return $ MSCHAPv2FailureRequestData msg
decodeData MSCHAPv2ChangePassword = do
  password   <- getLazyByteString 516
  hash       <- getLazyByteString 16
  challenge  <- getLazyByteString 16
  _reserved  <- getLazyByteString 8
  ntResponse <- getLazyByteString 24
  return $ MSCHAPv2ChangePasswordData password hash challenge ntResponse
