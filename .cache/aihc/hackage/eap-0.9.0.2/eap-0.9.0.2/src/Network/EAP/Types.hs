{-# LANGUAGE DeriveDataTypeable #-}
{-|
Module      : Network.EAP.Types
Description : Provides types and definitions for EAP as per RFC 3748
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

The types in this module are pretty much self explanatory. See RFC 3748 for further information
on what a given type or definition means.

Note that not nearly all 50+ EAP authentication types are supported. Contributions for further
types are definitely welcome.

-}
module Network.EAP.Types where

import Data.ByteString.Lazy.Char8        (ByteString)
import Data.Data                         (Data)
import Data.Word                         (Word8, Word16)

data Packet = Packet { getPacketType    :: PacketType,
                       getPacketId      :: Word8,
                       getPacketMessage :: Maybe Message }
            deriving (Show, Eq, Data)

data PacketType = RequestPacket
                | ResponsePacket
                | SuccessPacket
                | FailurePacket
                  deriving (Show, Eq, Data)

instance Enum PacketType where
    toEnum 1 = RequestPacket
    toEnum 2 = ResponsePacket
    toEnum 3 = SuccessPacket
    toEnum 4 = FailurePacket
    toEnum n = error $ "Invalid EAP packet type " ++ show n
    fromEnum RequestPacket  = 1
    fromEnum ResponsePacket = 2
    fromEnum SuccessPacket  = 3
    fromEnum FailurePacket  = 4

data Message = IdentityMessage         { getIdentityMessage         :: ByteString }
             | NotificationMessage     { getNotificationMessage     :: ByteString }
             | NakMessage              { getAuthenticationType      :: Word8 }
             | MD5ChallengeMessage     { getMD5ChallengeValue       :: ByteString,
                                         getMD5ChallengeName        :: ByteString }
             | OTPMessage              { getOTPMessage              :: ByteString }
             | GenericTokenCardMessage { getGenericTokenCardMessage :: ByteString }
             | MSCHAPv2Message         { getMSCHAPv2OpCode          :: MSCHAPv2OpCode,
                                         getMSCHAPv2Id              :: Word8,
                                         getMSCHAPv2Length          :: Word16,
                                         getMSCHAPv2Data            :: MSCHAPv2Data }
               deriving (Show, Eq, Data)

data AuthType = MD5ChallengeAuth
              | OTPAuth
              | GenericTokenCardAuth
              | RSAPubKeyAuth
              | TLSAuth
              | MSCHAPv2Auth
                deriving (Show, Eq, Data)

data MSCHAPv2OpCode = MSCHAPv2Challenge
                    | MSCHAPv2Response
                    | MSCHAPv2Success
                    | MSCHAPv2Failure
                    | MSCHAPv2ChangePassword
                    deriving (Show, Eq, Data)

data MSCHAPv2Data = MSCHAPv2ChallengeData { getMSCHAPv2Challenge :: ByteString,
                                            getMSCHAPv2ChallengeName :: ByteString }
                  | MSCHAPv2ResponseData { getMSCHAPv2ResponseData :: MSCHAPv2ResponseDataField,
                                           getMSCHAPv2ResponseName :: ByteString }
                  | MSCHAPv2SuccessRequestData { getMSCHAPv2SuccessRequestMessage :: ByteString }
                  | MSCHAPv2FailureRequestData { getMSCHAPv2FailureRequestMessage :: ByteString }
                  | MSCHAPv2ChangePasswordData { getMSCHAPv2EncryptedPassword :: ByteString,
                                                 getMSCHAPv2EncryptedHash     :: ByteString,
                                                 getMSCHAPv2PeerChallenge     :: ByteString,
                                                 getMSCHAPv2NTResponse        :: ByteString }
                  | MSCHAPv2NoData
                  deriving (Show, Eq, Data)

data MSCHAPv2ResponseDataField =
    MSCHAPv2ResponseDataField { getMSCHAPv2ResponsePeerChallenge :: ByteString,
                                getMSCHAPv2ResponseNTResponse    :: ByteString }
    deriving (Show, Eq, Data)

instance Enum AuthType where
    toEnum 4  = MD5ChallengeAuth
    toEnum 5  = OTPAuth
    toEnum 6  = GenericTokenCardAuth
    toEnum 9  = RSAPubKeyAuth
    toEnum 10 = RSAPubKeyAuth
    toEnum 13 = TLSAuth
    toEnum 26 = MSCHAPv2Auth
    toEnum x  = error $ "Unknown EAP authentication type " ++ show x
    fromEnum MD5ChallengeAuth     = 4
    fromEnum OTPAuth              = 5
    fromEnum GenericTokenCardAuth = 6
    fromEnum RSAPubKeyAuth        = 9
    fromEnum TLSAuth              = 13
    fromEnum MSCHAPv2Auth         = 26

instance Enum MSCHAPv2OpCode where
    toEnum 1 = MSCHAPv2Challenge
    toEnum 2 = MSCHAPv2Response
    toEnum 3 = MSCHAPv2Success
    toEnum 4 = MSCHAPv2Failure
    toEnum 7 = MSCHAPv2ChangePassword
    toEnum x = error $ "Unknown MSCHAPv2 Op Code: " ++ show x
    fromEnum MSCHAPv2Challenge      = 1
    fromEnum MSCHAPv2Response       = 2
    fromEnum MSCHAPv2Success        = 3
    fromEnum MSCHAPv2Failure        = 4
    fromEnum MSCHAPv2ChangePassword = 7
