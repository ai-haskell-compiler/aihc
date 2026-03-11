{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE TemplateHaskell    #-}
{-|
Module      : Network.RADIUS.Types
Description : Provides types and definitions for RADIUS as per RFC 2865
Copyright   : (c) Erick Gonzalez, 2017
License     : BSD3
Maintainer  : erick@codemonkeylabs.de
Stability   : experimental
Portability : POSIX

This module compiles the RADIUS packet definitions and different attributes as specified in
RFC 2865. The naming conventions from the RFC have been preserved as much as possible, so
it should be straightforward to look up a particular element and understand what it means etc.

RADIUS extensions in RFC 2869 are also supported, as well as RFC 3162 for IPv6 related attributes

-}
module Network.RADIUS.Types where

import Data.ByteString.Lazy.Char8    (ByteString)
import Data.Data                     (Data)
import Data.Word                     (Word8, Word16, Word32, Word64)
import Data.IP                       (IPv4, IPv6)
import Control.Lens.TH               (makePrisms, makeLenses)

data Header = Header { _getPacketType          :: PacketType,
                       _getPacketId            :: Word8,
                       _getPacketLength        :: Word16,
                       _getPacketAuthenticator :: ByteString }
              deriving (Show, Eq)

data Packet = Packet { _getHeader           :: Header,
                       _getPacketAttributes :: [PacketAttribute] }
              deriving (Show, Eq)

data PacketType = AccessRequest
                | AccessAccept
                | AccessReject
                | AccountingRequest
                | AccountingResponse
                | AccessChallenge
                | StatusServer
                | StatusClient
                | UnknownPacketType Int
                  deriving (Show, Eq)

instance Enum PacketType where
    fromEnum AccessRequest         = 1
    fromEnum AccessAccept          = 2
    fromEnum AccessReject          = 3
    fromEnum AccountingRequest     = 4
    fromEnum AccountingResponse    = 5
    fromEnum AccessChallenge       = 11
    fromEnum StatusServer          = 12
    fromEnum StatusClient          = 13
    fromEnum (UnknownPacketType x) = x
    toEnum 1  = AccessRequest
    toEnum 2  = AccessAccept
    toEnum 3  = AccessReject
    toEnum 4  = AccountingRequest
    toEnum 5  = AccountingResponse
    toEnum 11 = AccessChallenge
    toEnum 12 = StatusServer
    toEnum 13 = StatusClient
    toEnum x  = UnknownPacketType x

data PacketAttribute =
    UserNameAttribute               { getUserNameAttribute               :: ByteString        }
  | UserPasswordAttribute           { getUserPasswordAttribute           :: ByteString        }
  | CHAPPassword                    { getCHAPIdentity                    :: Word8,
                                      getCHAPPasswordAttribute           :: ByteString        }
  | NASIPAddress                    { getNASIPAddress                    :: IPv4              }
  | NASIPv6Address                  { getNASIPv6Address                  :: IPv6              }
  | NASPortAttribute                { getNASPortAttribute                :: Word32            }
  | ServiceTypeAttribute            { getServiceTypeAttribute            :: ServiceType       }
  | FramedProtocolAttribute         { getFramedProtocolAttribute         :: FramedProtocol    }
  | FramedIPAddressAttribute        { getFramedIPAddressAttribute        :: IPv4              }
  | FramedIPNetmaskAttribute        { getFramedIPNetmaskAttribute        :: IPv4              }
  | FramedRoutingAttribute          { getFramedRoutingAttribute          :: FramedRouting     }
  | FramedInterfaceIdAttribute      { getFramedInterfaceIdAttribute      :: Word64            }
  | FramedIPv6Prefix                { getFramedIPv6PrefixLength          :: Word8,
                                      getFramedIPv6Prefix                :: IPv6              }
  | FramedIPv6Route                 { getFramedIPv6RouteAttribute        :: ByteString        }
  | FramedIPv6Pool                  { getFramedIPv6PoolAttribute         :: ByteString        }
  | FilterIdAttribute               { getFilterIdAttribute               :: ByteString        }
  | FramedMTUAttribute              { getFramedMTUAttribute              :: Word32            }
  | FramedCompressionAttribute      { getFramedCompressionAttribute      :: FramedCompression }
  | LoginIPHostAttribute            { getLoginIPHostAttribute            :: IPv4              }
  | LoginIPv6HostAttribute          { getLoginIPv6HostAttribute          :: IPv6              }
  | LoginServiceAttribute           { getLoginServiceAttribute           :: LoginService      }
  | LoginTCPPortAttribute           { getLoginTCPPortAttribute           :: Word32            }
  | ReplyMessageAttribute           { getReplyMessageAttribute           :: ByteString        }
  | CallbackNumberAttribute         { getCallbackNumberAttribute         :: ByteString        }
  | CallbackIdAttribute             { getCallbackIdAttribute             :: ByteString        }
  | DelegatedIPv6Prefix             { getDelegatedIPv6PrefixLength       :: Word8,
                                      getDelegatedIPv6Prefix             :: IPv6              }
  | FramedRouteAttribute            { getFramedRouteAttribute            :: ByteString        }
  | FramedIPXNetworkAttribute       { getFramedIPXNetworkAttribute       :: Word32            }
  | StateAttribute                  { getStateAttribute                  :: ByteString        }
  | ClassAttribute                  { getClassAttribute                  :: ByteString        }
  | VendorSpecificAttribute         { getVendorIdAttribute               :: Word32,
                                      getVendorSpecificAttribute         :: ByteString        }
  | SessionTimeoutAttribute         { getSessionTimeoutAttribute         :: Word32            }
  | IdleTimeoutAttribute            { getIdleTimeoutAttribute            :: Word32            }
  | TerminationActionAttribute      { getTerminationActionAttribute      :: TerminationAction }
  | CalledStationIdAttribute        { getCalledStationIdAttribute        :: ByteString        }
  | CallingStationIdAttribute       { getCallingStationIdAttribute       :: ByteString        }
  | NASIdentifierAttribute          { getNASIdentifierAttribute          :: ByteString        }
  | ProxyStateAttribute             { getProxyStateAttribute             :: ByteString        }
  | LoginLATServiceAttribute        { getLoginLATServiceAttribute        :: ByteString        }
  | LoginLATNodeAttribute           { getLoginLATNodeAttribute           :: ByteString        }
  | LoginLATGroupAttribute          { getLoginLATGroupAttribute          :: ByteString        }
  | FramedAppleTalkLinkAttribute    { getFramedAppleTalkLinkAttribute    :: Word32            }
  | FramedAppleTalkNetworkAttribute { getFramedAppleTalkNetworkAttribute :: Word32            }
  | FramedAppleTalkZoneAttribute    { getFramedAppleTalkZoneAttribute    :: ByteString        }
  | CHAPChallengeAttribute          { getCHAPChallengeAttribute          :: ByteString        }
  | NASPortTypeAttribute            { getNASPortTypeAttribute            :: NASPortType       }
  | PortLimitAttribute              { getPortLimitAttribute              :: Word32            }
  | LoginLATPortAttribute           { getLoginLATPortAttribute           :: ByteString        }
  | AccountInputGigawordsAttribute  { getAccountInputGigawordsAttribute  :: Word32            }
  | AccountOutputGigawordsAttribute { getAccountOutputGigawordsAttribute :: Word32            }
  | EventTimeStampAttribute         { getEventTimeStampAttribute         :: Word32            }
  | ARAPPasswordAttribute           { getARAPPasswordAttribute           :: ByteString        }
  | ARAPFeaturesAttribute           { getARAPFeaturesAttribute           :: ByteString        }
  | ARAPZoneAccessAttribute         { getARAPZoneAccessAttribute         :: ARAPZoneAccess    }
  | ARAPSecurityAttribute           { getARAPSecurityAttribute           :: Word32            }
  | ARAPSecurityDataAttribute       { getARAPSecurityDataAttribute       :: ByteString        }
  | PasswordRetryAttribute          { getPasswordRetryAttribute          :: Word32            }
  | PromptAttribute                 { getPromptAttribute                 :: Word32            }
  | ConnectInfoAttribute            { getConnectInfoAttribute            :: ByteString        }
  | ConfigurationTokenAttribute     { getConfigurationTokenAttribute     :: ByteString        }
  | EAPMessageAttribute             { getEAPMessageAttribute             :: ByteString        }
  | MessageAuthenticatorAttribute   { getMessageAuthenticatorAttribute   :: ByteString        }
  | ARAPChallengeResponseAttribute  { getARAPChallengeResponseAttribute  :: ByteString        }
  | AcctInterimIntervalAttribute    { getAcctInterimIntervalAttribute    :: Word32            }
  | NASPortIdAttribute              { getNASPortIdAttribute              :: ByteString        }
  | FramedPoolAttribute             { getFramedPoolAttribute             :: ByteString        }
  | AcctStatusTypeAttribute         { getAcctStatusTypeAttribute         :: StatusType        }
  | AcctDelayTimeAttribute          { getAcctDelayTimeAttribute          :: Word32            }
  | AcctInputOctetsAttribute        { getAcctInputOctetsAttribute        :: Word32            }
  | AcctOutputOctetsAttribute       { getAcctOutputOctetsAttribute       :: Word32            }
  | AcctSessionIdAttribute          { getAcctSessionIdAttribute          :: ByteString        }
  | AcctAuthenticAttribute          { getAcctAuthenticAttribute          :: Authentic         }
  | AcctSessionTimeAttribute        { getAcctSessionTimeAttribute        :: Word32            }
  | AcctInputPacketsAttribute       { getAcctInputPacketsAttribute       :: Word32            }
  | AcctOutputPacketsAttribute      { getAcctOutputPacketsAttribute      :: Word32            }
  | AcctTerminateCauseAttribute     { getAcctTerminateCauseAttribute     :: TerminateCause    }
  | AcctMultiSessionIdAttribute     { getAcctMultiSessionIdAttribute     :: ByteString        }
  | AcctLinkCountAttribute          { getAcctLinkCountAttribute          :: Word32            }
  | DNSServerIPv6AddressAttribute   { getDNSServerIPv6AddressAttribute   :: IPv6              }
  | UnknownAttribute                { getUnknownType                     :: Word8,
                                      getUnknownAttribute                :: ByteString        }
  deriving (Show, Eq, Data)

data StatusType = Start
                | Stop
                | InterimUpdate
                | AccountingOn
                | AccountingOff
                | Failed
                | UnknownStatusType Int
  deriving (Show, Eq, Data)

instance Enum StatusType where
  fromEnum Start                 = 1
  fromEnum Stop                  = 2
  fromEnum InterimUpdate         = 3
  fromEnum AccountingOn          = 7
  fromEnum AccountingOff         = 8
  fromEnum Failed                = 15
  fromEnum (UnknownStatusType x) = x
  toEnum 1  = Start
  toEnum 2  = Stop
  toEnum 3  = InterimUpdate
  toEnum 7  = AccountingOn
  toEnum 8  = AccountingOff
  toEnum 15 = Failed
  toEnum x  = UnknownStatusType x

data Authentic = Radius
               | Local
               | Remote
               | UnknownAuthentic Int
  deriving (Show, Eq, Data)

instance Enum Authentic where
  fromEnum Radius = 1
  fromEnum Local  = 2
  fromEnum Remote = 3
  fromEnum (UnknownAuthentic x) = x
  toEnum 1 = Radius
  toEnum 2 = Local
  toEnum 3 = Remote
  toEnum x = UnknownAuthentic x

data TerminateCause = UserRequest
                    | LostCarrier
                    | LostService
                    | IdleTimeout
                    | SessionTimeout
                    | AdminReset
                    | AdminReboot
                    | PortError
                    | NASError
                    | NASRequest
                    | NASReboot
                    | PortUnneeded
                    | PortPreempted
                    | PortSuspended
                    | ServiceUnavailable
                    | Callback
                    | UserError
                    | HostRequest
                    | UnknownTerminateCause Int
  deriving (Show, Eq, Data)

instance Enum TerminateCause where
  fromEnum UserRequest        = 1
  fromEnum LostCarrier        = 2
  fromEnum LostService        = 3
  fromEnum IdleTimeout        = 4
  fromEnum SessionTimeout     = 5
  fromEnum AdminReset         = 6
  fromEnum AdminReboot        = 7
  fromEnum PortError          = 8
  fromEnum NASError           = 9
  fromEnum NASRequest         = 10
  fromEnum NASReboot          = 11
  fromEnum PortUnneeded       = 12
  fromEnum PortPreempted      = 13
  fromEnum PortSuspended      = 14
  fromEnum ServiceUnavailable = 15
  fromEnum Callback           = 16
  fromEnum UserError          = 17
  fromEnum HostRequest        = 18
  fromEnum (UnknownTerminateCause x) = x
  toEnum 1  = UserRequest
  toEnum 2  = LostCarrier
  toEnum 3  = LostService
  toEnum 4  = IdleTimeout
  toEnum 5  = SessionTimeout
  toEnum 6  = AdminReset
  toEnum 7  = AdminReboot
  toEnum 8  = PortError
  toEnum 9  = NASError
  toEnum 10 = NASRequest
  toEnum 11 = NASReboot
  toEnum 12 = PortUnneeded
  toEnum 13 = PortPreempted
  toEnum 14 = PortSuspended
  toEnum 15 = ServiceUnavailable
  toEnum 16 = Callback
  toEnum 17 = UserError
  toEnum 18 = HostRequest
  toEnum x  = UnknownTerminateCause x

data ServiceType = LoginService
                 | FramedService
                 | CallbackLoginService
                 | CallbackFramedService
                 | OutboundService
                 | AdministrativeService
                 | NASPromptService
                 | AuthenticateOnlyService
                 | CallbackNASPrompt
                 | CallCheckService
                 | CallbackAdministrativeService
                 | UnknownServiceType Int
                   deriving (Show, Eq, Data)

instance Enum ServiceType where
    fromEnum LoginService                  = 1
    fromEnum FramedService                 = 2
    fromEnum CallbackLoginService          = 3
    fromEnum CallbackFramedService         = 4
    fromEnum OutboundService               = 5
    fromEnum AdministrativeService         = 6
    fromEnum NASPromptService              = 7
    fromEnum AuthenticateOnlyService       = 8
    fromEnum CallbackNASPrompt             = 9
    fromEnum CallCheckService              = 10
    fromEnum CallbackAdministrativeService = 11
    fromEnum (UnknownServiceType x)        = x
    toEnum 1  = LoginService
    toEnum 2  = FramedService
    toEnum 3  = CallbackLoginService
    toEnum 4  = CallbackFramedService
    toEnum 5  = OutboundService
    toEnum 6  = AdministrativeService
    toEnum 7  = NASPromptService
    toEnum 8  = AuthenticateOnlyService
    toEnum 9  = CallbackNASPrompt
    toEnum 10 = CallCheckService
    toEnum 11 = CallbackAdministrativeService
    toEnum x  = UnknownServiceType x

data FramedProtocol = PPPFramedProtocol
                    | SLIPFramedProtocol
                    | ARAPFramedProtocol
                    | GandalfFramedProtocol
                    | XylogicsFramedProtocol
                    | X75FramedProtocol
                    | UnknownFramedProtocol Int
                      deriving (Show, Eq, Data)

instance Enum FramedProtocol where
    fromEnum PPPFramedProtocol         = 1
    fromEnum SLIPFramedProtocol        = 2
    fromEnum ARAPFramedProtocol        = 3
    fromEnum GandalfFramedProtocol     = 4
    fromEnum XylogicsFramedProtocol    = 5
    fromEnum X75FramedProtocol         = 6
    fromEnum (UnknownFramedProtocol x) = x
    toEnum 1 = PPPFramedProtocol
    toEnum 2 = SLIPFramedProtocol
    toEnum 3 = ARAPFramedProtocol
    toEnum 4 = GandalfFramedProtocol
    toEnum 5 = XylogicsFramedProtocol
    toEnum 6 = X75FramedProtocol
    toEnum x = UnknownFramedProtocol x

data FramedRouting = NoneFramedRouting
                   | SendFramedRouting
                   | ListenFramedRouting
                   | SendAndListenFramedRouting
                   | UnknownFramedRouting Int
                     deriving (Show, Eq, Data)

instance Enum FramedRouting where
  fromEnum NoneFramedRouting          = 1
  fromEnum SendFramedRouting          = 2
  fromEnum ListenFramedRouting        = 3
  fromEnum SendAndListenFramedRouting = 4
  fromEnum (UnknownFramedRouting x)   = x
  toEnum 1 = NoneFramedRouting
  toEnum 2 = SendFramedRouting
  toEnum 3 = ListenFramedRouting
  toEnum 4 = SendAndListenFramedRouting
  toEnum x = UnknownFramedRouting x

data FramedCompression = NoCompression
                       | VJTCPIPHeaderCompression
                       | IPXHeaderCompression
                       | StacLZSCompression
                       | UnknownFramedCompression Int
                         deriving (Show, Eq, Data)

instance Enum FramedCompression where
  fromEnum NoCompression                = 1
  fromEnum VJTCPIPHeaderCompression     = 2
  fromEnum IPXHeaderCompression         = 3
  fromEnum StacLZSCompression           = 4
  fromEnum (UnknownFramedCompression x) = x
  toEnum 1 = NoCompression
  toEnum 2 = VJTCPIPHeaderCompression
  toEnum 3 = IPXHeaderCompression
  toEnum 4 = StacLZSCompression
  toEnum x = UnknownFramedCompression x

data LoginService = TelnetService
                  | RloginService
                  | TCPClearService
                  | PortMasterService
                  | LATService
                  | X25PADService
                  | X25T3POSService
                  | UnusedService
                  | TCPClearQuietService
                  | UnknownLoginService Int
                    deriving (Show, Eq, Data)

instance Enum LoginService where
  fromEnum TelnetService           = 1
  fromEnum RloginService           = 2
  fromEnum TCPClearService         = 3
  fromEnum PortMasterService       = 4
  fromEnum LATService              = 5
  fromEnum X25PADService           = 6
  fromEnum X25T3POSService         = 7
  fromEnum UnusedService           = 8
  fromEnum TCPClearQuietService    = 9
  fromEnum (UnknownLoginService x) = x
  toEnum 1 = TelnetService
  toEnum 2 = RloginService
  toEnum 3 = TCPClearService
  toEnum 4 = PortMasterService
  toEnum 5 = LATService
  toEnum 6 = X25PADService
  toEnum 7 = X25T3POSService
  toEnum 8 = UnusedService
  toEnum 9 = TCPClearQuietService
  toEnum x = UnknownLoginService x

data TerminationAction = DefaultTerminationAction
                       | RADIUSRequestTerminationAction
                       | UnknownTerminationAction Int
                       deriving (Show, Eq, Data)

instance Enum TerminationAction where
  fromEnum DefaultTerminationAction       = 1
  fromEnum RADIUSRequestTerminationAction = 2
  fromEnum (UnknownTerminationAction x)   = x
  toEnum 1 = DefaultTerminationAction
  toEnum 2 = RADIUSRequestTerminationAction
  toEnum x = UnknownTerminationAction x

data NASPortType = AsyncNASPort
                 | SyncNASPort
                 | ISDNSyncPort
                 | ISDNAsyncV120Port
                 | ISDNAsyncV110Port
                 | VirtualNASPort
                 | PIAFSNASPort
                 | HDLCClearChannelNASPort
                 | X25NASPort
                 | X75NASPort
                 | G3FaxNASPort
                 | SDSLNASPort
                 | ADSLCAPNASPort
                 | ADSLDMTNASPort
                 | IDSLNASPort
                 | EthernetNASPort
                 | XDSLNASPort
                 | CableNASPort
                 | WirelessOtherNASPort
                 | WirelessIEEE80211NASPort
                 | UnknownNASPortType Int
                   deriving (Show, Eq, Data)

instance Enum NASPortType where
  fromEnum AsyncNASPort             = 1
  fromEnum SyncNASPort              = 2
  fromEnum ISDNSyncPort             = 3
  fromEnum ISDNAsyncV120Port        = 4
  fromEnum ISDNAsyncV110Port        = 5
  fromEnum VirtualNASPort           = 6
  fromEnum PIAFSNASPort             = 7
  fromEnum HDLCClearChannelNASPort  = 8
  fromEnum X25NASPort               = 9
  fromEnum X75NASPort               = 10
  fromEnum G3FaxNASPort             = 11
  fromEnum SDSLNASPort              = 12
  fromEnum ADSLCAPNASPort           = 13
  fromEnum ADSLDMTNASPort           = 14
  fromEnum IDSLNASPort              = 15
  fromEnum EthernetNASPort          = 16
  fromEnum XDSLNASPort              = 17
  fromEnum CableNASPort             = 18
  fromEnum WirelessOtherNASPort     = 19
  fromEnum WirelessIEEE80211NASPort = 20
  fromEnum (UnknownNASPortType x)   = x
  toEnum 1  = AsyncNASPort
  toEnum 2  = SyncNASPort
  toEnum 3  = ISDNSyncPort
  toEnum 4  = ISDNAsyncV120Port
  toEnum 5  = ISDNAsyncV110Port
  toEnum 6  = VirtualNASPort
  toEnum 7  = PIAFSNASPort
  toEnum 8  = HDLCClearChannelNASPort
  toEnum 9  = X25NASPort
  toEnum 10 = X75NASPort
  toEnum 11 = G3FaxNASPort
  toEnum 12 = SDSLNASPort
  toEnum 13 = ADSLCAPNASPort
  toEnum 14 = ADSLDMTNASPort
  toEnum 15 = IDSLNASPort
  toEnum 16 = EthernetNASPort
  toEnum 17 = XDSLNASPort
  toEnum 18 = CableNASPort
  toEnum 19 = WirelessOtherNASPort
  toEnum 20 = WirelessIEEE80211NASPort
  toEnum x  = UnknownNASPortType x

data ARAPZoneAccess = DefaultZoneOnlyARAPAccess
                    | UseZoneFilterInclusivelyARAPAccess
                    | UseZoneFilterExclusivelyARAPAccess
                    | UnknownARAPZoneAccess Int
                    deriving (Show, Eq, Data)

instance Enum ARAPZoneAccess where
    toEnum 1 = DefaultZoneOnlyARAPAccess
    toEnum 2 = UseZoneFilterInclusivelyARAPAccess
    toEnum 4 = UseZoneFilterExclusivelyARAPAccess
    toEnum x = UnknownARAPZoneAccess x
    fromEnum DefaultZoneOnlyARAPAccess          = 1
    fromEnum UseZoneFilterInclusivelyARAPAccess = 2
    fromEnum UseZoneFilterExclusivelyARAPAccess = 4
    fromEnum (UnknownARAPZoneAccess x)          = x

makeLenses ''Header
makeLenses ''Packet
makePrisms ''PacketType
makePrisms ''PacketAttribute
makePrisms ''StatusType
makePrisms ''Authentic
makePrisms ''TerminateCause
makePrisms ''ServiceType
makePrisms ''FramedProtocol
makePrisms ''FramedRouting
makePrisms ''FramedCompression
makePrisms ''LoginService
makePrisms ''TerminationAction
makePrisms ''NASPortType
makePrisms ''ARAPZoneAccess

