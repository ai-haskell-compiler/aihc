{-# LANGUAGE DeriveDataTypeable #-}

{- |
Module      : Network.Socks5.Types
License     : BSD-style
Copyright   : (c) 2010-2019 Vincent Hanquez <vincent@snarc.org>
Stability   : experimental
Portability : unknown
-}

module Network.Socks5.Types
  ( SocksVersion (..)
  , SocksCommand (..)
  , SocksMethod (..)
  , SocksHostAddress (..)
  , SocksAddress (..)
  , SocksReply (..)
  , SocksVersionNotSupported (..)
  , SocksError (..)
  , SocksFQDN
  ) where

import           Control.Exception ( Exception )
import           Data.ByteString ( ByteString )
import           Data.Data ( Data, Typeable )
import qualified Data.List as L
import           Data.Word ( Word8 )
import           Network.Socket ( HostAddress, HostAddress6, PortNumber )
import           Numeric ( showHex )

-- | Type representing SOCKS protocol versions.
data SocksVersion =
    SocksVer5
    -- ^ SOCKS Protocol Version 5. The only version implemented by the library.
  deriving (Eq, Ord, Show)

-- | Type representing commands that can be sent or received under the SOCKS
-- protocol.
data SocksCommand =
    SocksCommandConnect
    -- ^ The CONNECT request.
  | SocksCommandBind
    -- ^ The BIND request. Not implemented by the library.
  | SocksCommandUdpAssociate
    -- ^ The UDP ASSOCIATE request. Not implemented by the library.
  | SocksCommandOther !Word8
    -- ^ Other requests. None are specified by the SOCKS Protocol Version 5.
  deriving (Eq, Ord, Show)

-- | Type representing authentication methods available under the SOCKS
-- protocol.
--
-- Only 'SocksMethodNone' is effectively implemented, but other values are
-- enumerated for completeness.
data SocksMethod =
    SocksMethodNone
    -- ^ NO AUTHENTICATION REQUIRED.
  | SocksMethodGSSAPI
    -- ^ GSSAPI.
  | SocksMethodUsernamePassword
    -- ^ USERNAME/PASSWORD.
  | SocksMethodOther !Word8
    -- ^ IANA ASSIGNED or RESERVED FOR PRIVATE METHODS.
  | SocksMethodNotAcceptable
    -- ^ NO ACCEPTABLE METHODS.
  deriving (Eq, Ord, Show)

-- | Type representing host addresses under the SOCKS protocol.
data SocksHostAddress =
    SocksAddrIPV4 !HostAddress
    -- ^ A version-4 IP address.
  | SocksAddrDomainName !SocksFQDN
    -- ^ A fully-qualified domain name (FQDN).
  | SocksAddrIPV6 !HostAddress6
    -- ^ A version-6 IP address.
  deriving (Eq, Ord)

-- | Type synonym representing fully-qualified domain names (FQDN). The SOCKS
-- Protocol Version 5 does not specify an encoding for a FQDN other than there
-- is no terminating @NUL@ octet (byte).
--
-- This library's API assumes that FQDN values comprise only ASCII characters.
-- Domain names that include other Unicode code points should be
-- Punycode encoded.
type SocksFQDN = ByteString

instance Show SocksHostAddress where
  show (SocksAddrIPV4 ha) = "SocksAddrIPV4(" ++ showHostAddress ha ++ ")"
  show (SocksAddrIPV6 ha6) = "SocksAddrIPV6(" ++ showHostAddress6 ha6 ++ ")"
  show (SocksAddrDomainName dn) = "SocksAddrDomainName(" ++ showFQDN dn ++ ")"

-- | Converts the specified fully-qualified domain name (FQDN) to a 'String'.
showFQDN :: SocksFQDN -> String
showFQDN = show

-- | Converts the specified SOCKS host address to a 'String' in dot-decimal
-- notation.
showHostAddress :: HostAddress -> String
showHostAddress num = concat [show q1, ".", show q2, ".", show q3, ".", show q4]
 where
  (num', q1)   = num `quotRem` 256
  (num'', q2)  = num' `quotRem` 256
  (num''', q3) = num'' `quotRem` 256
  (_, q4)      = num''' `quotRem` 256

-- | Converts the specified IPv6 host address to standard hex notation.
showHostAddress6 :: HostAddress6 -> String
showHostAddress6 (a, b, c, d) =
  (L.intercalate ":" . map (`showHex` "")) [p1, p2, p3, p4, p5, p6, p7, p8]
 where
  (a', p2) = a `quotRem` 65536
  (_, p1)  = a' `quotRem` 65536
  (b', p4) = b `quotRem` 65536
  (_, p3)  = b' `quotRem` 65536
  (c', p6) = c `quotRem` 65536
  (_, p5)  = c' `quotRem` 65536
  (d', p8) = d `quotRem` 65536
  (_, p7)  = d' `quotRem` 65536

-- | Type representing socket addresses under the SOCKS protocol.
data SocksAddress = SocksAddress !SocksHostAddress !PortNumber
  deriving (Eq, Ord, Show)

-- | Type representing replies under the SOCKS protocol.
data SocksReply =
    SocksReplySuccess
    -- ^ The server reports that the request succeeded.
  | SocksReplyError SocksError
    -- ^ The server reports that the request did not succeed.
  deriving (Eq, Data, Ord, Show, Typeable)

-- | Type representing SOCKS errors that can be part of a SOCKS reply.
data SocksError =
    SocksErrorGeneralServerFailure
    -- ^ General SOCKS server failure.
  | SocksErrorConnectionNotAllowedByRule
    -- ^ Connection not allowed by ruleset.
  | SocksErrorNetworkUnreachable
    -- ^ Network unreachable.
  | SocksErrorHostUnreachable
    -- ^ Host unreachable.
  | SocksErrorConnectionRefused
    -- ^ Connection refused.
  | SocksErrorTTLExpired
    -- ^ TTL expired.
  | SocksErrorCommandNotSupported
    -- ^ Command not supported.
  | SocksErrorAddrTypeNotSupported
    -- ^ Address type not supported.
  | SocksErrorOther Word8
    -- ^ Other error. Unassigned in SOCKS Protocol Version 5.
  deriving (Eq, Data, Ord, Show, Typeable)

-- | Type representing exceptions.
data SocksVersionNotSupported =
    SocksVersionNotSupported
    -- ^ The SOCKS protocol version is not supported. This library only
    -- implements SOCKS Protocol Version 5.
  deriving (Data, Show, Typeable)

instance Exception SocksError

instance Exception SocksVersionNotSupported

instance Enum SocksCommand where
  toEnum 1 = SocksCommandConnect
  toEnum 2 = SocksCommandBind
  toEnum 3 = SocksCommandUdpAssociate
  toEnum w
    | w < 256   = SocksCommandOther $ fromIntegral w
    | otherwise = error "socks command is only 8 bits"
  fromEnum SocksCommandConnect      = 1
  fromEnum SocksCommandBind         = 2
  fromEnum SocksCommandUdpAssociate = 3
  fromEnum (SocksCommandOther w)    = fromIntegral w

instance Enum SocksMethod where
  toEnum 0    = SocksMethodNone
  toEnum 1    = SocksMethodGSSAPI
  toEnum 2    = SocksMethodUsernamePassword
  toEnum 0xff = SocksMethodNotAcceptable
  toEnum w
    | w < 256   = SocksMethodOther $ fromIntegral w
    | otherwise = error "socks method is only 8 bits"
  fromEnum SocksMethodNone             = 0
  fromEnum SocksMethodGSSAPI           = 1
  fromEnum SocksMethodUsernamePassword = 2
  fromEnum (SocksMethodOther w)        = fromIntegral w
  fromEnum SocksMethodNotAcceptable    = 0xff

instance Enum SocksError where
  fromEnum SocksErrorGeneralServerFailure       = 1
  fromEnum SocksErrorConnectionNotAllowedByRule = 2
  fromEnum SocksErrorNetworkUnreachable         = 3
  fromEnum SocksErrorHostUnreachable            = 4
  fromEnum SocksErrorConnectionRefused          = 5
  fromEnum SocksErrorTTLExpired                 = 6
  fromEnum SocksErrorCommandNotSupported        = 7
  fromEnum SocksErrorAddrTypeNotSupported       = 8
  fromEnum (SocksErrorOther w)                  = fromIntegral w
  toEnum 1 = SocksErrorGeneralServerFailure
  toEnum 2 = SocksErrorConnectionNotAllowedByRule
  toEnum 3 = SocksErrorNetworkUnreachable
  toEnum 4 = SocksErrorHostUnreachable
  toEnum 5 = SocksErrorConnectionRefused
  toEnum 6 = SocksErrorTTLExpired
  toEnum 7 = SocksErrorCommandNotSupported
  toEnum 8 = SocksErrorAddrTypeNotSupported
  toEnum w = SocksErrorOther $ fromIntegral w

instance Enum SocksReply where
  fromEnum SocksReplySuccess   = 0
  fromEnum (SocksReplyError e) = fromEnum e
  toEnum 0 = SocksReplySuccess
  toEnum n = SocksReplyError (toEnum n)
