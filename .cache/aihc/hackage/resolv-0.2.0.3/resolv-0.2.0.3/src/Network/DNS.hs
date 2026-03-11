{-# LANGUAGE CApiFFI            #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE Trustworthy        #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- This module implements an API for accessing
-- the [Domain Name Service (DNS)](https://tools.ietf.org/html/rfc1035)
-- resolver service via the standard @libresolv@ system library
-- on Unix systems.
--
module Network.DNS
    ( -- ** High level API
      queryA
    , queryAAAA
    , queryCNAME
    , queryPTR
    , querySRV
    , queryTXT

      -- * Mid-level API
    , query
    , DnsException(..)

      -- * Low-level API
    , resIsReentrant
    , queryRaw
    , sendRaw
    , mkQueryRaw

    , decodeMessage
    , encodeMessage
    , mkQueryMsg

      -- * Types
      -- ** Basic types

      -- *** Names/Labels
    , Label
    , Labels(..)
    , IsLabels(..)

    , Name(..)
    , caseFoldName

      -- *** Character strings
    , CharStr(..)

      -- *** IP addresses
    , IPv4(..), arpaIPv4
    , IPv6(..), arpaIPv6

      -- *** RR TTL & Class
    , TTL(..)

    , Class(..)
    , classIN

      -- *** Message types
    , Type(..)
    , TypeSym(..)
    , typeFromSym
    , typeToSym

      -- ** Messages

    , Msg(..)

    , MsgHeader(..)
    , MsgHeaderFlags(..), QR(..)
    , MsgQuestion(..)
    , MsgRR(..)

    , RData(..)
    , rdType

    , SRV(..)
    )
    where

import           Control.Exception
import           Data.Bits             (unsafeShiftR, (.&.))
import           Foreign.C
import           Foreign.Marshal.Alloc
import           Numeric               (showInt)
import           Prelude

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BSC

import           Compat

import           Network.DNS.FFI
import           Network.DNS.Message

-- | Exception thrown in case of errors while resolving or encoding/decoding into a 'Msg'.
--
-- @since 0.1.1.0
data DnsException
  = DnsEncodeException
  | DnsDecodeException
  | DnsHostNotFound
      -- ^ No such domain (authoritative)
      --
      -- @since 0.2.0.0
  | DnsNoData
      -- ^ No record for requested type
      --
      -- @since 0.2.0.0
  | DnsNoRecovery
      -- ^ Non recoverable errors, REFUSED, NOTIMP
      --
      -- @since 0.2.0.0
  | DnsTryAgain
      -- ^ No such domain (non-authoritative) or SERVERFAIL
      --
      -- @since 0.2.0.0
  deriving (Eq, Show)

instance Exception DnsException

-- | Send a query via @res_query(3)@ and decode its response into a 'Msg'
--
-- Throws 'DnsException' in case of resolving or encoding/decoding errors. May throw other IO exceptions in case of network errors.
--
-- === Example
--
-- >>> query classIN (Name "_mirrors.hackage.haskell.org") TypeTXT
-- Just (Msg{msgHeader = MsgHeader{mhId    = 56694,
--                                 mhFlags = MsgHeaderFlags{mhQR = IsResponse, mhOpcode = 0, mhAA = False,
--                                                          mhTC = False, mhRD = True, mhRA = True, mhZ = False,
--                                                          mhAD = False, mhCD = False, mhRCode = 0},
--                                 mhQDCount = 1, mhANCount = 1, mhNSCount = 0, mhARCount = 1},
--           msgQD = [MsgQuestion (Name "_mirrors.hackage.haskell.org.") (Type 16) (Class 1)],
--           msgAN = [MsgRR{rrName  = Name "_mirrors.hackage.haskell.org.",
--                          rrClass = Class 1, rrTTL = TTL 299,
--                          rrData  = RDataTXT ["0.urlbase=http://hackage.fpcomplete.com/",
--                                              "1.urlbase=http://objects-us-west-1.dream.io/hackage-mirror/"]}],
--           msgNS = [],
--           msgAR = [MsgRR{rrName = Name ".", rrClass = Class 512, rrTTL = TTL 32768, rrData = RDataOPT ""}]
--       })
--
query :: IsLabels n => Class -> n -> TypeSym -> IO (Msg n)
query cls name0 qtype
  | Just name <- toName name0 = do
      bs <- queryRaw cls name (typeFromSym qtype)
      msg <- evaluate (decodeMessage bs)
      maybe (throwIO DnsDecodeException) pure msg
  | otherwise = throwIO DnsEncodeException

-- | Send a query via @res_query(3)@, the return value is the raw binary response message.
--
-- You can use 'decodeMessage' to decode the response message.
queryRaw :: Class -> Name -> Type -> IO BS.ByteString
queryRaw (Class cls) (Name name) qtype = withCResState $ \stptr -> do
    allocaBytes max_msg_size $ \resptr -> do
        _ <- c_memset resptr 0 max_msg_size
        BS.useAsCString name $ \dn -> do

          withCResInit stptr $ do

            reslen <- c_res_query stptr dn (fromIntegral cls) qtypeVal resptr max_msg_size

            unless (reslen <= max_msg_size) $
                fail "res_query(3) message size overflow"

            errno <- getErrno

            when (reslen < 0) $ do
                unless (errno == eOK) $
                    throwErrno "res_query"

                h_errno <- c_get_h_errno stptr
                case h_errno of
                    1 -> throwIO DnsHostNotFound
                    2 -> throwIO DnsNoData
                    3 -> throwIO DnsNoRecovery
                    4 -> throwIO DnsTryAgain
                    _ -> fail "res_query(3) failed"

            BS.packCStringLen (resptr, fromIntegral reslen)

  where
    -- The DNS protocol is inherently 16-bit-offset based; so 64KiB is
    -- a reasonable maximum message size most implementations seem to
    -- support.
    max_msg_size :: Num a => a
    max_msg_size = 0x10000

    qtypeVal :: CInt
    qtypeVal = case qtype of Type w -> fromIntegral w

-- | Send a raw preformatted query via @res_send(3)@.
sendRaw :: BS.ByteString -> IO BS.ByteString
sendRaw req = withCResState $ \stptr -> do
    allocaBytes max_msg_size $ \resptr -> do
        _ <- c_memset resptr 0 max_msg_size
        BS.useAsCStringLen req $ \(reqptr,reqlen) -> do

          withCResInit stptr $ do
            reslen <- c_res_send stptr reqptr (fromIntegral reqlen) resptr max_msg_size

            unless (reslen <= max_msg_size) $
                fail "res_send(3) message size overflow"

            errno <- getErrno

            when (reslen < 0) $ do
                unless (errno == eOK) $
                    throwErrno "res_send"

                fail "res_send(3) failed"

            BS.packCStringLen (resptr, fromIntegral reslen)

  where
    -- The DNS protocol is inherently 16-bit-offset based; so 64KiB is
    -- a reasonable maximum message size most implementations seem to
    -- support.
    max_msg_size :: Num a => a
    max_msg_size = 0x10000

-- | Construct a DNS query 'Msg' in the style of 'mkQueryRaw'
mkQueryMsg :: IsLabels n => Class -> n -> Type -> Msg n
mkQueryMsg cls l qtype = Msg (MsgHeader{..})
                             [MsgQuestion l qtype cls]
                             []
                             []
                             [MsgRR {..}]
  where
    mhId      = 31337
    mhFlags   = MsgHeaderFlags
      { mhQR     = IsQuery
      , mhOpcode = 0
      , mhAA     = False
      , mhTC     = False
      , mhRD     = True
      , mhRA     = False
      , mhZ      = False
      , mhAD     = True
      , mhCD     = False
      , mhRCode  = 0
      }

    mhQDCount = 1
    mhANCount = 0
    mhNSCount = 0
    mhARCount = 1

    rrName  = fromLabels Root
    rrClass = Class 512
    rrTTL   = TTL 0x8000
    rrData  = RDataOPT ""



-- | Use @res_mkquery(3)@ to construct a DNS query message.
mkQueryRaw :: Class -> Name -> Type -> IO BS.ByteString
mkQueryRaw (Class cls) (Name name) qtype = withCResState $ \stptr -> do
    allocaBytes max_msg_size $ \resptr -> do
        _ <- c_memset resptr 0 max_msg_size
        BS.useAsCString name $ \dn -> do

          withCResInit stptr $ do

            reslen <- c_res_mkquery stptr dn (fromIntegral cls) qtypeVal resptr max_msg_size

            unless (reslen <= max_msg_size) $
                fail "res_mkquery(3) message size overflow"

            errno <- getErrno

            when (reslen < 0) $ do
                unless (errno == eOK) $
                    throwErrno "res_query"

                fail "res_mkquery(3) failed"

            BS.packCStringLen (resptr, fromIntegral reslen)

  where
    -- The DNS protocol is inherently 16-bit-offset based; so 64KiB is
    -- a reasonable maximum message size most implementations seem to
    -- support.
    max_msg_size :: Num a => a
    max_msg_size = 0x10000

    qtypeVal :: CInt
    qtypeVal = case qtype of Type w -> fromIntegral w


----------------------------------------------------------------------------
-- Common High-level queries

-- | Normalise 'Name'
--
-- This function case folds 'Name's as described in
-- in [RFC 4343, section 3](https://tools.ietf.org/html/rfc4343#section-3)
-- by subtracting @0x20@ from all octets in the inclusive range
-- @[0x61..0x7A]@ (i.e. mapping @['a'..'z']@ to @['A'..'Z']@).
--
-- This operation is idempotent.
caseFoldName :: Name -> Name
caseFoldName (Name n) = (Name n'')
  where
    n' = BS.map cf n
    n'' | BS.null n' = "."
        | BS.last n' == 0x2e {- '.' -} = n'
        | otherwise  = n' `mappend` "."

    -- case fold (c.f. RFC4343)
    cf w | 0x61 <= w && w <= 0x7a  = w - 0x20
         | otherwise               = w

----------------------------------------------------------------------------

-- | Query @A@ record (see [RFC 1035, section 3.4.1](https://tools.ietf.org/html/rfc1035#section-3.4.1)).
--
-- This query returns only exact matches (modulo 'foldCaseName').
-- E.g. in case of @CNAME@ responses even if the
-- answer section would contain @A@ records for the hostnames pointed
-- to by the @CNAME@. You can use 'query' if you need more control.
--
-- >>> queryA (Name "www.google.com")
-- [(TTL 72,IPv4 0xd83acde4)]
--
queryA :: Name -> IO [(TTL,IPv4)]
queryA n = do
    res <- query classIN n' TypeA
    pure [ (ttl,ip4) | MsgRR { rrData = RDataA ip4, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n

-- | Query @AAAA@ records (see [RFC 3596](https://tools.ietf.org/html/rfc3596)).
--
-- This query returns only exact matches (modulo 'foldCaseName').
-- E.g. in case of @CNAME@ responses even if the answer section would
-- contain @A@ records for the hostnames pointed to by the
-- @CNAME@. You can use 'query' if you need more control.
--
-- >>> queryAAAA (Name "www.google.com")
-- [(TTL 299,IPv6 0x2a0014504001081e 0x2004)]
--
queryAAAA :: Name -> IO [(TTL,IPv6)]
queryAAAA n = do
    res <- query classIN n' TypeAAAA
    pure [ (ttl,ip6) | MsgRR { rrData = RDataAAAA ip6, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n

-- | Query @CNAME@ records (see [RFC 1035, section 3.3.1](https://tools.ietf.org/html/rfc1035#section-3.3.1)).
--
-- >>> queryCNAME (Name "hackage.haskell.org")
-- [(TTL 299,Name "j.global-ssl.fastly.net.")]
--
queryCNAME :: Name -> IO [(TTL,Name)]
queryCNAME n = do
    res <- query classIN n' TypeAAAA
    pure [ (ttl,cname) | MsgRR { rrData = RDataCNAME cname, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n

-- | Query @PTR@ records (see [RFC 1035, section 3.3.12](https://tools.ietf.org/html/rfc1035#section-3.3.12)).
--
-- >>> queryPTR (Name "4.4.8.8.in-addr.arpa.")
-- [(TTL 14390,Name "dns.google.")]
--
-- See also 'arpaIPv6' and 'arpaIPv4' for converting 'IPv6' and 'IPv4' values to the respective @.arpa."@ domain name for reverse lookups.
--
-- @since 0.1.2.0
queryPTR :: Name -> IO [(TTL,Name)]
queryPTR n = do
    res <- query classIN n' TypePTR
    pure [ (ttl,ptrs) | MsgRR { rrData = RDataPTR ptrs, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n

-- | Query @TXT@ records (see [RFC 1035, section 3.3.14](https://tools.ietf.org/html/rfc1035#section-3.3.14)).
--
-- >>> queryTXT (Name "_mirrors.hackage.haskell.org")
-- [(TTL 299,["0.urlbase=http://hackage.fpcomplete.com/",
--            "1.urlbase=http://objects-us-west-1.dream.io/hackage-mirror/"])]
--
queryTXT :: Name -> IO [(TTL,[CharStr])]
queryTXT n = do
    res <- query classIN n' TypeTXT
    pure [ (ttl,txts) | MsgRR { rrData = RDataTXT txts, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n

-- | Query @SRV@ records (see [RFC 2782](https://tools.ietf.org/html/rfc2782)).
--
-- >>> querySRV (Name "_imap._tcp.gmail.com")
-- [(TTL 21599,SRV {srvPriority = 0, srvWeight = 0, srvPort = 0, srvTarget = Name "."})]
--
querySRV :: Name -> IO [(TTL,SRV Name)]
querySRV n = do
    res <- query classIN n' TypeSRV
    pure [ (ttl,srv) | MsgRR { rrData = RDataSRV srv, rrTTL = ttl, rrName = n1, rrClass = Class 1 } <- msgAN res, caseFoldName n1 == n' ]
  where
    n' = caseFoldName n


-- | Convert 'IPv4' address to @in-addr.arpa.@ 'Name' (see [RFC 1035, section 3.5](https://tools.ietf.org/html/rfc1035#section-3.5)).
--
-- >>> arpaIPv4 (IPv4 0x8080404)
-- Name "4.4.8.8.in-addr.arpa."
--
-- @since 0.1.2.0
arpaIPv4 :: IPv4 -> Name
arpaIPv4 (IPv4 w) = Name (BSC.pack s)
  where
    s = showInt o0 ('.' : showInt o1 ('.' : showInt o2 ('.' : showInt o3 ".in-addr.arpa.")))

    o0, o1, o2, o3 :: Word8
    o0 = fromIntegral $ w
    o1 = fromIntegral $ w `unsafeShiftR` 8
    o2 = fromIntegral $ w `unsafeShiftR` 16
    o3 = fromIntegral $ w `unsafeShiftR` 24

-- | Convert 'IPv4' address to @ip6.arpa.@ 'Name' (see [RFC 3596, section 2.5](https://tools.ietf.org/html/rfc3596#section-2.5)).
--
-- >>> arpaIPv6 (IPv6 0x2001486048600000 0x8844)
-- Name "4.4.8.8.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.6.8.4.0.6.8.4.1.0.0.2.ip6.arpa."
--
-- @since 0.1.2.0
arpaIPv6 :: IPv6 -> Name
arpaIPv6 (IPv6 hi lo) = Name (BSC.pack s)
  where
    s = go 16 lo (go 16 hi "ip6.arpa.")

    go :: Int -> Word64 -> ShowS
    go 0 _ cont = cont
    go n w cont = nib : '.' : go (n-1) w' cont
      where
        nib :: Char
        nib | x < 10    = toEnum (fromIntegral (0x30 + x))
            | otherwise = toEnum (fromIntegral (0x57 + x))
        x = w .&. 0xf
        w' = w `unsafeShiftR` 4
