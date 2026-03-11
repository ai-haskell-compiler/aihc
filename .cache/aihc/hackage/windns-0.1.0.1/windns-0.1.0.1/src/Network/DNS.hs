{-# LANGUAGE Trustworthy #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- License: GPLv3
--
-- This module implements an API for accessing
-- the [Domain Name Service (DNS)](https://tools.ietf.org/html/rfc1035)
-- resolver service via the standard [<windns.h>/dnsapi.dll](https://msdn.microsoft.com/en-us/library/windows/desktop/ms682100\(v=vs.85\).aspx)
-- system library on Win32 systems.
--
module Network.DNS
    ( -- ** High level API
      queryA
    , queryAAAA
    , queryCNAME
    , querySRV
    , queryTXT

      -- * Types
    , Name(..)
    , CharStr(..)
    , IPv4(..)
    , IPv6(..)
    , TTL(..)
    , SRV(..)
    ) where

import           Network.DNS.FFI

-- | Query @TXT@ records (see [RFC 1035, section 3.3.14](https://tools.ietf.org/html/rfc1035#section-3.3.14)).
--
-- >>> queryTXT (Name "_mirrors.hackage.haskell.org")
-- [(TTL 299,["0.urlbase=http://hackage.fpcomplete.com/",
--            "1.urlbase=http://objects-us-west-1.dream.io/hackage-mirror/"])]
--
queryTXT :: Name -> IO [(TTL, [CharStr])]
queryTXT n = do
  recs <- dnsQuery True n DnsTypeTXT
  case recs of
    Left err    -> fail ("DnsQuery_A failed with " ++ show err)
    Right recs' -> return [ (ttl,ts)
                          | DnsRecord { drTTL = ttl, drData = DnsDataTXT ts } <- recs'
                          ]


-- | Query @A@ record (see [RFC 1035, section 3.4.1](https://tools.ietf.org/html/rfc1035#section-3.4.1)).
--
-- This query returns only exact matches (modulo 'foldCaseName').
-- E.g. in case of @CNAME@ responses even if the
-- answer section would contain @A@ records for the hostnames pointed
-- to by the @CNAME@.
--
-- >>> queryA (Name "www.google.com")
-- [(TTL 72,IPv4 0xd83acde4)]
--
queryA :: Name -> IO [(TTL,IPv4)]
queryA n = do
  recs <- dnsQuery True n DnsTypeA
  case recs of
    Left err    -> fail ("DnsQuery_A failed with " ++ show err)
    Right recs' -> return [ (ttl,ip4)
                          | DnsRecord { drTTL = ttl, drData = DnsDataA ip4 } <- recs'
                          ]

-- | Query @AAAA@ records (see [RFC 3596](https://tools.ietf.org/html/rfc3596)).
--
-- This query returns only exact matches (modulo 'foldCaseName').
-- E.g. in case of @CNAME@ responses even if the answer section would
-- contain @A@ records for the hostnames pointed to by the
-- @CNAME@.
--
-- >>> queryAAAA (Name "www.google.com")
-- [(TTL 299,IPv6 0x2a0014504001081e 0x2004)]
--
queryAAAA :: Name -> IO [(TTL,IPv6)]
queryAAAA n = do
  recs <- dnsQuery True n DnsTypeAAAA
  case recs of
    Left err    -> fail ("DnsQuery_A failed with " ++ show err)
    Right recs' -> return [ (ttl,ip6)
                          | DnsRecord { drTTL = ttl, drData = DnsDataAAAA ip6 } <- recs'
                          ]


-- | Query @CNAME@ records (see [RFC 1035, section 3.3.1](https://tools.ietf.org/html/rfc1035#section-3.3.1)).
--
-- >>> queryCNAME (Name "hackage.haskell.org")
-- [(TTL 299,Name "j.global-ssl.fastly.net.")]
--
queryCNAME :: Name -> IO [(TTL,Name)]
queryCNAME n = do
  recs <- dnsQuery True n DnsTypeCNAME
  case recs of
    Left err    -> fail ("DnsQuery_A failed with " ++ show err)
    Right recs' -> return [ (ttl,cname)
                          | DnsRecord { drTTL = ttl, drData = DnsDataCNAME cname } <- recs'
                          ]


-- | Query @SRV@ records (see [RFC 2782](https://tools.ietf.org/html/rfc2782)).
--
-- >>> querySRV (Name "_imap._tcp.gmail.com")
-- [(TTL 21599,SRV {srvPriority = 0, srvWeight = 0, srvPort = 0, srvTarget = Name "."})]
--
querySRV :: Name -> IO [(TTL,SRV Name)]
querySRV n = do
  recs <- dnsQuery True n DnsTypeSRV
  case recs of
    Left err    -> fail ("DnsQuery_A failed with " ++ show err)
    Right recs' -> return [ (ttl,srv)
                          | DnsRecord { drTTL = ttl, drData = DnsDataSRV srv } <- recs'
                          ]
