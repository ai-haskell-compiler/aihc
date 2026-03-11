{-# LANGUAGE OverloadedStrings #-}

module Main where
 
import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit

import Text.IPv6Addr

main :: IO ()
main = defaultMain $ hUnitTestToTests tests

tests :: Test.HUnit.Test
tests = TestList
  [ (~?=) (maybeIPv6Addr ":") Nothing
  , (~?=) (maybeIPv6Addr "::") (Just (IPv6Addr "::"))
  , (~?=) (maybeIPv6Addr ":::") Nothing
  , (~?=) (maybeIPv6Addr "::::") Nothing
  , (~?=) (maybeIPv6Addr "::df0::") Nothing
  , (~?=) (maybeIPv6Addr "0:0:0:0:0:0:0") Nothing
  , (~?=) (maybeIPv6Addr "0:0:0:0:0:0:0:0") (Just (IPv6Addr "::"))
  , (~?=) (maybeIPv6Addr "0:0:0:0:0:0:0:0:0") Nothing
  , (~?=) (maybeIPv6Addr "1") Nothing
  , (~?=) (maybeIPv6Addr "::1") (Just (IPv6Addr "::1"))
  , (~?=) (maybeIPv6Addr ":::1") Nothing
  , (~?=) (maybeIPv6Addr "::1:") Nothing
  , (~?=) (maybeIPv6Addr "0000:0000:0000:0000:0000:0000:0000:0001") (Just (IPv6Addr "::1"))
  , (~?=) (maybeIPv6Addr "0:0:0:0:0:0:0:1") (Just (IPv6Addr "::1"))
  , (~?=) (maybeIPv6Addr "a") Nothing
  , (~?=) (maybeIPv6Addr "ab") Nothing
  , (~?=) (maybeIPv6Addr "abc") Nothing
  , (~?=) (maybeIPv6Addr "abcd") Nothing
  , (~?=) (maybeIPv6Addr "abcd:") Nothing
  , (~?=) (maybeIPv6Addr "abcd::") (Just (IPv6Addr "abcd::"))
  , (~?=) (maybeIPv6Addr "abcd:::") Nothing
  , (~?=) (maybeIPv6Addr "abcde::") Nothing
  , (~?=) (maybeIPv6Addr "a::") (Just (IPv6Addr "a::"))
  , (~?=) (maybeIPv6Addr "0a::") (Just (IPv6Addr "a::"))
  , (~?=) (maybeIPv6Addr "00a::") (Just (IPv6Addr "a::"))
  , (~?=) (maybeIPv6Addr "000a::") (Just (IPv6Addr "a::"))
  , (~?=) (maybeIPv6Addr "0000a::") Nothing
  , (~?=) (maybeIPv6Addr "adb6") Nothing
  , (~?=) (maybeIPv6Addr "adb6ce67") Nothing
  , (~?=) (maybeIPv6Addr "adb6:ce67") Nothing
  , (~?=) (maybeIPv6Addr "adb6::ce67") (Just (IPv6Addr "adb6::ce67"))
  , (~?=) (maybeIPv6Addr "::1.2.3.4") (Just (IPv6Addr "::1.2.3.4"))
  , (~?=) (maybeIPv6Addr "::ffff:1.2.3.4") (Just (IPv6Addr "::ffff:1.2.3.4"))
  , (~?=) (maybeIPv6Addr "::ffff:0:1.2.3.4") (Just (IPv6Addr "::ffff:0:1.2.3.4"))
  , (~?=) (maybeIPv6Addr "64:ff9b::1.2.3.4") (Just (IPv6Addr "64:ff9b::1.2.3.4"))
  , (~?=) (maybeIPv6Addr "fe80::5efe:1.2.3.4") (Just (IPv6Addr "fe80::5efe:1.2.3.4"))
  , (~?=) (maybeIPv6Addr "FE80:CD00:0000:0CDE:1257:0000:211E:729C") (Just (IPv6Addr "fe80:cd00:0:cde:1257:0:211e:729c"))
  , (~?=) (maybeIPv6Addr "FE80:CD00:0000:0CDE:1257:0000:211E:729X") Nothing
  , (~?=) (maybeIPv6Addr "FE80:CD00:0000:0CDE:1257:0000:211E:729CX") Nothing
  , (~?=) (maybeIPv6Addr "FE80:CD00:0000:0CDE:0000:211E:729C") Nothing
  , (~?=) (maybeIPv6Addr "FE80:CD00:0000:0CDE:FFFF:1257:0000:211E:729C") Nothing
  , (~?=) (maybeIPv6Addr "1111:2222:3333:4444:5555:6666:7777:8888") (Just (IPv6Addr "1111:2222:3333:4444:5555:6666:7777:8888"))
  , (~?=) (maybeIPv6Addr ":1111:2222:3333:4444:5555:6666:7777:8888") Nothing
  , (~?=) (maybeIPv6Addr "1111:2222:3333:4444:5555:6666:7777:8888:") Nothing
  , (~?=) (maybeIPv6Addr "1111::3333:4444:5555:6666::8888") Nothing
  , (~?=) (maybeIPv6Addr "AAAA:BBBB:CCCC:DDDD:EEEE:FFFF:0000:0000") (Just (IPv6Addr "aaaa:bbbb:cccc:dddd:eeee:ffff::"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:0001") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:001") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:01") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:1") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:eeee:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd::1") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:0:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:0:1") (Just (IPv6Addr "2001:db8:aaaa:bbbb:cccc:dddd:0:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:0:0:1:0:0:1") (Just (IPv6Addr "2001:db8::1:0:0:1"))
  , (~?=) (maybeIPv6Addr "2001:db8:0:1:0:0:0:1") (Just (IPv6Addr "2001:db8:0:1::1"))
  , (~?=) (maybeIPv6Addr "2001:DB8:0:0:0::1") (Just (IPv6Addr "2001:db8::1"))
  , (~?=) (maybeIPv6Addr "2001:0DB8:0:0::1") (Just (IPv6Addr "2001:db8::1"))
  , (~?=) (maybeIPv6Addr "2001:0dB8:0::1") (Just (IPv6Addr "2001:db8::1"))
  , (~?=) (maybeIPv6Addr "2001:db8::1") (Just (IPv6Addr "2001:db8::1"))
  , (~?=) (maybeIPv6Addr "2001:db8:0:1::1") (Just (IPv6Addr "2001:db8:0:1::1"))
  , (~?=) (maybeIPv6Addr "2001:0db8:0:1:0:0:0:1") (Just (IPv6Addr "2001:db8:0:1::1"))
  , (~?=) (maybeIPv6Addr "2001:DB8::1:1:1:1:1") (Just (IPv6Addr "2001:db8:0:1:1:1:1:1"))
  , (~?=) (maybeIPv6Addr "2001:DB8::1:1:0:1:1") (Just (IPv6Addr "2001:db8:0:1:1:0:1:1"))
  , (~?=) (maybeIPv6Addr "fe80") Nothing
  , (~?=) (maybeIPv6Addr "fe80::") (Just (IPv6Addr "fe80::"))
  , (~?=) (maybeIPv6Addr "0:0:0:0:0:ffff:192.0.2.1") (Just (IPv6Addr "::ffff:192.0.2.1"))
  , (~?=) (maybeIPv6Addr "::192.0.2.1") (Just (IPv6Addr "::192.0.2.1"))
  , (~?=) (maybeIPv6Addr "192.0.2.1::") Nothing
  , (~?=) (maybeIPv6Addr "::ffff:192.0.2.1") (Just (IPv6Addr "::ffff:192.0.2.1"))
  , (~?=) (maybeIPv6Addr "fe80:0:0:0:0:0:0:0") (Just (IPv6Addr "fe80::"))
  , (~?=) (maybeIPv6Addr "fe80:0000:0000:0000:0000:0000:0000:0000") (Just (IPv6Addr "fe80::"))
  , (~?=) (maybeIPv6Addr "2001:db8:Bad:0:0::0:1") (Just (IPv6Addr "2001:db8:bad::1"))
  , (~?=) (maybeIPv6Addr "2001:0:0:1:b:0:0:A") (Just (IPv6Addr "2001::1:b:0:0:a"))
  , (~?=) (maybeIPv6Addr "2001:0:0:1:000B:0:0:0") (Just (IPv6Addr "2001:0:0:1:b::"))
  , (~?=) (maybeIPv6Addr "2001:0DB8:85A3:0000:0000:8A2E:0370:7334") (Just (IPv6Addr "2001:db8:85a3::8a2e:370:7334"))
  , (~?=) (maybePureIPv6Addr "0:0:0:0:0:ffff:192.0.2.1") (Just (IPv6Addr "::ffff:c000:201"))
  , (~?=) (maybePureIPv6Addr "::ffff:192.0.2.1") (Just (IPv6Addr "::ffff:c000:201"))
  , (~?=) (maybeFullIPv6Addr "::") (Just (IPv6Addr "0000:0000:0000:0000:0000:0000:0000:0000"))
  , (~?=) (maybeFullIPv6Addr "0:0:0:0:0:0:0:0") (Just (IPv6Addr "0000:0000:0000:0000:0000:0000:0000:0000"))
  , (~?=) (maybeFullIPv6Addr "::1") (Just (IPv6Addr "0000:0000:0000:0000:0000:0000:0000:0001"))
  , (~?=) (maybeFullIPv6Addr "2001:db8::1") (Just (IPv6Addr "2001:0db8:0000:0000:0000:0000:0000:0001"))
  , (~?=) (maybeFullIPv6Addr "a:bb:ccc:dddd:1cDc::1") (Just (IPv6Addr "000a:00bb:0ccc:dddd:1cdc:0000:0000:0001"))
  , (~?=) (maybeFullIPv6Addr "FE80::0202:B3FF:FE1E:8329") (Just (IPv6Addr "fe80:0000:0000:0000:0202:b3ff:fe1e:8329"))
  , (~?=) (maybeFullIPv6Addr "aDb6::CE67") (Just (IPv6Addr "adb6:0000:0000:0000:0000:0000:0000:ce67"))
  , (~?=) (toIP6ARPA (IPv6Addr "::1")) "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.IP6.ARPA."
  , (~?=) (toIP6ARPA (IPv6Addr "2b02:0b08:0:7::0001")) "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.7.0.0.0.0.0.0.0.8.0.b.0.2.0.b.2.IP6.ARPA."
  , (~?=) (toIP6ARPA (IPv6Addr "2b02:b08:0:7::1")) "1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.7.0.0.0.0.0.0.0.8.0.b.0.2.0.b.2.IP6.ARPA."
  , (~?=) (toIP6ARPA (IPv6Addr "fdda:5cc1:23:4::1f")) "f.1.0.0.0.0.0.0.0.0.0.0.0.0.0.0.4.0.0.0.3.2.0.0.1.c.c.5.a.d.d.f.IP6.ARPA."
  , (~?=) (toIP6ARPA (IPv6Addr "2001:db8::")) "0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.8.b.d.0.1.0.0.2.IP6.ARPA."
  , (~?=) (toIP6ARPA (IPv6Addr "4321:0:1:2:3:4:567:89ab")) "b.a.9.8.7.6.5.0.4.0.0.0.3.0.0.0.2.0.0.0.1.0.0.0.0.0.0.0.1.2.3.4.IP6.ARPA."
  , (~?=) (toUNC (IPv6Addr "2001:0DB8:002a:1005:230:48ff:FE73:989d")) "2001-db8-2a-1005-230-48ff-fe73-989d.ipv6-literal.net"
  , (~?=) (toUNC (IPv6Addr "2001:0db8:85a3:0000:0000:8a2e:0370:7334")) "2001-db8-85a3--8a2e-370-7334.ipv6-literal.net"
  , (~?=) (macAddrToIPv6AddrTokens "fa:1d:58:cc:95:16") (Just [SixteenBit "fa1d", Colon, SixteenBit "58cc", Colon, SixteenBit "9516"])
  ]
