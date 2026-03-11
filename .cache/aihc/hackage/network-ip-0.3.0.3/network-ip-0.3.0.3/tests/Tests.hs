{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Tasty (defaultMain, testGroup)
import Test.Tasty.QuickCheck

import Data.DoubleWord
import Data.Textual
import Network.IP.Addr
#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

instance Arbitrary IP4 where
  arbitrary = IP4 <$> arbitrary
  shrink = fmap IP4 . shrink . unIP4

instance Arbitrary IP6 where
  arbitrary = fmap IP6 $ fromHiAndLo <$> arbitrary <*> arbitrary
  shrink = fmap IP6 . shrinkIntegral . unIP6

instance Arbitrary IP where
  arbitrary = oneof [IPv4 <$> arbitrary, IPv6 <$> arbitrary]
  shrink (IPv4 a) = IPv4 <$> shrink a
  shrink (IPv6 a) = IPv6 <$> shrink a

instance Arbitrary Net4Addr where
  arbitrary = netAddr <$> arbitrary <*> suchThat arbitrary (<= 32)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary Net6Addr where
  arbitrary = netAddr <$> arbitrary <*> suchThat arbitrary (<= 128)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary (NetAddr IP) where
  arbitrary = do
      a ← arbitrary
      m ← mask a
      return $ netAddr a m
    where
      mask (IPv4 _) = suchThat arbitrary (<= 32)
      mask (IPv6 _) = suchThat arbitrary (<= 128)
  shrink n = uncurry netAddr <$> shrink (netHost n, netLength n)

instance Arbitrary InetPort where
  arbitrary = InetPort <$> arbitrary
  shrink = fmap InetPort . shrink . unInetPort

instance Arbitrary a ⇒ Arbitrary (InetAddr a) where
  arbitrary = InetAddr <$> arbitrary <*> arbitrary
  shrink n = uncurry InetAddr <$> shrink (inetHost n, inetPort n)

main = defaultMain $ testGroup "Tests"
         [ testGroup "IP4"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ IP4)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP4)
             , testProperty "parsing \"1.2.3.4\"" $
                 fromString "1.2.3.4" == Just (IP4 0x01020304)
             , testProperty "parsing \"11.22.33.44\"" $
                 fromString "11.22.33.44" == Just (IP4 0x0b16212c)
             , testProperty "parsing \"1.256.3.4\" fails" $
                 fromStringAs anIP4 "1.256.3.4" == Nothing
             , testProperty "parsing \" 1.2.3.4\" fails" $
                 fromStringAs anIP4 " 1.2.3.4" == Nothing
             , testProperty "parsing \"1.2.3.4 \" fails" $
                 fromStringAs anIP4 "1.2.3.4 " == Nothing
             , testProperty "parsing \"1.2. 3.4\" fails" $
                 fromStringAs anIP4 "1.2. 3.4 " == Nothing
             , testProperty "parsing \"1.2.3..4\" fails" $
                 fromStringAs anIP4 "1.2.3..4" == Nothing
             , testProperty "parsing \"1.2.3.\" fails" $
                 fromStringAs anIP4 "1.2.3." == Nothing
             , testProperty "parsing \"1.2.3\" fails" $
                 fromStringAs anIP4 "1.2.3" == Nothing
             , testProperty "parsing \"1.2.3.4.5\" fails" $
                 fromStringAs anIP4 "1.2.3.4.5" == Nothing
             , testProperty "parsing \"1a.2.3.4\" fails" $
                 fromStringAs anIP4 "1a.2.3.4" == Nothing
             , testProperty "0.1.2.3 is in the 'This host' range" $
                 ip4Range (ip4FromOctets 0 1 2 3) == ThisHostIP4
             , testProperty "10.1.2.3 is in the 'Private-Use' range" $
                 ip4Range (ip4FromOctets 10 1 2 3) == PrivateUseIP4
             , testProperty "100.64.1.2 is in the 'Shared address space' range" $
                 ip4Range (ip4FromOctets 100 64 1 2) == SharedSpaceIP4
             , testProperty "100.97.1.2 is in the 'Shared address space' range" $
                 ip4Range (ip4FromOctets 100 97 1 2) == SharedSpaceIP4
             , testProperty "100.128.1.2 is in the 'General' range" $
                 ip4Range (ip4FromOctets 100 128 1 2) == GeneralIP4
             , testProperty "127.0.0.1 is in the 'Loopback' range" $
                 ip4Range loopbackIP4 == LoopbackIP4
             , testProperty "169.254.0.1 is in the 'Link Local' range" $
                 ip4Range (ip4FromOctets 169 254 0 1) == LinkLocalIP4
             , testProperty "172.16.1.2 is in the 'Private-Use' range" $
                 ip4Range (ip4FromOctets 172 16 1 2) == PrivateUseIP4
             , testProperty "172.31.1.2 is in the 'Private-Use' range" $
                 ip4Range (ip4FromOctets 172 31 1 2) == PrivateUseIP4
             , testProperty "172.48.1.2 is in the 'General' range" $
                 ip4Range (ip4FromOctets 172 48 1 2) == GeneralIP4
             , testProperty "192.0.0.7 is in the 'DS-Lite' range" $
                 ip4Range (ip4FromOctets 192 0 0 7) == DSLiteIP4
             , testProperty "192.0.0.8 is in the 'Reserved' range" $
                 ip4Range (ip4FromOctets 192 0 0 8) == ReservedIP4
             , testProperty "192.0.2.1 is in the 'Documentation' range" $
                 ip4Range (ip4FromOctets 192 0 2 1) == DocumentationIP4
             , testProperty "192.88.99.1 is in the '6to4' range" $
                 ip4Range (ip4FromOctets 192 88 99 1) == IP6To4IP4
             , testProperty "192.168.1.2 is in the 'Private-Use' range" $
                 ip4Range (ip4FromOctets 192 168 1 2) == PrivateUseIP4
             , testProperty "198.18.1.2 is in the 'Benchmarking' range" $
                 ip4Range (ip4FromOctets 198 18 1 2) == BenchmarkingIP4
             , testProperty "198.19.1.2 is in the 'Benchmarking' range" $
                 ip4Range (ip4FromOctets 198 19 1 2) == BenchmarkingIP4
             , testProperty "198.20.1.2 is in the 'General' range" $
                 ip4Range (ip4FromOctets 198 20 1 2) == GeneralIP4
             , testProperty "198.51.100.1 is in the 'Documentation' range" $
                 ip4Range (ip4FromOctets 198 51 100 1) == DocumentationIP4
             , testProperty "198.51.101.1 is in the 'General' range" $
                 ip4Range (ip4FromOctets 198 51 101 1) == GeneralIP4
             , testProperty "198.52.100.1 is in the 'General' range" $
                 ip4Range (ip4FromOctets 198 52 100 1) == GeneralIP4
             , testProperty "203.0.113.1 is in the 'Documentation' range" $
                 ip4Range (ip4FromOctets 203 0 113 1) == DocumentationIP4
             , testProperty "203.0.114.1 is in the 'General' range" $
                 ip4Range (ip4FromOctets 203 0 114 1) == GeneralIP4
             , testProperty "203.1.113.1 is in the 'General' range" $
                 ip4Range (ip4FromOctets 203 1 113 1) == GeneralIP4
             , testProperty "224.1.2.3 is in the 'Multicast' range" $
                 ip4Range (ip4FromOctets 224 1 2 3) == MulticastIP4
             , testProperty "239.1.2.3 is in the 'Multicast' range" $
                 ip4Range (ip4FromOctets 239 1 2 3) == MulticastIP4
             , testProperty "223.1.2.3 is in the 'General' range" $
                 ip4Range (ip4FromOctets 223 1 2 3) == GeneralIP4
             , testProperty "240.1.2.3 is in the 'Future use' range" $
                 ip4Range (ip4FromOctets 240 1 2 3) == FutureUseIP4
             , testProperty "255.255.255.254 is in the 'Future use' range" $
                 ip4Range (ip4FromOctets 255 255 255 254) == FutureUseIP4
             , testProperty "255.255.255.255 is in the 'Broadcast' range" $
                 ip4Range broadcastIP4 == BroadcastIP4
             , testProperty "ip4Range" $ \a →
                 let r = ip4Range a in
                   if | inNetwork a (net4Addr anyIP4 8)
                        → r == ThisHostIP4
                      | inNetwork a (net4Addr (ip4FromOctets 10 0 0 0) 8) ||
                        inNetwork a
                          (net4Addr (ip4FromOctets 172 16 0 0) 12) ||
                        inNetwork a (net4Addr (ip4FromOctets 192 168 0 0) 16)
                        → r == PrivateUseIP4
                      | inNetwork a (net4Addr (ip4FromOctets 100 64 0 0) 10)
                        → r == SharedSpaceIP4
                      | inNetwork a (net4Addr (ip4FromOctets 127 0 0 0) 8)
                        → r == LoopbackIP4
                      | inNetwork a (net4Addr (ip4FromOctets 169 254 0 0) 16)
                        → r == LinkLocalIP4
                      | inNetwork a (net4Addr (ip4FromOctets 192 0 0 0) 24)
                        → if inNetwork a
                               (net4Addr (ip4FromOctets 192 0 0 0) 29)
                          then r == DSLiteIP4
                          else r == ReservedIP4
                      | inNetwork a (net4Addr (ip4FromOctets 192 0 2 0) 24) ||
                        inNetwork a
                          (net4Addr (ip4FromOctets 198 51 100 0) 24) ||
                        inNetwork a (net4Addr (ip4FromOctets 203 0 113 0) 24)
                        → r == DocumentationIP4
                      | inNetwork a (net4Addr (ip4FromOctets 192 88 99 0) 24)
                        → r == IP6To4IP4
                      | inNetwork a (net4Addr (ip4FromOctets 198 18 0 0) 15)
                        → r == BenchmarkingIP4
                      | inNetwork a (net4Addr (ip4FromOctets 224 0 0 0) 4)
                        → r == MulticastIP4
                      | inNetwork a (net4Addr (ip4FromOctets 240 0 0 0) 4)
                        → if a == broadcastIP4
                          then r == BroadcastIP4
                          else r == FutureUseIP4
                      | otherwise
                        → r == GeneralIP4
             ]
         , testGroup "IP6"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ IP6)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP6)
             , testProperty "parsing \"::\"" $
                 fromString "::" == Just anyIP6
             , testProperty "parsing \":::\" fails" $
                 fromStringAs anIP6 ":::" == Nothing
             , testProperty "parsing \" 1::2\" fails" $
                 fromStringAs anIP6 " 1::2" == Nothing
             , testProperty "parsing \"1::2 \" fails" $
                 fromStringAs anIP6 "1::2 " == Nothing
             , testProperty "parsing \"1: :2\" fails" $
                 fromStringAs anIP6 "1: :2" == Nothing
             , testProperty "parsing \"1::2:\" fails" $
                 fromStringAs anIP6 "1::2:" == Nothing
             , testProperty "parsing \":1::2\" fails" $
                 fromStringAs anIP6 ":1::2" == Nothing
             , testProperty "parsing \"1:abcde::2\" fails" $
                 fromStringAs anIP6 "1:abcde::2" == Nothing
             , testProperty "parsing \"1:abcg::2\" fails" $
                 fromStringAs anIP6 "1:abcg::2" == Nothing
             , testProperty "parsing \"1:2:3:4a:5:6:7:8\"" $
                 fromString "1:2:3:4a:5:6:7:8" ==
                   Just (IP6 0x000100020003004a0005000600070008)
             , testProperty "parsing \"1:2:3:4:5bc:6:7::\"" $
                 fromString "1:2:3:4:5bc:6:7::" ==
                   Just (IP6 0x000100020003000405bc000600070000)
             , testProperty "parsing \"::1:2:3ad9:4:5:6:7\"" $
                 fromString "::1:2:3ad9:4:5:6:7" ==
                   Just (IP6 0x000100023ad90004000500060007)
             , testProperty "parsing \"1:2:3:4:a5:6::\"" $
                 fromString "1:2:3:4:a5:6::" ==
                   Just (IP6 0x000100020003000400a5000600000000)
             , testProperty "parsing \"::1:2:3:4:5:6\"" $
                 fromString "::1:2:3:4:5:6" ==
                   Just (IP6 0x000100020003000400050006)
             , testProperty "parsing \"1::2:3:4:5:6\"" $
                 fromString "1::2:3:4:5:6" ==
                   Just (IP6 0x00010000000000020003000400050006)
             , testProperty "parsing \"1:2:3::4:5:6\"" $
                 fromString "1:2:3::4:5:6" ==
                   Just (IP6 0x00010002000300000000000400050006)
             , testProperty "parsing \"1:2:3::4:5::6\" fails" $
                 fromStringAs anIP6 "1:2:3::4:5::6" == Nothing
             , testProperty "parsing \"::1:2:3::4:5:6\" fails" $
                 fromStringAs anIP6 "::1:2:3::4:5:6" == Nothing
             , testProperty "parsing \"1:2::3:4:5g\" fails" $
                 fromStringAs anIP6 "1:2::3:4:5g" == Nothing
             , testProperty "parsing \"1:2:3:4:5:6:7:8g\" fails" $
                 fromStringAs anIP6 "1:2:3:4:5:6:7:8g" == Nothing
             , testProperty ":: is in the 'Unspecified' range" $
                 ip6Range (IP6 0) == AnyIP6
             , testProperty "::1 is in the 'Loopback' range" $
                 ip6Range (IP6 1) == LoopbackIP6
             , testProperty "::ffff:0:0:102:304 is in the 'IPv4 mapped' range" $
                 ip6Range (ip6FromWords 0 0 0 0xFFFF 0 0 0x102 0x304) ==
                   IP4MappedIP6
             , testProperty "64:ff9b::102:304 is in the 'IPv4 embedded' range" $
                 ip6Range (ip6FromWords 0x64 0xFF9B 0 0 0 0 0x102 0x304) ==
                   IP4EmbeddedIP6
             , testProperty "100::1:2:3:4 is in the 'Discard' range" $
                 ip6Range (ip6FromWords 0x100 0 0 0 1 2 3 4) == DiscardIP6
             , testProperty "100::1:2:3:4:5 is in the 'General' range" $
                 ip6Range (ip6FromWords 0x100 0 0 1 2 3 4 5) == GeneralIP6
             , testProperty "2001::ffff:1:2:3:4:5 is in the 'Teredo' range" $
                 ip6Range (ip6FromWords 0x2001 0 0xFFFF 1 2 3 4 5) ==
                   TeredoIP6
             , testProperty "2001:2::0xFFFF:1:2:3:4 is in the 'Benchmarking' range" $
                 ip6Range (ip6FromWords 0x2001 2 0 0xFFFF 1 2 3 4) ==
                   BenchmarkingIP6
             , testProperty "2001:2:1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0x2001 2 1 2 3 4 5 6) == GeneralIP6
             , testProperty "2001:db8:0xFFFF:1:2:3:4:5 is in the 'Documentation' range" $
                 ip6Range (ip6FromWords 0x2001 0xDB8 0xFFFF 1 2 3 4 5) ==
                   DocumentationIP6
             , testProperty "2001:db9:1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0x2001 0xDB9 1 2 3 4 5 6) ==
                   GeneralIP6
             , testProperty "2001:10:0xFFFF:1:2:3:4:5 is in the 'Orchid' range" $
                 ip6Range (ip6FromWords 0x2001 0x10 0xFFFF 1 2 3 4 5) ==
                   OrchidIP6
             , testProperty "2001:18:1:2:3:4:5:6 is in the 'Orchid' range" $
                 ip6Range (ip6FromWords 0x2001 0x18 1 2 3 4 5 6) ==
                   OrchidIP6
             , testProperty "2001:100:1:2:3:4:5:6 is in the 'Reserved' range" $
                 ip6Range (ip6FromWords 0x2001 0x100 1 2 3 4 5 6) ==
                   ReservedIP6
             , testProperty "2001:200:1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0x2001 0x200 1 2 3 4 5 6) ==
                   GeneralIP6
             , testProperty "2002::1:2:3:4:5:6 is in the '6to4' range" $
                 ip6Range (ip6FromWords 0x2002 0 1 2 3 4 5 6) ==
                   IP6To4IP6
             , testProperty "2002:ffff:1:2:3:4:5:6 is in the '6to4' range" $
                 ip6Range (ip6FromWords 0x2002 0xFFFF 1 2 3 4 5 6) ==
                   IP6To4IP6
             , testProperty "fc00::1:2:3:4:5:6 is in the 'Unique Local' range" $
                 ip6Range (ip6FromWords 0xFC00 0 1 2 3 4 5 6) ==
                   UniqueLocalIP6
             , testProperty "fdff::1:2:3:4:5:6 is in the 'Unique Local' range" $
                 ip6Range (ip6FromWords 0xFDFF 0 1 2 3 4 5 6) ==
                   UniqueLocalIP6
             , testProperty "fe00::1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0xFE00 0 1 2 3 4 5 6) ==
                   GeneralIP6
             , testProperty "fe80::1:2:3:4:5:6 is in the 'Link Local' range" $
                 ip6Range (ip6FromWords 0xFE80 0 1 2 3 4 5 6) ==
                   LinkLocalIP6
             , testProperty "febf::1:2:3:4:5:6 is in the 'Link Local' range" $
                 ip6Range (ip6FromWords 0xFEBF 0 1 2 3 4 5 6) ==
                   LinkLocalIP6
             , testProperty "fec0::1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0xFEC0 0 1 2 3 4 5 6) ==
                   GeneralIP6
             , testProperty "fe00::1:2:3:4:5:6 is in the 'General' range" $
                 ip6Range (ip6FromWords 0xFE00 0 1 2 3 4 5 6) ==
                   GeneralIP6
             , testProperty "ff00::1:2:3:4:5:6 is in the 'Multicast' range" $
                 ip6Range (ip6FromWords 0xFF00 0 1 2 3 4 5 6) ==
                   MulticastIP6
             , testProperty "ffff::1:2:3:4:5:6 is in the 'Multicast' range" $
                 ip6Range (ip6FromWords 0xFFFF 0 1 2 3 4 5 6) ==
                   MulticastIP6
             , testProperty "ip6Range" $ \a →
                 let r = ip6Range a in
                   if | a == anyIP6
                        → r == AnyIP6
                      | a == loopbackIP6
                        → r == LoopbackIP6 
                      | inNetwork a
                          (net6Addr (ip6FromWords 0 0 0 0xFFFF 0 0 0 0) 96)
                        → r == IP4MappedIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0x100 0 0 0 0 0 0 0) 64)
                        → r == DiscardIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0x64 0xFF9B 0 0 0 0 0 0) 96)
                        → r == IP4EmbeddedIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0x2001 0 0 0 0 0 0 0) 23)
                        → if inNetwork a
                               (net6Addr
                                  (ip6FromWords 0x2001 0 0 0 0 0 0 0) 32)
                          then r == TeredoIP6
                          else
                            if inNetwork a
                                 (net6Addr
                                    (ip6FromWords 0x2001 0x10 0 0 0 0 0 0) 28)
                            then r == OrchidIP6
                            else r == ReservedIP6
                      | inNetwork a
                          (net6Addr
                             (ip6FromWords 0x2001 0xDB8 0 0 0 0 0 0) 32)
                        → r == DocumentationIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0x2002 0 0 0 0 0 0 0) 16)
                        → r == IP6To4IP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0xFC00 0 0 0 0 0 0 0) 7)
                        → r == UniqueLocalIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0xFE80 0 0 0 0 0 0 0) 10)
                        → r == LinkLocalIP6
                      | inNetwork a
                          (net6Addr (ip6FromWords 0xFF00 0 0 0 0 0 0 0) 8)
                        → r == MulticastIP6
                      | otherwise
                        → r == GeneralIP6
             ]
         , testGroup "IP"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ IP)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ IP)
             ]
         , testGroup "Net4Addr"
             [ testProperty "show -> read" $ \n →
                 read (show n) == (n ∷ Net4Addr)
             , testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ Net4Addr)
             , testProperty "parsing \"11.22.33.44/14\"" $
                 case fromStringAs aNet4Addr "11.22.33.44/14" of
                   Just n | netHost n   == IP4 0x0b16212c
                          , netHostIx n == IP4 0x0002212c
                          , netPrefix n == IP4 0x0b140000
                          , netMask n   == IP4 0xfffc0000
                          , netLength n == 14
                          → True
                   _ → False
             , testProperty "parsing \"1.2.3/16\" fails" $
                 fromStringAs aNet4Addr "1.2.3/16" == Nothing
             , testProperty "parsing \"1.2 .3.4/16\" fails" $
                 fromStringAs aNet4Addr "1.2 .3.4/16" == Nothing
             , testProperty "parsing \"1.2.3.4/ 16\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/ 16" == Nothing
             , testProperty "parsing \"1.2.3.4 / 16\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4 / 16" == Nothing
             , testProperty "parsing \"1.2.3.4/16 \" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/16 " == Nothing
             , testProperty "parsing \"1.2.3.4/33\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/33" == Nothing
             , testProperty "parsing \"1.2.3.4/012\" fails" $
                 fromStringAs aNet4Addr "1.2.3.4/012" == Nothing
             ]
         , testGroup "Net6Addr"
             [ testProperty "show -> read" $ \n →
                 read (show n) == (n ∷ Net6Addr)
             , testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ Net6Addr)
             , testProperty "parsing \"1234:5678:9abc:def::/22\"" $
                 case fromStringAs aNet6Addr "1234:5678:9abc:def::/22" of
                   Just n | netHost n   == IP6 0x123456789abc0def0000000000000000
                          , netHostIx n == IP6 0x000002789abc0def0000000000000000
                          , netPrefix n == IP6 0x12345400000000000000000000000000
                          , netMask n   == IP6 0xfffffc00000000000000000000000000
                          , netLength n == 22
                          → True
                   _ → False
             , testProperty "parsing \"1:2:3/16\" fails" $
                 fromStringAs aNet6Addr "1:2:3/16" == Nothing
             , testProperty "parsing \"1:2 :3:4::/16\" fails" $
                 fromStringAs aNet6Addr "1:2 :3:4::/16" == Nothing
             , testProperty "parsing \"::1:2:3:4/ 16\" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4/ 16" == Nothing
             , testProperty "parsing \"::1:2:3:4 / 16\" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4 / 16" == Nothing
             , testProperty "parsing \"::1:2:3:4/16 \" fails" $
                 fromStringAs aNet6Addr "::1:2:3:4/16 " == Nothing
             , testProperty "parsing \"::/129\" fails" $
                 fromStringAs aNet6Addr "::/129" == Nothing
             , testProperty "parsing \"::/01\" fails" $
                 fromStringAs aNet6Addr "::/01" == Nothing
             ]
         , testGroup "NetAddr"
             [ testProperty "show -> read" $ \n →
                 read (show n) == (n ∷ NetAddr IP)
             , testProperty "print -> parse" $ \n →
                 fromString (toString n) == Just (n ∷ NetAddr IP)
             ]
         , testGroup "InetPort"
             [ testProperty "show -> read" $ \p →
                 read (show p) == (p ∷ InetPort)
             , testProperty "print -> parse" $ \p →
                 fromString (toString p) == Just (p ∷ InetPort)
             , testProperty "parsing \"01\" fails" $
                 fromStringAs anInetPort "01" == Nothing
             , testProperty "parsing \"123456\" fails" $
                 fromStringAs anInetPort "123456" == Nothing
             , testProperty "parsing \"65536\" fails" $
                 fromStringAs anInetPort "65536" == Nothing
             ]
         , testGroup "Inet4Addr"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ Inet4Addr)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ Inet4Addr)
             , testProperty "parsing \"11.22.33.44:55\"" $
                 case fromString "11.22.33.44:55" of
                   Just (InetAddr (IP4 0x0b16212c) 55) → True
                   _ → False
             ]
         , testGroup "Inet6Addr"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ Inet6Addr)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ Inet6Addr)
             , testProperty "parsing \"[1234:5678:9abc:def::]:55\"" $
                 case fromString "[1234:5678:9abc:def::]:55" of
                   Just (InetAddr (IP6 0x123456789abc0def0000000000000000) 55) → True
                   _ → False
             ]
         , testGroup "InetAddr"
             [ testProperty "show -> read" $ \a →
                 read (show a) == (a ∷ InetAddr IP)
             , testProperty "print -> parse" $ \a →
                 fromString (toString a) == Just (a ∷ InetAddr IP)
             ]
         ]

