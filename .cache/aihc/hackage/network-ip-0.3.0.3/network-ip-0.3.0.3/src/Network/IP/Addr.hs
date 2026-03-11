{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE TypeFamilies #-}

-- | Internet Protocol addressing.
module Network.IP.Addr
  (
  -- * Host address
  -- ** IPv4 address
    IP4(..)
  , anIP4
  , ip4ToOctets
  , ip4ToOctetList
  , ip4FromOctets
  , ip4FromOctetList
  , anyIP4
  , loopbackIP4
  , broadcastIP4
  , Range4(..)
  , ip4Range
  -- ** IPv6 address
  , IP6(..)
  , anIP6
  , ip6ToWords
  , ip6ToWordList
  , ip6FromWords
  , ip6FromWordList
  , anyIP6
  , loopbackIP6
  , Range6(..)
  , ip6Range
  -- ** IP address
  , IP46(..)
  , anIP46
  , anIP46Of
  , IP
  , anIP
  -- * Network address
  , IsNetAddr(..)
  , Net4Addr
  , Net6Addr
  , NetAddr
  , aNetAddr
  , aNetAddrOf
  , aNet4Addr
  , aNet6Addr
  , aNetAddrIP
  , net4Addr
  , net6Addr
  , toNetAddr46
  , fromNetAddr46
  , printNetAddr
  , net4Parser
  , net6Parser
  , netParser
  , putNetAddr
  , getNetAddr
  -- * Port number
  , InetPort(..)
  , anInetPort
  -- * Socket address
  , InetAddr(..)
  , Inet4Addr
  , Inet6Addr
  , anInetAddr
  , anInetAddrOf
  , anInet4Addr
  , anInet6Addr
  , anInetAddrIP
  , toInetAddr46
  , fromInetAddr46
  ) where

import Prelude hiding (print)
import Data.Typeable (Typeable)
#if !MIN_VERSION_base(4,10,0)
import Data.Typeable (Typeable1)
#endif
import Data.Data (Data)
import Data.Word
import Data.Bits
import Data.DoubleWord (BinaryWord(..), DoubleWord(..), Word128)
import Data.Ix (Ix)
import Data.Endian
import Data.Default.Class
import Data.Hashable
import Data.Serializer (Serializer, Serializable, SizedSerializable)
import qualified Data.Serializer as S
import Data.Deserializer (Deserializer, Deserializable)
import qualified Data.Deserializer as D
import Text.Printer (Printer)
#if !MIN_VERSION_base(4,13,0)
import Text.Printer ((<>))
#endif
import qualified Text.Printer as P
import qualified Text.Printer.Integral as P
import Data.Textual
import Data.Textual.Integral hiding (Binary)
import Text.Parser.Combinators ((<?>), try)
import Text.Parser.Char (CharParsing)
import qualified Text.Parser.Char as PC
import Text.Printf (printf)
import Type.Hint
import Control.Applicative
import Control.Monad (void, when)
import Foreign.Ptr (castPtr)
import Foreign.Storable (Storable(..))

-- | IPv4 address.
newtype IP4 = IP4 { unIP4 ∷ Word32 }
  deriving (Typeable, Data, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable)

-- | 'IP4' proxy value.
anIP4 ∷ Proxy IP4
anIP4 = Proxy

instance Show IP4 where
  showsPrec p a = showParen (p > 10)
                $ showString "ip4FromOctets "
                . showsPrec 11 o1
                . showString " "
                . showsPrec 11 o2
                . showString " "
                . showsPrec 11 o3
                . showString " "
                . showsPrec 11 o4
    where (o1, o2, o3, o4) = ip4ToOctets a

instance Read IP4 where
  readsPrec p = readParen (p > 10) $ \i →
                  [ (ip4FromOctets o1 o2 o3 o4, i4)
                  | ("ip4FromOctets", i') ← lex i
                  , (o1, i1) ← readsPrec 11 i'
                  , (o2, i2) ← readsPrec 11 i1
                  , (o3, i3) ← readsPrec 11 i2
                  , (o4, i4) ← readsPrec 11 i3 ]

instance Printable IP4 where
  print = P.fsep (P.char7 '.') . (P.nonNegative Decimal <$>) . ip4ToOctetList
  {-# INLINE print #-}

instance Textual IP4 where
  textual  =  (<?> "IPv4 address")
           $  ip4FromOctets
          <$> (octet <* PC.char '.')
          <*> (octet <* PC.char '.')
          <*> (octet <* PC.char '.')
          <*> octet
    where octet = (<?> "IPv4 octet") $ do
            o ← nnUpTo Decimal 3
            if o > 255
              then D.unexpected "out of bounds"
              else return $ fromIntegral (o ∷ Int)

instance Storable IP4 where
  alignment _ = alignment (undefined ∷ Word32)
  sizeOf _    = 4
  peek p      = IP4 . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unIP4

instance Serializable IP4 where
  put = S.word32B . unIP4
  {-# INLINE put #-}

instance SizedSerializable IP4 where
  size _ = 4
  {-# INLINE size #-}

instance Deserializable IP4 where
  get = IP4 <$> D.word32B <?> "IPv4 address"
  {-# INLINE get #-}

-- | The octets of an IPv4 address.
ip4ToOctets ∷ IP4 → (Word8, Word8, Word8, Word8)
ip4ToOctets (IP4 w) = ( fromIntegral $ w `shiftR` 24
                      , fromIntegral $ w `shiftR` 16
                      , fromIntegral $ w `shiftR` 8
                      , fromIntegral w
                      )
{-# INLINABLE ip4ToOctets #-}

-- | List the octets of an IPv4 address.
ip4ToOctetList ∷ IP4 → [Word8]
ip4ToOctetList (IP4 w) = fromIntegral <$>
                           [w `shiftR` 24, w `shiftR` 16, w `shiftR` 8, w]
{-# INLINABLE ip4ToOctetList #-}

-- | Assemble IPv4 address from the octets.
ip4FromOctets ∷ Word8 → Word8 → Word8 → Word8 → IP4
ip4FromOctets o1 o2 o3 o4 =
  IP4  $  fromIntegral o1 `shiftL` 24
      .|. fromIntegral o2 `shiftL` 16
      .|. fromIntegral o3 `shiftL` 8
      .|. fromIntegral o4
{-# INLINABLE ip4FromOctets #-}

-- | Assemble IPv4 address from the octet list.
ip4FromOctetList ∷ [Word8] → Maybe IP4
ip4FromOctetList [o1, o2, o3, o4] = Just $ ip4FromOctets o1 o2 o3 o4
ip4FromOctetList _                = Nothing
{-# INLINABLE ip4FromOctetList #-}

-- | IPv4 address @0.0.0.0@.
anyIP4 ∷ IP4
anyIP4 = IP4 0
{-# INLINE anyIP4 #-}

instance Default IP4 where
  def = anyIP4
  {-# INLINE def #-}

-- | IPv4 address @127.0.0.1@.
loopbackIP4 ∷ IP4
loopbackIP4 = IP4 0x7F000001
{-# INLINE loopbackIP4 #-}

-- | IPv4 address @255.255.255.255@.
broadcastIP4 ∷ IP4
broadcastIP4 = IP4 0xFFFFFFFF
{-# INLINE broadcastIP4 #-}

-- | IPv4 address range classification (per RFC6890).
data Range4 = GeneralIP4       -- ^ General IPv4 address.
            | ThisHostIP4      -- ^ This host on this network.
            | PrivateUseIP4    -- ^ Private-Use networks.
            | SharedSpaceIP4   -- ^ Shared address space.
            | LoopbackIP4      -- ^ Loopback address.
            | LinkLocalIP4     -- ^ Link local address.
            | ReservedIP4      -- ^ Reserved address.
            | DSLiteIP4        -- ^ Dual-Stack Lite.
            | DocumentationIP4 -- ^ Reserved for documentation.
            | IP6To4IP4        -- ^ 6to4.
            | BenchmarkingIP4  -- ^ Benchmark testing.
            | MulticastIP4     -- ^ Multicast address.
            | FutureUseIP4     -- ^ Future use.
            | BroadcastIP4     -- ^ Limited broadcast.
            deriving (Typeable, Data, Show, Read, Eq, Ord, Enum)

-- | Determine the address range type.
ip4Range ∷ IP4 → Range4
ip4Range addr = case w1 of
    0   → ThisHostIP4
    10  → PrivateUseIP4
    100 → if w2 .&. 0xC0 == 0x40 then SharedSpaceIP4 else GeneralIP4
    127 → LoopbackIP4
    169 → if w2 == 254 then LinkLocalIP4 else GeneralIP4
    172 → if w2 .&. 0xF0 == 0x10 then PrivateUseIP4 else GeneralIP4
    192 → case w2 of
            0   → case w3 of
                    0 → if w4 <= 7 then DSLiteIP4 else ReservedIP4
                    2 → DocumentationIP4
                    _ → GeneralIP4
            88  → if w3 == 99 then IP6To4IP4 else GeneralIP4
            168 → PrivateUseIP4
            _   → GeneralIP4
    198 → case w2 of
            18  → BenchmarkingIP4
            19  → BenchmarkingIP4
            51  → if w3 == 100 then DocumentationIP4 else GeneralIP4
            _   → GeneralIP4
    203 → if w2 == 0 && w3 == 113 then DocumentationIP4 else GeneralIP4
    _   | addr == broadcastIP4 → BroadcastIP4
    _   → case w1 .&. 0xF0 of
            224 → MulticastIP4
            240 → FutureUseIP4
            _   → GeneralIP4
  where (w1, w2, w3, w4) = ip4ToOctets addr

-- | IPv6 address.
newtype IP6 = IP6 { unIP6 ∷ Word128 }
  deriving (Typeable, Data, Eq, Ord, Bounded, Enum, Ix, Num, Bits, Hashable)

-- | 'IP6' proxy value.
anIP6 ∷ Proxy IP6
anIP6 = Proxy

instance Show IP6 where
  showsPrec p a
      = showParen (p > 10)
      $ showString
      $ printf "ip6FromWords 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x 0x%x"
               w1 w2 w3 w4 w5 w6 w7 w8
    where
      (w1, w2, w3, w4, w5, w6, w7, w8) = ip6ToWords a

instance Read IP6 where
  readsPrec p = readParen (p > 10) $ \i →
                  [ (ip6FromWords w1 w2 w3 w4 w5 w6 w7 w8, i8)
                  | ("ip6FromWords", i') ← lex i
                  , (w1, i1) ← readsPrec 11 i'
                  , (w2, i2) ← readsPrec 11 i1
                  , (w3, i3) ← readsPrec 11 i2
                  , (w4, i4) ← readsPrec 11 i3
                  , (w5, i5) ← readsPrec 11 i4
                  , (w6, i6) ← readsPrec 11 i5
                  , (w7, i7) ← readsPrec 11 i6
                  , (w8, i8) ← readsPrec 11 i7 ]

instance Printable IP6 where
  print addr = case addrZeroes of
      Nothing → P.fsep (P.char7 ':') $ hex <$> addrWords
      Just (i, n) →  P.fsep (P.char7 ':') (hex <$> take i addrWords)
                  <> P.string7 "::"
                  <> P.fsep (P.char7 ':') (hex <$> drop (i + n) addrWords)
    where hex        = P.nonNegative LowHex
          addrWords  = ip6ToWordList addr
          addrZeroes = go 0 0 0 addrWords
          go !i zi zn = \case
            []           → if zn <= 1 then Nothing else Just (zi, zn)
            (0 : words') → inZeroes (i + 1) zi zn i 1 words'
            (_ : words') → go (i + 1) zi zn words'
          inZeroes !i zi zn zi' !zn' = \case
            [] | zn' <= 1  → if zn <= 1 then Nothing else Just (zi, zn)
               | zn <= 1   → Just (zi', zn')
               | otherwise → Just $ if zn >= zn' then (zi, zn) else (zi', zn')
            (0 : words') → inZeroes (i + 1) zi zn zi' (zn' + 1) words'
            (_ : words') | zn' > zn  → go (i + 1) zi' zn' words'
                         | otherwise → go (i + 1) zi zn words'

instance Textual IP6 where
  textual = (<?> "IPv6 address") $ optional (PC.char ':') >>= \case
              Just _ → do
                void $ PC.char ':'
                optional word >>= \case
                  Just w → after 6 0 (fromIntegral w)
                  Nothing → return anyIP6
              Nothing → do
                w ← word
                before 7 (fromIntegral w)
    where
      word = nnUpTo Hexadecimal 4 `hintTypeArg` aWord16 <?> "word"
      before 0 w = return $ IP6 w
      before 1 w = do
        void $ PC.char ':'
        optional (PC.char ':') >>= \case
          Just _ →
            return $ IP6 $ w `shiftL` 16
          Nothing → do
            w' ← word
            return $ IP6 $ (w `shiftL` 16) .|. fromIntegral w'
      before upTo w = do
        void $ PC.char ':'
        optional (PC.char ':') >>= \case
          Just _ → optional word >>= \case
            Just w' →
              after (upTo - 2) (w `shiftL` (upTo * 16)) (fromIntegral w')
            Nothing →
              return $ IP6 $ w `shiftL` (upTo * 16)
          Nothing → do
            w' ← word
            before (upTo - 1) ((w `shiftL` 16) .|. fromIntegral w')
      after 0 w1 w2 = return $ IP6 $ w1 .|. w2
      after upTo w1 w2 = optional (PC.char ':') >>= \case
        Just _ → do
          w ← word
          after (upTo - 1) w1 ((w2 `shiftL` 16) .|. fromIntegral w)
        Nothing →
          return $ IP6 $ w1 .|. w2

instance Storable IP6 where
  alignment _ = alignment (undefined ∷ Word64)
  sizeOf _    = 16
  peek p      = fmap IP6
              $ fromHiAndLo <$> (fromBigEndian <$> peek (castPtr p))
                            <*> (fromBigEndian <$> peek (castPtr p))
  poke p (IP6 w) = do
    poke (castPtr p) $ toBigEndian $ hiWord w
    poke (castPtr p) $ toBigEndian $ loWord w

instance Serializable IP6 where
  put (IP6 w) = S.word64B (hiWord w) <> S.word64B (loWord w)
  {-# INLINE put #-}

instance SizedSerializable IP6 where
  size _ = 16
  {-# INLINE size #-}

instance Deserializable IP6 where
  get = fmap IP6 $ fromHiAndLo <$> D.get <*> D.get <?> "IPv6 address"

-- | The 16-bit pieces of an IPv6 address.
ip6ToWords ∷ IP6 → (Word16, Word16, Word16, Word16,
                    Word16, Word16, Word16, Word16)
ip6ToWords (IP6 w) =
    ( fromIntegral $ hi `shiftR` 48
    , fromIntegral $ hi `shiftR` 32
    , fromIntegral $ hi `shiftR` 16
    , fromIntegral hi
    , fromIntegral $ lo `shiftR` 48
    , fromIntegral $ lo `shiftR` 32
    , fromIntegral $ lo `shiftR` 16
    , fromIntegral lo
    )
  where hi = hiWord w
        lo = loWord w
{-# INLINABLE ip6ToWords #-}

-- | List the 16-bit pieces of an IPv6 address.
ip6ToWordList ∷ IP6 → [Word16]
ip6ToWordList (IP6 w) = fromIntegral <$>
    [ hi `shiftR` 48
    , hi `shiftR` 32
    , hi `shiftR` 16
    , hi
    , lo `shiftR` 48
    , lo `shiftR` 32
    , lo `shiftR` 16
    , lo
    ]
  where hi = hiWord w
        lo = loWord w
{-# INLINABLE ip6ToWordList #-}

-- | Assemble IPv6 address from the 16-bit pieces.
ip6FromWords ∷ Word16 → Word16 → Word16 → Word16
             → Word16 → Word16 → Word16 → Word16
             → IP6
ip6FromWords w1 w2 w3 w4 w5 w6 w7 w8 = IP6 $ fromHiAndLo hi lo
  where hi  =  fromIntegral w1 `shiftL` 48
           .|. fromIntegral w2 `shiftL` 32
           .|. fromIntegral w3 `shiftL` 16
           .|. fromIntegral w4
        lo  =  fromIntegral w5 `shiftL` 48
           .|. fromIntegral w6 `shiftL` 32
           .|. fromIntegral w7 `shiftL` 16
           .|. fromIntegral w8
{-# INLINABLE ip6FromWords #-}

-- | Assemble IPv6 address from the list of 16-bit pieces.
ip6FromWordList ∷ [Word16] → Maybe IP6
ip6FromWordList [w1, w2, w3, w4, w5, w6, w7, w8] =
  Just $ ip6FromWords w1 w2 w3 w4 w5 w6 w7 w8
ip6FromWordList _ = Nothing
{-# INLINABLE ip6FromWordList #-}

-- | IPv6 address @::@.
anyIP6 ∷ IP6
anyIP6 = IP6 0
{-# INLINE anyIP6 #-}

instance Default IP6 where
  def = anyIP6
  {-# INLINE def #-}

-- | IPv6 address @::1@.
loopbackIP6 ∷ IP6
loopbackIP6 = IP6 1

-- | IPv6 address range classification (per RFC6890).
data Range6 = GeneralIP6       -- ^ General IPv6 address.
            | AnyIP6           -- ^ Unspecified address.
            | LoopbackIP6      -- ^ Loopback address.
            | IP4MappedIP6     -- ^ Mapped IPv4 address.
            | IP4EmbeddedIP6   -- ^ Embedded IPv4 address.
            | DiscardIP6       -- ^ Discard address.
            | ReservedIP6      -- ^ Reserved address.
            | TeredoIP6        -- ^ Teredo address.
            | BenchmarkingIP6  -- ^ Benchmark testing.
            | DocumentationIP6 -- ^ Reserved for documentation.
            | OrchidIP6        -- ^ ORCHID address.
            | IP6To4IP6        -- ^ 6to4.
            | UniqueLocalIP6   -- ^ Unique local address.
            | LinkLocalIP6     -- ^ Link local address.
            | MulticastIP6     -- ^ Multicast address.
            deriving (Typeable, Data, Show, Read, Eq, Ord, Enum)

-- | Determine the address range type.
ip6Range ∷ IP6 → Range6
ip6Range (IP6 w) = case hi of
    0 → case lo of
          0 → AnyIP6
          1 → LoopbackIP6
          _ → GeneralIP6
    0x000000000000FFFF → if q3 == 0 then IP4MappedIP6 else GeneralIP6
    0x0064FF9B00000000 → if q3 == 0 then IP4EmbeddedIP6 else GeneralIP6
    0x0100000000000000 → DiscardIP6
    _                  → case w1 of
      0x2001 → case w2 of
                 0     → TeredoIP6
                 2     → if w3 == 0 then BenchmarkingIP6 else GeneralIP6
                 0xDB8 → DocumentationIP6
                 _     | w2 .&. 0xFFF0 == 0x10 → OrchidIP6
                       | w2 .&. 0xFE00 == 0    → ReservedIP6
                       | otherwise             → GeneralIP6
      0x2002 → IP6To4IP6
      _      | w1 .&. 0xFE00 == 0xFC00 → UniqueLocalIP6
             | w1 .&. 0xFFC0 == 0xFE80 → LinkLocalIP6
             | w1 >= 0xFF00            → MulticastIP6
             | otherwise               → GeneralIP6
  where hi = hiWord w
        lo = loWord w
        q1 = hiWord hi
        q2 = loWord hi
        q3 = hiWord lo
        w1 = hiWord q1
        w2 = loWord q1
        w3 = hiWord q2

-- | IPv4- or IPv6-specific data.
data IP46 t₄ t₆ = IPv4 t₄
                | IPv6 t₆
                deriving (Typeable, Data, Eq, Ord, Show, Read)

-- | 'IP46' proxy value.
anIP46 ∷ Proxy IP46
anIP46 = Proxy

-- | 'IP46' /t₄/ /t₆/ proxy value.
anIP46Of ∷ Proxy t₄ → Proxy t₆ → Proxy (IP46 t₄ t₆)
anIP46Of _ _ = Proxy

-- | IP address.
type IP = IP46 IP4 IP6

-- | 'IP' proxy value.
anIP ∷ Proxy IP
anIP = Proxy

instance Printable IP where
  print (IPv4 a) = print a
  print (IPv6 a) = print a
  {-# INLINABLE print #-}

instance Textual IP where
  textual = try (IPv4 <$> textual) <|> (IPv6 <$> textual)
  {-# INLINABLE textual #-}

instance Serializable IP where
  put (IPv4 a) = S.put (Left a ∷ Either IP4 IP6)
  put (IPv6 a) = S.put (Right a ∷ Either IP4 IP6)

instance Deserializable IP where
  get = D.get >>= return . either IPv4 IPv6 <?> "IP address"

-- | Network address.
class IsNetAddr n where
  -- | The address type.
  type NetHost n
  -- | Host address.
  netHost   ∷ n → NetHost n
  -- | Host index.
  netHostIx ∷ n → NetHost n
  -- | Network prefix.
  netPrefix ∷ n → NetHost n
  -- | Network mask.
  netMask   ∷ n → NetHost n
  -- | Network prefix length.
  netLength ∷ n → Word8
  -- | Make a network address.
  netAddr   ∷ NetHost n -- ^ Host address
            → Word8     -- ^ Network prefix length
            → n
  -- | Test if the address is in the network.
  inNetwork ∷ NetHost n -- ^ Host address
            → n         -- ^ Network address
            → Bool

-- | Network address: host address + network prefix length.
data NetAddr a = NetAddr a {-# UNPACK #-} !Word8
                 deriving Eq

#if !MIN_VERSION_base(4,10,0)
deriving instance Typeable1 NetAddr
#endif
deriving instance Data a ⇒ Data (NetAddr a)

-- | IPv4 network address.
type Net4Addr = NetAddr IP4

-- | IPv6 network address.
type Net6Addr = NetAddr IP6

-- | 'NetAddr' proxy value.
aNetAddr ∷ Proxy NetAddr
aNetAddr = Proxy

-- | 'NetAddr' /a/ proxy value.
aNetAddrOf ∷ Proxy a → Proxy (NetAddr a)
aNetAddrOf _ = Proxy

-- | 'Net4Addr' proxy value.
aNet4Addr ∷ Proxy Net4Addr
aNet4Addr = Proxy

-- | 'Net6Addr' proxy value.
aNet6Addr ∷ Proxy Net6Addr
aNet6Addr = Proxy

-- | 'NetAddr' 'IP' proxy value.
aNetAddrIP ∷ Proxy (NetAddr IP)
aNetAddrIP = Proxy

instance Show a ⇒ Show (NetAddr a) where
  showsPrec p (NetAddr a w) = showParen (p > 10)
                            $ showString "netAddr "
                            . showsPrec 11 a
                            . showString " "
                            . showsPrec 11 w

instance Read Net4Addr where
  readsPrec p = readParen (p > 10) $ \i →
                  [ (netAddr a w, i2)
                  | ("netAddr", i') ← lex i
                  , (a, i1) ← readsPrec 11 i'
                  , (w, i2) ← readsPrec 11 i1 ]

instance Read Net6Addr where
  readsPrec p = readParen (p > 10) $ \i →
                  [ (netAddr a w, i2)
                  | ("netAddr", i') ← lex i
                  , (a, i1) ← readsPrec 11 i'
                  , (w, i2) ← readsPrec 11 i1 ]

instance Read (NetAddr IP) where
  readsPrec p = readParen (p > 10) $ \i →
                  [ (netAddr a w, i2)
                  | ("netAddr", i') ← lex i
                  , (a, i1) ← readsPrec 11 i'
                  , (w, i2) ← readsPrec 11 i1 ]

instance Printable a ⇒ Printable (NetAddr a) where
  print (NetAddr a m) = print a <> P.char7 '/' <> print m
  {-# INLINE print #-}

instance Textual Net4Addr where
  textual = net4Parser
  {-# INLINE textual #-}

instance Textual Net6Addr where
  textual = net6Parser
  {-# INLINE textual #-}

instance Textual (NetAddr IP) where
  textual = netParser
  {-# INLINE textual #-}

instance Serializable a ⇒ Serializable (NetAddr a) where
  put (NetAddr a w) = S.put a <> S.put w
  {-# INLINE put #-}

instance SizedSerializable a ⇒ SizedSerializable (NetAddr a) where
  size _ = S.size (Proxy ∷ Proxy a) + 1
  {-# INLINE size #-}

instance Deserializable Net4Addr where
  get = getNetAddr <?> "IPv4 network address"
  {-# INLINE get #-}

instance Deserializable Net6Addr where
  get = getNetAddr <?> "IPv6 network address"
  {-# INLINE get #-}

instance Deserializable (NetAddr IP) where
  get = getNetAddr <?> "IP network address"
  {-# INLINE get #-}

instance IsNetAddr Net4Addr where
  type NetHost Net4Addr = IP4
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = (a `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINE netHostIx #-}
  netPrefix (NetAddr a w) = (a `shiftR` l) `shiftL` l
    where l = 32 - fromIntegral w
  {-# INLINE netPrefix #-}
  netMask (NetAddr _ w) = IP4 $ (allOnes `shiftR` l) `shiftL` l
    where l = 32 - fromIntegral w
  {-# INLINE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` 32)
  {-# INLINE netAddr #-}
  inNetwork h (NetAddr a w) = h `shiftR` l == a `shiftR` l
    where l = 32 - fromIntegral w
  {-# INLINE inNetwork #-}

instance IsNetAddr Net6Addr where
  type NetHost Net6Addr = IP6
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = (a `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINE netHostIx #-}
  netPrefix (NetAddr a w) = (a `shiftR` l) `shiftL` l
    where l = 128 - fromIntegral w
  {-# INLINE netPrefix #-}
  netMask (NetAddr _ w) = IP6 $ (allOnes `shiftR` l) `shiftL` l
    where l = 128 - fromIntegral w
  {-# INLINE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` 128)
  {-# INLINE netAddr #-}
  inNetwork h (NetAddr a w) = h `shiftR` l == a `shiftR` l
    where l = 128 - fromIntegral w
  {-# INLINE inNetwork #-}

instance IsNetAddr (NetAddr IP) where
  type NetHost (NetAddr IP) = IP
  netHost (NetAddr a _) = a
  {-# INLINE netHost #-}
  netHostIx (NetAddr a w) = case a of
      IPv4 a1 → IPv4 $ (a1 `shiftL` l) `shiftR` l
      IPv6 a1 → IPv6 $ (a1 `shiftL` l) `shiftR` l
    where l = fromIntegral w
  {-# INLINABLE netHostIx #-}
  netPrefix (NetAddr a w) = case a of
    IPv4 a1 → IPv4 $ (a1 `shiftR` l) `shiftL` l
      where l = 32 - fromIntegral w
    IPv6 a1 → IPv6 $ (a1 `shiftR` l) `shiftL` l
      where l = 128 - fromIntegral w
  {-# INLINABLE netPrefix #-}
  netMask (NetAddr a w) = case a of
    IPv4 _ → IPv4 $ IP4 $ (allOnes `shiftR` l) `shiftL` l
      where l = 32 - fromIntegral w
    IPv6 _ → IPv6 $ IP6 $ (allOnes `shiftR` l) `shiftL` l
      where l = 128 - fromIntegral w
  {-# INLINABLE netMask #-}
  netLength (NetAddr _ w) = w
  {-# INLINE netLength #-}
  netAddr a w = NetAddr a (w `min` m)
    where m = case a of
                IPv4 _ → 32
                IPv6 _ → 128
  {-# INLINABLE netAddr #-}
  inNetwork (IPv4 h) (NetAddr (IPv4 a) w) = h `shiftR` l == a `shiftR` l
    where l = 32 - fromIntegral w
  inNetwork (IPv6 h) (NetAddr (IPv6 a) w) = h `shiftR` l == a `shiftR` l
    where l = 128 - fromIntegral w
  inNetwork _ _ = False

-- | An alias for 'netAddr'.
net4Addr ∷ IP4 → Word8 → Net4Addr
net4Addr = netAddr
{-# INLINE net4Addr #-}

-- | An alias for 'netAddr'.
net6Addr ∷ IP6 → Word8 → Net6Addr
net6Addr = netAddr
{-# INLINE net6Addr #-}

-- | Pull 'IP46' from a 'NetAddr'.
toNetAddr46 ∷ NetAddr IP → IP46 (NetAddr IP4) (NetAddr IP6)
toNetAddr46 (NetAddr (IPv4 a) w) = IPv4 (NetAddr a w)
toNetAddr46 (NetAddr (IPv6 a) w) = IPv6 (NetAddr a w)
{-# INLINABLE toNetAddr46 #-}

-- | Push 'IP46' into a 'NetAddr'.
fromNetAddr46 ∷ IP46 (NetAddr IP4) (NetAddr IP6) → NetAddr IP
fromNetAddr46 (IPv4 (NetAddr a w)) = NetAddr (IPv4 a) w
fromNetAddr46 (IPv6 (NetAddr a w)) = NetAddr (IPv6 a) w
{-# INLINABLE fromNetAddr46 #-}

-- | Print network address (CIDR notation).
printNetAddr ∷ (IsNetAddr n, Printable (NetHost n), Printer p) ⇒ n → p
printNetAddr n = print (netHost n) <> P.char7 '/' <> print (netLength n)
{-# INLINABLE printNetAddr #-}

ip4Mask ∷ (CharParsing μ, Monad μ) ⇒ μ Word8
ip4Mask = (<?> "network prefix length") $ do
  m ← nncUpTo Decimal 2
  when (m > 32) $ D.unexpected "out of bounds"
  return m

-- | IPv4 network address parser (CIDR notation).
net4Parser ∷ (CharParsing μ, Monad μ, IsNetAddr n, NetHost n ~ IP4) ⇒ μ n
net4Parser  =  netAddr <$> textual <*> (PC.char '/' *> ip4Mask)
           <?> "IPv4 network address"
{-# INLINE net4Parser #-}

ip6Mask ∷ (CharParsing μ, Monad μ) ⇒ μ Word8
ip6Mask = (<?> "network prefix length") $ do
  m ← nncUpTo Decimal 3
  when (m > (128 ∷ Int)) $ D.unexpected "out of bounds"
  return $ fromIntegral m

-- | IPv6 network address parser (CIDR notation).
net6Parser ∷ (CharParsing μ, Monad μ, IsNetAddr n, NetHost n ~ IP6) ⇒ μ n
net6Parser  =  netAddr <$> textual <*> (PC.char '/' *> ip6Mask)
           <?> "IPv6 network address"
{-# INLINE net6Parser #-}

-- | IP network address parser (CIDR notation).
netParser ∷ (IsNetAddr n, NetHost n ~ IP, CharParsing μ, Monad μ) ⇒ μ n
netParser = (<?> "IP network address") $ do
    a ← textual
    void $ PC.char '/'
    m ← mask a
    return $ netAddr a m
  where
    mask (IPv4 _) = ip4Mask
    mask (IPv6 _) = ip6Mask

-- | Serialize a network address (host address followed by 8-bit
--   prefix length).
putNetAddr ∷ (IsNetAddr n, Serializable (NetHost n), Serializer s) ⇒ n → s
putNetAddr n = S.put (netHost n) <> S.put (netPrefix n)
{-# INLINE putNetAddr #-}

-- | Deserialize a network address (host address followed by 8-bit
--   prefix length).
getNetAddr ∷ (IsNetAddr n, Deserializable (NetHost n), Deserializer μ) ⇒ μ n
getNetAddr = netAddr <$> D.get <*> D.get
{-# INLINE getNetAddr #-}

-- | Port number.
newtype InetPort = InetPort { unInetPort ∷ Word16 }
  deriving (Typeable, Data, Eq, Ord, Bounded, Enum, Ix, Num, Real, Integral,
            Bits, Hashable, Printable)

-- | 'InetPort' proxy value.
anInetPort ∷ Proxy InetPort
anInetPort = Proxy

instance Show InetPort where
  showsPrec p (InetPort w) = showsPrec p w
  {-# INLINE showsPrec #-}

instance Read InetPort where
  readsPrec p = fmap (\(w, s) → (InetPort w, s)) . readsPrec p
  {-# INLINE readsPrec #-}

instance Textual InetPort where
  textual = InetPort <$> nncBounded Decimal <?> "port number"

instance Storable InetPort where
  alignment _ = alignment (undefined ∷ Word16)
  sizeOf _    = 2
  peek p      = InetPort . fromBigEndian <$> peek (castPtr p)
  poke p      = poke (castPtr p) . toBigEndian . unInetPort

instance Serializable InetPort where
  put = S.word16B . unInetPort
  {-# INLINE put #-}

instance SizedSerializable InetPort where
  size _ = 2
  {-# INLINE size #-}

instance Deserializable InetPort where
  get = InetPort <$> D.word16B <?> "port number"
  {-# INLINE get #-}

-- | Socket address: host address + port number.
data InetAddr a = InetAddr { inetHost ∷ a
                           , inetPort ∷ {-# UNPACK #-} !InetPort
                           } deriving (Eq, Ord, Show, Read)


#if !MIN_VERSION_base(4,10,0)
deriving instance Typeable1 InetAddr
#endif
deriving instance Data a ⇒ Data (InetAddr a)

-- | IPv4 socket address.
type Inet4Addr = InetAddr IP4

-- | IPv6 socket address.
type Inet6Addr = InetAddr IP6

-- | 'InetAddr' proxy value.
anInetAddr ∷ Proxy InetAddr
anInetAddr = Proxy

-- | 'InetAddr' /a/ proxy value.
anInetAddrOf ∷ Proxy a → Proxy (InetAddr a)
anInetAddrOf _ = Proxy

-- | 'Inet4Addr' proxy value.
anInet4Addr ∷ Proxy Inet4Addr
anInet4Addr = Proxy

-- | 'Inet6Addr' proxy value.
anInet6Addr ∷ Proxy Inet6Addr
anInet6Addr = Proxy

-- | 'InetAddr' 'IP' proxy value.
anInetAddrIP ∷ Proxy (InetAddr IP)
anInetAddrIP = Proxy

instance Functor InetAddr where
  fmap f (InetAddr a p) = InetAddr (f a) p
  {-# INLINE fmap #-}

instance Printable Inet4Addr where
  print (InetAddr a p) = print a <> P.char7 ':' <> print p
  {-# INLINABLE print #-}

instance Printable Inet6Addr where
  print (InetAddr a p) = P.brackets (print a) <> P.char7 ':' <> print p
  {-# INLINABLE print #-}

instance Printable (InetAddr IP) where
  print (InetAddr (IPv4 a) p) = print (InetAddr a p)
  print (InetAddr (IPv6 a) p) = print (InetAddr a p)
  {-# INLINABLE print #-}

instance Textual Inet4Addr where
  textual  =  InetAddr <$> textual <*> (PC.char ':' *> textual)
          <?> "IPv4 socket address"
  {-# INLINE textual #-}

instance Textual Inet6Addr where
  textual  =  InetAddr <$> (PC.char '[' *> textual <* PC.char ']')
                      <*> (PC.char ':' *> textual)
          <?> "IPv6 socket address"
  {-# INLINE textual #-}

instance Textual (InetAddr IP) where
  textual  =  InetAddr
         <$> (optional (PC.char '[') >>= \case
                Nothing → IPv4 <$> textual
                Just _  → IPv6 <$> textual <* PC.char ']')
         <*> (PC.char ':' *> textual)
         <?> "IP socket address"
  {-# INLINABLE textual #-}

instance Hashable a ⇒ Hashable (InetAddr a) where
#if MIN_VERSION_hashable(1,2,0)
  hashWithSalt s (InetAddr a p) = s `hashWithSalt` a `hashWithSalt` p
  {-# INLINE hashWithSalt #-}
#else
  hash (InetAddr a p) = hash a `combine` hash p
  {-# INLINE hash #-}
#endif

instance Serializable a ⇒ Serializable (InetAddr a) where
  put (InetAddr a p) = S.put a <> S.put p
  {-# INLINE put #-}

instance SizedSerializable a ⇒ SizedSerializable (InetAddr a) where
  size _ = S.size (Proxy ∷ Proxy a) + 2
  {-# INLINE size #-}

instance Deserializable a ⇒ Deserializable (InetAddr a) where
  get = InetAddr <$> (D.get <?> "host address") <*> D.get <?> "socket address"
  {-# INLINE get #-}

-- | Pull 'IP46' from an 'InetAddr'.
toInetAddr46 ∷ InetAddr IP → IP46 (InetAddr IP4) (InetAddr IP6)
toInetAddr46 (InetAddr (IPv4 a) w) = IPv4 (InetAddr a w)
toInetAddr46 (InetAddr (IPv6 a) w) = IPv6 (InetAddr a w)
{-# INLINABLE toInetAddr46 #-}

-- | Push 'IP46' into an 'InetAddr'.
fromInetAddr46 ∷ IP46 (InetAddr IP4) (InetAddr IP6) → InetAddr IP
fromInetAddr46 (IPv4 (InetAddr a w)) = InetAddr (IPv4 a) w
fromInetAddr46 (IPv6 (InetAddr a w)) = InetAddr (IPv6 a) w
{-# INLINABLE fromInetAddr46 #-}
