{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE CApiFFI                    #-}
{-# LANGUAGE DeriveFoldable             #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

-- |
-- Copyright: © 2017 Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Internal module
module Network.DNS.Message where

import qualified Data.ByteString.Base16 as B16

import qualified Data.ByteString        as BS
import qualified Data.ByteString.Lazy   as BSL
import           Data.Function
import           Data.List              (groupBy)
import           Data.String
import           Numeric                (showHex)
import           Prelude

import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import           Data.Bits
import           Data.Map               (Map)
import qualified Data.Map               as Map
import           Data.Set               (Set)
import qualified Data.Set               as Set

import           Compat

-- | An IPv6 address
--
-- The IP address is represented in network order,
-- i.e. @2606:2800:220:1:248:1893:25c8:1946@ is
-- represented as @(IPv6 0x2606280002200001 0x248189325c81946)@.
data IPv6 = IPv6 !Word64 !Word64
          deriving (Eq,Ord,Read)

instance Show IPv6 where
    showsPrec p (IPv6 hi lo) = showParen (p >= 11) (showString "IPv6 0x" . showHex hi . showString " 0x" . showHex lo)

instance Binary IPv6 where
    put (IPv6 hi lo) = putWord64be hi >> putWord64be lo
    get              = IPv6 <$> getWord64be <*> getWord64be

-- | An IPv4 address
--
-- The IP address is represented in network order, i.e. @127.0.0.1@ is
-- represented as @(IPv4 0x7f000001)@.
data IPv4 = IPv4 !Word32
          deriving (Eq,Ord,Read)

instance Show IPv4 where
    showsPrec p (IPv4 n) = showParen (p >= 11) (showString "IPv4 0x" . showHex n)

instance Binary IPv4 where
    put (IPv4 w) = putWord32be w
    get = IPv4 <$> getWord32be

-- | @\<domain-name\>@ as per [RFC 1035, section 3.3](https://tools.ietf.org/html/rfc1035#section-3.3).
--
-- A domain-name represented as a series of labels separated by dots.
--
-- See also 'Labels' for list-based representation.
--
-- __NOTE__: The 'Labels' type is able to properly represent domain
-- names whose components contain dots which the 'Name' representation
-- cannot.
newtype Name = Name BS.ByteString
             deriving (Read,Show,Eq,Ord)

-- | @\<character-string\>@ as per [RFC 1035, section 3.3](https://tools.ietf.org/html/rfc1035#section-3.3).
--
-- A sequence of up to 255 octets
--
-- The limit of 255 octets is caused by the encoding which uses by a
-- prefixed octet denoting the length.
newtype CharStr = CharStr BS.ByteString
                deriving (Eq,Ord,IsString)

instance Show CharStr where
    showsPrec p (CharStr bs) = showsPrec p bs

instance Read CharStr where
    readsPrec p = map (\(x,y) -> (CharStr x,y)) <$> readsPrec p

instance Binary CharStr where
    put (CharStr bs)
      | BS.length bs > 0xff = error "putString: string too long"
      | otherwise = do
            putWord8 (fromIntegral $ BS.length bs)
            putByteString bs
    get = do
        len' <- getWord8
        CharStr <$> getByteString (fromIntegral len')

{- Resource records

 -- https://en.wikipedia.org/wiki/List_of_DNS_record_types

 RFC 1035

 A        1     a host address
 NS       2     an authoritative name server
 CNAME    5     the canonical name for an alias
 SOA      6     marks the start of a zone of authority
 PTR      12    a domain name pointer
 MX       15    mail exchange
 TXT      16    text strings

 RFC 3596

 AAAA     28    IPv6

 RFC 2782

 SRV      33    Location of services

 ----

 RFC3597            Handling of Unknown DNS Resource Record (RR) Types

-}

-- | Represents a DNS message as per [RFC 1035](https://tools.ietf.org/html/rfc1035)
data Msg l
    = Msg
      { msgHeader           :: !MsgHeader
      , msgQD               :: [MsgQuestion l]
      , msgAN, msgNS, msgAR :: [MsgRR l]
      } deriving (Read,Show,Functor,Foldable,Traversable)

-- | DNS message header section as per [RFC 1035, section 4.1.1](https://tools.ietf.org/html/rfc1035#section-4.1.1)
data MsgHeader
    = MsgHeader
      { mhId      :: !Word16

      , mhFlags   :: !MsgHeaderFlags

      , mhQDCount :: !Word16
      , mhANCount :: !Word16
      , mhNSCount :: !Word16
      , mhARCount :: !Word16
      } deriving (Read,Show)

-- | DNS message header section as per [RFC 1035, section 4.1.2](https://tools.ietf.org/html/rfc1035#section-4.1.2)
data MsgQuestion l
    = MsgQuestion !l !Type !Class
    deriving (Eq,Read,Show,Functor,Foldable,Traversable)

-- | DNS message header flags as per [RFC 1035, section 4.1.1](https://tools.ietf.org/html/rfc1035#section-4.1.1)
data MsgHeaderFlags
    = MsgHeaderFlags
      { mhQR     :: !QR
      , mhOpcode :: !Word8 -- actually Word4
      , mhAA     :: !Bool
      , mhTC     :: !Bool
      , mhRD     :: !Bool
      , mhRA     :: !Bool
      , mhZ      :: !Bool -- reserved/unused bit
      , mhAD     :: !Bool -- RFC4035
      , mhCD     :: !Bool -- RFC4035
      , mhRCode  :: !Word8 -- Word4
      } deriving (Read,Show)

-- | DNS resource record section as per [RFC 1035, section 4.1.3](https://tools.ietf.org/html/rfc1035#section-4.1.3)
data MsgRR l
    = MsgRR
      { rrName  :: !l
      , rrClass :: !Class
      , rrTTL   :: !TTL
      , rrData  :: !(RData l)
      } deriving (Eq,Read,Show,Functor,Foldable,Traversable)

-- | DNS resource record data (see also 'MsgRR' and 'TypeSym')
data RData l
    = RDataA      !IPv4
    | RDataAAAA   !IPv6
    | RDataCNAME  !l
    | RDataPTR    !l
    | RDataHINFO  !CharStr !CharStr
    | RDataNS     !l
    | RDataMX     !Word16 !l
    | RDataTXT    ![CharStr]
    | RDataSPF    ![CharStr]
    | RDataSOA    !l !l !Word32 !Word32 !Word32 !Word32 !Word32
    | RDataSRV    !(SRV l)

    -- RFC 1183
    | RDataAFSDB  !Word16 !l

    -- RFC 2915
    | RDataNAPTR  !Word16 !Word16 !CharStr !CharStr !CharStr !l

    -- RFC 7553
    | RDataURI    !Word16 !Word16 !BS.ByteString

    -- RFC 4034
    | RDataRRSIG  !Word16 !Word8 !Word8 !Word32 !Word32 !Word32 !Word16 !l !BS.ByteString
    | RDataDNSKEY !Word16 !Word8 !Word8 !BS.ByteString
    | RDataDS     !Word16 !Word8 !Word8 !BS.ByteString
    | RDataNSEC   !l !(Set Type)

    -- RFC 4255
    | RDataSSHFP  !Word8 !Word8 !BS.ByteString

    -- RFC 5155
    | RDataNSEC3PARAM !Word8 !Word8 !Word16 !CharStr
    | RDataNSEC3      !Word8 !Word8 !Word16 !CharStr  !CharStr !(Set Type)

    -- RFC 6844
    | RDataCAA !Word8 !CharStr !BS.ByteString

    -- pseudo-record
    | RDataOPT !BS.ByteString -- FIXME

    -- unknown/unsupported
    | RData    !Type !BS.ByteString -- ^ Unknown/undecoded resource record type
    deriving (Eq,Read,Show,Functor,Foldable,Traversable)


-- | @SRV@ Record data as per [RFC 2782](https://tools.ietf.org/html/rfc2782)
data SRV l = SRV { srvPriority :: !Word16
                 , srvWeight   :: !Word16
                 , srvPort     :: !Word16
                 , srvTarget   :: !l
                 } deriving (Eq,Read,Show,Functor,Foldable,Traversable)

----------------------------------------------------------------------------

decodeMessage' :: BS.ByteString -> Maybe (Msg Labels)
decodeMessage' bs = do
    (rest, _, v) <- either handleParseFail Just $
                    decodeOrFail (fromStrict bs)

    -- don't allow trailing garbage
    guard (BSL.null rest)

    let ofss = Set.fromList $ mapMaybe labelsPtr (toList v)
    ofsmap <- retrieveLabelPtrs bs ofss

    traverse (resolveLabelPtr ofsmap) v
  where
    -- handleParseFail _ = Nothing
    handleParseFail (rest, n, e) = error $ show (e, n, BSL.length rest, BS.length bs) ++ "\n" ++ show (B16.encode $ toStrict rest)

-- | Decode a raw DNS message (query or response)
--
-- Returns 'Nothing' on decoding failures.
decodeMessage :: IsLabels n => BS.ByteString -> Maybe (Msg n)
decodeMessage = fmap (fmap fromLabels) . decodeMessage'

encodeMessage' :: Msg Labels -> BS.ByteString
encodeMessage' m = toStrict $ encode (fmap labels2labelsPtr m)

-- | Construct a raw DNS message (query or response)
--
-- May return 'Nothing' in input parameters are detected to be invalid.
encodeMessage :: IsLabels n => Msg n -> Maybe BS.ByteString
encodeMessage m = encodeMessage' <$> traverse toLabels m


instance Binary l => Binary (Msg l) where
    get = do
        hdr@MsgHeader{..} <- get

        Msg hdr <$> replicateM (fromIntegral mhQDCount) get
                <*> replicateM (fromIntegral mhANCount) get
                <*> replicateM (fromIntegral mhNSCount) get
                <*> replicateM (fromIntegral mhARCount) get

    put (Msg hdr qds ans nss ars) = do
        put hdr
        mapM_ put qds
        mapM_ put ans
        mapM_ put nss
        mapM_ put ars

instance Binary MsgHeader where
    get = MsgHeader <$> getWord16be
                    <*> get
                    <*> getWord16be
                    <*> getWord16be
                    <*> getWord16be
                    <*> getWord16be

    put (MsgHeader{..}) = do
        putWord16be mhId
        put mhFlags
        putWord16be mhQDCount
        putWord16be mhANCount
        putWord16be mhNSCount
        putWord16be mhARCount

instance Binary MsgHeaderFlags where
    put = putWord16be . encodeFlags
    get = decodeFlags <$> getWord16be

-- | Decode message header flag field
--
-- >  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
-- >  |QR|   Opcode  |AA|TC|RD|RA|??|AD|CD|   RCODE   |
-- >  +--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+--+
--
decodeFlags :: Word16 -> MsgHeaderFlags
decodeFlags w = MsgHeaderFlags{..}
  where
    mhQR      = if testBit w 15 then IsResponse else IsQuery
    mhOpcode  = shiftR'   11 .&. 0xf
    mhAA      = testBit w 10
    mhTC      = testBit w  9
    mhRD      = testBit w  8
    mhRA      = testBit w  7
    mhZ       = testBit w  6
    mhAD      = testBit w  5
    mhCD      = testBit w  4
    mhRCode   = fromIntegral w .&. 0xf

    shiftR' = fromIntegral . shiftR w

encodeFlags :: MsgHeaderFlags -> Word16
encodeFlags MsgHeaderFlags{..} =
    (case mhQR of
        IsResponse -> bit 15
        IsQuery    -> 0) .|.
    (fromIntegral mhOpcode `shiftL` 11) .|.
    (if mhAA then bit 10 else 0) .|.
    (if mhTC then bit  9 else 0) .|.
    (if mhRD then bit  8 else 0) .|.
    (if mhRA then bit  7 else 0) .|.
    (if mhZ  then bit  6 else 0) .|.
    (if mhAD then bit  5 else 0) .|.
    (if mhCD then bit  4 else 0) .|.
    (fromIntegral mhRCode)

-- | Encodes whether message is a query or a response
--
-- @since 0.1.1.0
data QR = IsQuery | IsResponse
        deriving (Eq,Read,Show)

----------------------------------------------------------------------------

infixr 5 :.:

-- | A DNS Label
--
-- Must be non-empty and at most 63 octets.
type Label = BS.ByteString

-- | A @<domain-name>@ as per [RFC 1035, section 3.3](https://tools.ietf.org/html/rfc1035#section-3.3) expressed as list of 'Label's.
--
-- See also 'Name'
data Labels = !Label :.: !Labels | Root
            deriving (Read,Show,Eq,Ord)

labelsToList :: Labels -> [Label]
labelsToList (x :.: xs) = x : labelsToList xs
labelsToList Root       = [""]

-- | Types that represent @<domain-name>@ as per [RFC 1035, section 3.3](https://tools.ietf.org/html/rfc1035#section-3.3) and can be converted to and from 'Labels'.
class IsLabels s where
  toLabels   :: s -> Maybe Labels
  fromLabels :: Labels -> s

instance IsLabels Labels where
  fromLabels = id

  toLabels ls
    | all isLabelValid (init (labelsToList ls)) = Just ls
    | otherwise = Nothing
    where
      isLabelValid l = not (BS.null l) && BS.length l < 0x40

instance IsLabels Name where
  fromLabels = labels2name
  toLabels   = name2labels

toName :: IsLabels n => n -> Maybe Name
toName = fmap fromLabels . toLabels

name2labels :: Name -> Maybe Labels
name2labels (Name n)
  | all (\l -> not (BS.null l) && BS.length l < 0x40) n' = Just $! foldr (:.:) Root n'
  | otherwise = Nothing
  where
    n' | BS.isSuffixOf "." n = BS.split 0x2e (BS.init n)
       | otherwise           = BS.split 0x2e n

labels2name :: Labels -> Name
labels2name Root = Name "."
labels2name ls   = Name (BS.intercalate "." $ labelsToList ls)

-- | IOW, a domain-name
--
-- May contain pointers
--
-- Can be resolved into a 'Labels' without label ptrs.
data LabelsPtr = Label !Label !LabelsPtr -- ^ See RC2181: a label must be between 1-63 octets; can be arbitrary binary data
               | LPtr  !Word16
               | LNul
               deriving (Eq,Read,Show)

labels2labelsPtr :: Labels -> LabelsPtr
labels2labelsPtr Root         = LNul
labels2labelsPtr (l :.: rest) = Label l (labels2labelsPtr rest)

instance Binary LabelsPtr where
    get = go []
      where
        go acc = do
            l0 <- getLabel
            case l0 of
              Right bs | BS.null bs -> pure (foldr Label LNul $ reverse acc)
                       | otherwise  -> go (bs:acc)
              Left ofs              -> pure (foldr Label (LPtr ofs) $ reverse acc)

        getLabel :: Get (Either Word16 BS.ByteString)
        getLabel = do
            len <- getWord8

            if len >= 0x40
             then do
                when (len .&. 0xc0 /= 0xc0) $ fail ("invalid length octet " ++ show len)
                ofs <- fromIntegral <$> getWord8
                pure $ Left $ (fromIntegral (len .&. 0x3f) `shiftL` 8) .|. ofs
             else Right <$> getByteString (fromIntegral len)

    put LNul = putWord8 0
    put (Label l next)
      | BS.length l < 1 || BS.length l >= 0x40 = error "put (Label {}): invalid label size"
      | otherwise = do
            putWord8 (fromIntegral (BS.length l))
            putByteString l
            put next
    put (LPtr ofs)
      | ofs < 0x4000 = putWord16be (0xc000 .|. ofs)
      | otherwise  = error "put (LPtr {}): invalid offset"

-- | Compute serialised size of 'LabelsPtr'
labelsSize :: LabelsPtr -> Word16
labelsSize = fromIntegral . go 0
  where
    go n (LPtr _)        = n+2
    go n  LNul           = n+1
    go n (Label bs rest) = go (n + 1 + BS.length bs) rest

-- | Extract pointer-offset from 'LabelsPtr' (if it exists)
labelsPtr :: LabelsPtr -> Maybe Word16
labelsPtr (Label _ ls) = labelsPtr ls
labelsPtr LNul         = Nothing
labelsPtr (LPtr ofs)   = Just ofs

----------------------------------------------------------------------------

instance Binary l => Binary (MsgQuestion l) where
    get = MsgQuestion <$> get <*> get <*> get
    put (MsgQuestion l qt cls) = put l >> put qt >> put cls


instance Binary l => Binary (MsgRR l) where
    get = do
        rrName  <- get
        rrType  <- get
        rrClass <- get
        rrTTL   <- get
        rrData  <- getRData rrType
        pure (MsgRR {..})

    put (MsgRR{..}) = do
        put         rrName
        put         (either id typeFromSym $ rdType rrData)
        put         rrClass
        put         rrTTL
        putRData    rrData

getRData :: Binary l => Type -> Get (RData l)
getRData qt = do
    len     <- fromIntegral <$> getWord16be

    let unknownRdata = RData qt <$> getByteString len

        getByteStringRest = consumeRestWith getByteString

        consumeRestWith act = do
            curofs <- fromIntegral <$> bytesRead
            act (len - curofs)

    isolate len $
      case typeToSym qt of
        Nothing -> unknownRdata
        Just ts -> case ts of
          TypeA      -> RDataA      <$> get

          TypeAFSDB  -> RDataAFSDB  <$> getWord16be
                                    <*> get

          TypeNS     -> RDataNS     <$> get

          TypeCNAME  -> RDataCNAME  <$> get

          TypeSOA    -> RDataSOA    <$> get
                                    <*> get
                                    <*> getWord32be
                                    <*> getWord32be
                                    <*> getWord32be
                                    <*> getWord32be
                                    <*> getWord32be

          TypePTR    -> RDataPTR    <$> get

          TypeHINFO  -> RDataHINFO  <$> get
                                    <*> get

          TypeMX     -> RDataMX     <$> getWord16be
                                    <*> get

          TypeTXT    -> RDataTXT    <$> getUntilEmpty
          TypeSPF    -> RDataSPF    <$> getUntilEmpty

          TypeAAAA   -> RDataAAAA   <$> get

          TypeSRV    -> RDataSRV    <$> get

          TypeNAPTR  -> RDataNAPTR  <$> getWord16be -- order
                                    <*> getWord16be --preference
                                    <*> get -- flags
                                    <*> get -- services
                                    <*> get -- regexp
                                    <*> get -- replacement

          TypeRRSIG  -> RDataRRSIG  <$> getWord16be
                                    <*> getWord8
                                    <*> getWord8
                                    <*> getWord32be
                                    <*> getWord32be
                                    <*> getWord32be
                                    <*> getWord16be
                                    <*> get -- uncompressed
                                    <*> getByteStringRest

          TypeDNSKEY -> RDataDNSKEY <$> getWord16be
                                    <*> getWord8
                                    <*> getWord8
                                    <*> getByteString (len - 4)

          TypeDS     -> RDataDS     <$> getWord16be
                                    <*> getWord8
                                    <*> getWord8
                                    <*> getByteString (len - 4)

          TypeNSEC   -> RDataNSEC   <$> get
                                    <*> decodeNsecTypeMap

          TypeURI    -> RDataURI    <$> getWord16be -- prio
                                    <*> getWord16be -- weight
                                    <*> getByteString (len - 4)

          TypeSSHFP  -> RDataSSHFP  <$> getWord8
                                    <*> getWord8
                                    <*> getByteString (len - 2)

          TypeNSEC3PARAM -> RDataNSEC3PARAM <$> getWord8
                                            <*> getWord8
                                            <*> getWord16be
                                            <*> get -- salt

          TypeNSEC3      -> RDataNSEC3      <$> getWord8
                                            <*> getWord8
                                            <*> getWord16be
                                            <*> get -- salt
                                            <*> get -- next hashed owner name
                                            <*> decodeNsecTypeMap

          TypeCAA        -> RDataCAA        <$> getWord8 -- flags
                                            <*> get -- tag -- TODO: must be non-empty
                                            <*> getByteStringRest

          TypeOPT -> RDataOPT <$> getByteString len -- FIXME

          TypeANY    -> unknownRdata -- shouldn't happen

putRData :: Binary l => RData l -> Put
putRData rd = do
    let rdata = runPut (putRData' rd)
        rdataLen = BSL.length rdata

    unless (rdataLen < 0x10000) $
        error "rdata too large"

    putWord16be (fromIntegral rdataLen)
    putLazyByteString rdata

putRData' :: Binary l => RData l -> Put
putRData' rd = case rd of
  RDataA ip4 -> put ip4
  RDataAAAA ip6 -> put ip6
  RDataCNAME cname -> put cname
  RDataOPT d -> putByteString d
  RDataMX prio l -> putWord16be prio >> put l
  RDataSOA l1 l2 w1 w2 w3 w4 w5 -> do
      put l1
      put l2
      putWord32be w1
      putWord32be w2
      putWord32be w3
      putWord32be w4
      putWord32be w5

  RDataPTR l -> put l
  RDataNS  l -> put l
  RDataTXT ss -> mapM_ put ss
  RDataSPF ss -> mapM_ put ss
  RDataSRV srv -> put srv

  RDataAFSDB w l -> putWord16be w >> put l

  RDataHINFO s1 s2 -> put s1 >> put s2

  RDataRRSIG w1 w2 w3 w4 w5 w6 w7 l s -> do
      putWord16be w1
      putWord8    w2
      putWord8    w3
      putWord32be w4
      putWord32be w5
      putWord32be w6
      putWord16be w7
      put l
      putByteString s

  RDataDNSKEY w1 w2 w3 s -> do
      putWord16be w1
      putWord8    w2
      putWord8    w3
      putByteString s

  RDataNSEC3PARAM w1 w2 w3 s -> do
      putWord8 w1
      putWord8 w2
      putWord16be w3
      put s

  RDataNSEC3 w1 w2 w3 s1 s2 tm -> do
      putWord8 w1
      putWord8 w2
      putWord16be w3
      put s1
      put s2
      encodeNsecTypeMap tm

  RDataCAA fl s1 s2 -> do
      putWord8 fl
      put s1
      putByteString s2

  RDataURI w1 w2 s -> do
      putWord16be w1
      putWord16be w2
      putByteString s

  RDataDS w1 w2 w3 s -> do
      putWord16be w1
      putWord8 w2
      putWord8 w3
      putByteString s

  RDataNSEC l tm -> do
      put l
      encodeNsecTypeMap tm

  RDataNAPTR w1 w2 s1 s2 s3 l -> do
      putWord16be w1
      putWord16be w2
      put s1
      put s2
      put s3
      put l

  RDataSSHFP w1 w2 s -> do
      putWord8 w1
      putWord8 w2
      putByteString s

  RData _ raw -> putByteString raw

  -- _ -> error ("putRData: " ++ show rd)


instance Binary l => Binary (SRV l) where
    get = SRV <$> getWord16be
              <*> getWord16be
              <*> getWord16be
              <*> get

    put (SRV w1 w2 w3 l) = do
      putWord16be w1
      putWord16be w2
      putWord16be w3
      put l

{- NSEC type-bitmap example:

 A NS SOA TXT AAAA RRSIG NSEC DNSKEY

'00 07 62 00 80 08 00 03 80'
'00000000 00000111 01100010 00000000 10000000 00001000 00000000 00000011 10000000'
 Win=#0    len=7         ^{SOA}      ^{TXT}       ^{AAAA}                ^{DNSKEY}
                    ^^{A,NS}                                          ^^{RRSIG,NSEC}
-}

decodeNsecTypeMap :: Get (Set Type)
decodeNsecTypeMap = do
    r <- concat <$> untilEmptyWith decode1
    -- TODO: enforce uniqueness
    pure (Set.fromList r)
  where
    -- decode single window
    decode1 = do
        wi <- getWord8
        l  <- getWord8
        unless (0 < l && l <= 32) $
            fail "invalid bitmap length"

        bmap <- getByteString (fromIntegral l)

        let winofs = (fromIntegral wi)*0x100 :: Int
            lst = [ Type (fromIntegral (winofs+j*8+7-i))
                  | (j,x) <- zip [0..] (BS.unpack bmap)
                  , i <- [7,6..0]
                  , testBit x i ]

        pure lst

encodeNsecTypeMap :: Set Type -> Put
encodeNsecTypeMap bmap = do
    when (Set.null bmap) $ error "invalid empty type-map"
    -- when (Set.member 0 bmap) $ fail "invalid TYPE0 set in type-map"
    -- TODO: verify that Meta-TYPES and QTYPEs aren't contained in bmap

    forM_ (Map.toList bmap') $ \(wi, tm) -> do
        putWord8 wi
        put (CharStr $ BS.pack tm)
  where
    bmap' = fmap set2bitmap . splitToBlocks $ Set.map (\(Type w)->w) bmap

set2bitmap :: Set Word8 -> [Word8]
set2bitmap = go 0 0 . Set.toList
  where
    go _ acc [] = if acc == 0 then [] else [acc]
    go j acc (i:is)
      | j'  > j  = acc : go (j+1) 0 (i:is)
      | j' == j  = go j' (acc .|. bit (7 - fromIntegral i')) is
      | otherwise = error "set2bitmap: the impossible happened"
      where
        (j',i') = i `quotRem` 8

splitToBlocks :: Set Word16 -> Map Word8 (Set Word8)
splitToBlocks js = Map.fromList $ map (\xs -> (fst $ head xs, Set.fromList (map snd xs))) js'
  where
    hi16 :: Word16 -> Word8
    hi16 = fromIntegral . flip shiftR 8

    lo16 :: Word16 -> Word8
    lo16 = fromIntegral . (.&. 0xff)

    js' :: [[(Word8,Word8)]]
    js' = groupBy ((==) `on` fst) (map ((,) <$> hi16 <*> lo16) (Set.toList js))


-- | Resolves/parses label pointer used for label compressing
--
-- Returns 'Nothing' on failure
retrieveLabelPtr :: BS.ByteString -> Word16 -> Maybe LabelsPtr
retrieveLabelPtr msg ofs
    = case decodeOrFail (fromStrict $ BS.drop (fromIntegral ofs) msg) of
        Left _          -> Nothing
        Right (_, _, v) -> Just v

-- | Resolve set of label pointer offsets
--
-- Invariants (/iff/ result is not 'Nothing')
--
--  * all requested offsets will be contained in the result map
--
--  * any offsets contained in the resolved 'Labels' will be part of
--    the result map as well
--
-- NB: No cycle detection is performed, nor are 'Labels' flattened
retrieveLabelPtrs :: BS.ByteString -> Set Word16 -> Maybe (Map Word16 LabelsPtr)
retrieveLabelPtrs msg ofss0 = go =<< lupPtrs1 ofss0
  where
    go :: Map Word16 LabelsPtr -> Maybe (Map Word16 LabelsPtr)
    go m0 = do
        let missingOfss = Set.fromList (mapMaybe labelsPtr (toList m0)) Set.\\ Map.keysSet m0

        if Set.null missingOfss
         then pure m0 -- fix-point reached
         else do
            m1 <- lupPtrs1 missingOfss
            go (Map.union m0 m1)

    -- single lookup step
    lupPtrs1 :: Set Word16 -> Maybe (Map Word16 LabelsPtr)
    lupPtrs1 ofss1 = Map.fromList . zip (toList ofss1) <$> traverse (retrieveLabelPtr msg) (toList ofss1)

-- | Checks for maximum name length (255) and (therefore indirectly) cycle-checking
resolveLabelPtr :: Map Word16 LabelsPtr -> LabelsPtr -> Maybe Labels
resolveLabelPtr ofsmap = go 0 []
  where
    go :: Int -> [BS.ByteString] -> LabelsPtr -> Maybe Labels
    go !n acc (Label x ls) = go (n+1+BS.length x) (x:acc) ls
    go n acc LNul
        | n < 255    = Just $! foldr (:.:) Root (reverse acc)
        | otherwise  = Nothing -- length violation
    go n acc (LPtr ofs)
        | n < 255    = go n acc =<< lup ofs
        | otherwise  = Nothing

    lup :: Word16 -> Maybe LabelsPtr
    lup ofs = Map.lookup ofs ofsmap


{- Resource records

 -- https://en.wikipedia.org/wiki/List_of_DNS_record_types

 RFC 1035

 A        1     a host address
 NS       2     an authoritative name server
 CNAME    5     the canonical name for an alias
 SOA      6     marks the start of a zone of authority
 PTR      12    a domain name pointer
 MX       15    mail exchange
 TXT      16    text strings

 RFC 3596

 AAAA     28    IPv6

 RFC 2782

 SRV      33    Location of services

 ----

 RFC3597            Handling of Unknown DNS Resource Record (RR) Types

-}

-- | Raw DNS record type code
--
-- See also 'TypeSym'
newtype Type = Type Word16
             deriving (Eq,Ord,Read,Show)

instance Binary Type where
    put (Type w) = putWord16be w
    get = Type <$> getWord16be

-- | DNS @CLASS@ code as per [RFC 1035, section 3.2.4](https://tools.ietf.org/html/rfc1035#section-3.2.4)
--
-- The most commonly used value is 'classIN'.
newtype Class = Class Word16
              deriving (Eq,Ord,Read,Show)

-- | The 'Class' constant for @IN@ (Internet)
classIN :: Class
classIN = Class 1

instance Binary Class where
    put (Class w) = putWord16be w
    get = Class <$> getWord16be

-- | Cache time-to-live expressed in seconds
newtype TTL = TTL Int32
            deriving (Eq,Ord,Read,Show)

instance Binary TTL where
    put (TTL i) = putInt32be i
    get = TTL <$> getInt32be

-- http://www.bind9.net/dns-parameters

-- | Symbolic DNS record type
data TypeSym
    = TypeA          -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeAAAA       -- ^ [RFC 3596](https://tools.ietf.org/html/rfc3596)
    | TypeAFSDB      -- ^ [RFC 1183](https://tools.ietf.org/html/rfc1183)
    | TypeANY        -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035) (query)
    | TypeCAA        -- ^ [RFC 6844](https://tools.ietf.org/html/rfc6844)
    | TypeCNAME      -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeDNSKEY     -- ^ [RFC 4034](https://tools.ietf.org/html/rfc4034)
    | TypeDS         -- ^ [RFC 4034](https://tools.ietf.org/html/rfc4034)
    | TypeHINFO      -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeMX         -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeNAPTR      -- ^ [RFC 2915](https://tools.ietf.org/html/rfc2915)
    | TypeNS         -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeNSEC       -- ^ [RFC 4034](https://tools.ietf.org/html/rfc4034)
    | TypeNSEC3      -- ^ [RFC 5155](https://tools.ietf.org/html/rfc5155)
    | TypeNSEC3PARAM -- ^ [RFC 5155](https://tools.ietf.org/html/rfc5155)
    | TypeOPT        -- ^ [RFC 6891](https://tools.ietf.org/html/rfc6891) (meta)
    | TypePTR        -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeRRSIG      -- ^ [RFC 4034](https://tools.ietf.org/html/rfc4034)
    | TypeSOA        -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeSPF        -- ^ [RFC 4408](https://tools.ietf.org/html/rfc4408)
    | TypeSRV        -- ^ [RFC 2782](https://tools.ietf.org/html/rfc2782)
    | TypeSSHFP      -- ^ [RFC 4255](https://tools.ietf.org/html/rfc4255)
    | TypeTXT        -- ^ [RFC 1035](https://tools.ietf.org/html/rfc1035)
    | TypeURI        -- ^ [RFC 7553](https://tools.ietf.org/html/rfc7553)
    deriving (Eq,Ord,Enum,Bounded,Read,Show)

-- | Convert  symbolic 'TypeSym' to numeric 'Type' code
typeFromSym :: TypeSym -> Type
typeFromSym ts = Type $ case ts of
                  TypeA          -> 1
                  TypeNS         -> 2
                  TypeCNAME      -> 5
                  TypeSOA        -> 6
                  TypePTR        -> 12
                  TypeHINFO      -> 13
                  TypeMX         -> 15
                  TypeTXT        -> 16
                  TypeAFSDB      -> 18
                  TypeAAAA       -> 28
                  TypeSRV        -> 33
                  TypeNAPTR      -> 35
                  TypeOPT        -> 41
                  TypeDS         -> 43
                  TypeSSHFP      -> 44
                  TypeRRSIG      -> 46
                  TypeNSEC       -> 47
                  TypeDNSKEY     -> 48
                  TypeNSEC3      -> 50
                  TypeNSEC3PARAM -> 51
                  TypeSPF        -> 99
                  TypeANY        -> 255
                  TypeURI        -> 256
                  TypeCAA        -> 257

-- | Convert 'Type' code to symbolic 'TypeSym'
typeToSym :: Type -> Maybe TypeSym
typeToSym (Type w) = case w of
                  1   -> Just TypeA
                  2   -> Just TypeNS
                  5   -> Just TypeCNAME
                  6   -> Just TypeSOA
                  12  -> Just TypePTR
                  13  -> Just TypeHINFO
                  15  -> Just TypeMX
                  16  -> Just TypeTXT
                  18  -> Just TypeAFSDB
                  28  -> Just TypeAAAA
                  33  -> Just TypeSRV
                  35  -> Just TypeNAPTR
                  41  -> Just TypeOPT
                  43  -> Just TypeDS
                  44  -> Just TypeSSHFP
                  46  -> Just TypeRRSIG
                  47  -> Just TypeNSEC
                  48  -> Just TypeDNSKEY
                  50  -> Just TypeNSEC3
                  51  -> Just TypeNSEC3PARAM
                  99  -> Just TypeSPF
                  255 -> Just TypeANY
                  256 -> Just TypeURI
                  257 -> Just TypeCAA
                  _   -> Nothing

-- | Extract the resource record type of a 'RData' object
rdType :: RData l -> Either Type TypeSym
rdType rd = case rd of
              RDataA          {} -> Right TypeA
              RDataAAAA       {} -> Right TypeAAAA
              RDataAFSDB      {} -> Right TypeAFSDB
              RDataCAA        {} -> Right TypeCAA
              RDataCNAME      {} -> Right TypeCNAME
              RDataDNSKEY     {} -> Right TypeDNSKEY
              RDataDS         {} -> Right TypeDS
              RDataHINFO      {} -> Right TypeHINFO
              RDataMX         {} -> Right TypeMX
              RDataNAPTR      {} -> Right TypeNAPTR
              RDataNS         {} -> Right TypeNS
              RDataNSEC       {} -> Right TypeNSEC
              RDataNSEC3      {} -> Right TypeNSEC3
              RDataNSEC3PARAM {} -> Right TypeNSEC3PARAM
              RDataOPT        {} -> Right TypeOPT
              RDataPTR        {} -> Right TypePTR
              RDataRRSIG      {} -> Right TypeRRSIG
              RDataSOA        {} -> Right TypeSOA
              RDataSRV        {} -> Right TypeSRV
              RDataTXT        {} -> Right TypeTXT
              RDataSPF        {} -> Right TypeSPF
              RDataURI        {} -> Right TypeURI
              RDataSSHFP      {} -> Right TypeSSHFP
              --
              RData        ty _  -> maybe (Left ty) Right (typeToSym ty)


{- TODO:


type-bitmap:

 A NS SOA TXT AAAA RRSIG NSEC DNSKEY

'00 07 62 00 80 08 00 03 80'
'00000000 00000111 01100010 00000000 10000000 00001000 00000000 00000011 10000000'
 Win=#0    len=7         ^{SOA}      ^{TXT}       ^{AAAA}                ^{DNSKEY}
                    ^^{A,NS}                                          ^^{RRSIG,NSEC}

" ".join(map("{:08b}".format,[0,7,98,0,128,8,0,3,128]))


"\NUL\a\"\NUL\NUL\NUL\NUL\ETX\128"   NS SOA RRSIG NSEC DNSKEY

[ (winofs+j*8+7-i)   | (j,x) <- zip [0..] xs, i <- [7,6..0], testBit x i ]

-}



-- helpers

getUntilEmpty :: Binary a => Get [a]
getUntilEmpty = untilEmptyWith get

untilEmptyWith :: Get a -> Get [a]
untilEmptyWith g = go []
  where
    go acc = do
        e <- isEmpty
        if e
         then pure (reverse acc)
         else do
            v <- g
            go (v : acc)



{- TODO:


   MsgRR{rrName = Name "stanford.edu.", rrClass = 1, rrTTL = 1799,
         rrData =
           RData 29
             "\NUL\DC2\SYN\DC3\136\a\244\212e\200\252\194\NUL\152\150\128"},


https://en.wikipedia.org/wiki/LOC_record


LOC record statdns.net.   IN LOC   52 22 23.000 N 4 53 32.000 E -2.00m 0.00m 10000m 10m


SW1A2AA.find.me.uk.	86399	IN	LOC	51 30 12.748 N 0 7 39.611 W 0.00m 0.00m 0.00m 0.00m


https://tools.ietf.org/html/rfc1876

-}

