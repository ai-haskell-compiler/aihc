{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.CQL.Protocol.Codec
    ( encodeByte
    , decodeByte

    , encodeSignedByte
    , decodeSignedByte

    , encodeShort
    , decodeShort

    , encodeSignedShort
    , decodeSignedShort

    , encodeInt
    , decodeInt

    , encodeString
    , decodeString

    , encodeLongString
    , decodeLongString

    , encodeBytes
    , decodeBytes

    , encodeShortBytes
    , decodeShortBytes

    , encodeUUID
    , decodeUUID

    , encodeList
    , decodeList

    , encodeMap
    , decodeMap

    , encodeMultiMap
    , decodeMultiMap

    , encodeSockAddr
    , decodeSockAddr

    , encodeConsistency
    , decodeConsistency

    , encodeOpCode
    , decodeOpCode

    , encodePagingState
    , decodePagingState

    , decodeKeyspace
    , decodeTable
    , decodeColumnType
    , decodeQueryId

    , putValue
    , getValue
    ) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.ByteString (ByteString)
import Data.Decimal
import Data.Int
import Data.IP
#ifdef INCOMPATIBLE_VARINT
import Data.List (unfoldr)
#else
import Data.List (foldl')
#endif
import Data.Text (Text)
import Data.UUID (UUID)
import Data.Word
import Data.Serialize hiding (decode, encode)
import Database.CQL.Protocol.Types
import Network.Socket (SockAddr (..), PortNumber)
import Prelude

import qualified Data.ByteString         as B
import qualified Data.ByteString.Lazy    as LB
import qualified Data.Text.Encoding      as T
import qualified Data.Text.Lazy          as LT
import qualified Data.Text.Lazy.Encoding as LT
import qualified Data.UUID               as UUID

------------------------------------------------------------------------------
-- Byte

encodeByte :: Putter Word8
encodeByte = put

decodeByte :: Get Word8
decodeByte = get

------------------------------------------------------------------------------
-- Signed Byte

encodeSignedByte :: Putter Int8
encodeSignedByte = put

decodeSignedByte :: Get Int8
decodeSignedByte = get

------------------------------------------------------------------------------
-- Short

encodeShort :: Putter Word16
encodeShort = put

decodeShort :: Get Word16
decodeShort = get

------------------------------------------------------------------------------
-- Signed Short

encodeSignedShort :: Putter Int16
encodeSignedShort = put

decodeSignedShort :: Get Int16
decodeSignedShort = get

------------------------------------------------------------------------------
-- Int

encodeInt :: Putter Int32
encodeInt = put

decodeInt :: Get Int32
decodeInt = get

------------------------------------------------------------------------------
-- String

encodeString :: Putter Text
encodeString = encodeShortBytes . T.encodeUtf8

decodeString :: Get Text
decodeString = T.decodeUtf8 <$> decodeShortBytes

------------------------------------------------------------------------------
-- Long String

encodeLongString :: Putter LT.Text
encodeLongString = encodeBytes . LT.encodeUtf8

decodeLongString :: Get LT.Text
decodeLongString = do
    n <- get :: Get Int32
    LT.decodeUtf8 <$> getLazyByteString (fromIntegral n)

------------------------------------------------------------------------------
-- Bytes

encodeBytes :: Putter LB.ByteString
encodeBytes bs = do
    put (fromIntegral (LB.length bs) :: Int32)
    putLazyByteString bs

decodeBytes :: Get (Maybe LB.ByteString)
decodeBytes = do
    n <- get :: Get Int32
    if n < 0
        then return Nothing
        else Just <$> getLazyByteString (fromIntegral n)

------------------------------------------------------------------------------
-- Short Bytes

encodeShortBytes :: Putter ByteString
encodeShortBytes bs = do
    put (fromIntegral (B.length bs) :: Word16)
    putByteString bs

decodeShortBytes :: Get ByteString
decodeShortBytes = do
    n <- get :: Get Word16
    getByteString (fromIntegral n)

------------------------------------------------------------------------------
-- UUID

encodeUUID :: Putter UUID
encodeUUID = putLazyByteString . UUID.toByteString

decodeUUID :: Get UUID
decodeUUID = do
    uuid <- UUID.fromByteString <$> getLazyByteString 16
    maybe (fail "decode-uuid: invalid") return uuid

------------------------------------------------------------------------------
-- String List

encodeList :: Putter [Text]
encodeList sl = do
    put (fromIntegral (length sl) :: Word16)
    mapM_ encodeString sl

decodeList :: Get [Text]
decodeList = do
    n <- get :: Get Word16
    replicateM (fromIntegral n) decodeString

------------------------------------------------------------------------------
-- String Map

encodeMap :: Putter [(Text, Text)]
encodeMap m = do
    put (fromIntegral (length m) :: Word16)
    forM_ m $ \(k, v) -> encodeString k >> encodeString v

decodeMap :: Get [(Text, Text)]
decodeMap = do
    n <- get :: Get Word16
    replicateM (fromIntegral n) ((,) <$> decodeString <*> decodeString)

------------------------------------------------------------------------------
-- String Multi-Map

encodeMultiMap :: Putter [(Text, [Text])]
encodeMultiMap mm = do
    put (fromIntegral (length mm) :: Word16)
    forM_ mm $ \(k, v) -> encodeString k >> encodeList v

decodeMultiMap :: Get [(Text, [Text])]
decodeMultiMap = do
    n <- get :: Get Word16
    replicateM (fromIntegral n) ((,) <$> decodeString <*> decodeList)

------------------------------------------------------------------------------
-- Inet Address

encodeSockAddr :: Putter SockAddr
encodeSockAddr (SockAddrInet p a) = do
    putWord8 4
    putWord32le a
    putWord32be (fromIntegral p)
encodeSockAddr (SockAddrInet6 p _ (a, b, c, d) _) = do
    putWord8 16
    putWord32host a
    putWord32host b
    putWord32host c
    putWord32host d
    putWord32be (fromIntegral p)
encodeSockAddr (SockAddrUnix _) = error "encode-socket: unix address not supported"
#if MIN_VERSION_network(2,6,1) && !MIN_VERSION_network(3,0,0)
encodeSockAddr (SockAddrCan _) = error "encode-socket: can address not supported"
#endif

decodeSockAddr :: Get SockAddr
decodeSockAddr = do
    n <- getWord8
    case n of
        4  -> do
            i <- getIPv4
            p <- getPort
            return $ SockAddrInet p i
        16 -> do
            i <- getIPv6
            p <- getPort
            return $ SockAddrInet6 p 0 i 0
        _  -> fail $ "decode-socket: unknown: " ++ show n
  where
    getPort :: Get PortNumber
    getPort = fromIntegral <$> getWord32be

    getIPv4 :: Get Word32
    getIPv4 = getWord32le

    getIPv6 :: Get (Word32, Word32, Word32, Word32)
    getIPv6 = (,,,) <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host

------------------------------------------------------------------------------
-- Consistency

encodeConsistency :: Putter Consistency
encodeConsistency Any         = encodeShort 0x00
encodeConsistency One         = encodeShort 0x01
encodeConsistency Two         = encodeShort 0x02
encodeConsistency Three       = encodeShort 0x03
encodeConsistency Quorum      = encodeShort 0x04
encodeConsistency All         = encodeShort 0x05
encodeConsistency LocalQuorum = encodeShort 0x06
encodeConsistency EachQuorum  = encodeShort 0x07
encodeConsistency Serial      = encodeShort 0x08
encodeConsistency LocalSerial = encodeShort 0x09
encodeConsistency LocalOne    = encodeShort 0x0A

decodeConsistency :: Get Consistency
decodeConsistency = decodeShort >>= mapCode
      where
        mapCode 0x00 = return Any
        mapCode 0x01 = return One
        mapCode 0x02 = return Two
        mapCode 0x03 = return Three
        mapCode 0x04 = return Quorum
        mapCode 0x05 = return All
        mapCode 0x06 = return LocalQuorum
        mapCode 0x07 = return EachQuorum
        mapCode 0x08 = return Serial
        mapCode 0x09 = return LocalSerial
        mapCode 0x0A = return LocalOne
        mapCode code = fail $ "decode-consistency: unknown: " ++ show code

------------------------------------------------------------------------------
-- OpCode

encodeOpCode :: Putter OpCode
encodeOpCode OcError         = encodeByte 0x00
encodeOpCode OcStartup       = encodeByte 0x01
encodeOpCode OcReady         = encodeByte 0x02
encodeOpCode OcAuthenticate  = encodeByte 0x03
encodeOpCode OcOptions       = encodeByte 0x05
encodeOpCode OcSupported     = encodeByte 0x06
encodeOpCode OcQuery         = encodeByte 0x07
encodeOpCode OcResult        = encodeByte 0x08
encodeOpCode OcPrepare       = encodeByte 0x09
encodeOpCode OcExecute       = encodeByte 0x0A
encodeOpCode OcRegister      = encodeByte 0x0B
encodeOpCode OcEvent         = encodeByte 0x0C
encodeOpCode OcBatch         = encodeByte 0x0D
encodeOpCode OcAuthChallenge = encodeByte 0x0E
encodeOpCode OcAuthResponse  = encodeByte 0x0F
encodeOpCode OcAuthSuccess   = encodeByte 0x10

decodeOpCode :: Get OpCode
decodeOpCode = decodeByte >>= mapCode
  where
    mapCode 0x00 = return OcError
    mapCode 0x01 = return OcStartup
    mapCode 0x02 = return OcReady
    mapCode 0x03 = return OcAuthenticate
    mapCode 0x05 = return OcOptions
    mapCode 0x06 = return OcSupported
    mapCode 0x07 = return OcQuery
    mapCode 0x08 = return OcResult
    mapCode 0x09 = return OcPrepare
    mapCode 0x0A = return OcExecute
    mapCode 0x0B = return OcRegister
    mapCode 0x0C = return OcEvent
    mapCode 0x0D = return OcBatch
    mapCode 0x0E = return OcAuthChallenge
    mapCode 0x0F = return OcAuthResponse
    mapCode 0x10 = return OcAuthSuccess
    mapCode word = fail $ "decode-opcode: unknown: " ++ show word

------------------------------------------------------------------------------
-- ColumnType

decodeColumnType :: Get ColumnType
decodeColumnType = decodeShort >>= toType
  where
    toType 0x0000 = CustomColumn <$> decodeString
    toType 0x0001 = return AsciiColumn
    toType 0x0002 = return BigIntColumn
    toType 0x0003 = return BlobColumn
    toType 0x0004 = return BooleanColumn
    toType 0x0005 = return CounterColumn
    toType 0x0006 = return DecimalColumn
    toType 0x0007 = return DoubleColumn
    toType 0x0008 = return FloatColumn
    toType 0x0009 = return IntColumn
    toType 0x000A = return TextColumn
    toType 0x000B = return TimestampColumn
    toType 0x000C = return UuidColumn
    toType 0x000D = return VarCharColumn
    toType 0x000E = return VarIntColumn
    toType 0x000F = return TimeUuidColumn
    toType 0x0010 = return InetColumn
    toType 0x0011 = return DateColumn
    toType 0x0012 = return TimeColumn
    toType 0x0013 = return SmallIntColumn
    toType 0x0014 = return TinyIntColumn
    toType 0x0020 = ListColumn  <$> (decodeShort >>= toType)
    toType 0x0021 = MapColumn   <$> (decodeShort >>= toType) <*> (decodeShort >>= toType)
    toType 0x0022 = SetColumn   <$> (decodeShort >>= toType)
    toType 0x0030 = do
        _ <- decodeString -- Keyspace (not used by this library)
        t <- decodeString -- Type name
        UdtColumn t <$> do
            n <- fromIntegral <$> decodeShort
            replicateM n ((,) <$> decodeString <*> (decodeShort >>= toType))
    toType 0x0031 = TupleColumn <$> do
        n <- fromIntegral <$> decodeShort
        replicateM n (decodeShort >>= toType)
    toType other  = fail $ "decode-type: unknown: " ++ show other

------------------------------------------------------------------------------
-- Paging State

encodePagingState :: Putter PagingState
encodePagingState (PagingState s) = encodeBytes s

decodePagingState :: Get (Maybe PagingState)
decodePagingState = fmap PagingState <$> decodeBytes

------------------------------------------------------------------------------
-- Value

putValue :: Version -> Putter Value
putValue _ (CqlCustom x)    = toBytes $ putLazyByteString x
putValue _ (CqlBoolean x)   = toBytes $ putWord8 $ if x then 1 else 0
putValue _ (CqlInt x)       = toBytes $ put x
putValue _ (CqlBigInt x)    = toBytes $ put x
putValue _ (CqlFloat x)     = toBytes $ putFloat32be x
putValue _ (CqlDouble x)    = toBytes $ putFloat64be x
putValue _ (CqlText x)      = toBytes $ putByteString (T.encodeUtf8 x)
putValue _ (CqlUuid x)      = toBytes $ encodeUUID x
putValue _ (CqlTimeUuid x)  = toBytes $ encodeUUID x
putValue _ (CqlTimestamp x) = toBytes $ put x
putValue _ (CqlAscii x)     = toBytes $ putByteString (T.encodeUtf8 x)
putValue _ (CqlBlob x)      = toBytes $ putLazyByteString x
putValue _ (CqlCounter x)   = toBytes $ put x
putValue _ (CqlInet x)      = toBytes $ case x of
    IPv4 i -> putWord32le (toHostAddress i)
    IPv6 i -> do
        let (a, b, c, d) = toHostAddress6 i
        putWord32host a
        putWord32host b
        putWord32host c
        putWord32host d
putValue _ (CqlVarInt x)   = toBytes $ integer2bytes x
putValue _ (CqlDecimal x)  = toBytes $ do
    put (fromIntegral (decimalPlaces x) :: Int32)
    integer2bytes (decimalMantissa x)
putValue V4   (CqlDate x)     = toBytes $ put x
putValue _  v@(CqlDate _)     = error $ "putValue: date: " ++ show v
putValue V4   (CqlTime x)     = toBytes $ put x
putValue _  v@(CqlTime _)     = error $ "putValue: time: " ++ show v
putValue V4   (CqlSmallInt x) = toBytes $ put x
putValue _  v@(CqlSmallInt _) = error $ "putValue: smallint: " ++ show v
putValue V4   (CqlTinyInt x)  = toBytes $ put x
putValue _  v@(CqlTinyInt _)  = error $ "putValue: tinyint: " ++ show v
putValue v    (CqlUdt   x)    = toBytes $ mapM_ (putValue v . snd) x
putValue v    (CqlList x)     = toBytes $ do
    encodeInt (fromIntegral (length x))
    mapM_ (putValue v) x
putValue v (CqlSet x) = toBytes $ do
    encodeInt (fromIntegral (length x))
    mapM_ (putValue v) x
putValue v (CqlMap x) = toBytes $ do
    encodeInt (fromIntegral (length x))
    forM_ x $ \(k, w) -> putValue v k >> putValue v w
putValue v (CqlTuple x)        = toBytes $ mapM_ (putValue v) x
putValue _ (CqlMaybe Nothing)  = put (-1 :: Int32)
putValue v (CqlMaybe (Just x)) = putValue v x

getValue :: Version -> ColumnType -> Get Value
getValue v (ListColumn t) = CqlList <$> getList (do
    len <- decodeInt
    replicateM (fromIntegral len) (getValue v t))
getValue v (SetColumn t) = CqlSet <$> getList (do
    len <- decodeInt
    replicateM (fromIntegral len) (getValue v t))
getValue v (MapColumn t u) = CqlMap <$> getList (do
    len <- decodeInt
    replicateM (fromIntegral len) ((,) <$> getValue v t <*> getValue v u))
getValue v (TupleColumn t) = withBytes $ CqlTuple <$> mapM (getValue v) t
getValue v (MaybeColumn t) = do
    n <- lookAhead (get :: Get Int32)
    if n < 0
        then uncheckedSkip 4 >> return (CqlMaybe Nothing)
        else CqlMaybe . Just <$> getValue v t
getValue _ (CustomColumn _) = withBytes $ CqlCustom <$> remainingBytesLazy
getValue _ BooleanColumn    = withBytes $ CqlBoolean . (/= 0) <$> getWord8
getValue _ IntColumn        = withBytes $ CqlInt <$> get
getValue _ BigIntColumn     = withBytes $ CqlBigInt <$> get
getValue _ FloatColumn      = withBytes $ CqlFloat  <$> getFloat32be
getValue _ DoubleColumn     = withBytes $ CqlDouble <$> getFloat64be
getValue _ TextColumn       = withBytes $ CqlText . T.decodeUtf8 <$> remainingBytes
getValue _ VarCharColumn    = withBytes $ CqlText . T.decodeUtf8 <$> remainingBytes
getValue _ AsciiColumn      = withBytes $ CqlAscii . T.decodeUtf8 <$> remainingBytes
getValue _ BlobColumn       = withBytes $ CqlBlob <$> remainingBytesLazy
getValue _ UuidColumn       = withBytes $ CqlUuid <$> decodeUUID
getValue _ TimeUuidColumn   = withBytes $ CqlTimeUuid <$> decodeUUID
getValue _ TimestampColumn  = withBytes $ CqlTimestamp <$> get
getValue _ CounterColumn    = withBytes $ CqlCounter <$> get
getValue _ InetColumn       = withBytes $ CqlInet <$> do
    len <- remaining
    case len of
        4  -> IPv4 . fromHostAddress <$> getWord32le
        16 -> do
            a <- (,,,) <$> getWord32host <*> getWord32host <*> getWord32host <*> getWord32host
            return $ IPv6 (fromHostAddress6 a)
        n  -> fail $ "getNative: invalid Inet length: " ++ show n
getValue V4 DateColumn     = withBytes $ CqlDate <$> get
getValue _  DateColumn     = fail "getNative: date type"
getValue V4 TimeColumn     = withBytes $ CqlTime <$> get
getValue _  TimeColumn     = fail "getNative: time type"
getValue V4 SmallIntColumn = withBytes $ CqlSmallInt <$> get
getValue _  SmallIntColumn = fail "getNative: smallint type"
getValue V4 TinyIntColumn  = withBytes $ CqlTinyInt <$> get
getValue _  TinyIntColumn  = fail "getNative: tinyint type"
getValue _  VarIntColumn   = withBytes $ CqlVarInt <$> bytes2integer
getValue _  DecimalColumn  = withBytes $ do
    x <- get :: Get Int32
    y <- bytes2integer
    return (CqlDecimal (Decimal (fromIntegral x) y))
getValue v (UdtColumn _ x) = withBytes $ CqlUdt <$> do
    let (n, t) = unzip x
    zip n <$> mapM (getValue v) t

getList :: Get [a] -> Get [a]
getList m = do
    n <- lookAhead (get :: Get Int32)
    if n < 0 then uncheckedSkip 4 >> return []
             else withBytes m

withBytes :: Get a -> Get a
withBytes p = do
    n <- fromIntegral <$> (get :: Get Int32)
    when (n < 0) $
        fail $ "withBytes: null (length = " ++ show n ++ ")"
    b <- getBytes n
    case runGet p b of
        Left  e -> fail $ "withBytes: " ++ e
        Right x -> return x

remainingBytes :: Get ByteString
remainingBytes = remaining >>= getByteString . fromIntegral

remainingBytesLazy :: Get LB.ByteString
remainingBytesLazy = remaining >>= getLazyByteString . fromIntegral

toBytes :: Put -> Put
toBytes p = do
    let bytes = runPut p
    put (fromIntegral (B.length bytes) :: Int32)
    putByteString bytes

#ifdef INCOMPATIBLE_VARINT

-- 'integer2bytes' and 'bytes2integer' implementations are taken
-- from cereal's instance declaration of 'Serialize' for 'Integer'
-- except that no distinction between small and large integers is made.
-- Cf. to LICENSE for copyright details.
integer2bytes :: Putter Integer
integer2bytes n = do
    put sign
    put (unroll (abs n))
  where
    sign = fromIntegral (signum n) :: Word8

    unroll :: Integer -> [Word8]
    unroll = unfoldr step
      where
        step 0 = Nothing
        step i = Just (fromIntegral i, i `shiftR` 8)

bytes2integer :: Get Integer
bytes2integer = do
    sign  <- get
    bytes <- get
    let v = roll bytes
    return $! if sign == (1 :: Word8) then v else - v
  where
    roll :: [Word8] -> Integer
    roll = foldr unstep 0
      where
        unstep b a = a `shiftL` 8 .|. fromIntegral b

#else

integer2bytes :: Putter Integer
integer2bytes n
    | n == 0  = putWord8 0x00
    | n == -1 = putWord8 0xFF
    | n <  0  = do
        let bytes = explode (-1) n
        unless (head bytes >= 0x80) $
            putWord8 0xFF
        mapM_ putWord8 bytes
    | otherwise = do
        let bytes = explode 0 n
        unless (head bytes < 0x80) $
            putWord8 0x00
        mapM_ putWord8 bytes

explode :: Integer -> Integer -> [Word8]
explode x n = loop n []
  where
    loop !i !acc
        | i == x    = acc
        | otherwise = loop (i `shiftR` 8) (fromIntegral i : acc)

bytes2integer :: Get Integer
bytes2integer = do
    msb   <- getWord8
    bytes <- B.unpack <$> remainingBytes
    if msb < 0x80
        then return (implode (msb:bytes))
        else return (- (implode (map complement (msb:bytes)) + 1))

implode :: [Word8] -> Integer
implode = foldl' fun 0
  where
    fun i b = i `shiftL` 8 .|. fromIntegral b

#endif
------------------------------------------------------------------------------
-- Various

decodeKeyspace :: Get Keyspace
decodeKeyspace = Keyspace <$> decodeString

decodeTable :: Get Table
decodeTable = Table <$> decodeString

decodeQueryId :: Get (QueryId k a b)
decodeQueryId = QueryId <$> decodeShortBytes
