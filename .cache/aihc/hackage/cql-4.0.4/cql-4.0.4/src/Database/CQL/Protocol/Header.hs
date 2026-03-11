module Database.CQL.Protocol.Header
    ( Header     (..)
    , HeaderType (..)
    , header
    , encodeHeader
    , decodeHeader

      -- ** Length
    , Length     (..)
    , encodeLength
    , decodeLength

      -- ** StreamId
    , StreamId
    , mkStreamId
    , fromStreamId
    , encodeStreamId
    , decodeStreamId

      -- ** Flags
    , Flags
    , compress
    , customPayload
    , tracing
    , warning
    , isSet
    , encodeFlags
    , decodeFlags
    ) where

import Control.Applicative
import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Int
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Serialize
import Data.Word
import Database.CQL.Protocol.Codec
import Database.CQL.Protocol.Types
import Prelude

-- | Protocol frame header.
data Header = Header
    { headerType :: !HeaderType
    , version    :: !Version
    , flags      :: !Flags
    , streamId   :: !StreamId
    , opCode     :: !OpCode
    , bodyLength :: !Length
    } deriving Show

data HeaderType
    = RqHeader -- ^ A request frame header.
    | RsHeader -- ^ A response frame header.
    deriving Show

encodeHeader :: Version -> HeaderType -> Flags -> StreamId -> OpCode -> Length -> PutM ()
encodeHeader v t f i o l = do
    encodeByte $ case t of
        RqHeader -> mapVersion v
        RsHeader -> mapVersion v `setBit` 7
    encodeFlags f
    encodeStreamId v i
    encodeOpCode o
    encodeLength l

decodeHeader :: Version -> Get Header
decodeHeader v = do
    b <- getWord8
    Header (mapHeaderType b)
        <$> toVersion (b .&. 0x7F)
        <*> decodeFlags
        <*> decodeStreamId v
        <*> decodeOpCode
        <*> decodeLength

mapHeaderType :: Word8 -> HeaderType
mapHeaderType b = if b `testBit` 7 then RsHeader else RqHeader

-- | Deserialise a frame header using the version specific decoding format.
header :: Version -> ByteString -> Either String Header
header v = runGetLazy (decodeHeader v)

------------------------------------------------------------------------------
-- Version

mapVersion :: Version -> Word8
mapVersion V4 = 4
mapVersion V3 = 3

toVersion :: Word8 -> Get Version
toVersion 3 = return V3
toVersion 4 = return V4
toVersion w = fail $ "decode-version: unknown: " ++ show w

------------------------------------------------------------------------------
-- Length

-- | The type denoting a protocol frame length.
newtype Length = Length { lengthRepr :: Int32 } deriving (Eq, Show)

encodeLength :: Putter Length
encodeLength (Length x) = encodeInt x

decodeLength :: Get Length
decodeLength = Length <$> decodeInt

------------------------------------------------------------------------------
-- StreamId

-- | Streams allow multiplexing of requests over a single communication
-- channel. The 'StreamId' correlates 'Request's with 'Response's.
newtype StreamId = StreamId Int16 deriving (Eq, Show)

-- | Create a StreamId from the given integral value. In version 2,
-- a StreamId is an 'Int8' and in version 3 an 'Int16'.
mkStreamId :: Integral i => i -> StreamId
mkStreamId = StreamId . fromIntegral

-- | Convert the stream ID to an integer.
fromStreamId :: StreamId -> Int
fromStreamId (StreamId i) = fromIntegral i

encodeStreamId :: Version -> Putter StreamId
encodeStreamId V4 (StreamId x) = encodeSignedShort (fromIntegral x)
encodeStreamId V3 (StreamId x) = encodeSignedShort (fromIntegral x)

decodeStreamId :: Version -> Get StreamId
decodeStreamId V4 = StreamId <$> decodeSignedShort
decodeStreamId V3 = StreamId <$> decodeSignedShort

------------------------------------------------------------------------------
-- Flags

-- | Type representing header flags. Flags form a monoid and can be used
-- as in @compress <> tracing <> mempty@.
newtype Flags = Flags Word8 deriving (Eq, Show)

instance Semigroup Flags where
    (Flags a) <> (Flags b) = Flags (a .|. b)

instance Monoid Flags where
    mempty  = Flags 0
    mappend = (<>)

encodeFlags :: Putter Flags
encodeFlags (Flags x) = encodeByte x

decodeFlags :: Get Flags
decodeFlags = Flags <$> decodeByte

-- | Compression flag. If set, the frame body is compressed.
compress :: Flags
compress = Flags 1

-- | Tracing flag. If a request support tracing and the tracing flag was set,
-- the response to this request will have the tracing flag set and contain
-- tracing information.
tracing :: Flags
tracing = Flags 2

customPayload :: Flags
customPayload = Flags 4

warning :: Flags
warning = Flags 8

-- | Check if a particular flag is present.
isSet :: Flags -> Flags -> Bool
isSet (Flags a) (Flags b) = a .&. b == a
