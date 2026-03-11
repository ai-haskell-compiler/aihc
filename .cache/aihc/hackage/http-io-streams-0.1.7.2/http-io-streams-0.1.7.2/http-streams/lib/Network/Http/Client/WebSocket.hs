{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE NamedFieldPuns     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

{-# OPTIONS_GHC -Wall #-}
-- |
-- Copyright: © 2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
-- Basic WebSocket <https://tools.ietf.org/html/rfc6455 RFC 6455> support.
--
-- @since 0.1.4.0
module Network.Http.Client.WebSocket
    ( -- * WebSocket Frames
      -- ** WebSocket Frame Header
      WSFrameHdr(..)
    , wsFrameHdrSize
    , wsFrameHdrToBuilder

    , WSOpcode(..)
    , WSOpcodeReserved(..)
    , wsIsDataFrame

      -- ** Mid-level I/O primitives

      -- *** Sending WebSocket frames
    , writeWSFrame
    , sendWSFragData

      -- *** Receiving WebSocket frames
--  , readWSFrameHdr
    , readWSFrame
    , receiveWSFrame

      -- * WebSocket handshake

      -- ** HTTP/1.1 WebSocket connection upgrade

    , wsUpgradeConnection

      -- ** Low-level primitives for WebSocket handshake
    , SecWebSocketKey
    , wsKeyToAcceptB64

    , secWebSocketKeyFromB64
    , secWebSocketKeyToB64
    , secWebSocketKeyFromWords

      -- * Exception
    , WsException(..)
    ) where

import           Blaze.ByteString.Builder (Builder)
import qualified Blaze.ByteString.Builder as Builder
import           Control.Exception
import           Control.Monad            (unless, when)
import qualified Crypto.Hash.SHA1         as SHA1
import qualified Data.Binary              as Bin
import qualified Data.Binary.Get          as Bin
import qualified Data.Binary.Put          as Bin
import           Data.Bits
import           Data.ByteString          (ByteString)
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Base64   as B64
import qualified Data.ByteString.Lazy     as BL
import qualified Data.CaseInsensitive     as CI
import           Data.IORef
import           Data.Maybe               (isJust)
import           Data.Monoid              (Monoid (..))
import           Data.Word
import           Data.XOR                 (xor32LazyByteString, xor32StrictByteString')
import           Network.Http.Client      as HC
import qualified Network.Http.Connection  as HC
import           System.IO.Streams        (InputStream, OutputStream)
import qualified System.IO.Streams        as Streams

-- | Exception type thrown by WebSocket routines.
--
-- These exceptions mostly denote WebSocket protocol violations from either side, client and server.
--
-- @since 0.1.4.0
data WsException = WsException String
  deriving (Show)

instance Exception WsException

-- | WebSocket Frame as per <https://tools.ietf.org/html/rfc6455#section-5.2 RFC 6455 section 5.2>
--
--
-- >  0                   1                   2                   3
-- >  0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1 2 3 4 5 6 7 8 9 0 1
-- > +-+-+-+-+-------+-+-------------+-------------------------------+
-- > |F|R|R|R| opcode|M| Payload len |    Extended payload length    |
-- > |I|S|S|S|  (4)  |A|     (7)     |             (16/64)           |
-- > |N|V|V|V|       |S|             |   (if payload len==126/127)   |
-- > | |1|2|3|       |K|             |                               |
-- > +-+-+-+-+-------+-+-------------+ - - - - - - - - - - - - - - - +
-- > |     Extended payload length continued, if payload len == 127  |
-- > + - - - - - - - - - - - - - - - +-------------------------------+
-- > |                               |Masking-key, if MASK set to 1  |
-- > +-------------------------------+-------------------------------+
-- > | Masking-key (continued)       |          Payload Data         |
-- > +-------------------------------- - - - - - - - - - - - - - - - +
-- > :                     Payload Data continued ...                :
-- > + - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - +
-- > |                     Payload Data continued ...                |
-- > +---------------------------------------------------------------+
--
-- @since 0.1.4.0
data WSFrameHdr = WSFrameHdr
  { ws'FIN    :: !Bool -- ^ MUST be set for control-frames (control-frames MUST NOT be fragmented).
  , ws'RSV1   :: !Bool
  , ws'RSV2   :: !Bool
  , ws'RSV3   :: !Bool
  , ws'opcode :: !WSOpcode -- see 'WSOpcode'
  , ws'length :: !Word64 -- ^ MUST be smaller than 2^63 for data-frames;
                         --   MUST be smaller than 126 for control-frames.
  , ws'mask   :: !(Maybe Word32)
    -- ^ Whether the frame is masked and which masking key is used;
    -- 'Nothing' denotes unmasked frames.
    --
    -- A client MUST mask all frames that it sends to the server; A
    -- server MUST NOT mask any frames that it sends to the client.
  } deriving Show

-- | Size of serialized WebSocket frame header (i.e. without payload data) in octets.
--
-- @since 0.1.4.0
wsFrameHdrSize :: WSFrameHdr -> Int
wsFrameHdrSize WSFrameHdr{ws'mask = Nothing, ws'length}
  | ws'length <  126    = 2
  | ws'length <= 0xffff = 4
  | otherwise           = 10
wsFrameHdrSize WSFrameHdr{ws'mask = Just _, ws'length}
  | ws'length <  126    = 6
  | ws'length <= 0xffff = 8
  | otherwise           = 14

-- not exported yet
-- | Try reading a 'WSFrameHdr' from the 'InputStream'.
--
-- Returns 'Nothing' is EOF is encountered before any WebSocket header byte is read
readWSFrameHdr :: Connection -> IO (Maybe WSFrameHdr)
readWSFrameHdr (HC.Connection { cIn = is }) = do
    mchunk <- Streams.read is
    case mchunk of
      Nothing -> return Nothing
      Just chunk -> go $ (if BS.null chunk then id else flip Bin.pushChunk chunk)
                       $ Bin.runGetIncremental Bin.get
  where
    go :: Bin.Decoder WSFrameHdr -> IO (Maybe WSFrameHdr)
    go (Bin.Fail rest _ err) = do
        unless (BS.null rest) $
          Streams.unRead rest is
        throwIO $ WsException ("readWSFrameHdr: " ++ err)
    go partial@(Bin.Partial cont) = do
        mchunk <- Streams.read is
        case mchunk of
          Nothing -> go (cont Nothing) -- eof, will likely trigger failure
          Just chunk
            | BS.null chunk -> go partial -- loop
            | otherwise     -> go (cont (Just chunk))
    go (Bin.Done rest _ x) = do
        unless (BS.null rest) $
          Streams.unRead rest is
        return (Just x)

-- | Receive a single WebSocket frame as 'InputStream'.
--
-- This operation does not perform any defragmentation nor automatically deal with control-frames (those will be returned to the caller as-is).
--
-- See also 'readWSFrame' for a simple non-streaming version.
--
-- @since 0.1.4.0
receiveWSFrame :: Connection -> (WSFrameHdr -> InputStream ByteString -> IO a) -> IO (Maybe a)
receiveWSFrame (conn@HC.Connection { cIn = is }) cont = do
  mhdr <- readWSFrameHdr conn
  case mhdr of
    Nothing -> return Nothing
    Just hdr
      | ws'length hdr == 0 -> do
          is' <- Streams.nullInput
          Just `fmap` cont hdr is'
      | otherwise -> do
          is' <- Streams.takeExactly (fromIntegral (ws'length hdr)) is
          is'' <- xor32InputStream (maybe 0 id (ws'mask hdr)) is'
          res <- cont hdr is''
          Streams.skipToEof is'
          return $! Just $! res

-- | Send WebSocket message as fragmented data-frames.
--
-- This function can be used if the size of the data payload to be sent is not known in advance.
--
-- This operation does not flush automatically after every chunk; write an empty chunk to the 'OutputStream' to trigger flushing pending data onto the WebSocket connection.
--
-- The 'ws'length' and 'ws'FIN' fields are ignored and computed from the chunks to be sent.
--
-- Pre-conditions:
--
-- - This function does not support sending control-frames as those MUST NOT be fragmented.
-- - 'ws'opcode' MUST NOT be 'WSOpcode'Continuation'
--
-- @since 0.1.4.0
sendWSFragData :: Connection -> WSFrameHdr -> (OutputStream ByteString -> IO a) -> IO a
sendWSFragData _ hdr0  _
  | not (wsIsDataFrame (ws'opcode hdr0))
      = throwIO (WsException "sendWSFragData: sending control-frame requested")
  | ws'opcode hdr0 == WSOpcode'Continuation
      = throwIO (WsException "sendWSFragData: sending continuation frame requested")
sendWSFragData (HC.Connection { cOut = os }) hdr0 cont = do
    opcodeRef <- newIORef (ws'opcode hdr0)
    let go Nothing = return ()
        go (Just chunk)
          | BS.null chunk = Streams.write (Just Builder.flush) os
          | otherwise = do
              let (_,chunk') = xor32StrictByteString' (maybe 0 id $ ws'mask hdr0) chunk
              opcode <- readIORef opcodeRef
              writeIORef opcodeRef WSOpcode'Continuation
              let fraghdr = hdr0 { ws'FIN = False
                                 , ws'length = fromIntegral (BS.length chunk)
                                 , ws'opcode = opcode
                                 }
              Streams.write (Just $! wsFrameHdrToBuilder fraghdr `mappend` Builder.fromByteString chunk') os

    os' <- Streams.makeOutputStream go
    !res <- cont os'

    -- send FINal frame
    opcode <- readIORef opcodeRef
    let final = (hdr0 { ws'FIN    = True
                      , ws'length = 0
                      , ws'opcode = opcode
                      , ws'mask   = Just 0
                      })
    Streams.write (Just $ wsFrameHdrToBuilder final `mappend` Builder.flush) os

    return $! res

-- | Convenience function for writing simple non-fragmented frames to an established WebSocket connection.
--
-- Control-frames MUST have a payload smaller than 126 bytes.
--
-- @since 0.1.4.0
writeWSFrame :: Connection -> WSOpcode -> Maybe Word32 -> BL.ByteString -> IO ()
writeWSFrame (HC.Connection { cOut = os }) opcode mmask payload = do
    when (not (wsIsDataFrame opcode) && plen >= 126) $
      throwIO (WsException "writeWSFrame: over-sized control-frame")

    let hdr = wsFrameHdrToBuilder (WSFrameHdr True False False False opcode plen mmask)
        dat = case mmask of
                Nothing  -> Builder.fromLazyByteString payload
                Just 0   -> Builder.fromLazyByteString payload
                Just msk -> Builder.fromLazyByteString (xor32LazyByteString msk payload)
    Streams.write (Just $ hdr `mappend` dat `mappend` Builder.flush) os
  where
    plen = fromIntegral (BL.length payload)

-- | Convenience function for reading a single (possibly fragmented) frame from an established WebSocket connection.
--
-- The first argument is the maximum expected frame size to receive; if a larger frame is encountered an exception is raised.
--
-- This operation does not perform any defragmentation nor automatically deal with control-frames (those will be returned to the caller as-is).
--
-- Returns 'Nothing' if the 'InputStream' is terminated before reading the first octet.
--
-- @since 0.1.4.0
readWSFrame :: Int -> Connection -> IO (Maybe (WSFrameHdr,ByteString))
readWSFrame maxSize (conn@HC.Connection { cIn = is }) = do
    mhdr <- readWSFrameHdr conn
    case mhdr of
      Nothing -> return Nothing
      Just hdr
        | ws'length hdr == 0 ->
            return $ Just (hdr,BS.empty)
        | ws'length hdr >= fromIntegral maxSize ->
            throwIO (WsException "readWSFrame: frame larger than maxSize")
        | otherwise -> do
            dat <- Streams.readExactly (fromIntegral (ws'length hdr)) is
            let dat' = case ws'mask hdr of
                         Nothing -> dat
                         Just 0  -> dat
                         Just m  -> snd (xor32StrictByteString' m dat)
            return $ Just (hdr,dat')

-- | Serialize 'WSFrameHdr' to 'BB.Builder'
--
-- @since 0.1.4.0
wsFrameHdrToBuilder :: WSFrameHdr -> Builder
wsFrameHdrToBuilder WSFrameHdr{..} = mconcat
    [ Builder.fromWord8 $!
        (if ws'FIN  then 0x80 else 0) .|.
        (if ws'RSV1 then 0x40 else 0) .|.
        (if ws'RSV2 then 0x20 else 0) .|.
        (if ws'RSV3 then 0x10 else 0) .|.
        (encodeWSOpcode ws'opcode)

    , Builder.fromWord8 $!
        (if isJust ws'mask then 0x80 else 0) .|. len7

    -- extended payload length
    , case len7 of
        126 -> Builder.fromWord16be (fromIntegral ws'length)
        127 -> Builder.fromWord64be ws'length
        _   -> Data.Monoid.mempty

    , maybe mempty Builder.fromWord32be ws'mask
    ]
  where
    len7 | ws'length < 126     = fromIntegral ws'length
         | ws'length <= 0xffff = 126 -- 16bit
         | otherwise           = 127 -- 64bit

instance Bin.Binary WSFrameHdr where
  -- put :: WSFrameHdr -> Bin.Put
  put WSFrameHdr{..} = do
      Bin.putWord8 $!
        (if ws'FIN  then 0x80 else 0) .|.
        (if ws'RSV1 then 0x40 else 0) .|.
        (if ws'RSV2 then 0x20 else 0) .|.
        (if ws'RSV3 then 0x10 else 0) .|.
        (encodeWSOpcode ws'opcode)

      Bin.putWord8 $!
        (if isJust ws'mask then 0x80 else 0) .|. len7

      -- extended payload length
      case len7 of
        126 -> Bin.putWord16be (fromIntegral ws'length)
        127 -> Bin.putWord64be ws'length
        _   -> return ()

      maybe (return ()) Bin.putWord32be ws'mask
    where
      len7 | ws'length < 126     = fromIntegral ws'length
           | ws'length <= 0xffff = 126 -- 16bit
           | otherwise           = 127 -- 64bit

  ----------------------------------------------------------------------------

  -- get :: Bin.Get WSFrameHdr
  get = do
      o0 <- Bin.getWord8

      let ws'FIN    = testBit o0 7
          ws'RSV1   = testBit o0 6
          ws'RSV2   = testBit o0 5
          ws'RSV3   = testBit o0 4
          ws'opcode = decodeWSOpcode o0

      when (not ws'FIN && not (wsIsDataFrame ws'opcode)) $
        fail "invalid fragmented control-frame"

      o1 <- Bin.getWord8

      let len7 = o1 .&. 0x7f
          msk  = o1  >= 0x80

      ws'length <- case len7 of
          127 -> do
            unless (wsIsDataFrame ws'opcode) $ fail "invalid 64-bit extended length (control-frame)"
            v <- Bin.getWord64be
            unless (v > 0xffff) $ fail "invalid 64-bit extended length (<= 0xffff)"
            unless (v < 0x8000000000000000) $ fail "invalid 64-bit extended length (MSB set)"
            return v
          126 -> do
            unless (wsIsDataFrame ws'opcode) $ fail "invalid 16-bit extended length (control-frame)"
            v <- Bin.getWord16be
            unless (v > 125) $ fail "invalid 16-bit extended length (<= 0x7d)"
            return (fromIntegral v)
          _ -> return (fromIntegral len7)

      ws'mask <- if msk
       then Just `fmap` Bin.getWord32be
       else return Nothing

      return WSFrameHdr{..}

-- | WebSocket frame opcode.
--
-- See also 'wsIsDataFrame'.
--
-- @since 0.1.4.0
data WSOpcode
  = WSOpcode'Continuation -- ^ /data/ fragmented data-frame
  | WSOpcode'Text         -- ^ /data/ payload must utf-8 encoded
  | WSOpcode'Binary       -- ^ /data/ binary data payload
  | WSOpcode'Close        -- ^ /control/ connection close frame (optional payload with reason-code)
  | WSOpcode'Ping         -- ^ /control/ PING frame
  | WSOpcode'Pong         -- ^ /control/ PONG frame
  | WSOpcode'Reserved !WSOpcodeReserved -- ^ reserved frame kind not defined by RFC 6455
  deriving (Eq,Show)

-- | WebSocket frame opcodes reserved for future use by RFC 6455
--
-- @since 0.1.4.0
data WSOpcodeReserved
  = WSOpcode'Reserved3
  | WSOpcode'Reserved4
  | WSOpcode'Reserved5
  | WSOpcode'Reserved6
  | WSOpcode'Reserved7
  | WSOpcode'Reserved11
  | WSOpcode'Reserved12
  | WSOpcode'Reserved13
  | WSOpcode'Reserved14
  | WSOpcode'Reserved15
  deriving (Eq,Show)

-- | Whether 'WSOpcode' denotes a data-frame.
--
-- There are two kinds of WebSocket frames, data-frames and
-- control-frames. Consequently, this predicate is 'False' for
-- control-frames.
--
-- @since 0.1.4.0
wsIsDataFrame :: WSOpcode -> Bool
wsIsDataFrame x = case x of
  WSOpcode'Continuation                 -> True
  WSOpcode'Text                         -> True
  WSOpcode'Binary                       -> True

  WSOpcode'Reserved WSOpcode'Reserved3  -> True
  WSOpcode'Reserved WSOpcode'Reserved4  -> True
  WSOpcode'Reserved WSOpcode'Reserved5  -> True
  WSOpcode'Reserved WSOpcode'Reserved6  -> True
  WSOpcode'Reserved WSOpcode'Reserved7  -> True

  WSOpcode'Close                        -> False
  WSOpcode'Ping                         -> False
  WSOpcode'Pong                         -> False

  WSOpcode'Reserved WSOpcode'Reserved11 -> False
  WSOpcode'Reserved WSOpcode'Reserved12 -> False
  WSOpcode'Reserved WSOpcode'Reserved13 -> False
  WSOpcode'Reserved WSOpcode'Reserved14 -> False
  WSOpcode'Reserved WSOpcode'Reserved15 -> False


-- NB: ignores high nibble
decodeWSOpcode :: Word8 -> WSOpcode
decodeWSOpcode x = case x .&. 0xf of
  0x0 -> WSOpcode'Continuation
  0x1 -> WSOpcode'Text
  0x2 -> WSOpcode'Binary

  0x3 -> WSOpcode'Reserved WSOpcode'Reserved3
  0x4 -> WSOpcode'Reserved WSOpcode'Reserved4
  0x5 -> WSOpcode'Reserved WSOpcode'Reserved5
  0x6 -> WSOpcode'Reserved WSOpcode'Reserved6
  0x7 -> WSOpcode'Reserved WSOpcode'Reserved7

  0x8 -> WSOpcode'Close
  0x9 -> WSOpcode'Ping
  0xA -> WSOpcode'Pong

  0xB -> WSOpcode'Reserved WSOpcode'Reserved11
  0xC -> WSOpcode'Reserved WSOpcode'Reserved12
  0xD -> WSOpcode'Reserved WSOpcode'Reserved13
  0xE -> WSOpcode'Reserved WSOpcode'Reserved14
  0xF -> WSOpcode'Reserved WSOpcode'Reserved15

  _   -> undefined -- impossible

-- internal
encodeWSOpcode :: WSOpcode -> Word8
encodeWSOpcode x = case x of
  WSOpcode'Continuation                 -> 0x0
  WSOpcode'Text                         -> 0x1
  WSOpcode'Binary                       -> 0x2

  WSOpcode'Reserved WSOpcode'Reserved3  -> 0x3
  WSOpcode'Reserved WSOpcode'Reserved4  -> 0x4
  WSOpcode'Reserved WSOpcode'Reserved5  -> 0x5
  WSOpcode'Reserved WSOpcode'Reserved6  -> 0x6
  WSOpcode'Reserved WSOpcode'Reserved7  -> 0x7

  WSOpcode'Close                        -> 0x8
  WSOpcode'Ping                         -> 0x9
  WSOpcode'Pong                         -> 0xA

  WSOpcode'Reserved WSOpcode'Reserved11 -> 0xB
  WSOpcode'Reserved WSOpcode'Reserved12 -> 0xC
  WSOpcode'Reserved WSOpcode'Reserved13 -> 0xD
  WSOpcode'Reserved WSOpcode'Reserved14 -> 0xE
  WSOpcode'Reserved WSOpcode'Reserved15 -> 0xF

-- | Compute @Sec-WebSocket-Accept@ header value from @Sec-WebSocket-Key@ header value according to RFC 6455
--
-- >>> wsKeyToAcceptB64 <$> secWebSocketKeyFromB64 "dGhlIHNhbXBsZSBub25jZQ=="
-- Just "s3pPLMBiTxaQ9kYGzzhZRbK+xOo="
--
-- @since 0.1.4.0
wsKeyToAcceptB64 :: SecWebSocketKey -> ByteString
wsKeyToAcceptB64 key = B64.encode (SHA1.hash (secWebSocketKeyToB64 key `BS.append` rfc6455Guid))
  where
    rfc6455Guid :: ByteString
    rfc6455Guid = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11"

-- | @Sec-WebSocket-Key@ header value according to RFC 6455
--
-- @since 0.1.4.0
newtype SecWebSocketKey = WSKey ByteString deriving (Eq,Ord,Show)

-- | Construct 'SecWebSocketKey' from its HTTP header base64 representation.
--
-- The input must be a valid @Sec-WebSocket-Key@ value, i.e. a @base64@ encoded 16-octet value (i.e. 24 base64 characters) with optional surrounding whitespace (@TAB@ or @SPC@) characters or this function will return 'Nothing'.
--
-- @since 0.1.4.0
secWebSocketKeyFromB64 :: ByteString -> Maybe SecWebSocketKey
secWebSocketKeyFromB64 key
  | BS.length key' /= 24      = Nothing -- invalid base64 or wrong length
  | Left _ <- B64.decode key' = Nothing -- invalid base64
  | otherwise                 = Just $! WSKey key'
  where
    -- strip leading/trailing OWS from key
    key' = fst (BS.spanEnd isOWS (BS.dropWhile isOWS key))

    -- optional whitespace
    isOWS :: Word8 -> Bool
    isOWS 0x09 = True
    isOWS 0x20 = True
    isOWS _    = False

-- | Emit 'SecWebSocketKey' as base64-encoded value suitable for use in the @Sec-WebSocket-Accept@ HTTP header.
--
-- @since 0.1.4.0
secWebSocketKeyToB64 :: SecWebSocketKey -> ByteString
secWebSocketKeyToB64 (WSKey bs) = bs

-- | Construct 'SecWebSocketKey' from two 'Word64' values.
--
-- @since 0.1.4.0
secWebSocketKeyFromWords :: Word64 -> Word64 -> SecWebSocketKey
secWebSocketKeyFromWords h l = WSKey (B64.encode key)
  where
    key = BS.concat $ BL.toChunks $ Bin.runPut (Bin.putWord64be h >> Bin.putWord64be l)

-- upgradeToWebSockets :: HC.Connection
--                     -> (Response -> InputStream ByteString                         -> IO a) -- ^ Non-code @101@ response handler
--                     -> (Response -> InputStream ByteString -> OutputStream Builder -> IO a) -- ^ Code @101@ response handler
--                     -> IO a
-- upgradeToWebSockets = do
--

xor32InputStream :: Word32 -> InputStream ByteString -> IO (InputStream ByteString)
xor32InputStream 0    is = return is
xor32InputStream msk0 is = do
    mskref <- newIORef msk0
    let go = do
          mchunk <- Streams.read is
          case mchunk of
            Nothing -> return Nothing
            Just chunk -> do
              msk <- readIORef mskref
              let (msk',chunk') = xor32StrictByteString' msk chunk
              writeIORef mskref msk'
              return $! Just $! chunk'
    Streams.makeInputStream go

-- xor32OutputStream :: Word32 -> OutputStream ByteString -> IO (OutputStream ByteString)
-- xor32OutputStream msk0 os = do
--     mskref <- newIORef msk0
--     let go Nothing = Streams.write Nothing os
--         go (Just chunk) = do
--               msk <- readIORef mskref
--               let (msk',chunk') = xor32StrictByteString' msk chunk
--               writeIORef mskref msk'
--               Streams.write (Just $! chunk') os
--     Streams.makeOutputStream go

----------------------------------------------------------------------------

-- | Perform an opening WebSocket handshake as per <https://tools.ietf.org/html/rfc6455#section-4 RFC 6455 section 4>
--
-- This operation sets the @host@, @upgrade@, @connection@, @sec-websocket-version@, @sec-websocket-key@ HTTP headers; if you need to customize the handshake request further use the 'BuildRequest'-modifier argument to inject more headers into the request.
--
-- @since 0.1.4.0
wsUpgradeConnection :: Connection -- ^ Connection in HTTP/1.1 protocol state (i.e. not yet upgraded)
                    -> ByteString -- ^ resource name (i.e. the argument to the @GET@ verb)
                    -> RequestBuilder α -- ^ Additional Handshake request builder operations (i.e. to add additional HTTP headers to Handshake HTTP request)
                    -> SecWebSocketKey -- ^ The @sec-websocket-key@ value to use
                    -> (Response -> InputStream ByteString -> IO b) -- ^ failure continuation if handshake fails with a non-101 response code
                    -> (Response -> Connection -> IO b) -- ^ success continuation; the 'Connection' has been succesfully upgraded to WebSocket mode and only WebSocket operations shall be performed over this 'Connection'.
                    -> IO b
wsUpgradeConnection conn resource rqmod wskey failedToUpgrade success = do
    let rqToWS = HC.buildRequest1 $ do
          HC.http HC.GET resource
          HC.setHeader "upgrade" "websocket"
          HC.setHeader "connection" "Upgrade"
          HC.setHeader "sec-websocket-version" "13"
          HC.setHeader "sec-websocket-key" (secWebSocketKeyToB64 wskey)
          rqmod

    HC.sendRequest conn rqToWS HC.emptyBody

    HC.receiveUpgradeResponse conn failedToUpgrade $ \resp _is _os -> do

      case CI.mk `fmap` HC.getHeader resp "connection" of
        Nothing        -> abort "missing 'connection' header"
        Just "upgrade" -> return ()
        Just _         -> abort "'connection' header has non-'upgrade' value"

      case CI.mk `fmap` HC.getHeader resp "upgrade" of
        Nothing          -> abort "missing 'upgrade' header"
        Just "websocket" -> return ()
        Just _           -> abort "'upgrade' header has non-'websocket' value"

      case HC.getHeader resp "sec-websocket-accept" of
        Nothing -> abort "missing 'sec-websocket-accept' header"
        Just wsacc
          | wsacc /= wsKeyToAcceptB64 wskey -> abort "sec-websocket-accept header mismatch"
          | otherwise -> return () -- pass

      success resp conn
  where
    abort msg = throwIO (WsException ("wsUpgradeConnection: "++msg))
