{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}

--------------------------------------------------------------------------------
module Codec.Compression.Heatshrink
    (
      -- * decoding
      Decoder
    , DecodeParams(..)
    , defaultDecodeParams
    , decoderWithParams
    , decode
    , decodeWith

      -- * encoding

    ) where


--------------------------------------------------------------------------------
import           Codec.Compression.Heatshrink.Internal
import qualified Data.ByteString          as B
import           Data.ByteString.Internal hiding (ByteString)
import qualified Data.ByteString.Lazy     as BL
import           Foreign.ForeignPtr
import           Foreign.Ptr


--------------------------------------------------------------------------------
data DecodeParams = DecodeParams {
      inputBufferSize :: Int
    , expansionBufferBits :: Int
    , lookAheadBits :: Int
    } deriving (Show, Eq)


--------------------------------------------------------------------------------
defaultDecodeParams :: DecodeParams
defaultDecodeParams = DecodeParams {
                        inputBufferSize = 1024
                      , expansionBufferBits = 12
                      , lookAheadBits = 8
                      }


--------------------------------------------------------------------------------
data Decoder = Decoder !HeatshrinkDecoderTPtr !DecodeParams
             deriving (Eq, Show)


--------------------------------------------------------------------------------
decoderWithParams :: DecodeParams -> Decoder
decoderWithParams p = Decoder ptr p
    where
      ptr = heatshrinkDecoderAlloc
            (inputBufferSize p)
            (expansionBufferBits p)
            (lookAheadBits p)


--------------------------------------------------------------------------------
decodeWith :: DecodeParams -> BL.ByteString -> IO BL.ByteString
decodeWith p bs = decode decoder bs
    where decoder = decoderWithParams p


--------------------------------------------------------------------------------
decode :: Decoder -> BL.ByteString -> IO BL.ByteString
decode decoder bs = do
  let Decoder d _ = decoder
  heatshrinkDecoderReset d
  decodedChunks <- mapM (decodeChunk d) (BL.toChunks bs)
  let decoded = BL.concat decodedChunks
  f <- heatshrinkDecoderFinish d
  case f of
    HsdrFinishDone -> return decoded
    HsdrFinishMore -> do
                      leftover <- pollAll d 4096
                      return $ (BL.concat decodedChunks) `BL.append` leftover
    e -> fail $ show e


--------------------------------------------------------------------------------
decodeChunk :: HeatshrinkDecoderTPtr -> B.ByteString -> IO BL.ByteString
decodeChunk d bs = do
  go d bs BL.empty
    where
      go :: HeatshrinkDecoderTPtr -> B.ByteString -> BL.ByteString -> IO BL.ByteString
      go d bs acc = if bs == B.empty then
                        return acc
                    else do
                      res <- sink d bs
                      case res of
                        (HsdrSinkOk, n) -> do
                                 let rem = B.drop n bs
                                 dec <- pollAll d 4096
                                 go d rem (BL.append acc dec)
                        (e, _) -> fail $ show e


--------------------------------------------------------------------------------
sink :: HeatshrinkDecoderTPtr -> B.ByteString -> IO (HsdSinkRes, Int)
sink d bs = do
  let (PS p o l) = bs
  (st, sz) <- withForeignPtr p (\ptr -> heatshrinkDecoderSink d (plusPtr ptr o) (fromIntegral l))
  return (st, fromIntegral sz)


--------------------------------------------------------------------------------
poll :: HeatshrinkDecoderTPtr -> Int -> IO (HsdPollRes, BL.ByteString)
poll d sz = do
  (bs, st) <- createAndTrim' sz (\p -> do
                                 (s, sz') <- heatshrinkDecoderPoll d p (fromIntegral sz)
                                 return (0, fromIntegral sz', s)
                              )
  return (st, BL.fromStrict bs)


--------------------------------------------------------------------------------
pollAll :: HeatshrinkDecoderTPtr -> Int -> IO (BL.ByteString)
pollAll d sz = pollAll' BL.empty d sz
    where
      pollAll' :: BL.ByteString -> HeatshrinkDecoderTPtr -> Int -> IO BL.ByteString
      pollAll' acc d sz = do
        res <- poll d sz
        case res of
          (HsdrPollMore, !bs) -> pollAll' (BL.append acc bs) d sz
          (HsdrPollEmpty, !bs) -> return $ BL.append acc bs
          (_, _) -> fail "error in pollAll"


--------------------------------------------------------------------------------
