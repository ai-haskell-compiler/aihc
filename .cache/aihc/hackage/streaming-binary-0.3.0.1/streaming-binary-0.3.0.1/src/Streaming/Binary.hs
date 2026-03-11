-- | This module implements a method to ingest a sequence of "Data.Binary"
-- encoded records using bounded memory. Minimal example:
--
-- > {-# LANGUAGE TypeApplications #-}
-- >
-- > import Data.Function ((&))
-- > import qualified Data.ByteString.Streaming as Q
-- > import Streaming
-- > import Streaming.Binary
-- > import qualified Streaming.Prelude as S
-- >
-- > -- Interpret all bytes on stdin as a sequence of integers.
-- > -- Print them on-the-fly on stdout.
-- > main = Q.getContents & decoded @Int & S.print

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Streaming.Binary
  ( decode
  , decodeWith
  , decoded
  , decodedWith
  , encode
  , encodeWith
  , encoded
  , encodedWith
  ) where

import qualified Data.Binary.Get as Binary
import qualified Data.Binary.Put as Binary
import Data.Binary (Binary(..))
import qualified Data.ByteString.Builder.Extra as BS
import qualified Data.ByteString.Streaming as Q
import Data.ByteString.Streaming (ByteString)
import Data.Int (Int64)
import Streaming
import qualified Streaming.Prelude as S

-- | Decode a single element from a streaming bytestring. Returns any leftover
-- input, the number of bytes consumed, and either an error string or the
-- element if decoding succeeded.
decode
  :: (Binary a, Monad m)
  => ByteString m r
  -> m (ByteString m r, Int64, Either String a)
decode = decodeWith get

-- | Like 'decode', but with an explicitly provided decoder.
decodeWith
  :: Monad m
  => Binary.Get a
  -> ByteString m r
  -> m (ByteString m r, Int64, Either String a)
decodeWith getter = go 0 (Binary.runGetIncremental getter)
  where
    go !total (Binary.Fail leftover nconsumed err) p = do
        return (Q.chunk leftover >> p, total + nconsumed, Left err)
    go !total (Binary.Done leftover nconsumed x) p = do
        return (Q.chunk leftover >> p, total + nconsumed, Right x)
    go !total (Binary.Partial k) p = do
      Q.nextChunk p >>= \case
        Left res -> go total (k Nothing) (return res)
        Right (bs, p') -> go total (k (Just bs)) p'


-- | Decode a sequence of elements from a streaming bytestring. Returns any
-- leftover input, the number of bytes consumed, and either an error string or
-- the return value if there were no errors. Decoding stops at the first error.
decoded
  :: (Binary a, Monad m)
  => ByteString m r
  -> Stream (Of a) m (ByteString m r, Int64, Either String r)
decoded = decodedWith get

-- | Like 'decoded', but with an explicitly provided decoder.
decodedWith
  :: Monad m
  => Binary.Get a
  -> ByteString m r
  -> Stream (Of a) m (ByteString m r, Int64, Either String r)
decodedWith getter = go 0 decoder0
  where
    decoder0 = Binary.runGetIncremental getter
    go !total (Binary.Fail leftover nconsumed err) p = do
        return (Q.chunk leftover >> p, total + nconsumed, Left err)
    go !total (Binary.Done "" nconsumed x) p = do
        S.yield x
        lift (Q.nextChunk p) >>= \case
          Left res -> return (return res, total + nconsumed, Right res)
          Right (bs, p') -> do
            go (total + nconsumed) decoder0 (Q.chunk bs >> p')
    go !total (Binary.Done leftover nconsumed x) p = do
        S.yield x
        go (total + nconsumed) (decoder0 `Binary.pushChunk` leftover) p
    go !total (Binary.Partial k) p = do
      lift (Q.nextChunk p) >>= \case
        Left res -> go total (k Nothing) (return res)
        Right (bs, p') -> go total (k (Just bs)) p'

-- | Encode a single element.
encode
  :: (Binary a, MonadIO m)
  => a
  -> ByteString m ()
encode = encodeWith put

-- | Like 'encode', but with an explicitly provided encoder.
encodeWith
  :: MonadIO m
  => (a -> Binary.Put)
  -> a
  -> ByteString m ()
encodeWith putter x =
    Q.toStreamingByteStringWith
      (BS.untrimmedStrategy BS.smallChunkSize BS.defaultChunkSize)
      (Binary.execPut (putter x))

-- | Encode a stream of elements to a streaming bytestring.
encoded
  :: (Binary a, MonadIO m)
  => Stream (Of a) IO ()
  -> ByteString m ()
encoded = encodedWith put

-- | Like 'encoded', but with an explicitly provided encoder.
encodedWith
  :: MonadIO m
  => (a -> Binary.Put)
  -> Stream (Of a) IO ()
  -> ByteString m ()
encodedWith putter xs =
    hoist liftIO $
    Q.toStreamingByteStringWith strategy $
    Q.concatBuilders $
    S.map (Binary.execPut . putter) xs
  where
    strategy = BS.untrimmedStrategy BS.smallChunkSize BS.defaultChunkSize
