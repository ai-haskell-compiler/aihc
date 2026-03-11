{-# LANGUAGE CPP, Rank2Types, DeriveDataTypeable #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (c) 2006-2008 Duncan Coutts
-- License     :  BSD-style
--
-- Maintainer  :  duncan@haskell.org
-- Stability   :  provisional
-- Portability :  portable (H98 + FFI)
--
-- Pure stream based interface to lower level bzlib wrapper
--
-----------------------------------------------------------------------------
module Codec.Compression.BZip.Internal (
  -- * Pure interface
  compress,
  decompress,

  -- * Monadic incremental interface
  -- ** Incremental compression
  CompressStream(..),
  compressST,
  compressIO,
  foldCompressStream,
  foldCompressStreamWithInput,

  -- ** Incremental decompression
  DecompressStream(..),
  decompressST,
  decompressIO,
  foldDecompressStream,
  foldDecompressStreamWithInput,

  -- * The compression parameter types
  CompressParams(..),
  defaultCompressParams,
  DecompressParams(..),
  defaultDecompressParams,
  Stream.BlockSize(..),
  Stream.WorkFactor(..),
  Stream.MemoryLevel(..),
  ) where

import Prelude hiding (length)
import Control.Monad (when)
import Control.Exception (Exception, throw, assert)
import Control.Monad.ST.Lazy hiding (stToIO)
import Control.Monad.ST.Strict (stToIO)
#if __GLASGOW_HASKELL__ >= 702
import qualified Control.Monad.ST.Unsafe as Unsafe (unsafeIOToST)
#else
import qualified Control.Monad.ST.Strict as Unsafe (unsafeIOToST)
#endif
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Internal as L
import qualified Data.ByteString as S
import qualified Data.ByteString.Internal as S
import Data.Typeable (Typeable)
import GHC.IO (noDuplicate)

import qualified Codec.Compression.BZip.Stream as Stream
import Codec.Compression.BZip.Stream (Stream)

-- | The full set of parameters for compression. The defaults are
-- 'defaultCompressParams'.
--
-- The 'compressBufferSize' is the size of the first output buffer containing
-- the compressed data. If you know an approximate upper bound on the size of
-- the compressed data then setting this parameter can save memory. The default
-- compression output buffer size is @16k@. If your estimate is wrong it does
-- not matter too much, the default buffer size will be used for the remaining
-- chunks.
--
data CompressParams = CompressParams {
  compressBlockSize   :: Stream.BlockSize,
  compressWorkFactor  :: Stream.WorkFactor,
  compressBufferSize  :: Int
} deriving (Show)

-- | The full set of parameters for decompression. The defaults are
-- 'defaultDecompressParams'.
--
-- The 'decompressBufferSize' is the size of the first output buffer,
-- containing the uncompressed data. If you know an exact or approximate upper
-- bound on the size of the decompressed data then setting this parameter can
-- save memory. The default decompression output buffer size is @32k@. If your
-- estimate is wrong it does not matter too much, the default buffer size will
-- be used for the remaining chunks.
--
-- One particular use case for setting the 'decompressBufferSize' is if you
-- know the exact size of the decompressed data and want to produce a strict
-- 'Data.ByteString.ByteString'. The compression and decompression functions
-- use lazy 'Data.ByteString.Lazy.ByteString's but if you set the
-- 'decompressBufferSize' correctly then you can generate a lazy
-- 'Data.ByteString.Lazy.ByteString' with exactly one chunk, which can be
-- converted to a strict 'Data.ByteString.ByteString' in @O(1)@ time using
-- @'Data.ByteString.concat' . 'Data.ByteString.Lazy.toChunks'@.
--
data DecompressParams = DecompressParams {
  decompressMemoryLevel :: Stream.MemoryLevel,
  decompressBufferSize  :: Int
} deriving (Show)

-- | The default set of parameters for compression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultCompressParams :: CompressParams
defaultCompressParams = CompressParams {
  compressBlockSize   = Stream.DefaultBlockSize,
  compressWorkFactor  = Stream.DefaultWorkFactor,
  compressBufferSize  = defaultCompressBufferSize
}

-- | The default set of parameters for decompression. This is typically used with
-- the @compressWith@ function with specific parameters overridden.
--
defaultDecompressParams :: DecompressParams
defaultDecompressParams = DecompressParams {
  decompressMemoryLevel = Stream.DefaultMemoryLevel,
  decompressBufferSize  = defaultDecompressBufferSize
}

-- | The default chunk sizes for the output of compression and decompression
-- are 16k and 32k respectively (less a small accounting overhead).
--
defaultCompressBufferSize, defaultDecompressBufferSize :: Int
defaultCompressBufferSize   = 16 * 1024 - L.chunkOverhead
defaultDecompressBufferSize = 32 * 1024 - L.chunkOverhead


-- | The unfolding of the compression process, where you provide a sequence
-- of uncompressed data chunks as input and receive a sequence of compressed
-- data chunks as output. The process is incremental, in that the demand for
-- input and provision of output are interleaved.
--
data CompressStream m =
     CompressInputRequired {
         compressSupplyInput :: S.ByteString -> m (CompressStream m)
       }

   | CompressOutputAvailable {
        compressOutput :: !S.ByteString,
        compressNext   :: m (CompressStream m)
      }

   | CompressStreamEnd

-- | A fold over the 'CompressStream' in the given monad.
--
-- One way to look at this is that it runs the stream, using callback functions
-- for the three stream events.
--
foldCompressStream :: Monad m
                   => ((S.ByteString -> m a) -> m a)
                   -> (S.ByteString -> m a -> m a)
                   -> m a
                   -> CompressStream m -> m a
foldCompressStream input output end = fold
  where
    fold (CompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (CompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold CompressStreamEnd =
      end

-- | A variant on 'foldCompressStream' that is pure rather than operating in a
-- monad and where the input is provided by a lazy 'L.ByteString'. So we only
-- have to deal with the output and end parts, making it just like a foldr on a
-- list of output chunks.
--
-- For example:
--
-- > toChunks = foldCompressStreamWithInput (:) []
--
foldCompressStreamWithInput :: (S.ByteString -> a -> a)
                            -> a
                            -> (forall s. CompressStream (ST s))
                            -> L.ByteString
                            -> a
foldCompressStreamWithInput chunk end = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (CompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (CompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (CompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold CompressStreamEnd _inchunks =
      return end

compress   :: CompressParams -> L.ByteString -> L.ByteString
compressST :: CompressParams -> CompressStream (ST s)
compressIO :: CompressParams -> CompressStream IO

compress   params = foldCompressStreamWithInput
                      L.Chunk L.Empty
                      (compressStreamST params)
compressST params = compressStreamST params
compressIO params = compressStreamIO params

compressStream
  :: CompressParams -> S.ByteString -> Stream (CompressStream Stream)
compressStream (CompressParams blockSize workFactor initChunkSize) =
    \chunk -> do
      Stream.compressInit blockSize Stream.Silent workFactor
      case chunk of
        _ | S.null chunk -> fillBuffers 14 --bzip2 header is 14 bytes
        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          fillBuffers initChunkSize
  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int -> Stream (CompressStream Stream)
  fillBuffers outChunkSize = do
#ifdef DEBUG
    Stream.consistencyCheck
#endif

    -- in this state there are two possibilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ CompressInputRequired $ \chunk ->
           case chunk of
           _ | S.null chunk          -> drainBuffers True
           S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (CompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress

    let action = if lastChunk then Stream.Finish else Stream.Run
    status <- Stream.compress action

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ CompressOutputAvailable chunk $ do
                    fillBuffers defaultCompressBufferSize
          else do fillBuffers defaultCompressBufferSize

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        assert inputBufferEmpty $ return ()
        outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
        if outputBufferBytesAvailable > 0
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  Stream.finalise
                  return $ CompressOutputAvailable chunk (return CompressStreamEnd)
          else do Stream.finalise
                  return CompressStreamEnd

      Stream.Error _ msg -> fail msg


data DecompressStream m =

     DecompressInputRequired {
         decompressSupplyInput :: S.ByteString -> m (DecompressStream m)
       }

   | DecompressOutputAvailable {
         decompressOutput :: !S.ByteString,
         decompressNext   :: m (DecompressStream m)
       }

   -- | Includes any trailing unconsumed /input/ data.
   | DecompressStreamEnd {
         decompressUnconsumedInput :: S.ByteString
       }

   -- | An error code
   | DecompressStreamError {
         decompressStreamError :: DecompressError
       }

data DecompressError =
     TruncatedInput
   | DataFormatError String
   deriving (Typeable)

instance Show DecompressError where
  show TruncatedInput     = modprefix "premature end of compressed data stream"
  show (DataFormatError detail) = modprefix ("compressed data stream format error (" ++ detail ++ ")")

modprefix :: ShowS
modprefix = ("Codec.Compression.BZip: " ++)

instance Exception DecompressError

foldDecompressStream :: Monad m
                     => ((S.ByteString -> m a) -> m a)
                     -> (S.ByteString -> m a -> m a)
                     -> (S.ByteString -> m a)
                     -> (DecompressError -> m a)
                     -> DecompressStream m -> m a
foldDecompressStream input output end err = fold
  where
    fold (DecompressInputRequired next) =
      input (\x -> next x >>= fold)

    fold (DecompressOutputAvailable outchunk next) =
      output outchunk (next >>= fold)

    fold (DecompressStreamEnd inchunk) = end inchunk
    fold (DecompressStreamError derr)  = err derr

foldDecompressStreamWithInput :: (S.ByteString -> a -> a)
                              -> (L.ByteString -> a)
                              -> (DecompressError -> a)
                              -> (forall s. DecompressStream (ST s))
                              -> L.ByteString
                              -> a
foldDecompressStreamWithInput chunk end err = \s lbs ->
    runST (fold s (L.toChunks lbs))
  where
    fold (DecompressInputRequired next) [] =
      next S.empty >>= \strm -> fold strm []

    fold (DecompressInputRequired next) (inchunk:inchunks) =
      next inchunk >>= \s -> fold s inchunks

    fold (DecompressOutputAvailable outchunk next) inchunks = do
      r <- next >>= \s -> fold s inchunks
      return $ chunk outchunk r

    fold (DecompressStreamEnd inchunk) inchunks =
      return $ end (L.fromChunks (inchunk:inchunks))

    fold (DecompressStreamError derr) _ =
      return $ err derr

decompress   :: DecompressParams -> L.ByteString -> L.ByteString
decompressST :: DecompressParams -> DecompressStream (ST s)
decompressIO :: DecompressParams -> DecompressStream IO

decompress   params = foldDecompressStreamWithInput
                        L.Chunk (const L.Empty) throw
                        (decompressStreamST params)
decompressST params = decompressStreamST params
decompressIO params = decompressStreamIO params

decompressStream
  :: DecompressParams -> S.ByteString -> Stream (DecompressStream Stream)
decompressStream (DecompressParams memLevel initChunkSize) =
    \chunk -> do
      Stream.decompressInit Stream.Silent memLevel
      case chunk of
        _ | S.null chunk -> fillBuffers 4 --always an error anyway
        S.PS inFPtr offset length -> do
          Stream.pushInputBuffer inFPtr offset length
          fillBuffers initChunkSize

  where
    -- we flick between two states:
    --   * where one or other buffer is empty
    --       - in which case we refill one or both
    --   * where both buffers are non-empty
    --       - in which case we compress until a buffer is empty

  fillBuffers :: Int -> Stream (DecompressStream Stream)
  fillBuffers outChunkSize = do

    -- in this state there are two possibilities:
    --   * no outbut buffer space is available
    --       - in which case we must make more available
    --   * no input buffer is available
    --       - in which case we must supply more
    inputBufferEmpty <- Stream.inputBufferEmpty
    outputBufferFull <- Stream.outputBufferFull

    assert (inputBufferEmpty || outputBufferFull) $ return ()

    when outputBufferFull $ do
      outFPtr <- Stream.unsafeLiftIO (S.mallocByteString outChunkSize)
      Stream.pushOutputBuffer outFPtr 0 outChunkSize

    if inputBufferEmpty
      then return $ DecompressInputRequired $ \chunk ->
           case chunk of
             _ | S.null chunk -> drainBuffers True
             S.PS inFPtr offset length -> do
                Stream.pushInputBuffer inFPtr offset length
                drainBuffers False
      else drainBuffers False


  drainBuffers :: Bool -> Stream (DecompressStream Stream)
  drainBuffers lastChunk = do

    inputBufferEmpty' <- Stream.inputBufferEmpty
    outputBufferFull' <- Stream.outputBufferFull
    assert(not outputBufferFull'
       && (lastChunk || not inputBufferEmpty')) $ return ()
    -- this invariant guarantees we can always make forward progress or at
    -- least detect premature EOF

    status <- Stream.decompress

    case status of
      Stream.Ok -> do
        outputBufferFull <- Stream.outputBufferFull
        if outputBufferFull
          then do (outFPtr, offset, length) <- Stream.popOutputBuffer
                  let chunk = S.PS outFPtr offset length
                  return $ DecompressOutputAvailable chunk $ do
                    fillBuffers defaultDecompressBufferSize
          else do -- We need to detect if we ran out of input:
                  inputBufferEmpty <- Stream.inputBufferEmpty
                  if inputBufferEmpty && lastChunk
                    then return (DecompressStreamError TruncatedInput)
                    else fillBuffers defaultDecompressBufferSize

      Stream.StreamEnd -> do
        inputBufferEmpty <- Stream.inputBufferEmpty
        if inputBufferEmpty
          then do finish (DecompressStreamEnd S.empty)
          else do (inFPtr, offset, length) <- Stream.popRemainingInputBuffer
                  let inchunk = S.PS inFPtr offset length
                  finish (DecompressStreamEnd inchunk)

      Stream.Error code msg -> case code of
          Stream.DataError -> finish (DecompressStreamError (DataFormatError msg))
          _                -> fail msg

  finish end = do
    outputBufferBytesAvailable <- Stream.outputBufferBytesAvailable
    if outputBufferBytesAvailable > 0
      then do (outFPtr, offset, length) <- Stream.popOutputBuffer
              return (DecompressOutputAvailable (S.PS outFPtr offset length) (return end))
      else return end


------------------------------------------------------------------------------

mkStateST :: ST s (Stream.State s)
mkStateIO :: IO (Stream.State RealWorld)
mkStateST = strictToLazyST Stream.mkState
mkStateIO = stToIO Stream.mkState

runStreamST :: Stream a -> Stream.State s -> ST s (a, Stream.State s)
runStreamIO :: Stream a -> Stream.State RealWorld -> IO (a, Stream.State RealWorld)
runStreamST strm zstate = strictToLazyST (Unsafe.unsafeIOToST noDuplicate >> Stream.runStream strm zstate)
runStreamIO strm zstate = stToIO (Stream.runStream strm zstate)

compressStreamIO :: CompressParams -> CompressStream IO
compressStreamIO params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateIO
        let next = compressStream params
        (strm', zstate') <- runStreamIO (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State RealWorld -> CompressStream IO
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamIO (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd


compressStreamST :: CompressParams -> CompressStream (ST s)
compressStreamST params =
    CompressInputRequired {
      compressSupplyInput = \chunk -> do
        zstate <- mkStateST
        let next = compressStream params
        (strm', zstate') <- runStreamST (next chunk) zstate
        return (go strm' zstate')
    }
  where
    go :: CompressStream Stream -> Stream.State s -> CompressStream (ST s)
    go (CompressInputRequired next) zstate =
      CompressInputRequired {
        compressSupplyInput = \chunk -> do
          (strm', zstate') <- runStreamST (next chunk) zstate
          return (go strm' zstate')
      }

    go (CompressOutputAvailable chunk next) zstate =
      CompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        return (go strm' zstate')

    go CompressStreamEnd _ = CompressStreamEnd


decompressStreamIO :: DecompressParams -> DecompressStream IO
decompressStreamIO params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateIO
        let next = decompressStream params
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate'
  where
    go :: DecompressStream Stream -> Stream.State RealWorld
       -> IO (DecompressStream IO)
    go (DecompressInputRequired next) zstate =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamIO (next chunk) zstate
        go strm' zstate'

    go (DecompressOutputAvailable chunk next) zstate =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamIO next zstate
        go strm' zstate'

    go (DecompressStreamEnd unconsumed) zstate =
        finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate = finaliseStreamError err zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamIO Stream.finalise zstate
        return (DecompressStreamError err)


decompressStreamST :: DecompressParams -> DecompressStream (ST s)
decompressStreamST params =
      DecompressInputRequired $ \chunk -> do
        zstate <- mkStateST
        let next = decompressStream params
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate'
  where
    go :: DecompressStream Stream -> Stream.State s
       -> ST s (DecompressStream (ST s))
    go (DecompressInputRequired next) zstate =
      return $ DecompressInputRequired $ \chunk -> do
        (strm', zstate') <- runStreamST (next chunk) zstate
        go strm' zstate'

    go (DecompressOutputAvailable chunk next) zstate =
      return $ DecompressOutputAvailable chunk $ do
        (strm', zstate') <- runStreamST next zstate
        go strm' zstate'

    go (DecompressStreamEnd unconsumed) zstate =
        finaliseStreamEnd unconsumed zstate

    go (DecompressStreamError err) zstate = finaliseStreamError err zstate

    finaliseStreamEnd unconsumed zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamEnd unconsumed)

    finaliseStreamError err zstate = do
        _ <- runStreamST Stream.finalise zstate
        return (DecompressStreamError err)
