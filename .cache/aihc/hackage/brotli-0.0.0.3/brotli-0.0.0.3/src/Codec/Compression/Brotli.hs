{-# LANGUAGE BangPatterns       #-}
{-# LANGUAGE CPP                #-}
{-# LANGUAGE Trustworthy        #-}

-- Copyright (C) 2016  Herbert Valerio Riedel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

-- |
-- Module      : Codec.Compression.Brotli
-- Copyright   : © 2016 Herbert Valerio Riedel
--
-- Maintainer  : hvr@gnu.org
--
-- Compression and decompression of data streams in the \"Brotli\" format (<https://tools.ietf.org/html/rfc7932 RFC7932>)
--
module Codec.Compression.Brotli
    ( -- * Simple (de)compression with default parameters
      compress
    , decompress

    , BrotliException(..)

      -- * Extended API with control over parameters
    , compressWith
    , decompressWith

      -- * Monadic incremental (de)compression API
      --
      -- | See <http://hackage.haskell.org/package/zlib-0.6.1.1/docs/Codec-Compression-Zlib-Internal.html#g:2 zlib's incremental API documentation> for more information.

      -- ** Compression
    , CompressStream(..)
    , compressIO
    , compressST

      -- ** Decompression
    , DecompressStream(..)
    , decompressIO
    , decompressST

    , BrotliDecoderErrorCode(..)
    , showBrotliDecoderErrorCode

      -- * Parameters
      -- ** Compression parameters
    , defaultCompressParams

    , CompressParams
    , compressLevel
    , compressWindowSize
    , compressMode
    , compressSizeHint

    , CompressionLevel(..)
    , CompressionWindowSize(..)
    , CompressionMode(..)

      -- ** Decompression parameters
    , defaultDecompressParams

    , DecompressParams
    , decompressDisableRingBufferReallocation
    ) where

import           Control.Applicative
import           Prelude
import           Control.Exception
import           Control.Monad
import           Control.Monad.ST              (stToIO)
import           Control.Monad.ST.Lazy         (ST, runST, strictToLazyST)
import qualified Control.Monad.ST.Strict       as ST.Strict (ST)
import           Control.Monad.ST.Unsafe       (unsafeIOToST)
import           Data.ByteString               (ByteString)
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import qualified Data.ByteString.Lazy.Internal as BSL
import           GHC.IO                        (noDuplicate)

import           LibBrotli

-- | 'Exception' thrown on decoding errors or internal erros
--
-- Note that 'BrotliDecoderErrorCode' will be thrown instead of
-- 'BrotliException' when possible.
newtype BrotliException = BrotliException String deriving (Show)

instance Exception BrotliException

-- | Compress lazy 'ByteString' into Brotli stream using 'defaultCompressParams'.
compress :: BSL.ByteString -> BSL.ByteString
compress = compressWith defaultCompressParams

-- | Like 'compress' but with the ability to specify various compression
-- parameters. Typical usage:
--
-- > compressWith defaultCompressParams { compress... = ... }
compressWith :: CompressParams -> BSL.ByteString -> BSL.ByteString
compressWith parms input = runST (compress' input)
  where
    compress' :: BSL.ByteString -> ST s BSL.ByteString
    compress' ibs0 = loop ibs0 =<< compressST parms
      where
        loop BSL.Empty  CompressStreamEnd =
            pure BSL.Empty
        loop (BSL.Chunk _ _) CompressStreamEnd =
            throwST (BrotliException "internal error")
        loop BSL.Empty (CompressInputRequired _ supply) =
            loop BSL.Empty =<< supply BS.empty
        loop (BSL.Chunk c bs') (CompressInputRequired _ supply) =
            loop bs' =<< supply c
        loop ibs (CompressOutputAvailable oc next) = do
            obs <- loop ibs =<< next
            pure (BSL.chunk oc obs)
{-# NOINLINE compressWith #-}


-- | Decompress lazy 'ByteString' from a Brotli stream.
decompress :: BSL.ByteString -> BSL.ByteString
decompress = decompressWith defaultDecompressParams

-- | Like 'decompress' but with the ability to specify various decompression
-- parameters. Typical usage:
--
-- > decompressWith defaultDecompressParams { decompress... = ... }
decompressWith :: DecompressParams -> BSL.ByteString -> BSL.ByteString
decompressWith parms input = runST (decompress' input)
  where
    decompress' :: BSL.ByteString -> ST s BSL.ByteString
    decompress' ibs0 = loop ibs0 =<< decompressST parms
      where
        loop BSL.Empty  (DecompressStreamEnd rest)
          | BS.null rest = pure BSL.Empty
          | otherwise = throwST (BrotliException "extra trailing data")
        loop (BSL.Chunk _ _) (DecompressStreamEnd _) =
            throwST (BrotliException "extra trailing data")
        loop _ (DecompressStreamError ec) =
            throwST ec
        loop BSL.Empty (DecompressInputRequired supply) =
            loop BSL.Empty =<< supply BS.empty
        loop (BSL.Chunk c bs') (DecompressInputRequired supply) =
            loop bs' =<< supply c
        loop ibs (DecompressOutputAvailable oc next) = do
            obs <- loop ibs =<< next
            pure (BSL.chunk oc obs)
{-# NOINLINE decompressWith #-}

----------------------------------------------------------------------------

data CompressStream m =
     CompressInputRequired {- flush -}  (m (CompressStream m))
                           {- supply -} (ByteString -> m (CompressStream m))
       -- ^ Compression process requires input to proceed. You can
       -- either flush the stream (first field), supply an input chunk
       -- (second field), or signal the end of input (via empty
       -- chunk).
   | CompressOutputAvailable !ByteString (m (CompressStream m)) -- ^ Output chunk available.
   | CompressStreamEnd

-- | Incremental compression in the 'IO' monad.
compressIO :: CompressParams -> IO (CompressStream IO)
compressIO parms = stToIO (newBrotliEncoder parms)
                   >>= maybe (throwIO (BrotliException "failed to initialize encoder")) go
  where
    bUFSIZ = 32752

    go ls = pure inputRequired
      where
        inputRequired = CompressInputRequired goFlush (withChunk goFinish goInput)

        unexpectedState = throwIO (BrotliException "internal error (unexpected state)")
        encoderFailure  = throwIO (BrotliException "encoder failure")
        internalError   = throwIO (BrotliException "internal error")

        goInput chunk = do -- assert (not $ BS.null chunk)
            (rc, unused) <- stToIO (runBrotliEncoder ls chunk BrotliEncOpProcess)

            let chunk' = BS.drop used chunk
                used   = BS.length chunk - unused

            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> unexpectedState
                BSNeedsInput    -> do unless (used > 0) internalError
                                      withChunk (pure inputRequired) goInput chunk'

                BSHasOutput     -> drainOutput (withChunk (pure inputRequired) goInput chunk') unexpectedState

        goFlush  = do
            (rc, 0) <- stToIO (runBrotliEncoder ls mempty BrotliEncOpFlush)
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> unexpectedState
                BSNeedsInput    -> unexpectedState
                BSHasOutput     -> drainOutput (pure inputRequired) unexpectedState

        goFinish = do
            (rc, 0) <- stToIO (runBrotliEncoder ls mempty BrotliEncOpFinish)
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> do
                  !() <- stToIO (finalizeBrotliEncoder ls)
                  pure CompressStreamEnd
                BSNeedsInput    -> unexpectedState
                BSHasOutput     -> drainOutput unexpectedState (pure CompressStreamEnd)

        drainOutput needsInputCont finishedCont = do
            (rc, obuf) <- stToIO (readBrotliEncoder ls bUFSIZ)
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSHasOutput     -> do
                  pure (CompressOutputAvailable obuf (drainOutput needsInputCont finishedCont))
                BSNeedsInput    -> do
                  pure (CompressOutputAvailable obuf needsInputCont)
                BSFinished      -> do
                  !() <- stToIO (finalizeBrotliEncoder ls)
                  pure (CompressOutputAvailable obuf finishedCont)


-- | Incremental compression in the lazy 'ST' monad.
compressST :: CompressParams -> ST s (CompressStream (ST s))
compressST parms = strictToLazyST (newBrotliEncoder parms)
                   >>= maybe (throwST (BrotliException "failed to initialize encoder")) go
  where
    bUFSIZ = 32752

    go ls = pure inputRequired
      where
        inputRequired = CompressInputRequired goFlush (withChunk goFinish goInput)

        unexpectedState = throwST (BrotliException "internal error (unexpected state)")
        encoderFailure  = throwST (BrotliException "encoder failure")
        internalError   = throwST (BrotliException "internal error")

        goInput :: ByteString -> ST s (CompressStream (ST s))
        goInput chunk = do -- assert (not $ BS.null chunk)
            (rc, unused) <- strictToLazyST (noDuplicateST >> runBrotliEncoder ls chunk BrotliEncOpProcess)

            let chunk' = BS.drop used chunk
                used   = BS.length chunk - unused

            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> unexpectedState
                BSNeedsInput    -> do unless (used > 0) internalError
                                      withChunk (pure inputRequired) goInput chunk'

                BSHasOutput     -> drainOutput (withChunk (pure inputRequired) goInput chunk') unexpectedState

        goFlush :: ST s (CompressStream (ST s))
        goFlush  = do
            (rc, n) <- strictToLazyST (noDuplicateST >> runBrotliEncoder ls mempty BrotliEncOpFlush)
            unless (n == 0) internalError
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> unexpectedState
                BSNeedsInput    -> unexpectedState
                BSHasOutput     -> drainOutput (pure inputRequired) unexpectedState

        goFinish :: ST s (CompressStream (ST s))
        goFinish = do
            (rc, n) <- strictToLazyST (noDuplicateST >> runBrotliEncoder ls mempty BrotliEncOpFinish)
            unless (n == 0) internalError
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSFinished      -> do
                  !() <- strictToLazyST (noDuplicateST >> finalizeBrotliEncoder ls)
                  pure CompressStreamEnd
                BSNeedsInput    -> unexpectedState
                BSHasOutput     -> drainOutput unexpectedState (pure CompressStreamEnd)

        drainOutput :: ST s (CompressStream (ST s)) -> ST s (CompressStream (ST s)) -> ST s (CompressStream (ST s))
        drainOutput needsInputCont finishedCont = do
            (rc, obuf) <- strictToLazyST (noDuplicateST >> readBrotliEncoder ls bUFSIZ)
            case rc of
                BSFail          -> encoderFailure
                BSInternalError -> internalError
                BSHasOutput     -> do
                  pure (CompressOutputAvailable obuf (drainOutput needsInputCont finishedCont))
                BSNeedsInput    -> do
                  pure (CompressOutputAvailable obuf needsInputCont)
                BSFinished      -> do
                  !() <- strictToLazyST (noDuplicateST >> finalizeBrotliEncoder ls)
                  pure (CompressOutputAvailable obuf finishedCont)

data DecompressStream m =
     DecompressInputRequired (ByteString -> m (DecompressStream m)) -- ^ Decoding process requires input to proceed. An empty 'ByteString' chunk signals end of input.
   | DecompressOutputAvailable !ByteString (m (DecompressStream m)) -- ^ Decompressed output chunk available.
   | DecompressStreamEnd ByteString -- ^ Decoded stream is finished. Any unconsumed leftovers from the input stream are returned via the 'ByteString' field
   | DecompressStreamError !BrotliDecoderErrorCode

-- | Incremental decompression in the 'IO' monad.
decompressIO :: DecompressParams -> IO (DecompressStream IO)
decompressIO parms = stToIO (newBrotliDecoder parms)
                     >>= maybe (throwIO (BrotliException "failed to initialize decoder")) go
  where
    bUFSIZ = 32752

    go ls = pure inputRequired
      where
        inputRequired = DecompressInputRequired (withChunk goFinish goInput)

        unexpectedState = throwIO (BrotliException "internal error (unexpected state)")
        internalError   = throwIO (BrotliException "internal error")

        truncatedError = DecompressStreamError (BrotliDecoderErrorCode 2)

        goInput chunk = do -- assert (not $ BS.null chunk)
            (rc, ecode, unused) <- stToIO (runBrotliDecoder ls chunk)

            let chunk' = BS.drop used chunk
                used   = BS.length chunk - unused

            case rc of
                BSFail          -> pure (DecompressStreamError ecode)
                BSInternalError -> internalError
                BSFinished      -> pure (DecompressStreamEnd chunk')
                BSNeedsInput    -> do
                  unless (used > 0) internalError
                  withChunk (pure inputRequired) goInput chunk'

                BSHasOutput     -> drainOutput (withChunk (pure inputRequired) goInput chunk')
                                               (pure (DecompressStreamEnd chunk'))

        goFinish = do
            (rc, ecode, 0) <- stToIO (runBrotliDecoder ls mempty)
            case rc of
                BSFail          -> pure (DecompressStreamError ecode)
                BSInternalError -> internalError
                BSFinished      -> do
                  !() <- stToIO (finalizeBrotliDecoder ls)
                  pure (DecompressStreamEnd mempty)
                BSNeedsInput    -> pure truncatedError
                BSHasOutput     -> drainOutput (pure truncatedError)
                                               (pure (DecompressStreamEnd mempty))

        drainOutput needsInputCont finishedCont = do
            (rc, obuf) <- stToIO (readBrotliDecoder ls bUFSIZ)
            case rc of
              BSFail          -> unexpectedState -- cannot happen
              BSInternalError -> internalError
              BSHasOutput     ->
                pure (DecompressOutputAvailable obuf (drainOutput needsInputCont finishedCont))
              BSNeedsInput    ->
                pure (DecompressOutputAvailable obuf needsInputCont)
              BSFinished      -> do
                !() <- stToIO (finalizeBrotliDecoder ls)
                pure (DecompressOutputAvailable obuf finishedCont)


-- | Incremental decompression in the lazy 'ST' monad.
decompressST :: DecompressParams -> ST s (DecompressStream (ST s))
decompressST parms = strictToLazyST (newBrotliDecoder parms)
                     >>= maybe (throwST (BrotliException "failed to initialize decoder")) go
  where
    bUFSIZ = 32752

    go ls = pure inputRequired
      where
        inputRequired = DecompressInputRequired (withChunk goFinish goInput)

        unexpectedState = throwST (BrotliException "internal error (unexpected state)")
        internalError   = throwST (BrotliException "internal error")

        truncatedError = DecompressStreamError (BrotliDecoderErrorCode 2)

        goInput :: ByteString -> ST s (DecompressStream (ST s))
        goInput chunk = do -- assert (not $ BS.null chunk)
            (rc, ecode, unused) <- strictToLazyST (noDuplicateST >> runBrotliDecoder ls chunk)

            let chunk' = BS.drop used chunk
                used   = BS.length chunk - unused

            case rc of
                BSFail          -> pure (DecompressStreamError ecode)
                BSInternalError -> internalError
                BSFinished      -> pure (DecompressStreamEnd chunk')
                BSNeedsInput    -> do
                  unless (used > 0) internalError
                  withChunk (pure inputRequired) goInput chunk'

                BSHasOutput     -> drainOutput (withChunk (pure inputRequired) goInput chunk')
                                               (pure (DecompressStreamEnd chunk'))

        goFinish :: ST s (DecompressStream (ST s))
        goFinish = do
            (rc, ecode, n) <- strictToLazyST (noDuplicateST >> runBrotliDecoder ls mempty)
            unless (n == 0) internalError
            case rc of
                BSFail          -> pure (DecompressStreamError ecode)
                BSInternalError -> internalError
                BSFinished      -> do
                  !() <- strictToLazyST (noDuplicateST >> finalizeBrotliDecoder ls)
                  pure (DecompressStreamEnd mempty)
                BSNeedsInput    -> pure truncatedError
                BSHasOutput     -> drainOutput (pure truncatedError)
                                               (pure (DecompressStreamEnd mempty))

        drainOutput :: ST s (DecompressStream (ST s)) -> ST s (DecompressStream (ST s)) -> ST s (DecompressStream (ST s))
        drainOutput needsInputCont finishedCont = do
            (rc, obuf) <- strictToLazyST (noDuplicateST >> readBrotliDecoder ls bUFSIZ)
            case rc of
              BSFail          -> unexpectedState -- cannot happen
              BSInternalError -> internalError
              BSHasOutput     ->
                pure (DecompressOutputAvailable obuf (drainOutput needsInputCont finishedCont))
              BSNeedsInput    ->
                pure (DecompressOutputAvailable obuf needsInputCont)
              BSFinished      -> do
                !() <- strictToLazyST (noDuplicateST >> finalizeBrotliDecoder ls)
                pure (DecompressOutputAvailable obuf finishedCont)

-- | Small 'maybe'-ish helper distinguishing between empty and
-- non-empty 'ByteString's
withChunk :: t -> (ByteString -> t) -> ByteString -> t
withChunk emptyChunk nemptyChunk chunk
  | BS.null chunk = emptyChunk
  | otherwise     = nemptyChunk chunk

-- | See <https://github.com/haskell/zlib/issues/7>
noDuplicateST :: ST.Strict.ST s ()
noDuplicateST = unsafeIOToST noDuplicate

throwST :: Exception e => e -> ST s a
throwST = throw
