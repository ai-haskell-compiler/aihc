{-# LANGUAGE BangPatterns, CPP, MagicHash, OverloadedStrings #-}
------------------------------------------------------------------------------
-- |
-- Module:      Blaze.ByteString.Builder.HTTP
-- Copyright:   (c) 2013 Simon Meier
-- License:     BSD3
-- Maintainer:  https://github.com/blaze-builder
-- Stability:   stable
--
-- Support for HTTP response encoding.
--
------------------------------------------------------------------------------

module Blaze.ByteString.Builder.HTTP (
  -- * Chunked HTTP transfer encoding
    chunkedTransferEncoding
  , chunkedTransferTerminator
  ) where

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#include "MachDeps.h"
#endif

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
import GHC.Base
import GHC.Word (Word32(..))
#else
import Data.Word
#endif

import Foreign

import qualified Data.ByteString       as S
import Data.ByteString.Char8 ()

import Blaze.ByteString.Builder.Internal.Write
import Data.ByteString.Builder
import Data.ByteString.Builder.Internal
import Blaze.ByteString.Builder.ByteString (copyByteString)

import qualified Blaze.ByteString.Builder.Char8 as Char8

{-# INLINE shiftr_w32 #-}
shiftr_w32 :: Word32 -> Int -> Word32

#if defined(__GLASGOW_HASKELL__) && !defined(__HADDOCK__)
#if MIN_VERSION_base(4,16,0)
-- base >= 4.16 proxy for GHC >= 9.2 which fixes ghc-prim >= 0.8
shiftr_w32 (W32# w) (I# i) = W32# (wordToWord32# ((word32ToWord# w) `uncheckedShiftRL#` i))
#else
shiftr_w32 (W32# w) (I# i) = W32# (w `uncheckedShiftRL#`   i)
#endif
#else
shiftr_w32 = shiftR
#endif


-- | Write a CRLF sequence.
writeCRLF :: Write
writeCRLF = Char8.writeChar '\r' `mappend` Char8.writeChar '\n'
{-# INLINE writeCRLF #-}

-- | Execute a write
{-# INLINE execWrite #-}
execWrite :: Write -> Ptr Word8 -> IO ()
execWrite w op = do
    _ <- runPoke (getPoke w) op
    return ()


------------------------------------------------------------------------------
-- Hex Encoding Infrastructure
------------------------------------------------------------------------------

pokeWord32HexN :: Int -> Word32 -> Ptr Word8 -> IO ()
pokeWord32HexN n0 w0 op0 =
    go w0 (op0 `plusPtr` (n0 - 1))
  where
    go !w !op
      | op < op0  = return ()
      | otherwise = do
          let nibble :: Word8
              nibble = fromIntegral w .&. 0xF
              hex | nibble < 10 = 48 + nibble
                  | otherwise   = 55 + nibble
          poke op hex
          go (w `shiftr_w32` 4) (op `plusPtr` (-1))
{-# INLINE pokeWord32HexN #-}

iterationsUntilZero :: Integral a => (a -> a) -> a -> Int
iterationsUntilZero f = go 0
  where
    go !count 0  = count
    go !count !x = go (count+1) (f x)
{-# INLINE iterationsUntilZero #-}

-- | Length of the hex-string required to encode the given 'Word32'.
word32HexLength :: Word32 -> Int
word32HexLength = max 1 . iterationsUntilZero (`shiftr_w32` 4)
{-# INLINE word32HexLength #-}

-- | Maximum length of a hex string encoding any 'Word32'.
--   Same as @word32HexLength maxBound@.
maxWord32HexLength :: Int
maxWord32HexLength = 8

writeWord32Hex :: Word32 -> Write
writeWord32Hex w =
    boundedWrite maxWord32HexLength (pokeN len $ pokeWord32HexN len w)
  where
    len = word32HexLength w
{-# INLINE writeWord32Hex #-}


------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
chunkedTransferEncoding :: Builder -> Builder
chunkedTransferEncoding innerBuilder =
    builder transferEncodingStep
  where
    transferEncodingStep k =
        go (runBuilder innerBuilder)
      where
        go innerStep !(BufferRange op ope)
          -- FIXME: Assert that outRemaining < maxBound :: Word32
          | outRemaining < minimalBufferSize =
              return $ bufferFull minimalBufferSize op (go innerStep)
          | otherwise = do
              let !brInner@(BufferRange opInner _) = BufferRange
                     (op  `plusPtr` (chunkSizeLength + crlfLength)) -- leave space for chunk header
                     (ope `plusPtr` (-maxAfterBufferOverhead))      -- leave space at end of data

                  -- wraps the chunk, if it is non-empty, and returns the
                  -- signal constructed with the correct end-of-data pointer
                  {-# INLINE wrapChunk #-}
                  wrapChunk :: Ptr Word8 -> (Ptr Word8 -> IO (BuildSignal a))
                            -> IO (BuildSignal a)
                  wrapChunk !opInner' mkSignal
                    | opInner' == opInner = mkSignal op
                    | otherwise           = do
                        pokeWord32HexN chunkSizeLength
                            (fromIntegral $ opInner' `minusPtr` opInner)
                            op
                        execWrite writeCRLF (opInner `plusPtr` (-crlfLength))
                        execWrite writeCRLF opInner'
                        mkSignal (opInner' `plusPtr` crlfLength)

                  -- prepare handlers
                  doneH opInner' _ = wrapChunk opInner' $ \op' -> do
                                         let !br' = BufferRange op' ope
                                         k br'

                  fullH opInner' minRequiredSize nextInnerStep =
                      wrapChunk opInner' $ \op' ->
                        return $! bufferFull
                          (minRequiredSize + maxEncodingOverhead)
                          op'
                          (go nextInnerStep)

                  insertChunkH opInner' bs nextInnerStep
                    | S.null bs =                         -- flush
                        wrapChunk opInner' $ \op' ->
                          return $! insertChunk op' S.empty (go nextInnerStep)

                    | otherwise =                         -- insert non-empty bytestring
                        wrapChunk opInner' $ \op' -> do
                          -- add header for inserted bytestring
                          -- FIXME: assert(S.length bs < maxBound :: Word32)
                          !op'' <- (`runPoke` op') $ getPoke $
                              writeWord32Hex (fromIntegral $ S.length bs)
                              `mappend` writeCRLF

                          -- insert bytestring and write CRLF in next buildstep
                          return $! insertChunk
                            op'' bs
                            (runBuilderWith (fromWrite writeCRLF) $ go nextInnerStep)

              -- execute inner builder with reduced boundaries
              fillWithBuildStep innerStep doneH fullH insertChunkH brInner
          where
            -- minimal size guaranteed for actual data no need to require more
            -- than 1 byte to guarantee progress the larger sizes will be
            -- hopefully provided by the driver or requested by the wrapped
            -- builders.
            minimalChunkSize  = 1

            -- overhead computation which is when (re)sizing the output buffer.
            -- We make sure we have enough space
            -- - at the beginning of the chunk for the chunk length followed by CRLF
            -- - at the end of the chunk for the terminating CRLF and
            --   the chunk header (see above) of the next chunk.
            crlfLength = 2
            maxBeforeBufferOverhead = maxWord32HexLength + crlfLength
            maxAfterBufferOverhead  = crlfLength + maxWord32HexLength + crlfLength

            maxEncodingOverhead = maxBeforeBufferOverhead + maxAfterBufferOverhead

            minimalBufferSize = minimalChunkSize + maxEncodingOverhead

            -- remaining and required space computation
            outRemaining :: Int
            outRemaining    = ope `minusPtr` op
            chunkSizeLength = word32HexLength $ fromIntegral outRemaining


-- | The zero-length chunk '0\r\n\r\n' signaling the termination of the data transfer.
chunkedTransferTerminator :: Builder
chunkedTransferTerminator = copyByteString "0\r\n\r\n"
