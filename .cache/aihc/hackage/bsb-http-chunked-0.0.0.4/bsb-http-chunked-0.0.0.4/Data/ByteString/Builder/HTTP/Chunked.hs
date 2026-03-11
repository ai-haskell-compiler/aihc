{-# LANGUAGE BangPatterns, MagicHash, OverloadedStrings, ScopedTypeVariables #-}
-- | HTTP/1.1 chunked transfer encoding as defined
-- in [RFC 7230 Section 4.1](https://tools.ietf.org/html/rfc7230#section-4.1)

module Data.ByteString.Builder.HTTP.Chunked (
    chunkedTransferEncoding
  , chunkedTransferTerminator
  ) where

import           Control.Monad                         (void, when)
import           Foreign                               (Ptr, Word8, Word32, (.&.))
import qualified Foreign                               as F

import           Data.ByteString                       (ByteString)
import qualified Data.ByteString                       as S
import           Data.ByteString.Builder               (Builder)
import           Data.ByteString.Builder.Internal      (BufferRange(..), BuildSignal, BuildStep)
import qualified Data.ByteString.Builder.Internal      as B
import qualified Data.ByteString.Builder.Prim          as P
import qualified Data.ByteString.Builder.Prim.Internal as P
import           Data.ByteString.Char8                 () -- For the IsString instance

------------------------------------------------------------------------------
-- CRLF utils
------------------------------------------------------------------------------

{-# INLINE writeCRLF #-}
writeCRLF :: Ptr Word8 -> IO (Ptr Word8)
writeCRLF op = do
    P.runF (P.char8 P.>*< P.char8) ('\r', '\n') op
    pure $ op `F.plusPtr` crlfLength

{-# INLINE crlfBuilder #-}
crlfBuilder :: Builder
crlfBuilder = P.primFixed (P.char8 P.>*< P.char8) ('\r', '\n')

------------------------------------------------------------------------------
-- Hex Encoding Infrastructure
------------------------------------------------------------------------------

-- | Pad the chunk size with leading zeros?
data Padding
  = NoPadding
  | PadTo !Int

{-# INLINE writeWord32Hex #-}
writeWord32Hex :: Padding -> Word32 -> Ptr Word8 -> IO (Ptr Word8)
writeWord32Hex NoPadding w op = writeWord32Hex' (word32HexLength w) w op
writeWord32Hex (PadTo len) w op = writeWord32Hex' len w op

-- | @writeWord32Hex' len w op@ writes the hex encoding of @w@ to @op@ and 
-- returns @op `'F.plusPtr'` len@.
--
-- If writing @w@ doesn't consume all @len@ bytes, leading zeros are added. 
{-# INLINE writeWord32Hex' #-}
writeWord32Hex' :: Int -> Word32 -> Ptr Word8 -> IO (Ptr Word8)
writeWord32Hex' len w0 op0 = do
    go w0 (op0 `F.plusPtr` (len - 1))
    pure $ op0 `F.plusPtr` len
  where
    go !w !op =
        when (op >= op0) $ do
          let nibble :: Word8
              nibble = fromIntegral w .&. 0xF
              hex | nibble < 10 = 48 + nibble
                  | otherwise   = 55 + nibble
          F.poke op hex
          go (w `F.unsafeShiftR` 4) (op `F.plusPtr` (-1))

-- | Length of the hex-string required to encode the given 'Word32'.
{-# INLINE word32HexLength #-}
word32HexLength :: Word32 -> Int
word32HexLength w = maxW32HexLength - (F.countLeadingZeros w `F.unsafeShiftR` 2)

------------------------------------------------------------------------------
-- Constants
------------------------------------------------------------------------------

crlfLength, maxW32HexLength, minimalChunkSize, maxBeforeBufferOverhead,
  maxAfterBufferOverhead, maxEncodingOverhead, minimalBufferSize :: Int
crlfLength = 2
maxW32HexLength = 8 -- 4 bytes, 2 hex digits per byte
minimalChunkSize  = 1
maxBeforeBufferOverhead = maxW32HexLength + crlfLength
maxAfterBufferOverhead = crlfLength + maxW32HexLength + crlfLength
maxEncodingOverhead = maxBeforeBufferOverhead + maxAfterBufferOverhead
minimalBufferSize = minimalChunkSize + maxEncodingOverhead

------------------------------------------------------------------------------
-- Chunked transfer encoding
------------------------------------------------------------------------------

-- | Transform a builder such that it uses chunked HTTP transfer encoding.
--
-- >>> :set -XOverloadedStrings
-- >>> import Data.ByteString.Builder as B
-- >>> let f = B.toLazyByteString . chunkedTransferEncoding . B.lazyByteString
-- >>> f "data"
-- "004\r\ndata\r\n"
--
-- >>> f ""
-- ""
--
-- /Note/: While for many inputs, the bytestring chunks that can be obtained from the output
-- via @'Data.ByteString.Lazy.toChunks' . 'Data.ByteString.Builder.toLazyByteString'@
-- each form a chunk in the sense
-- of [RFC 7230 Section 4.1](https://tools.ietf.org/html/rfc7230#section-4.1),
-- this correspondence doesn't hold in general.
chunkedTransferEncoding :: Builder -> Builder
chunkedTransferEncoding innerBuilder =
    B.builder transferEncodingStep
  where
    transferEncodingStep :: forall a.  BuildStep a -> BuildStep a
    transferEncodingStep k =
        go (B.runBuilder innerBuilder)
      where
        go :: (BufferRange -> IO (BuildSignal _x)) -> BuildStep a
        go innerStep (BufferRange op ope)
          -- FIXME: Assert that outRemaining < maxBound :: Word32
          | outRemaining < minimalBufferSize =
              pure $ B.bufferFull minimalBufferSize op (go innerStep)
          | otherwise =
              -- execute inner builder with reduced boundaries
              B.fillWithBuildStep innerStep doneH fullH insertChunkH brInner
          where
            outRemaining = ope `F.minusPtr` op
            maxChunkSizeLength = word32HexLength $ fromIntegral outRemaining

            !brInner@(BufferRange opInner _) = BufferRange
                (op  `F.plusPtr` (maxChunkSizeLength + crlfLength)) -- leave space for chunk header
                (ope `F.plusPtr` (-maxAfterBufferOverhead))         -- leave space at end of data

            doneH :: Ptr Word8 -> _x
                  -> IO (BuildSignal a)
            doneH opInner' _ =
                wrapChunk opInner' $ \op' ->
                  k $! BufferRange op' ope

            fullH :: Ptr Word8 -> Int -> BuildStep _x
                  -> IO (BuildSignal a)
            fullH opInner' minRequiredSize nextInnerStep =
                wrapChunk opInner' $ \op' ->
                  pure $! B.bufferFull
                    (minRequiredSize + maxEncodingOverhead)
                    op'
                    (go nextInnerStep)

            insertChunkH :: Ptr Word8 -> ByteString -> BuildStep _x
                         -> IO (BuildSignal a)
            insertChunkH opInner' bs nextInnerStep =
                wrapChunk opInner' $ \op' ->
                  if S.null bs                      -- flush
                  then pure $! B.insertChunk op' S.empty (go nextInnerStep)
                  else do                           -- insert non-empty bytestring
                    -- add header for inserted bytestring
                    -- FIXME: assert(S.length bs < maxBound :: Word32)
                    let chunkSize = fromIntegral $ S.length bs
                    !op'' <- writeWord32Hex NoPadding chunkSize op'
                    !op''' <- writeCRLF op''
                    -- insert bytestring and write CRLF in next buildstep
                    pure $! B.insertChunk
                      op''' bs
                      (B.runBuilderWith crlfBuilder $ go nextInnerStep)

            -- wraps the chunk, if it is non-empty, and returns the
            -- signal constructed with the correct end-of-data pointer
            {-# INLINE wrapChunk #-}
            wrapChunk :: Ptr Word8 -> (Ptr Word8 -> IO (BuildSignal a))
                      -> IO (BuildSignal a)
            wrapChunk !chunkDataEnd mkSignal
              | chunkDataEnd == opInner = mkSignal op
              | otherwise = do
                  let chunkSize = fromIntegral $ chunkDataEnd `F.minusPtr` opInner
                  void $ writeWord32Hex (PadTo maxChunkSizeLength) chunkSize op
                  void $ writeCRLF (opInner `F.plusPtr` (-crlfLength))
                  void $ writeCRLF chunkDataEnd
                  mkSignal (chunkDataEnd `F.plusPtr` crlfLength)


-- | The zero-length chunk @0\\r\\n\\r\\n@ signalling the termination of the data transfer.
chunkedTransferTerminator :: Builder
chunkedTransferTerminator = B.byteStringCopy "0\r\n\r\n"
