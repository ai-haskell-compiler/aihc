{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

{-|
Module      : Codec.Compression.LZ4.Conduit
Description : Conduit for the lz4 compression codec.
Copyright   : (c) Niklas Hambüchen, 2020
License     : MIT
Maintainer  : mail@nh2.me
Stability   : stable


Help Wanted / TODOs

Please feel free to send me a pull request for any of the following items:

* TODO Block checksumming

* TODO Dictionary support

* TODO Performance:
   Write a version of `compress` that emits ByteStrings of known
   constant length. That will allow us to do compression in a zero-copy
   fashion, writing compressed bytes directly into a the ByteStrings
   (e.g using `unsafePackMallocCString` or equivalent).
   We currently don't do that (instead, use allocaBytes + copying packCStringLen)
   to ensure that the ByteStrings generated are as compact as possible
   (for the case that `written < size`), since the current `compress`
   conduit directly yields the outputs of LZ4F_compressUpdate()
   (unless they are of 0 length when they are buffered in the context
   tmp buffer).

* TODO Try enabling checksums, then corrupt a bit and see if lz4c detects it.

* TODO Add `with*` style bracketed functions for creating the
   LZ4F_createCompressionContext and Lz4FramePreferencesPtr
   for prompt resource release,
   in addition to the GC'd variants below.
   This would replace our use of `finalizeForeignPtr` in the conduit.
   `finalizeForeignPtr` seems almost as good, but note that it
   doesn't guarantee prompt resource release on exceptions;
   a `with*` style function that uses `bracket` does.
   However, it isn't clear yet which one would be faster
   (what the cost of `mask` is compared to foreign pointer finalizers).
   Also note that prompt freeing has side benefits,
   such as reduced malloc() fragmentation (the closer malloc()
   and free() are to each other, the smaller is the chance to
   have malloc()s on top of the our malloc() in the heap,
   thus the smaller the chance that we cannot decrease the
   heap pointer upon free() (because "mallocs on top" render
   heap memory unreturnable to the OS; memory fragmentation).
-}

module Codec.Compression.LZ4.Conduit
  ( Lz4FrameException(..)
  , BlockSizeID(..)
  , BlockMode(..)
  , ContentChecksum(..)
  , BlockChecksum(..)
  , FrameType(..)
  , FrameInfo(..)
  , Preferences(..)

  , lz4DefaultPreferences

  , compress
  , compressYieldImmediately
  , compressWithOutBufferSize

  , decompress

  , bsChunksOf

  -- * Internals
  , Lz4FrameCompressionContext(..)
  , ScopedLz4FrameCompressionContext(..)
  , ScopedLz4FramePreferencesPtr(..)
  , Lz4FramePreferencesPtr(..)
  , Lz4FrameDecompressionContext(..)
  , lz4fCreatePreferences
  , lz4fCreateCompressonContext
  , lz4fCreateDecompressionContext
  , withScopedLz4fPreferences
  , withScopedLz4fCompressionContext
  ) where

import           UnliftIO.Exception (throwString, bracket)
import           Control.Monad (foldM, when)
import           Control.Monad.IO.Unlift (MonadUnliftIO)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Resource (MonadResource)
import           Data.Bits (testBit)
import           Data.ByteString (ByteString, packCStringLen)
import           Data.ByteString.Unsafe (unsafePackCString, unsafeUseAsCStringLen)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as BSL
import           Data.Conduit
import qualified Data.Conduit.Binary as CB
import           Data.Monoid ((<>))
import           Foreign.C.Types (CChar, CSize)
import           Foreign.ForeignPtr (ForeignPtr, addForeignPtrFinalizer, mallocForeignPtr, mallocForeignPtrBytes, finalizeForeignPtr, withForeignPtr)
import           Foreign.Marshal.Alloc (alloca, allocaBytes, malloc, free)
import           Foreign.Marshal.Array (mallocArray, reallocArray)
import           Foreign.Marshal.Utils (with, new)
import           Foreign.Ptr (Ptr, nullPtr, FunPtr, plusPtr)
import           Foreign.Storable (Storable(..), poke)
import           GHC.Stack (HasCallStack)
import qualified Language.C.Inline as C
import qualified Language.C.Inline.Context as C
import qualified Language.C.Inline.Unsafe as CUnsafe
import           Text.RawString.QQ

import           Codec.Compression.LZ4.CTypes (LZ4F_cctx, LZ4F_dctx, lz4FrameTypesTable, Lz4FrameException(..), BlockSizeID(..), BlockMode(..), ContentChecksum(..), BlockChecksum(..), FrameType(..), FrameInfo(..), Preferences(..))

#include "lz4frame.h"

C.context (C.baseCtx <> C.fptrCtx <> mempty { C.ctxTypesTable = lz4FrameTypesTable })

C.include "<lz4frame.h>"
C.include "<stdlib.h>"
C.include "<stdio.h>"


newtype Lz4FrameCompressionContext = Lz4FrameCompressionContext { unLz4FrameCompressionContext :: ForeignPtr (Ptr LZ4F_cctx) }
  deriving (Eq, Ord, Show)

newtype ScopedLz4FrameCompressionContext = ScopedLz4FrameCompressionContext { unScopedLz4FrameCompressionContext :: Ptr LZ4F_cctx }
  deriving (Eq, Ord, Show)

newtype ScopedLz4FramePreferencesPtr = ScopedLz4FramePreferencesPtr { unScopedLz4FramePreferencesPtr :: Ptr Preferences }
  deriving (Eq, Ord, Show)

newtype Lz4FramePreferencesPtr = Lz4FramePreferencesPtr { unLz4FramePreferencesPtr :: ForeignPtr Preferences }
  deriving (Eq, Ord, Show)

newtype Lz4FrameDecompressionContext = Lz4FrameDecompressionContext { unLz4FrameDecompressionContext :: ForeignPtr (Ptr LZ4F_dctx) }
  deriving (Eq, Ord, Show)


handleLz4Error :: (HasCallStack, MonadUnliftIO m) => IO CSize -> m CSize
handleLz4Error f = do
  ret <- liftIO f
  -- We use an unsafe foreign call here so that it's fast (it cannot block).
  staticErrMsgPtr <- liftIO [CUnsafe.exp| const char * {
    LZ4F_isError($(size_t ret))
      ? LZ4F_getErrorName($(size_t ret))
      : NULL
  } |]
  if staticErrMsgPtr == nullPtr
    then return ret
    else do
      -- LZ4F error strings are static memory so they don't need to be GC'd,
      -- so we should use `unsafePackCString` here.
      errMsgBs <- liftIO $ unsafePackCString staticErrMsgPtr
      throwString ("lz4frame error: " ++ BS8.unpack errMsgBs)


C.verbatim [r|
void haskell_lz4_freeCompressionContext(LZ4F_cctx** ctxPtr)
{
  // We know ctxPtr can be dereferenced because it was created with
  // mallocForeignPtr, so it is always pointing to something valid
  // during the lifetime of the ForeignPtr (and this function is
  // a finalizer function, which is called only at the very end
  // inside this lifetime).
  LZ4F_cctx* ctx = *ctxPtr;
  // See note [Initialize LZ4 context pointer to NULL]:
  // If ctx is null, we never made a successful call to
  // LZ4F_createCompressionContext().
  // Note at the time of writing the implementation of
  // LZ4F_createCompressionContext() handles null pointers gracefully,
  // but that is an undocumented implementation detail so we don't
  // rely on it here.
  if (ctx != NULL)
  {
    size_t err = LZ4F_freeCompressionContext(ctx);
    if (LZ4F_isError(err))
    {
      fprintf(stderr, "LZ4F_freeCompressionContext failed: %s\n", LZ4F_getErrorName(err));
      exit(1);
    }
  }
}
|]

foreign import ccall "&haskell_lz4_freeCompressionContext" haskell_lz4_freeCompressionContext :: FunPtr (Ptr (Ptr LZ4F_cctx) -> IO ())


allocateLz4fScopedCompressionContext :: IO ScopedLz4FrameCompressionContext
allocateLz4fScopedCompressionContext = do
  alloca $ \(ctxPtrPtr :: Ptr (Ptr LZ4F_cctx)) -> do
    _ <- handleLz4Error [C.block| size_t {
      LZ4F_cctx** ctxPtr = $(LZ4F_cctx** ctxPtrPtr);
      LZ4F_errorCode_t err = LZ4F_createCompressionContext(ctxPtr, LZ4F_VERSION);
      return err;
    } |]
    ctxPtr <- peek ctxPtrPtr
    return (ScopedLz4FrameCompressionContext ctxPtr)


freeLz4ScopedCompressionContext :: ScopedLz4FrameCompressionContext -> IO ()
freeLz4ScopedCompressionContext (ScopedLz4FrameCompressionContext ctxPtr) = do
  _ <- handleLz4Error
    [C.block| size_t {
      return LZ4F_freeCompressionContext($(LZ4F_cctx* ctxPtr));
    } |]
  return ()


withScopedLz4fCompressionContext :: (HasCallStack) => (ScopedLz4FrameCompressionContext -> IO a) -> IO a
withScopedLz4fCompressionContext f =
  bracket
    allocateLz4fScopedCompressionContext
    freeLz4ScopedCompressionContext
    f


lz4fCreateCompressonContext :: (HasCallStack) => IO Lz4FrameCompressionContext
lz4fCreateCompressonContext = do
  ctxForeignPtr :: ForeignPtr (Ptr LZ4F_cctx) <- mallocForeignPtr
  -- Note [Initialize LZ4 context pointer to NULL]:
  -- We explicitly set it to NULL so that in the finalizer we know
  -- whether there is a context to free or not.
  withForeignPtr ctxForeignPtr $ \ptr -> poke ptr nullPtr

  -- Attach finalizer *before* we call LZ4F_createCompressionContext(),
  -- to ensure there cannot be a time where the context was created
  -- but not finalizer is attached (receiving an async exception at
  -- that time would make us leak memory).
  addForeignPtrFinalizer haskell_lz4_freeCompressionContext ctxForeignPtr
  -- TODO The whole idea above is to avoid `mask`.
  --      But we should check if `addForeignPtrFinalizer` itself is actually
  --      async exception safe; if not, this is pointless.

  _ <- handleLz4Error [C.block| size_t {
    LZ4F_cctx** ctxPtr = $fptr-ptr:(LZ4F_cctx** ctxForeignPtr);
    LZ4F_errorCode_t err = LZ4F_createCompressionContext(ctxPtr, LZ4F_VERSION);
    return err;
  } |]
  return (Lz4FrameCompressionContext ctxForeignPtr)


lz4DefaultPreferences :: Preferences
lz4DefaultPreferences =
  Preferences
    { frameInfo = FrameInfo
      { blockSizeID = LZ4F_default
      , blockMode = LZ4F_blockLinked
      , contentChecksumFlag = LZ4F_noContentChecksum
      , frameType = LZ4F_frame
      , contentSize = 0
      , dictID = 0
      , blockChecksumFlag = LZ4F_noBlockChecksum
      }
    , compressionLevel = 0
    , autoFlush = False
    , favorDecSpeed = False
    }


newForeignPtr :: (Storable a) => a -> IO (ForeignPtr a)
newForeignPtr x = do
  fptr <- mallocForeignPtr
  withForeignPtr fptr $ \ptr -> poke ptr x
  return fptr


lz4fCreatePreferences :: IO Lz4FramePreferencesPtr
lz4fCreatePreferences =
  Lz4FramePreferencesPtr <$> newForeignPtr lz4DefaultPreferences


withScopedLz4fPreferences :: (HasCallStack) => (ScopedLz4FramePreferencesPtr -> IO a) -> IO a
withScopedLz4fPreferences f =
  bracket
    (ScopedLz4FramePreferencesPtr <$> new lz4DefaultPreferences)
    (\(ScopedLz4FramePreferencesPtr ptr) -> free ptr)
    f


lz4fCompressBegin :: (HasCallStack) => ScopedLz4FrameCompressionContext -> ScopedLz4FramePreferencesPtr -> Ptr CChar -> CSize -> IO CSize
lz4fCompressBegin (ScopedLz4FrameCompressionContext ctx) (ScopedLz4FramePreferencesPtr prefsPtr) headerBuf headerBufLen = do
  headerSize <- handleLz4Error [C.block| size_t {

    LZ4F_preferences_t* lz4_preferences_ptr = $(LZ4F_preferences_t* prefsPtr);

    size_t err_or_headerSize = LZ4F_compressBegin($(LZ4F_cctx* ctx), $(char* headerBuf), $(size_t headerBufLen), lz4_preferences_ptr);
    return err_or_headerSize;
  } |]

  return headerSize


lz4fCompressBound :: (HasCallStack) => CSize -> ScopedLz4FramePreferencesPtr -> IO CSize
lz4fCompressBound srcSize (ScopedLz4FramePreferencesPtr prefsPtr) = do
  handleLz4Error [C.block| size_t {
    size_t err_or_frame_size = LZ4F_compressBound($(size_t srcSize), $(LZ4F_preferences_t* prefsPtr));
    return err_or_frame_size;
  } |]


-- | TODO allow passing in cOptPtr instead of NULL.
lz4fCompressUpdate :: (HasCallStack) => ScopedLz4FrameCompressionContext -> Ptr CChar -> CSize -> Ptr CChar -> CSize -> IO CSize
lz4fCompressUpdate (ScopedLz4FrameCompressionContext ctx) destBuf destBufLen srcBuf srcBufLen = do
  written <- handleLz4Error [C.block| size_t {
    size_t err_or_written = LZ4F_compressUpdate($(LZ4F_cctx* ctx), $(char* destBuf), $(size_t destBufLen), $(char* srcBuf), $(size_t srcBufLen), NULL);
    return err_or_written;
  } |]
  return written


-- | TODO allow passing in cOptPtr instead of NULL.
lz4fCompressEnd :: (HasCallStack) => ScopedLz4FrameCompressionContext -> Ptr CChar -> CSize -> IO CSize
lz4fCompressEnd (ScopedLz4FrameCompressionContext ctx) footerBuf footerBufLen = do
  footerWritten <- handleLz4Error [C.block| size_t {
    size_t err_or_footerWritten = LZ4F_compressEnd($(LZ4F_cctx* ctx), $(char* footerBuf), $(size_t footerBufLen), NULL);
    return err_or_footerWritten;
  } |]
  return footerWritten


-- | Note [Single call to LZ4F_compressUpdate() can create multiple blocks]
-- A single call to LZ4F_compressUpdate() can create multiple blocks,
-- and handles buffers > 32-bit sizes; see:
--   https://github.com/lz4/lz4/blob/52cac9a97342641315c76cfb861206d6acd631a8/lib/lz4frame.c#L601
-- So we don't need to loop around LZ4F_compressUpdate() to compress
-- an arbitrarily large amount of input data, as long as the destination
-- buffer is large enough.


compress :: (MonadUnliftIO m, MonadResource m) => ConduitT ByteString ByteString m ()
compress = compressWithOutBufferSize 0


withLz4CtxAndPrefsConduit ::
  (MonadUnliftIO m, MonadResource m)
  => ((ScopedLz4FrameCompressionContext, ScopedLz4FramePreferencesPtr) -> ConduitT i o m r)
  -> ConduitT i o m r
withLz4CtxAndPrefsConduit f = bracketP
  (do
    ctx <- allocateLz4fScopedCompressionContext
    prefPtr <- new lz4DefaultPreferences
    return (ctx, ScopedLz4FramePreferencesPtr prefPtr)
  )
  (\(ctx, ScopedLz4FramePreferencesPtr prefPtr) -> do
    freeLz4ScopedCompressionContext ctx
    free prefPtr
  )
  f


-- | Compresses the incoming stream of ByteStrings with the lz4 frame format.
--
-- Yields every LZ4 output as a ByteString as soon as the lz4 frame
-- library produces it.
--
-- Note that this does not imply ZL4 frame autoFlush (which affects
-- when the lz4 frame library produces outputs).
compressYieldImmediately :: (MonadUnliftIO m, MonadResource m) => ConduitT ByteString ByteString m ()
compressYieldImmediately =
  withLz4CtxAndPrefsConduit $ \(ctx, prefs) -> do
    let _LZ4F_HEADER_SIZE_MAX = #{const LZ4F_HEADER_SIZE_MAX}

    -- Header

    headerBs <- liftIO $ allocaBytes (fromIntegral _LZ4F_HEADER_SIZE_MAX) $ \headerBuf -> do
      headerSize <- lz4fCompressBegin ctx prefs headerBuf _LZ4F_HEADER_SIZE_MAX
      packCStringLen (headerBuf, fromIntegral headerSize)

    yield headerBs

    -- Chunks

    awaitForever $ \bs -> do


      m'outBs <- liftIO $ unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) -> do
        let bsLenSize = fromIntegral bsLen

        size <- lz4fCompressBound bsLenSize prefs

        -- TODO Performance: Check if reusing this buffer obtained with `allocaBytes`
        --      makes it faster.
        --      LZ4F_compressBound() always returns a number > the block size (e.g. 256K),
        --      even when the input size passed to it is just a few bytes.
        --      As a result, we allocate at least a full block size each time
        --      (and `allocaBytes` calls `malloc()`), but not using most of it.
        --      Worse, with autoflush=0, most small inputs go into the context buffer,
        --      in which case the `allocaBytes` is completely wasted.
        --      This could be avoided by keeping the last `allocaBytes` buffer around,
        --      and reusing it if it is big enough for the number returned by
        --      LZ4F_compressUpdate() next time.
        --      That should avoid most allocations in the case that we `await` lots of
        --      small ByteStrings.
        m'outBs <- liftIO $ allocaBytes (fromIntegral size) $ \buf -> do

          -- See note [Single call to LZ4F_compressUpdate() can create multiple blocks].
          written <- lz4fCompressUpdate ctx buf size bsPtr bsLenSize

          if written == 0 -- everything fit into the context buffer, no new compressed data was emitted
            then return Nothing
            else Just <$> packCStringLen (buf, fromIntegral written)

        return m'outBs

      case m'outBs of
        Nothing -> return ()
        Just outBs -> yield outBs

    -- Footer

    -- Passing srcSize==0 provides bound for LZ4F_compressEnd(),
    -- see docs of LZ4F_compressBound() for that.
    footerSize <- liftIO $ lz4fCompressBound 0 prefs

    footerBs <- liftIO $ allocaBytes (fromIntegral footerSize) $ \footerBuf -> do
      footerWritten <- lz4fCompressEnd ctx footerBuf footerSize
      packCStringLen (footerBuf, fromIntegral footerWritten)

    yield footerBs


bsChunksOf :: Int -> ByteString -> [ByteString]
bsChunksOf chunkSize bs
  | chunkSize < 1 = error $ "bsChunksOf: chunkSize < 1: " ++ show chunkSize
  | BS.length bs <= chunkSize = [bs]
  | otherwise =
      let (x, rest) = BS.splitAt chunkSize bs in x : bsChunksOf chunkSize rest


-- | Compresses the incoming stream of ByteStrings with the lz4 frame format.
--
-- This function implements two optimisations to reduce unnecessary
-- allocations:
--
-- * Incoming ByteStrings are processed in blocks of 16 KB, allowing us
--   to use a single intermediate output buffer through the lifetime of
--   the conduit.
-- * The `bufferSize` of the output buffer can controlled by the caller
--   via the `bufferSize` argument, to reduce the number of small
--   `ByteString`s being `yield`ed (especially in the case that the
--   input data compresses very well, e.g. a stream of zeros).
--
-- Note that the given `bufferSize` is not a hard limit, it can only be
-- used to *increase* the amount of output buffer we're allowed to use:
-- The function will choose `max(bufferSize, minBufferSizeNeededByLz4)`
-- as the eventual output buffer size.
--
-- Setting `bufferSize = 0` is the legitimate way to set the output buffer
-- size to be the minimum required to compress 16 KB inputs and is still a
-- fast default.
compressWithOutBufferSize :: forall m . (MonadUnliftIO m, MonadResource m) => CSize -> ConduitT ByteString ByteString m ()
compressWithOutBufferSize bufferSize =
  withLz4CtxAndPrefsConduit $ \(ctx, prefs) -> do

    -- We split any incoming ByteString into chunks of this size, so that
    -- we can pass this size to `lz4fCompressBound` once and reuse a buffer
    -- of constant size for the compression.
    let bsInChunkSize = 16*1024

    compressBound <- liftIO $ lz4fCompressBound (#{const LZ4F_HEADER_SIZE_MAX} + bsInChunkSize) prefs
    let outBufferSize = max bufferSize compressBound

    outBuf <- liftIO $ mallocForeignPtrBytes (fromIntegral outBufferSize)
    let withOutBuf f = liftIO $ withForeignPtr outBuf f
    let yieldOutBuf outBufLen = do
          outBs <- withOutBuf $ \buf -> packCStringLen (buf, fromIntegral outBufLen)
          yield outBs

    headerSize <- withOutBuf $ \buf -> lz4fCompressBegin ctx prefs buf outBufferSize

    let writeFooterAndYield remainingCapacity = do
          let offset = fromIntegral $ outBufferSize - remainingCapacity
          footerWritten <- withOutBuf $ \buf -> lz4fCompressEnd ctx (buf `plusPtr` offset) remainingCapacity
          let outBufLen = outBufferSize - remainingCapacity + footerWritten
          yieldOutBuf outBufLen

    let loop remainingCapacity = do
          await >>= \case
            Nothing -> do
              -- Done, write footer.

              -- Passing srcSize==0 provides bound for LZ4F_compressEnd(),
              -- see docs of LZ4F_compressBound() for that.
              footerSize <- liftIO $ lz4fCompressBound 0 prefs

              if remainingCapacity >= footerSize
                then do
                  writeFooterAndYield remainingCapacity
                else do
                  -- Footer doesn't fit: Yield buffer, put footer into now-free buffer
                  yieldOutBuf (outBufferSize - remainingCapacity)
                  writeFooterAndYield outBufferSize

            Just bs -> do
              let bss = bsChunksOf (fromIntegral bsInChunkSize) bs
              newRemainingCapacity <- foldM compressSingleBs remainingCapacity bss
              loop newRemainingCapacity

        compressSingleBs :: CSize -> ByteString -> ConduitM i ByteString m CSize
        compressSingleBs remainingCapacity bs
          | remainingCapacity < compressBound = do
              -- Not enough space in outBuf to guarantee that the next call
              -- to `lz4fCompressUpdate` will fit; so yield (a copy of) the
              -- current outBuf, then it's all free again.
              yieldOutBuf (outBufferSize - remainingCapacity)
              compressSingleBsFitting outBufferSize bs
          | otherwise = do
              compressSingleBsFitting remainingCapacity bs

        compressSingleBsFitting :: CSize -> ByteString -> ConduitM i ByteString m CSize
        compressSingleBsFitting remainingCapacity bs = do
          when (remainingCapacity < compressBound) $ error "precondition violated"

          written <- liftIO $ unsafeUseAsCStringLen bs $ \(bsPtr, bsLen) -> do
            let bsLenSize = fromIntegral bsLen

            -- See note [Single call to LZ4F_compressUpdate() can create multiple blocks]
            let offset = fromIntegral $ outBufferSize - remainingCapacity
            withOutBuf $ \buf -> lz4fCompressUpdate ctx (buf `plusPtr` offset) remainingCapacity bsPtr bsLenSize

          when (written > remainingCapacity) $ do -- sanity check
            error $ "lz4fCompressUpdate wrote past buffer: " ++ show (written, remainingCapacity)

          let newRemainingCapacity = remainingCapacity - written
          return newRemainingCapacity

    loop (outBufferSize - headerSize)




-- | All notes that apply to `haskell_lz4_freeCompressionContext` apply here as well.
C.verbatim [r|
void haskell_lz4_freeDecompressionContext(LZ4F_dctx** ctxPtr)
{
  LZ4F_dctx* ctx = *ctxPtr;
  if (ctx != NULL)
  {
    size_t err = LZ4F_freeDecompressionContext(ctx);
    if (LZ4F_isError(err))
    {
      fprintf(stderr, "LZ4F_freeDecompressionContext failed: %s\n", LZ4F_getErrorName(err));
      exit(1);
    }
  }
}
|]

foreign import ccall "&haskell_lz4_freeDecompressionContext" haskell_lz4_freeDecompressionContext :: FunPtr (Ptr (Ptr LZ4F_dctx) -> IO ())


-- | All notes that apply to `lz4fCreateCompressonContext` apply here as well.
lz4fCreateDecompressionContext :: (HasCallStack) => IO Lz4FrameDecompressionContext
lz4fCreateDecompressionContext = do
  ctxForeignPtr :: ForeignPtr (Ptr LZ4F_dctx) <- mallocForeignPtr
  withForeignPtr ctxForeignPtr $ \ptr -> poke ptr nullPtr

  addForeignPtrFinalizer haskell_lz4_freeDecompressionContext ctxForeignPtr

  _ <- handleLz4Error [C.block| size_t {
    LZ4F_dctx** ctxPtr = $fptr-ptr:(LZ4F_dctx** ctxForeignPtr);
    LZ4F_errorCode_t err = LZ4F_createDecompressionContext(ctxPtr, LZ4F_VERSION);
    return err;
  } |]
  return (Lz4FrameDecompressionContext ctxForeignPtr)


lz4fGetFrameInfo :: (HasCallStack) => Lz4FrameDecompressionContext -> Ptr FrameInfo -> Ptr CChar -> Ptr CSize -> IO CSize
lz4fGetFrameInfo (Lz4FrameDecompressionContext ctxForeignPtr) frameInfoPtr srcBuffer srcSizePtr = do

  decompressSizeHint <- handleLz4Error [C.block| size_t {
    LZ4F_dctx* ctxPtr = *$fptr-ptr:(LZ4F_dctx** ctxForeignPtr);
    LZ4F_errorCode_t err_or_decompressSizeHint = LZ4F_getFrameInfo(ctxPtr, $(LZ4F_frameInfo_t* frameInfoPtr), $(char* srcBuffer), $(size_t* srcSizePtr));
    return err_or_decompressSizeHint;
  } |]
  return decompressSizeHint


-- TODO allow passing in dOptPtr instead of NULL.
lz4fDecompress :: (HasCallStack) => Lz4FrameDecompressionContext -> Ptr CChar -> Ptr CSize -> Ptr CChar -> Ptr CSize -> IO CSize
lz4fDecompress (Lz4FrameDecompressionContext ctxForeignPtr) dstBuffer dstSizePtr srcBuffer srcSizePtr = do

  decompressSizeHint <- handleLz4Error [C.block| size_t {
    LZ4F_dctx* ctxPtr = *$fptr-ptr:(LZ4F_dctx** ctxForeignPtr);
    LZ4F_errorCode_t err_or_decompressSizeHint = LZ4F_decompress(ctxPtr, $(char* dstBuffer), $(size_t* dstSizePtr), $(char* srcBuffer), $(size_t* srcSizePtr), NULL);
    return err_or_decompressSizeHint;
  } |]
  return decompressSizeHint


-- | TODO check why decompressSizeHint is always 4
decompress :: forall m . (MonadUnliftIO m, MonadResource m) => ConduitT ByteString ByteString m ()
decompress = do
  ctx <- liftIO lz4fCreateDecompressionContext

  -- OK, now here it gets a bit ugly.
  -- The lz4frame library provides no function with which we can
  -- determine how large the header is.
  -- It depends on the "Content Size" bit in the "FLG" Byte (first
  -- Byte of the frame descriptor, just after the 4 Byte magic number).
  -- As a solution, we `await` the first 5 Bytes, look at the relevant
  -- bit in the FLG Byte and thus decide how many more bytes to await
  -- for the header.
  -- Is ugly because ideally we would rely only on the lz4frame API
  -- and not on the spec of the frame format, but we have no other
  -- choice in this case.

  first5Bytes <- CB.take 5
  when (BSL.length first5Bytes /= 5) $ do
    throwString $ "lz4 decompress error: not enough bytes for header; expected 5, got " ++ show (BSL.length first5Bytes)

  let byteFLG = BSL.index first5Bytes 4
  let contentSizeBit = testBit byteFLG 3

  let numRemainingHeaderBytes
        | contentSizeBit = 2 + 8
        | otherwise      = 2

  remainingHeaderBytes <- CB.take numRemainingHeaderBytes

  let headerBs = BSL.toStrict $ BSL.concat [first5Bytes, remainingHeaderBytes]

  headerDecompressSizeHint <- liftIO $ alloca $ \frameInfoPtr -> do
    unsafeUseAsCStringLen headerBs $ \(headerBsPtr, headerBsLen) -> do
      with (fromIntegral headerBsLen :: CSize) $ \headerBsLenPtr -> do
        lz4fGetFrameInfo ctx frameInfoPtr headerBsPtr headerBsLenPtr

  let dstBufferSizeDefault :: CSize
      dstBufferSizeDefault = 16 * 1024

  bracketP
    (do
      dstBufferPtr <- malloc
      dstBufferSizePtr <- malloc
      poke dstBufferPtr =<< mallocArray (fromIntegral dstBufferSizeDefault)
      poke dstBufferSizePtr dstBufferSizeDefault
      return (dstBufferPtr, dstBufferSizePtr)
    )
    (\(dstBufferPtr, dstBufferSizePtr) -> do
      free =<< peek dstBufferPtr
      free dstBufferPtr
      free dstBufferSizePtr
    )
    $ \(dstBufferPtr, dstBufferSizePtr) -> do

    let ensureDstBufferSize :: CSize -> IO (Ptr CChar)
        ensureDstBufferSize size = do
          dstBufferSize <- peek dstBufferSizePtr
          when (size > dstBufferSize) $ do
            dstBuffer <- peek dstBufferPtr
            poke dstBufferPtr =<< reallocArray dstBuffer (fromIntegral size)
            poke dstBufferSizePtr size
          peek dstBufferPtr

    let loopSingleBs :: CSize -> ByteString -> ConduitT ByteString ByteString m CSize
        loopSingleBs decompressSizeHint bs = do
          (outBs, srcRead, newDecompressSizeHint) <- liftIO $
            unsafeUseAsCStringLen bs $ \(srcBuffer, srcSize) -> do
              let outBufSize = max decompressSizeHint dstBufferSizeDefault -- TODO check why decompressSizeHint is always 4

              -- Increase destination buffer size if necessary.
              dstBuffer <- ensureDstBufferSize outBufSize

              with outBufSize $ \dstSizePtr -> do
                with (fromIntegral srcSize :: CSize) $ \srcSizePtr -> do
                  newDecompressSizeHint <-
                    lz4fDecompress ctx dstBuffer dstSizePtr srcBuffer srcSizePtr
                  srcRead <- peek srcSizePtr
                  dstWritten <- peek dstSizePtr
                  outBs <- packCStringLen (dstBuffer, fromIntegral dstWritten)
                  return (outBs, srcRead, newDecompressSizeHint)

          yield outBs

          let srcReadInt = fromIntegral srcRead
          if
            | srcReadInt < BS.length bs -> loopSingleBs newDecompressSizeHint (BS.drop srcReadInt bs)
            | srcReadInt == BS.length bs -> return newDecompressSizeHint
            | otherwise -> error $ "lz4 decompress: assertion failed: srcRead < BS.length bs: " ++ show (srcRead, BS.length bs)

    let loop decompressSizeHint =
          await >>= \case
            Nothing -> throwString $ "lz4 decompress error: stream ended before EndMark"
            Just bs -> do
              newDecompressSizeHint <- loopSingleBs decompressSizeHint bs

              -- When a frame is fully decoded, LZ4F_decompress returns 0 (no more data expected),
              -- see https://github.com/lz4/lz4/blob/7cf0bb97b2a988cb17435780d19e145147dd9f70/lib/lz4frame.h#L324
              when (newDecompressSizeHint /= 0) $ loop newDecompressSizeHint

    loop headerDecompressSizeHint

    -- Force resource release here to guarantee memory constantness
    -- of the conduit (and not rely on GC to do it "at some point in the future").
    liftIO $ finalizeForeignPtr (unLz4FrameDecompressionContext ctx)
