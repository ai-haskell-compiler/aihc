module Sound.SoxLib (
   with, formatWith,
   openRead,  withRead,  readStorableVector,  readStorableVectorLazy,
   openWrite, withWrite, writeStorableVector, writeStorableVectorLazy,
   storableVectorLazyFromByteString,
   close, seek,
   FFI.Mode(..), FFI.ReadMode, FFI.WriteMode,
   ReaderInfo(..), defaultReaderInfo,
   WriterInfo(..), defaultWriterInfo,

   FFI.Rate,
   FFI.FileType(..),
   FFI.Format(..),
   FFI.SignalInfo(..),
   FFI.EncodingInfo(..),
   FFI.defaultSignalInfo,

   FFI.IOType,
   FFI.ioFile,
   FFI.ioPipe,
   FFI.ioURL,


   FFI.Option,
   FFI.optionNo,
   FFI.optionYes,
   FFI.optionDefault,

   FFI.Encoding,
   FFI.encodingUnknown,
   FFI.encodingSign2,
   FFI.encodingUnsigned,
   FFI.encodingFloat,
   FFI.encodingFloatText,
   FFI.encodingFlac,
   FFI.encodingHcom,
   FFI.encodingWavpack,
   FFI.encodingWavpackf,
   FFI.encodingUlaw,
   FFI.encodingAlaw,
   FFI.encodingG721,
   FFI.encodingG723,
   FFI.encodingClADPCM,
   FFI.encodingClADPCM16,
   FFI.encodingMsADPCM,
   FFI.encodingImaADPCM,
   FFI.encodingOkiADPCM,
   FFI.encodingDPCM,
   FFI.encodingDWVW,
   FFI.encodingDWVWN,
   FFI.encodingGSM,
   FFI.encodingMP3,
   FFI.encodingVorbis,
   FFI.encodingAmrWB,
   FFI.encodingAmrNB,
   FFI.encodingCVSD,
   FFI.encodingLPC10,
   ) where

import qualified Sound.SoxLib.FFI as FFI

import qualified Foreign.Marshal.Utils as U
import qualified Foreign.C.String as CStr
import Foreign.Storable (Storable, peek, )
import Foreign.ForeignPtr (ForeignPtr, FinalizerPtr, newForeignPtr, withForeignPtr, )
import Foreign.Ptr (Ptr, nullFunPtr, nullPtr, castPtr, )

import Control.Exception (bracket_, bracket, )
import System.IO.Error (mkIOError, doesNotExistErrorType, )

import qualified Data.StorableVector.Base as SVB
import qualified Data.StorableVector.Lazy as SVL
import qualified Data.StorableVector as SV
import qualified Data.ByteString as B

import qualified Control.Monad.Trans.Cont as MC
import qualified Control.Monad.Trans.Class as MT
import Control.Functor.HT (void, )
import Control.Monad (when, )

import qualified Data.Traversable as Trav
import Data.Maybe.HT (toMaybe, )

import System.IO.Unsafe (unsafePerformIO, unsafeInterleaveIO, )

import Data.Int (Int32, )


{-
DEPRECATED with
   "I found no documentation for it and thus think that it is deprecated. Use formatWith instead."
-}
-- ToDo: on the other hand, example5.c uses it, but not formatInit
with :: IO a -> IO a
with = bracket_ FFI.init FFI.quit

{- |
All SoxLib operations must be enclosed in 'formatWith'.
You must only call it once per program.
-}
formatWith :: IO a -> IO a
formatWith = bracket_ FFI.formatInit FFI.formatQuit


withMaybe ::
   Storable b =>
   (a -> (Ptr b -> IO c) -> IO c) ->
   Maybe a -> MC.ContT c IO (Ptr b)
withMaybe _ Nothing = return nullPtr
withMaybe f (Just a) = MC.ContT $ f a


data ReaderInfo =
   ReaderInfo {
      readerSignalInfo :: Maybe FFI.SignalInfo,
      readerEncodingInfo :: Maybe FFI.EncodingInfo,
      readerFileType :: Maybe FFI.FileType
   }

defaultReaderInfo :: ReaderInfo
defaultReaderInfo = ReaderInfo Nothing Nothing Nothing

withRead ::
   ReaderInfo -> FilePath ->
   (FFI.FormatPtr FFI.ReadMode -> IO a) -> IO a
withRead info path =
   bracket (openRead info path) close

{- |
This function will never return a 'nullPtr'.
Instead it throws a user exception if the file cannot be opened.
-}
openRead :: ReaderInfo -> FilePath -> IO (FFI.FormatPtr FFI.ReadMode)
openRead info path =
   flip MC.runContT return $ do
      si  <- withMaybe U.with $ readerSignalInfo info
      enc <- withMaybe U.with $ readerEncodingInfo info
      cft <- withMaybe CStr.withCString $
                 fmap FFI.unFileType $ readerFileType info
      cpath <- MC.ContT $ CStr.withCString path
      MT.lift $
         checkHandle "SoxLib.openRead" path =<<
         FFI.openRead cpath si enc cft


data WriterInfo =
   WriterInfo {
      writerSignalInfo :: Maybe FFI.SignalInfo,
      writerEncodingInfo :: Maybe FFI.EncodingInfo,
      writerFileType :: Maybe FFI.FileType
   }

defaultWriterInfo :: WriterInfo
defaultWriterInfo = WriterInfo Nothing Nothing Nothing

withWrite ::
   WriterInfo -> FilePath ->
   (FFI.FormatPtr FFI.WriteMode -> IO a) -> IO a
withWrite info path =
   bracket (openWrite info path) close

{- |
This function will never return a 'nullPtr'.
Instead it throws a user exception if the file cannot be opened.
-}
openWrite :: WriterInfo -> FilePath -> IO (FFI.FormatPtr FFI.WriteMode)
openWrite info path =
   flip MC.runContT return $ do
      si  <- withMaybe U.with $ writerSignalInfo info
      enc <- withMaybe U.with $ writerEncodingInfo info
      cft <- withMaybe CStr.withCString $
                 fmap FFI.unFileType $ writerFileType info
      cpath <- MC.ContT $ CStr.withCString path
      MT.lift $
         checkHandle "SoxLib.openWrite" path =<<
         FFI.openWrite cpath si enc cft nullPtr nullFunPtr


checkHandle :: String -> FilePath -> Ptr a -> IO (Ptr a)
checkHandle name path fmt =
   if fmt==nullPtr
     then throwDoesNotExist name path
     else return fmt

throwDoesNotExist :: String -> FilePath -> IO a
throwDoesNotExist name path =
   ioError $
      mkIOError doesNotExistErrorType
         name Nothing (Just path)



close :: FFI.Mode mode => FFI.FormatPtr mode -> IO ()
close = void . FFI.close


{- |
Multi-channel data is interleaved.
@size@ must be divisible by the number of channels.
-}
readStorableVector ::
   FFI.FormatPtr FFI.ReadMode -> Int -> IO (SV.Vector Int32)
readStorableVector fmt size =
   SVB.createAndTrim size $ \ptr ->
   fmap fromIntegral $ FFI.read fmt ptr (fromIntegral size)

{- |
Multi-channel data is interleaved.
@size@ must be divisible by the number of channels.

Caution:
Writing large chunks (e.g. more than 8192 samples) may crash the FLAC backend.
-}
writeStorableVector ::
   FFI.FormatPtr FFI.WriteMode -> SV.Vector Int32 -> IO ()
writeStorableVector fmt chunk =
   SVB.withStartPtr chunk $ \ptr len -> do
      written <- FFI.write fmt ptr (fromIntegral len)
      when (written < fromIntegral len) $ do
         f <- peek fmt
         ioError $ userError $ FFI.soxErrStr f


readChunks ::
   IO (SV.Vector Int32) -> Int ->
   IO [SV.Vector Int32] ->
   IO [SV.Vector Int32]
readChunks readChunk size go = do
   chunk <- readChunk
   if SV.length chunk >= size
     then fmap (chunk:) go
     else return $
          if SV.length chunk == 0
            then []
            else [chunk]

{- |
Read complete file lazily into chunky storable vector.
The chunkSize must be divisible by the number of channels.
-}
readStorableVectorLazy ::
   FFI.FormatPtr FFI.ReadMode -> SVL.ChunkSize -> IO (SVL.Vector Int32)
readStorableVectorLazy fmt (SVL.ChunkSize size) =
   let go =
          unsafeInterleaveIO $
             readChunks (readStorableVector fmt size) size go
   in  fmap SVL.fromChunks go

{- |
The chunkSize must be divisible by the number of channels.
-}
writeStorableVectorLazy ::
   FFI.FormatPtr FFI.WriteMode -> SVL.Vector Int32 -> IO ()
writeStorableVectorLazy fmt =
   mapM_ (writeStorableVector fmt) . SVL.chunks


seek :: (FFI.Mode mode) => FFI.FormatPtr mode -> Int -> IO ()
seek fmt pos = do
   res <- FFI.seek fmt (fromIntegral pos) 0
   when (res /= 0) $ do
      f <- peek fmt
      ioError $ userError $ FFI.soxErrStr f


maybeNewForeignPtr :: FinalizerPtr a -> Ptr a -> IO (Maybe (ForeignPtr a))
maybeNewForeignPtr finalizer ptr =
   Trav.sequence $ toMaybe (ptr/=nullPtr) $ newForeignPtr finalizer ptr

{- |
It reads lazily to lazy storable vector.
That is, the whole 'ByteString' is kept as long as we process the lazy storable vector.
-}
-- ToDo: return (Either String) containing error message from Format.soxErrStr
storableVectorLazyFromByteString ::
   ReaderInfo -> B.ByteString -> SVL.ChunkSize -> Maybe (SVL.Vector Int32)
storableVectorLazyFromByteString info bytes (SVL.ChunkSize size) =
   unsafePerformIO $
   flip MC.runContT return $ do
      si  <- withMaybe U.with $ readerSignalInfo info
      enc <- withMaybe U.with $ readerEncodingInfo info
      cft <- withMaybe CStr.withCString $
                 fmap FFI.unFileType $ readerFileType info
      (src, len) <- MC.ContT $ B.useAsCStringLen bytes
      MT.lift $ do
         maybeFmt <-
            maybeNewForeignPtr FFI.closeFun
             =<< FFI.openMemRead (castPtr src) (fromIntegral len) si enc cft
         Trav.for maybeFmt $ \fmtForeign -> do
            let readChunk =
                  withForeignPtr fmtForeign $ \fmt ->
                     readStorableVector fmt size
            let go = unsafeInterleaveIO $ readChunks readChunk size go
            fmap SVL.fromChunks go
