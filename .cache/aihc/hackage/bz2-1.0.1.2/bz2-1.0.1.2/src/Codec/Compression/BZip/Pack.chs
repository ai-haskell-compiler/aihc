{-# LANGUAGE TupleSections #-}

module Codec.Compression.BZip.Pack ( compress
                                   , compressWith
                                   ) where

import Codec.Compression.BZip.Foreign.Common
import Codec.Compression.BZip.Foreign.Compress
import Codec.Compression.BZip.Common
import Control.Applicative
import Control.Monad.ST.Lazy as LazyST
import Control.Monad.ST.Lazy.Unsafe as LazyST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Unsafe as BS
import Foreign.C.Types (CInt)
import Foreign.ForeignPtr (castForeignPtr, ForeignPtr, newForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)

#include <bzlib.h>

-- | @since 0.1.1.0
compress :: BSL.ByteString -> BSL.ByteString
compress = compressWith 9 30

type Step = Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> (BZAction -> IO BZError) -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])

-- | See [bzlib manual](https://www.sourceware.org/bzip2/manual/manual.html#bzcompress-init)
-- for information on compression parameters.
--
-- @since 0.1.1.0
compressWith :: CInt -- ^ Block size (@1-9@)
             -> CInt -- ^ Work factor (@0-250@)
             -> BSL.ByteString
             -> BSL.ByteString
compressWith blkSize wf bsl =
    let bss = BSL.toChunks bsl in
    BSL.fromChunks $ LazyST.runST $ do
        (p, bufOut) <- LazyST.unsafeIOToST $ do
            ptr <- bzStreamInit
            p <- castForeignPtr <$> newForeignPtr bZ2BzCompressEnd (castPtr ptr)
            bzCompressInit blkSize wf p
            bufOut <- mallocForeignPtrBytes bufSz
            pure (p, bufOut)

        bzCompressChunks p bss bufOut

bzCompressChunks :: ForeignPtr BzStream -> [BS.ByteString] -> ForeignPtr a -> LazyST.ST s [BS.ByteString]
bzCompressChunks ptr' bs bufO = do

    fillBuf ptr' Nothing bs pushBytes bufO

    where

        -- corresponds to inner loop in zlib example
        fillBuf :: ForeignPtr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> Step -> ForeignPtr a -> LazyST.ST s [BS.ByteString]
        fillBuf pForeign passFwd bs' step bufOutForeign = do
            (ret, szOut, newBSAp, bs'', keepAlive) <- LazyST.unsafeIOToST $ do
                withForeignPtr pForeign $ \p ->
                    withForeignPtr bufOutForeign $ \bufOut -> do

                        let act f = do

                                {# set bz_stream.avail_out #} p bufSz
                                {# set bz_stream.next_out #} p (castPtr bufOut)

                                bZ2BzCompress ptr' f

                        (ret, keepAlive, bs'') <- step p passFwd bs' act

                        szOut <- fromIntegral <$> {# get bz_stream->avail_out #} p

                        let bytesAvail = bufSz - szOut

                        newBSAp <- if bytesAvail /= 0
                            then (:) <$> BS.packCStringLen (castPtr bufOut, bytesAvail)
                            else pure id

                        pure (ret, szOut, newBSAp, bs'', keepAlive)

            let step' = if szOut == 0
                then keepBytesAlive
                else pushBytes

            if ret == BzStreamEnd
                then pure (newBSAp [])
                else newBSAp <$> fillBuf pForeign keepAlive bs'' step' bufOutForeign

        keepBytesAlive :: Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> (BZAction -> IO BZError) -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])
        keepBytesAlive _ Nothing [] act = (, Nothing, []) <$> act BzFinish
        keepBytesAlive _ Nothing bs' act = (, Nothing, bs') <$> act BzRun
        keepBytesAlive _ passFwd@(Just b) [] act =
            BS.unsafeUseAsCStringLen b $ \_ ->

                (, passFwd, []) <$> act BzFinish
        keepBytesAlive _ passFwd@(Just b) bs' act =
            BS.unsafeUseAsCStringLen b $ \_ ->

                (, passFwd, bs') <$> act BzRun

        pushBytes :: Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> (BZAction -> IO BZError) -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])
        pushBytes _ _ [] act = (, Nothing, []) <$> act BzFinish
        pushBytes p _ (b:bs') act =
            BS.unsafeUseAsCStringLen b $ \(buf, sz) -> do

                {# set bz_stream.avail_in #} p (fromIntegral sz)
                {# set bz_stream.next_in #} p buf

                (, Just b, bs') <$> act BzRun

bufSz :: Integral a => a
bufSz = 16 * 1024

bzCompressInit :: CInt -> CInt -> ForeignPtr BzStream -> IO ()
bzCompressInit blkSize wf ptr' = do

    withForeignPtr ptr' $ \p -> do

        {# set bz_stream.next_in #} p nullPtr
        {# set bz_stream.avail_in #} p 0

    bZ2BzCompressInit ptr' blkSize 0 wf
