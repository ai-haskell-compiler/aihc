{-# LANGUAGE TupleSections #-}

module Codec.Compression.BZip.Unpack ( decompress
                                     , decompressErr
                                     ) where

import Codec.Compression.BZip.Foreign.Common
import Codec.Compression.BZip.Foreign.Decompress
import Codec.Compression.BZip.Common
import Control.Applicative
import Control.Exception (evaluate, throw, try)
import Control.Monad.ST.Lazy as LazyST
import Control.Monad.ST.Lazy.Unsafe as LazyST
import qualified Data.ByteString as BS
import qualified Data.ByteString.Unsafe as BS
import qualified Data.ByteString.Lazy as BSL
import Data.Maybe (isNothing)
import Foreign.ForeignPtr (newForeignPtr, castForeignPtr, ForeignPtr, mallocForeignPtrBytes, withForeignPtr)
import Foreign.Ptr (Ptr, castPtr, nullPtr)
import System.IO.Unsafe (unsafePerformIO)

#include <bzlib.h>

-- | Return an error rather than throwing an exception.
--
-- @since 1.0.1.0
decompressErr :: BSL.ByteString -> Either BZError BSL.ByteString
decompressErr = unsafePerformIO . try . evaluate . decompress

-- | Don't use this on pathological input; it may not be secure
--
-- This does not handle nested streams
--
-- @since 0.1.1.0
decompress :: BSL.ByteString -> BSL.ByteString
decompress bsl =
    let bss = BSL.toChunks bsl in
    BSL.fromChunks $ LazyST.runST $ do
        (p, bufOut) <- LazyST.unsafeIOToST $ do
            ptr <- bzStreamInit
            p <- castForeignPtr <$> newForeignPtr bZ2BzDecompressEnd (castPtr ptr)
            bzDecompressInit p
            bufOut <- mallocForeignPtrBytes bufSz
            pure (p, bufOut)

        bzDecompressChunks p bss bufOut

type Step = Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> IO BZError -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])

bzDecompressChunks :: ForeignPtr BzStream -> [BS.ByteString] -> ForeignPtr a -> LazyST.ST s [BS.ByteString]
bzDecompressChunks ptr' bs bufO =

    fillBuf ptr' Nothing bs pushBytes bufO

    where

        fillBuf :: ForeignPtr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> Step -> ForeignPtr a -> LazyST.ST s [BS.ByteString]
        fillBuf pForeign passFwd bs' step bufOutForeign = do
            (ret, szOut, newBSAp, bs'', keepAlive) <- LazyST.unsafeIOToST $ do
                withForeignPtr pForeign $ \p ->
                    withForeignPtr bufOutForeign $ \bufOut -> do

                        let act = do

                                {# set bz_stream.avail_out #} p bufSz
                                {# set bz_stream.next_out #} p (castPtr bufOut)

                                bZ2BzDecompress ptr'

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
                else if null bs' && isNothing keepAlive && bufSz == szOut
                    then throw BzUnexpectedEof
                    else newBSAp <$> fillBuf pForeign keepAlive bs'' step' bufOutForeign

        keepBytesAlive :: Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> IO BZError -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])
        keepBytesAlive _ Nothing bs' act = (, Nothing, bs') <$> act
        keepBytesAlive _ passFwd@(Just b) bs' act = do
            BS.unsafeUseAsCStringLen b $ \_ ->

                (, passFwd, bs') <$> act

        pushBytes :: Ptr BzStream -> Maybe BS.ByteString -> [BS.ByteString] -> IO BZError -> IO (BZError, Maybe BS.ByteString, [BS.ByteString])
        pushBytes _ _ [] act = (, Nothing, []) <$> act
        pushBytes p _ (b:bs') act =
            BS.unsafeUseAsCStringLen b $ \(buf, sz) -> do

                {# set bz_stream.avail_in #} p (fromIntegral sz)
                {# set bz_stream.next_in #} p buf

                (, Just b, bs') <$> act

bufSz :: Integral a => a
bufSz = 64 * 1024

bzDecompressInit :: ForeignPtr BzStream -> IO ()
bzDecompressInit ptr' = do

    withForeignPtr ptr' $ \p -> do

        {# set bz_stream.next_in #} p nullPtr
        {# set bz_stream.avail_in #} p 0

    bZ2BzDecompressInit ptr' 0 False
