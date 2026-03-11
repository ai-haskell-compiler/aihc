{-# LANGUAGE ForeignFunctionInterface, CPP #-}
-- |
-- Module      : Crypto.Cipher.RC4
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : stable
-- Portability : Good
--
-- Simple implementation of the RC4 stream cipher.
-- http://en.wikipedia.org/wiki/RC4
--
-- Initial FFI implementation by Peter White <peter@janrain.com>
--
-- Reorganized and simplified to have an opaque context.
--
module Crypto.Cipher.RC4
    (
      RC4
    -- * deprecated types
    , Ctx(..)
    -- * deprecated functions, use crypto-cipher-types StreamCipher function
    , initCtx
    , generate
    , combine
    , encrypt
    , decrypt
    ) where

import Data.Word
import Data.Byteable
import Foreign.Ptr
import Foreign.ForeignPtr
import System.IO.Unsafe
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Control.Applicative ((<$>))
import Crypto.Cipher.Types

----------------------------------------------------------------------
unsafeDoIO :: IO a -> a
#if __GLASGOW_HASKELL__ > 704
unsafeDoIO = unsafeDupablePerformIO
#else
unsafeDoIO = unsafePerformIO
#endif

-- | RC4 Stream cipher
newtype RC4 = RC4 Ctx

instance Byteable RC4 where
    toBytes (RC4 (Ctx b)) = b

instance Cipher RC4 where
    cipherInit key  = RC4 (initCtx $ toBytes key)
    cipherName _    = "RC4"
    cipherKeySize _ = KeySizeRange 1 1024

instance StreamCipher RC4 where
    streamCombine (RC4 ctx) b = (\(ctx2, r) -> (r, RC4 ctx2)) $ combine ctx b

-- | The encryption context for RC4
newtype Ctx = Ctx B.ByteString

instance Show Ctx where
    show _ = "RC4.Ctx"

-- | C Call for initializing the encryptor
foreign import ccall unsafe "rc4.h rc4_init"
    c_rc4_init :: Ptr Word8 -- ^ The rc4 key
               -> Word32    -- ^ The key length
               -> Ptr Ctx   -- ^ The context
               -> IO ()

foreign import ccall unsafe "rc4.h rc4_combine"
    c_rc4_combine :: Ptr Ctx        -- ^ Pointer to the permutation
                  -> Ptr Word8      -- ^ Pointer to the clear text
                  -> Word32         -- ^ Length of the clear text
                  -> Ptr Word8      -- ^ Output buffer
                  -> IO ()

withByteStringPtr :: ByteString -> (Ptr Word8 -> IO a) -> IO a
withByteStringPtr b f = withForeignPtr fptr $ \ptr -> f (ptr `plusPtr` off)
    where (fptr, off, _) = B.toForeignPtr b

-- | RC4 context initialization.
--
-- seed the context with an initial key. the key size need to be
-- adequate otherwise security takes a hit.
initCtx :: B.ByteString -- ^ The key
        -> Ctx          -- ^ The RC4 context with the key mixed in
initCtx key = unsafeDoIO $
    Ctx <$> (B.create 264 $ \ctx -> B.useAsCStringLen key $ \(keyPtr,keyLen) -> c_rc4_init (castPtr keyPtr) (fromIntegral keyLen) (castPtr ctx))

-- | generate the next len bytes of the rc4 stream without combining
-- it to anything.
generate :: Ctx -> Int -> (Ctx, B.ByteString)
generate ctx len = combine ctx (B.replicate len 0)

-- | RC4 xor combination of the rc4 stream with an input
combine :: Ctx                 -- ^ rc4 context
        -> B.ByteString        -- ^ input
        -> (Ctx, B.ByteString) -- ^ new rc4 context, and the output
combine (Ctx cctx) clearText = unsafeDoIO $
    B.mallocByteString 264 >>= \dctx ->
    B.mallocByteString len >>= \outfptr ->
    withByteStringPtr clearText $ \clearPtr ->
    withByteStringPtr cctx $ \srcCtx ->
    withForeignPtr dctx $ \dstCtx -> do
    withForeignPtr outfptr $ \outptr -> do
        B.memcpy dstCtx srcCtx 264
        c_rc4_combine (castPtr dstCtx) clearPtr (fromIntegral len) outptr
        return $! (Ctx $! B.PS dctx 0 264, B.PS outfptr 0 len)
    where len = B.length clearText

{-# DEPRECATED encrypt "use combine instead" #-}
{-# DEPRECATED decrypt "use combine instead" #-}
encrypt,decrypt :: Ctx -> B.ByteString -> (Ctx, B.ByteString)
encrypt = combine
decrypt = combine
