-- |
-- Module      : Crypto.OpenSSL.AES
-- License     : BSD-style
-- Stability   : experimental
-- Portability : Unix
--
-- Small Ad-Hoc bindings to AES-GCM encryption and decryption function.
--
-- Doesn't support incremental mode yet.
--
-- TODO: full fledge AES bindings (CBC, CTR, GCM...), with incremental processing.
--
module Crypto.OpenSSL.AES
    ( isSupportedGCM
    , encryptGCM
    , decryptGCM
    , OpenSSLGcmError(..)
    ) where

import           Crypto.OpenSSL.AES.Foreign
import           Crypto.OpenSSL.Misc
import           Control.Monad
import           Foreign.Marshal.Alloc
import           Foreign.ForeignPtr
import           Foreign.Ptr
import           Foreign.C.Types
import           Foreign.Storable
import           Data.ByteString (ByteString)
import qualified Data.ByteArray as B

type GCMCtx = ForeignPtr EVP_CIPHER_CTX

data Direction = DirectionEncrypt | DirectionDecrypt

-- | Get whether the OpenSSL version linked supports GCM mode (at least 1.0.x and above)
isSupportedGCM :: Bool
isSupportedGCM = doIO $ do
    cipher <- ssl_c_aes_256_gcm
    return (cipher /= nullPtr)
{-# NOINLINE isSupportedGCM #-}

withGCM :: Direction -> ByteString -> ByteString -> (Ptr EVP_CIPHER_CTX -> IO a) -> a
withGCM direction key iv f = doIO $ do
    cipher <- ssl_c_aes_256_gcm
    when (cipher == nullPtr) $ error "openssl doesn't have a GCM cipher"
    fptr <- contextNew $ \ctx -> checkRet "encryptinit_ex" (ssl_c_encryptinit_ex ctx cipher nullEngine nullPtr nullPtr)
    withForeignPtr fptr $ \ctx    ->
        B.withByteArray key $ \keyPtr ->
        B.withByteArray iv  $ \ivPtr  -> do
            checkRet "ctx_ctrl_set_ivlen" (ssl_c_cipher_ctx_ctrl ctx ctrl_GCM_SET_IVLEN 12 nullPtr)
            case direction of
                DirectionEncrypt -> checkRet "encryptinit_ex" (ssl_c_encryptinit_ex ctx nullPtr nullEngine keyPtr ivPtr)
                DirectionDecrypt -> checkRet "decryptinit_ex" (ssl_c_decryptinit_ex ctx nullPtr nullEngine keyPtr ivPtr)
            f ctx
{-# NOINLINE withGCM #-}

-- | One shot function to GCM data without any incremental handling
encryptGCM :: ByteString -- ^ Key
           -> ByteString -- ^ IV
           -> ByteString -- ^ Header (Authenticated input, will be not be copied to output)
           -> ByteString -- ^ Plaintext to encrypt
           -> ByteString -- ^ Encrypted input including the authentication tag (but not the header)
encryptGCM key iv header input = withGCM DirectionEncrypt key iv $ \ctx -> do
    -- consume the header as authenticated data
    when (headerLength > 0) $ do
        B.withByteArray header $ \h ->
            checkRet "encryptupdate-header" (alloca $ \outl -> ssl_c_encryptupdate ctx nullPtr outl h (fromIntegral headerLength))

    -- consume the input data and, create output data + GCM tag
    alloca $ \ptrOutl ->
        B.withByteArray input $ \inp -> do
        B.alloc ciphertextLength $ \out -> do
            checkRet "encryptupdate-input" (ssl_c_encryptupdate ctx out ptrOutl inp (fromIntegral inputLength))
            encryptedLen <- peek ptrOutl
            checkRet "encryptfinal_ex" (ssl_c_encryptfinal_ex ctx (out `plusPtr` (fromIntegral encryptedLen)) ptrOutl)
            checkRet "ctx_ctrl_get_tag" (ssl_c_cipher_ctx_ctrl ctx ctrl_GCM_GET_TAG (fromIntegral gcmTagLength) (out `plusPtr` inputLength))
  where
        ciphertextLength = B.length input + gcmTagLength
        headerLength     = B.length header
        inputLength      = B.length input
{-# NOINLINE encryptGCM #-}

-- | One shot function to decrypt GCM data without any incremental handling
decryptGCM :: ByteString -- ^ Key
           -> ByteString -- ^ IV
           -> ByteString -- ^ Header (Authenticated input)
           -> ByteString -- ^ Encrypted data
           -> Maybe ByteString -- ^ Decrypted data if authentication successful
decryptGCM key iv header input
    | inputLength < gcmTagLength = Nothing
    | otherwise                  = withGCM DirectionDecrypt key iv $ \ctx -> do
        -- consume the header as authenticated data
        when (headerLength > 0) $ do
            B.withByteArray header $ \h ->
                checkRet "decryptupdate-header" (alloca $ \outl -> ssl_c_decryptupdate ctx nullPtr outl h (fromIntegral headerLength))

        -- consume the input data and, create output data + GCM tag
        B.withByteArray input $ \inp -> do
            (r, output) <- B.allocRet plaintextLength $ \out -> do
                alloca $ \ptrOutl -> do
                    checkRet "decryptupdate-input" (ssl_c_decryptupdate ctx out ptrOutl inp (fromIntegral plaintextLength))
                    checkRet "ctx_ctrl_set_tag" (ssl_c_cipher_ctx_ctrl ctx ctrl_GCM_SET_TAG (fromIntegral gcmTagLength) (inp `plusPtr` plaintextLength))
                    ssl_c_decryptfinal_ex ctx out ptrOutl
            if r == 0
                then return Nothing -- validation failed
                else return $ Just output
  where
        plaintextLength = B.length input - gcmTagLength
        headerLength    = B.length header
        inputLength     = B.length input
{-# NOINLINE decryptGCM #-}

checkRet :: String -> IO CInt -> IO ()
checkRet = checkCtx OpenSSLGcmError

contextNew :: (Ptr EVP_CIPHER_CTX -> IO ()) -> IO GCMCtx
contextNew f = do
    fptr <- compatNewEvpCipherCtx
    withForeignPtr fptr f
    return fptr

nullEngine :: Ptr ENGINE
nullEngine = nullPtr
