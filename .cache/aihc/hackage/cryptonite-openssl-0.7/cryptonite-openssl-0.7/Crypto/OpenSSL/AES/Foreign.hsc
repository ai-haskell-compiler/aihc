{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE EmptyDataDecls #-}
module Crypto.OpenSSL.AES.Foreign where

#include <openssl/opensslv.h>
#include <openssl/evp.h>

#if OPENSSL_VERSION_NUMBER >= 0x10100000
#define OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
#endif

#if OPENSSL_VERSION_NUMBER >= 0x10001000
#define OPENSSL_HAS_PBKDF2
#define OPENSSL_HAS_GCM
#endif

import Foreign.Marshal.Alloc
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types
import Data.Word
#ifndef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
import qualified Data.Memory.PtrMethods as B (memSet)
#endif

gcmTagLength :: Int
gcmTagLength = 16

#ifndef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
sizeofEVP :: Int
sizeofEVP = (#const sizeof(EVP_CIPHER_CTX))
#endif

data ENGINE

data EVP_CIPHER
data EVP_CIPHER_CTX

type KeyBuf = Ptr Word8

type IvBuf = Ptr Word8
type DataBuf = Ptr Word8
type OutputOffset = Ptr CInt
type InputLength = CInt

compatNewEvpCipherCtx :: IO (ForeignPtr EVP_CIPHER_CTX)
compatNewEvpCipherCtx = do
#ifdef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
    ptr <- ssl_c_cipher_ctx_new
    newForeignPtr ssl_c_cipher_ctx_free ptr
#else
    ptr <- mallocBytes sizeofEVP
    B.memSet (castPtr ptr) 0 (fromIntegral sizeofEVP)
    ssl_c_cipher_ctx_init ptr
    newForeignPtr ssl_c_cipher_ctx_cleanup ptr
#endif

#ifdef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
foreign import ccall unsafe "EVP_CIPHER_CTX_new"
    ssl_c_cipher_ctx_new :: IO (Ptr EVP_CIPHER_CTX)
#endif

#ifndef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
foreign import ccall unsafe "EVP_CIPHER_CTX_init"
    ssl_c_cipher_ctx_init :: Ptr EVP_CIPHER_CTX -> IO ()
#endif

foreign import ccall unsafe "&EVP_CIPHER_CTX_free"
    ssl_c_cipher_ctx_free :: FunPtr (Ptr EVP_CIPHER_CTX -> IO ())

#ifndef OPENSSL_HAS_OPAQUE_EVP_CIPHER_CTX
foreign import ccall unsafe "&EVP_CIPHER_CTX_cleanup"
    ssl_c_cipher_ctx_cleanup :: FunPtr (Ptr EVP_CIPHER_CTX -> IO ())
#endif

foreign import ccall unsafe "EVP_CIPHER_CTX_ctrl"
    ssl_c_cipher_ctx_ctrl :: Ptr EVP_CIPHER_CTX -> CInt -> CInt -> Ptr a -> IO CInt

foreign import ccall unsafe "EVP_CIPHER_CTX_set_padding"
    ssl_c_cipher_ctx_set_padding :: Ptr EVP_CIPHER_CTX -> CInt -> IO CInt

foreign import ccall unsafe "EVP_CIPHER_CTX_set_key_length"
    ssl_c_cipher_ctx_set_key_length :: Ptr EVP_CIPHER_CTX -> CInt -> IO CInt

foreign import ccall unsafe "EVP_EncryptInit_ex"
    ssl_c_encryptinit_ex :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr ENGINE -> KeyBuf -> IvBuf -> IO CInt

foreign import ccall unsafe "EVP_DecryptInit_ex"
    ssl_c_decryptinit_ex :: Ptr EVP_CIPHER_CTX -> Ptr EVP_CIPHER -> Ptr ENGINE -> KeyBuf -> IvBuf -> IO CInt

foreign import ccall unsafe "EVP_EncryptUpdate"
    ssl_c_encryptupdate :: Ptr EVP_CIPHER_CTX -> DataBuf -> OutputOffset -> DataBuf -> InputLength -> IO CInt

foreign import ccall unsafe "EVP_DecryptUpdate"
    ssl_c_decryptupdate :: Ptr EVP_CIPHER_CTX -> DataBuf -> OutputOffset -> DataBuf -> InputLength -> IO CInt

foreign import ccall unsafe "EVP_EncryptFinal_ex"
    ssl_c_encryptfinal_ex :: Ptr EVP_CIPHER_CTX -> DataBuf -> OutputOffset -> IO CInt

foreign import ccall unsafe "EVP_DecryptFinal_ex"
    ssl_c_decryptfinal_ex :: Ptr EVP_CIPHER_CTX -> DataBuf -> OutputOffset -> IO CInt

#ifdef OPENSSL_HAS_GCM
foreign import ccall unsafe "EVP_aes_256_gcm"
    ssl_c_aes_256_gcm :: IO (Ptr EVP_CIPHER)
#else
ssl_c_aes_256_gcm :: IO (Ptr EVP_CIPHER)
ssl_c_aes_256_gcm = return nullPtr
#endif

ctrl_GCM_SET_IVLEN, ctrl_GCM_GET_TAG, ctrl_GCM_SET_TAG :: CInt
#ifdef OPENSSL_HAS_GCM
ctrl_GCM_SET_IVLEN = (#const EVP_CTRL_GCM_SET_IVLEN)
ctrl_GCM_GET_TAG =  (#const EVP_CTRL_GCM_GET_TAG)
ctrl_GCM_SET_TAG =  (#const EVP_CTRL_GCM_SET_TAG)
#else
-- not sure if this is a good idea to hardcode it.
ctrl_GCM_SET_IVLEN = 0x9
ctrl_GCM_GET_TAG = 0x10
ctrl_GCM_SET_TAG = 0x11
#endif
