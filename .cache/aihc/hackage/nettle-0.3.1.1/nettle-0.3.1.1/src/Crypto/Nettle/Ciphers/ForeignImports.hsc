{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Crypto.Nettle.Ciphers.ForeignImports
	( NettleCryptFunc
	, NettleBlockMode
	, NettleGCMMode

	, c_cbc_encrypt
	, c_cbc_decrypt

	, c_cfb_encrypt
	, c_cfb_decrypt

	, c_ctr_crypt

	, c_gcm_ctx_size
	, c_gcm_key_size
	, c_gcm_set_key
	, c_gcm_set_iv
	, c_gcm_update
	, c_gcm_encrypt
	, c_gcm_decrypt
	, c_gcm_digest

	, c_hs_aes_ctx_size
	, c_hs_aes_init
	, c_hs_aes_encrypt
	, p_hs_aes_encrypt
	, c_hs_aes_decrypt
	, p_hs_aes_decrypt

	, c_hs_aes128_ctx_size
	, c_hs_aes128_ctx_encrypt
	, c_hs_aes128_ctx_decrypt
	, c_hs_aes128_init
	, c_aes128_encrypt
	, p_aes128_encrypt
	, c_aes128_decrypt
	, p_aes128_decrypt

	, c_hs_aes192_ctx_size
	, c_hs_aes192_ctx_encrypt
	, c_hs_aes192_ctx_decrypt
	, c_hs_aes192_init
	, c_aes192_encrypt
	, p_aes192_encrypt
	, c_aes192_decrypt
	, p_aes192_decrypt

	, c_hs_aes256_ctx_size
	, c_hs_aes256_ctx_encrypt
	, c_hs_aes256_ctx_decrypt
	, c_hs_aes256_init
	, c_aes256_encrypt
	, p_aes256_encrypt
	, c_aes256_decrypt
	, p_aes256_decrypt

	, c_arctwo_ctx_size
	, c_arctwo_set_key
	, c_arctwo_set_key_ekb
	, c_arctwo_encrypt
	, p_arctwo_encrypt
	, c_arctwo_decrypt
	, p_arctwo_decrypt
	, c_arctwo_set_key_gutmann

	, c_blowfish_ctx_size
	, c_blowfish_set_key
	, c_blowfish_encrypt
	, p_blowfish_encrypt
	, c_blowfish_decrypt
	, p_blowfish_decrypt

	, c_hs_camellia_ctx_size
	, c_hs_camellia_init
	, c_hs_camellia_encrypt
	, p_hs_camellia_encrypt
	, c_hs_camellia_decrypt
	, p_hs_camellia_decrypt

	, c_hs_camellia128_ctx_size
	, c_hs_camellia128_ctx_encrypt
	, c_hs_camellia128_ctx_decrypt
	, c_hs_camellia128_init
	, c_camellia128_crypt
	, p_camellia128_crypt

	, c_hs_camellia192_ctx_size
	, c_hs_camellia192_ctx_encrypt
	, c_hs_camellia192_ctx_decrypt
	, c_hs_camellia192_init
	, c_camellia192_crypt
	, p_camellia192_crypt

	, c_hs_camellia256_ctx_size
	, c_hs_camellia256_ctx_encrypt
	, c_hs_camellia256_ctx_decrypt
	, c_hs_camellia256_init
	, c_camellia256_crypt
	, p_camellia256_crypt

	, c_cast128_ctx_size
	, c_cast5_set_key
	, c_cast128_encrypt
	, p_cast128_encrypt
	, c_cast128_decrypt
	, p_cast128_decrypt

	, c_des_ctx_size
	, c_des_set_key
	, c_des_encrypt
	, p_des_encrypt
	, c_des_decrypt
	, p_des_decrypt

	, c_des3_ctx_size
	, c_des3_set_key
	, c_des3_encrypt
	, p_des3_encrypt
	, c_des3_decrypt
	, p_des3_decrypt

	, c_serpent_ctx_size
	, c_serpent_set_key
	, c_serpent_encrypt
	, p_serpent_encrypt
	, c_serpent_decrypt
	, p_serpent_decrypt

	, c_twofish_ctx_size
	, c_twofish_set_key
	, c_twofish_encrypt
	, p_twofish_encrypt
	, c_twofish_decrypt
	, p_twofish_decrypt

	, c_arcfour_ctx_size
	, c_arcfour_set_key
	, c_arcfour_crypt

	, c_chacha_ctx_size
	, c_chacha_set_key
	, c_chacha_set_nonce
	, c_chacha_crypt

	, c_salsa20_ctx_size
	, c_salsa20_set_key
	, c_salsa20_set_nonce
	, c_salsa20_crypt
	, c_salsa20r12_crypt

	, c_chacha_poly1305_ctx_size
	, c_chacha_poly1305_set_key
	, c_chacha_poly1305_set_nonce
	, c_chacha_poly1305_update
	, c_chacha_poly1305_encrypt
	, c_chacha_poly1305_decrypt
	, c_chacha_poly1305_digest
	) where

import Nettle.Utils

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

#include "nettle-ciphers.h"

type NettleCryptFunc = Ptr Word8 -> Word -> Ptr Word8 -> Ptr Word8 -> IO ()
type NettleBlockMode = Ptr Word8 -> FunPtr NettleCryptFunc -> Word -> Ptr Word8 -> Word -> Ptr Word8 -> Ptr Word8 -> IO ()
type NettleGCMMode = Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> FunPtr NettleCryptFunc -> Word -> Ptr Word8 -> Ptr Word8 -> IO ()

foreign import ccall unsafe "nettle_cbc_encrypt"
	c_cbc_encrypt :: NettleBlockMode
foreign import ccall unsafe "nettle_cbc_decrypt"
	c_cbc_decrypt :: NettleBlockMode

foreign import ccall unsafe "hs_nettle_cfb_encrypt"
	c_cfb_encrypt :: NettleBlockMode
foreign import ccall unsafe "hs_nettle_cfb_decrypt"
	c_cfb_decrypt :: NettleBlockMode

foreign import ccall unsafe "nettle_ctr_crypt"
	c_ctr_crypt :: NettleBlockMode

c_gcm_ctx_size :: Int
c_gcm_ctx_size = #{size struct gcm_ctx}
c_gcm_key_size :: Int
c_gcm_key_size = #{size struct gcm_key}
foreign import ccall unsafe "nettle_gcm_set_key"
	c_gcm_set_key :: Ptr Word8 -> Ptr Word8 -> FunPtr NettleCryptFunc -> IO ()
foreign import ccall unsafe "nettle_gcm_set_iv"
	c_gcm_set_iv :: Ptr Word8 -> Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_gcm_update"
	c_gcm_update :: Ptr Word8 -> Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_gcm_encrypt"
	c_gcm_encrypt :: NettleGCMMode
foreign import ccall unsafe "nettle_gcm_decrypt"
	c_gcm_decrypt :: NettleGCMMode
foreign import ccall unsafe "nettle_gcm_digest"
	c_gcm_digest :: Ptr Word8 -> Ptr Word8 -> Ptr Word8 -> FunPtr NettleCryptFunc -> Word -> Ptr Word8 -> IO ()

-- block ciphers

c_hs_aes_ctx_size :: Int
c_hs_aes_ctx_size = #{size struct hs_aes_ctx}
foreign import ccall unsafe "hs_nettle_aes_init"
	c_hs_aes_init :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "hs_nettle_aes_encrypt"
	c_hs_aes_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&hs_nettle_aes_encrypt"
	p_hs_aes_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "hs_nettle_aes_decrypt"
	c_hs_aes_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&hs_nettle_aes_decrypt"
	p_hs_aes_decrypt :: FunPtr NettleCryptFunc

c_hs_aes128_ctx_size :: Int
c_hs_aes128_ctx_size = #{size struct hs_aes128_ctx}
c_hs_aes128_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes128_ctx_encrypt = #ptr struct hs_aes128_ctx, encrypt
c_hs_aes128_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes128_ctx_decrypt = #ptr struct hs_aes128_ctx, decrypt
foreign import ccall unsafe "hs_nettle_aes128_init"
	c_hs_aes128_init :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_aes128_encrypt"
	c_aes128_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes128_encrypt"
	p_aes128_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_aes128_decrypt"
	c_aes128_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes128_decrypt"
	p_aes128_decrypt :: FunPtr NettleCryptFunc

c_hs_aes192_ctx_size :: Int
c_hs_aes192_ctx_size = #{size struct hs_aes192_ctx}
c_hs_aes192_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes192_ctx_encrypt = #ptr struct hs_aes192_ctx, encrypt
c_hs_aes192_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes192_ctx_decrypt = #ptr struct hs_aes192_ctx, decrypt
foreign import ccall unsafe "hs_nettle_aes192_init"
	c_hs_aes192_init :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_aes192_encrypt"
	c_aes192_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes192_encrypt"
	p_aes192_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_aes192_decrypt"
	c_aes192_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes192_decrypt"
	p_aes192_decrypt :: FunPtr NettleCryptFunc

c_hs_aes256_ctx_size :: Int
c_hs_aes256_ctx_size = #{size struct hs_aes256_ctx}
c_hs_aes256_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes256_ctx_encrypt = #ptr struct hs_aes256_ctx, encrypt
c_hs_aes256_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_aes256_ctx_decrypt = #ptr struct hs_aes256_ctx, decrypt
foreign import ccall unsafe "hs_nettle_aes256_init"
	c_hs_aes256_init :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_aes256_encrypt"
	c_aes256_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes256_encrypt"
	p_aes256_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_aes256_decrypt"
	c_aes256_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_aes256_decrypt"
	p_aes256_decrypt :: FunPtr NettleCryptFunc

c_arctwo_ctx_size :: Int
c_arctwo_ctx_size = #{size struct arctwo_ctx}
foreign import ccall unsafe "nettle_arctwo_set_key"
	c_arctwo_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_arctwo_set_key_ekb"
	c_arctwo_set_key_ekb :: Ptr Word8 -> Word -> Ptr Word8 -> Word -> IO ()
foreign import ccall unsafe "nettle_arctwo_set_key_gutmann"
	c_arctwo_set_key_gutmann :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_arctwo_encrypt"
	c_arctwo_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_arctwo_encrypt"
	p_arctwo_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_arctwo_decrypt"
	c_arctwo_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_arctwo_decrypt"
	p_arctwo_decrypt :: FunPtr NettleCryptFunc

c_blowfish_ctx_size :: Int
c_blowfish_ctx_size = #{size struct blowfish_ctx}
foreign import ccall unsafe "nettle_blowfish_set_key"
	c_blowfish_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_blowfish_encrypt"
	c_blowfish_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_blowfish_encrypt"
	p_blowfish_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_blowfish_decrypt"
	c_blowfish_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_blowfish_decrypt"
	p_blowfish_decrypt :: FunPtr NettleCryptFunc

c_hs_camellia_ctx_size :: Int
c_hs_camellia_ctx_size = #{size struct hs_camellia_ctx}
foreign import ccall unsafe "hs_nettle_camellia_init"
	c_hs_camellia_init :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "hs_nettle_camellia_encrypt"
	c_hs_camellia_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&hs_nettle_camellia_encrypt"
	p_hs_camellia_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "hs_nettle_camellia_decrypt"
	c_hs_camellia_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&hs_nettle_camellia_decrypt"
	p_hs_camellia_decrypt :: FunPtr NettleCryptFunc

c_hs_camellia128_ctx_size :: Int
c_hs_camellia128_ctx_size = #{size struct hs_camellia128_ctx}
c_hs_camellia128_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia128_ctx_encrypt = #ptr struct hs_camellia128_ctx, encrypt
c_hs_camellia128_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia128_ctx_decrypt = #ptr struct hs_camellia128_ctx, decrypt
foreign import ccall unsafe "hs_nettle_camellia128_init"
	c_hs_camellia128_init :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_camellia128_crypt"
	c_camellia128_crypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_camellia128_crypt"
	p_camellia128_crypt :: FunPtr NettleCryptFunc

c_hs_camellia192_ctx_size :: Int
c_hs_camellia192_ctx_size = #{size struct hs_camellia192_ctx}
c_hs_camellia192_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia192_ctx_encrypt = #ptr struct hs_camellia192_ctx, encrypt
c_hs_camellia192_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia192_ctx_decrypt = #ptr struct hs_camellia192_ctx, decrypt
foreign import ccall unsafe "hs_nettle_camellia192_init"
	c_hs_camellia192_init :: Ptr Word8 -> Ptr Word8 -> IO ()
-- 192 and 256 bit variants use same crypt function
foreign import ccall unsafe "nettle_camellia256_crypt"
	c_camellia192_crypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_camellia256_crypt"
	p_camellia192_crypt :: FunPtr NettleCryptFunc

c_hs_camellia256_ctx_size :: Int
c_hs_camellia256_ctx_size = #{size struct hs_camellia256_ctx}
c_hs_camellia256_ctx_encrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia256_ctx_encrypt = #ptr struct hs_camellia256_ctx, encrypt
c_hs_camellia256_ctx_decrypt :: Ptr Word8 -> Ptr Word8
c_hs_camellia256_ctx_decrypt = #ptr struct hs_camellia256_ctx, decrypt
foreign import ccall unsafe "hs_nettle_camellia256_init"
	c_hs_camellia256_init :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_camellia256_crypt"
	c_camellia256_crypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_camellia256_crypt"
	p_camellia256_crypt :: FunPtr NettleCryptFunc

c_cast128_ctx_size :: Int
c_cast128_ctx_size = #{size struct cast128_ctx}
-- cast128_set_key uses a 128-bit fixed size key, cast-5 supports the variable length
foreign import ccall unsafe "nettle_cast5_set_key"
	c_cast5_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_cast128_encrypt"
	c_cast128_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_cast128_encrypt"
	p_cast128_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_cast128_decrypt"
	c_cast128_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_cast128_decrypt"
	p_cast128_decrypt :: FunPtr NettleCryptFunc

c_des_ctx_size :: Int
c_des_ctx_size = #{size struct des_ctx}
foreign import ccall unsafe "nettle/des.h nettle_des_set_key"
	c_des_set_key :: Ptr Word8 -> Ptr Word8 -> IO () -- ignore return value
foreign import ccall unsafe "nettle/des.h nettle_des_encrypt"
	c_des_encrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle/des.h &nettle_des_encrypt"
	p_des_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle/des.h nettle_des_decrypt"
	c_des_decrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle/des.h &nettle_des_decrypt"
	p_des_decrypt :: FunPtr NettleCryptFunc

c_des3_ctx_size :: Int
c_des3_ctx_size = #{size struct des3_ctx}
foreign import ccall unsafe "nettle/des.h nettle_des3_set_key"
	c_des3_set_key :: Ptr Word8 -> Ptr Word8 -> IO () -- ignore return value
foreign import ccall unsafe "nettle/des.h nettle_des3_encrypt"
	c_des3_encrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle/des.h &nettle_des3_encrypt"
	p_des3_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle/des.h nettle_des3_decrypt"
	c_des3_decrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle/des.h &nettle_des3_decrypt"
	p_des3_decrypt :: FunPtr NettleCryptFunc

c_serpent_ctx_size :: Int
c_serpent_ctx_size = #{size struct serpent_ctx}
foreign import ccall unsafe "nettle_serpent_set_key"
	c_serpent_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_serpent_encrypt"
	c_serpent_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_serpent_encrypt"
	p_serpent_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_serpent_decrypt"
	c_serpent_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_serpent_decrypt"
	p_serpent_decrypt :: FunPtr NettleCryptFunc

c_twofish_ctx_size :: Int
c_twofish_ctx_size = #{size struct twofish_ctx}
foreign import ccall unsafe "nettle_twofish_set_key"
	c_twofish_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_twofish_encrypt"
	c_twofish_encrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_twofish_encrypt"
	p_twofish_encrypt :: FunPtr NettleCryptFunc
foreign import ccall unsafe "nettle_twofish_decrypt"
	c_twofish_decrypt :: NettleCryptFunc
foreign import ccall unsafe "&nettle_twofish_decrypt"
	p_twofish_decrypt :: FunPtr NettleCryptFunc


-- stream ciphers
c_arcfour_ctx_size :: Int
c_arcfour_ctx_size = #{size struct arcfour_ctx}
foreign import ccall unsafe "nettle_arcfour_set_key"
	c_arcfour_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_arcfour_crypt"
	c_arcfour_crypt :: NettleCryptFunc

c_chacha_ctx_size :: Int
c_chacha_ctx_size = #{size struct chacha_ctx}
foreign import ccall unsafe "nettle_chacha_set_key"
	c_chacha_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_chacha_set_nonce"
	c_chacha_set_nonce :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_chacha_crypt"
	c_chacha_crypt :: NettleCryptFunc

c_salsa20_ctx_size :: Int
c_salsa20_ctx_size = #{size struct salsa20_ctx}
foreign import ccall unsafe "nettle_salsa20_set_key"
	c_salsa20_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_salsa20_set_nonce"
	c_salsa20_set_nonce :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_salsa20_crypt"
	c_salsa20_crypt :: NettleCryptFunc
foreign import ccall unsafe "nettle_salsa20r12_crypt"
	c_salsa20r12_crypt :: NettleCryptFunc

c_chacha_poly1305_ctx_size :: Int
c_chacha_poly1305_ctx_size = #{size struct chacha_poly1305_ctx}
foreign import ccall unsafe "nettle_chacha_poly1305_set_key"
	c_chacha_poly1305_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_chacha_poly1305_set_nonce"
	c_chacha_poly1305_set_nonce :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_chacha_poly1305_update"
	c_chacha_poly1305_update :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_chacha_poly1305_encrypt"
	c_chacha_poly1305_encrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle_chacha_poly1305_decrypt"
	c_chacha_poly1305_decrypt :: NettleCryptFunc
foreign import ccall unsafe "nettle_chacha_poly1305_digest"
	c_chacha_poly1305_digest :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
