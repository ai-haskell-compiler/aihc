{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE ForeignFunctionInterface #-}

module Crypto.Nettle.Hash.ForeignImports
	( NettleHashInit
	, NettleHashUpdate
	, NettleHashDigest

	, c_sha256_ctx_size
	, c_sha256_digest_size
	, c_sha256_block_size
	, c_sha256_init
	, c_sha256_update
	, c_sha256_digest

	, c_sha224_ctx_size
	, c_sha224_digest_size
	, c_sha224_block_size
	, c_sha224_init
	, c_sha224_update
	, c_sha224_digest

	, c_sha512_ctx_size
	, c_sha512_digest_size
	, c_sha512_block_size
	, c_sha512_init
	, c_sha512_update
	, c_sha512_digest

	, c_sha384_ctx_size
	, c_sha384_digest_size
	, c_sha384_block_size
	, c_sha384_init
	, c_sha384_update
	, c_sha384_digest

	, c_sha3_224_ctx_size
	, c_sha3_224_digest_size
	, c_sha3_224_block_size
	, c_sha3_224_init
	, c_sha3_224_update
	, c_sha3_224_digest

	, c_sha3_256_ctx_size
	, c_sha3_256_digest_size
	, c_sha3_256_block_size
	, c_sha3_256_init
	, c_sha3_256_update
	, c_sha3_256_digest

	, c_sha3_384_ctx_size
	, c_sha3_384_digest_size
	, c_sha3_384_block_size
	, c_sha3_384_init
	, c_sha3_384_update
	, c_sha3_384_digest

	, c_sha3_512_ctx_size
	, c_sha3_512_digest_size
	, c_sha3_512_block_size
	, c_sha3_512_init
	, c_sha3_512_update
	, c_sha3_512_digest

	, c_md5_ctx_size
	, c_md5_digest_size
	, c_md5_block_size
	, c_md5_init
	, c_md5_update
	, c_md5_digest

	, c_md2_ctx_size
	, c_md2_digest_size
	, c_md2_block_size
	, c_md2_init
	, c_md2_update
	, c_md2_digest

	, c_md4_ctx_size
	, c_md4_digest_size
	, c_md4_block_size
	, c_md4_init
	, c_md4_update
	, c_md4_digest

	, c_ripemd160_ctx_size
	, c_ripemd160_digest_size
	, c_ripemd160_block_size
	, c_ripemd160_init
	, c_ripemd160_update
	, c_ripemd160_digest

	, c_sha1_ctx_size
	, c_sha1_digest_size
	, c_sha1_block_size
	, c_sha1_init
	, c_sha1_update
	, c_sha1_digest

	, c_gosthash94_ctx_size
	, c_gosthash94_digest_size
	, c_gosthash94_block_size
	, c_gosthash94_init
	, c_gosthash94_update
	, c_gosthash94_digest

	, c_umac32_ctx_size
	, c_umac32_digest_size
	, c_umac32_set_key
	, c_umac32_set_nonce
	, c_umac32_update
	, c_umac32_digest

	, c_umac64_ctx_size
	, c_umac64_digest_size
	, c_umac64_set_key
	, c_umac64_set_nonce
	, c_umac64_update
	, c_umac64_digest

	, c_umac96_ctx_size
	, c_umac96_digest_size
	, c_umac96_set_key
	, c_umac96_set_nonce
	, c_umac96_update
	, c_umac96_digest

	, c_umac128_ctx_size
	, c_umac128_digest_size
	, c_umac128_set_key
	, c_umac128_set_nonce
	, c_umac128_update
	, c_umac128_digest
	) where

import Nettle.Utils

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

#include "nettle-hash.h"

type NettleHashInit = Ptr Word8 -> IO ()
type NettleHashUpdate = Ptr Word8 -> Word -> Ptr Word8 -> IO ()
type NettleHashDigest = Ptr Word8 -> Word -> Ptr Word8 -> IO ()

c_sha256_ctx_size :: Int
c_sha256_ctx_size = #{size struct sha256_ctx}
c_sha256_digest_size :: Int
c_sha256_digest_size = #{const SHA256_DIGEST_SIZE}
c_sha256_block_size :: Int
c_sha256_block_size = #{const SHA256_DATA_SIZE}
foreign import ccall unsafe "nettle_sha256_init"
	c_sha256_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha256_update"
	c_sha256_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha256_digest"
	c_sha256_digest :: NettleHashDigest

c_sha224_ctx_size :: Int
c_sha224_ctx_size = #{size struct sha224_ctx}
c_sha224_digest_size :: Int
c_sha224_digest_size = #{const SHA224_DIGEST_SIZE}
c_sha224_block_size :: Int
c_sha224_block_size = #{const SHA224_DATA_SIZE}
foreign import ccall unsafe "nettle_sha224_init"
	c_sha224_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha256_update"
	c_sha224_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha224_digest"
	c_sha224_digest :: NettleHashDigest

c_sha512_ctx_size :: Int
c_sha512_ctx_size = #{size struct sha512_ctx}
c_sha512_digest_size :: Int
c_sha512_digest_size = #{const SHA512_DIGEST_SIZE}
c_sha512_block_size :: Int
c_sha512_block_size = #{const SHA512_DATA_SIZE}
foreign import ccall unsafe "nettle_sha512_init"
	c_sha512_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha512_update"
	c_sha512_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha512_digest"
	c_sha512_digest :: NettleHashDigest

c_sha384_ctx_size :: Int
c_sha384_ctx_size = #{size struct sha384_ctx}
c_sha384_digest_size :: Int
c_sha384_digest_size = #{const SHA384_DIGEST_SIZE}
c_sha384_block_size :: Int
c_sha384_block_size = #{const SHA384_DATA_SIZE}
foreign import ccall unsafe "nettle_sha384_init"
	c_sha384_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha512_update"
	c_sha384_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha384_digest"
	c_sha384_digest :: NettleHashDigest

c_sha3_224_ctx_size :: Int
c_sha3_224_ctx_size = #{size struct sha3_224_ctx}
c_sha3_224_digest_size :: Int
c_sha3_224_digest_size = #{const SHA3_224_DIGEST_SIZE}
c_sha3_224_block_size :: Int
c_sha3_224_block_size = #{const SHA3_224_DATA_SIZE}
foreign import ccall unsafe "nettle_sha3_224_init"
	c_sha3_224_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha3_224_update"
	c_sha3_224_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha3_224_digest"
	c_sha3_224_digest :: NettleHashDigest

c_sha3_256_ctx_size :: Int
c_sha3_256_ctx_size = #{size struct sha3_256_ctx}
c_sha3_256_digest_size :: Int
c_sha3_256_digest_size = #{const SHA3_256_DIGEST_SIZE}
c_sha3_256_block_size :: Int
c_sha3_256_block_size = #{const SHA3_256_DATA_SIZE}
foreign import ccall unsafe "nettle_sha3_256_init"
	c_sha3_256_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha3_256_update"
	c_sha3_256_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha3_256_digest"
	c_sha3_256_digest :: NettleHashDigest

c_sha3_384_ctx_size :: Int
c_sha3_384_ctx_size = #{size struct sha3_384_ctx}
c_sha3_384_digest_size :: Int
c_sha3_384_digest_size = #{const SHA3_384_DIGEST_SIZE}
c_sha3_384_block_size :: Int
c_sha3_384_block_size = #{const SHA3_384_DATA_SIZE}
foreign import ccall unsafe "nettle_sha3_384_init"
	c_sha3_384_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha3_384_update"
	c_sha3_384_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha3_384_digest"
	c_sha3_384_digest :: NettleHashDigest

c_sha3_512_ctx_size :: Int
c_sha3_512_ctx_size = #{size struct sha3_512_ctx}
c_sha3_512_digest_size :: Int
c_sha3_512_digest_size = #{const SHA3_512_DIGEST_SIZE}
c_sha3_512_block_size :: Int
c_sha3_512_block_size = #{const SHA3_512_DATA_SIZE}
foreign import ccall unsafe "nettle_sha3_512_init"
	c_sha3_512_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha3_512_update"
	c_sha3_512_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha3_512_digest"
	c_sha3_512_digest :: NettleHashDigest

c_md5_ctx_size :: Int
c_md5_ctx_size = #{size struct md5_ctx}
c_md5_digest_size :: Int
c_md5_digest_size = #{const MD5_DIGEST_SIZE}
c_md5_block_size :: Int
c_md5_block_size = #{const MD5_DATA_SIZE}
foreign import ccall unsafe "nettle_md5_init"
	c_md5_init :: NettleHashInit
foreign import ccall unsafe "nettle_md5_update"
	c_md5_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_md5_digest"
	c_md5_digest :: NettleHashDigest

c_md2_ctx_size :: Int
c_md2_ctx_size = #{size struct md2_ctx}
c_md2_digest_size :: Int
c_md2_digest_size = #{const MD2_DIGEST_SIZE}
c_md2_block_size :: Int
c_md2_block_size = #{const MD2_DATA_SIZE}
foreign import ccall unsafe "nettle_md2_init"
	c_md2_init :: NettleHashInit
foreign import ccall unsafe "nettle_md2_update"
	c_md2_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_md2_digest"
	c_md2_digest :: NettleHashDigest

c_md4_ctx_size :: Int
c_md4_ctx_size = #{size struct md4_ctx}
c_md4_digest_size :: Int
c_md4_digest_size = #{const MD4_DIGEST_SIZE}
c_md4_block_size :: Int
c_md4_block_size = #{const MD4_DATA_SIZE}
foreign import ccall unsafe "nettle_md4_init"
	c_md4_init :: NettleHashInit
foreign import ccall unsafe "nettle_md4_update"
	c_md4_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_md4_digest"
	c_md4_digest :: NettleHashDigest

c_ripemd160_ctx_size :: Int
c_ripemd160_ctx_size = #{size struct ripemd160_ctx}
c_ripemd160_digest_size :: Int
c_ripemd160_digest_size = #{const RIPEMD160_DIGEST_SIZE}
c_ripemd160_block_size :: Int
c_ripemd160_block_size = #{const RIPEMD160_DATA_SIZE}
foreign import ccall unsafe "nettle_ripemd160_init"
	c_ripemd160_init :: NettleHashInit
foreign import ccall unsafe "nettle_ripemd160_update"
	c_ripemd160_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_ripemd160_digest"
	c_ripemd160_digest :: NettleHashDigest

c_sha1_ctx_size :: Int
c_sha1_ctx_size = #{size struct sha1_ctx}
c_sha1_digest_size :: Int
c_sha1_digest_size = #{const SHA1_DIGEST_SIZE}
c_sha1_block_size :: Int
c_sha1_block_size = #{const SHA1_DATA_SIZE}
foreign import ccall unsafe "nettle_sha1_init"
	c_sha1_init :: NettleHashInit
foreign import ccall unsafe "nettle_sha1_update"
	c_sha1_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_sha1_digest"
	c_sha1_digest :: NettleHashDigest

c_gosthash94_ctx_size :: Int
c_gosthash94_ctx_size = #{size struct gosthash94_ctx}
c_gosthash94_digest_size :: Int
c_gosthash94_digest_size = #{const GOSTHASH94_DIGEST_SIZE}
c_gosthash94_block_size :: Int
c_gosthash94_block_size = #{const GOSTHASH94_DATA_SIZE}
foreign import ccall unsafe "nettle_gosthash94_init"
	c_gosthash94_init :: NettleHashInit
foreign import ccall unsafe "nettle_gosthash94_update"
	c_gosthash94_update :: NettleHashUpdate
foreign import ccall unsafe "nettle_gosthash94_digest"
	c_gosthash94_digest :: NettleHashDigest


c_umac32_ctx_size :: Int
c_umac32_ctx_size = #{size struct umac32_ctx}
c_umac32_digest_size :: Int
c_umac32_digest_size = #{const UMAC32_DIGEST_SIZE}
foreign import ccall unsafe "nettle_umac32_set_key"
	c_umac32_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac32_set_nonce"
	c_umac32_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac32_update"
	c_umac32_update :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac32_digest"
	c_umac32_digest :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()

c_umac64_ctx_size :: Int
c_umac64_ctx_size = #{size struct umac64_ctx}
c_umac64_digest_size :: Int
c_umac64_digest_size = #{const UMAC64_DIGEST_SIZE}
foreign import ccall unsafe "nettle_umac64_set_key"
	c_umac64_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac64_set_nonce"
	c_umac64_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac64_update"
	c_umac64_update :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac64_digest"
	c_umac64_digest :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()

c_umac96_ctx_size :: Int
c_umac96_ctx_size = #{size struct umac96_ctx}
c_umac96_digest_size :: Int
c_umac96_digest_size = #{const UMAC96_DIGEST_SIZE}
foreign import ccall unsafe "nettle_umac96_set_key"
	c_umac96_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac96_set_nonce"
	c_umac96_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac96_update"
	c_umac96_update :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac96_digest"
	c_umac96_digest :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()

c_umac128_ctx_size :: Int
c_umac128_ctx_size = #{size struct umac128_ctx}
c_umac128_digest_size :: Int
c_umac128_digest_size = #{const UMAC128_DIGEST_SIZE}
foreign import ccall unsafe "nettle_umac128_set_key"
	c_umac128_set_key :: Ptr Word8 -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac128_set_nonce"
	c_umac128_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac128_update"
	c_umac128_update :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
foreign import ccall unsafe "nettle_umac128_digest"
	c_umac128_digest :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
