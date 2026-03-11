{-# LANGUAGE CPP #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.Hash
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports hash algorithms supported by nettle:
--   <http://www.lysator.liu.se/~nisse/nettle/>
--
-----------------------------------------------------------------------------

module Crypto.Nettle.Hash (
	-- * HashAlgorithm class
	  HashAlgorithm(..)

	, hash
	, hash'
	, hashLazy
	, hashLazy'

	-- * hash algorithms
	-- | Only members of the SHA2 and SHA3 family have no known weaknesses (according to <http://www.lysator.liu.se/~nisse/nettle/nettle.html#Hash-functions>)

	-- ** GOSTHASH94
	, GOSTHASH94
	-- ** MD family
	, MD2
	, MD4
	, MD5
	-- ** RIPEMD160
	, RIPEMD160
	-- ** SHA1
	, SHA1
	-- ** SHA2 family
	-- | The SHA2 family supports digests lengths of 28, 32, 48 or 64 bytes (224, 256, 384, 512 bits),
	--   and the variants are named after the bit length.
	--
	--   The SHA2 family of hash functions were specified by NIST, intended as a replacement for 'SHA1'.
	, SHA224
	, SHA256
	, SHA384
	, SHA512
	-- ** SHA3 family
	-- | The SHA3 family supports (like SHA2) digests lengths of 28, 32, 48 or 64 bytes (224, 256, 384, 512 bits),
	--   and the variants are named after the bit length.
	--
	--   The SHA3 hash functions were specified by NIST in response to weaknesses in SHA1, and doubts about
	--   SHA2 hash functions which structurally are very similar to SHA1. The standard is a result of a competition,
	--   where the winner, also known as Keccak, was designed by Guido Bertoni, Joan Daemen, Michaël Peeters and
	--   Gilles Van Assche. It is structurally very different from all widely used earlier hash functions.
	, SHA3_224
	, SHA3_256
	, SHA3_384
	, SHA3_512
	) where

import Crypto.Nettle.Hash.ForeignImports
import Crypto.Nettle.Hash.Types
import Nettle.Utils

import Data.SecureMem
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

nettleHashBlockSize  :: NettleHashAlgorithm a => Tagged a Int
nettleHashBlockSize = nha_block_size
nettleHashDigestSize :: NettleHashAlgorithm a => Tagged a Int
nettleHashDigestSize = nha_digest_size
nettleHashName       :: NettleHashAlgorithm a => Tagged a String
nettleHashName = nha_name
nettleHashInit       :: NettleHashAlgorithm a => a
nettleHashInit = untagSelf $ do
		size <- nha_ctx_size
		initfun <- nha_init
		return $ nha_Ctx $ unsafeCreateSecureMem size $ \ctxptr ->
			initfun ctxptr
nettleHashUpdate     :: NettleHashAlgorithm a => a -> B.ByteString -> a
nettleHashUpdate c msg = untagSelf $ do
	updatefun <- nha_update
	return $ nha_Ctx $ unsafeDupablePerformIO $
		withSecureMemCopy (nha_ctx c) $ \ctxptr ->
		withByteStringPtr msg $ \msglen msgptr ->
			updatefun ctxptr msglen msgptr
nettleHashFinalize   :: NettleHashAlgorithm a => a -> B.ByteString
nettleHashFinalize c = flip witness c $ do
	digestSize <- nha_digest_size
	digestfun <- nha_digest
	return $ unsafeDupablePerformIO $
		B.create digestSize $ \digestptr -> do
			_ <- withSecureMemCopy (nha_ctx c) $ \ctxptr ->
				digestfun ctxptr (fromIntegral digestSize) digestptr
			return ()

class NettleHashAlgorithm a where
	nha_ctx_size    :: Tagged a Int
	nha_block_size  :: Tagged a Int
	nha_digest_size :: Tagged a Int
	nha_name        :: Tagged a String
	nha_init        :: Tagged a NettleHashInit
	nha_update      :: Tagged a NettleHashUpdate
	nha_digest      :: Tagged a NettleHashDigest
	nha_ctx         :: a -> SecureMem
	nha_Ctx         :: SecureMem -> a

#define INSTANCE_HASH(Typ) \
instance HashAlgorithm Typ where \
	{ hashBlockSize  = nettleHashBlockSize \
	; hashDigestSize = nettleHashDigestSize \
	; hashName       = nettleHashName \
	; hashInit       = nettleHashInit \
	; hashUpdate     = nettleHashUpdate \
	; hashFinalize   = nettleHashFinalize \
	}

-- | The GOST94 or GOST R 34.11-94 hash algorithm is a Soviet-era algorithm used in Russian government standards (see RFC 4357).
--   It outputs message digests of 32 bytes (256 bits).
data GOSTHASH94 = GOSTHASH94 { gosthash94_ctx :: SecureMem }
instance NettleHashAlgorithm GOSTHASH94 where
	nha_ctx_size    = Tagged c_gosthash94_ctx_size
	nha_block_size  = Tagged c_gosthash94_block_size
	nha_digest_size = Tagged c_gosthash94_digest_size
	nha_name        = Tagged "GOSTHAST94"
	nha_init        = Tagged c_gosthash94_init
	nha_update      = Tagged c_gosthash94_update
	nha_digest      = Tagged c_gosthash94_digest
	nha_ctx         = gosthash94_ctx
	nha_Ctx         = GOSTHASH94
INSTANCE_HASH(GOSTHASH94)



-- | 'MD2' is a hash function of Ronald Rivest's, described in RFC 1319. It outputs message digests of 16 bytes (128 bits).
data MD2 = MD2 { md2_ctx :: SecureMem }
instance NettleHashAlgorithm MD2 where
	nha_ctx_size    = Tagged c_md2_ctx_size
	nha_block_size  = Tagged c_md2_block_size
	nha_digest_size = Tagged c_md2_digest_size
	nha_name        = Tagged "MD2"
	nha_init        = Tagged c_md2_init
	nha_update      = Tagged c_md2_update
	nha_digest      = Tagged c_md2_digest
	nha_ctx         = md2_ctx
	nha_Ctx         = MD2
INSTANCE_HASH(MD2)

-- | 'MD4' is a hash function of Ronald Rivest's, described in RFC 1320. It outputs message digests of 16 bytes (128 bits).
data MD4 = MD4 { md4_ctx :: SecureMem }
instance NettleHashAlgorithm MD4 where
	nha_ctx_size    = Tagged c_md4_ctx_size
	nha_block_size  = Tagged c_md4_block_size
	nha_digest_size = Tagged c_md4_digest_size
	nha_name        = Tagged "MD4"
	nha_init        = Tagged c_md4_init
	nha_update      = Tagged c_md4_update
	nha_digest      = Tagged c_md4_digest
	nha_ctx         = md4_ctx
	nha_Ctx         = MD4
INSTANCE_HASH(MD4)

-- | 'MD5' is a hash function of Ronald Rivest's, described in RFC 1321. It outputs message digests of 16 bytes (128 bits).
data MD5 = MD5 { md5_ctx :: SecureMem }
instance NettleHashAlgorithm MD5 where
	nha_ctx_size    = Tagged c_md5_ctx_size
	nha_block_size  = Tagged c_md5_block_size
	nha_digest_size = Tagged c_md5_digest_size
	nha_name        = Tagged "MD5"
	nha_init        = Tagged c_md5_init
	nha_update      = Tagged c_md5_update
	nha_digest      = Tagged c_md5_digest
	nha_ctx         = md5_ctx
	nha_Ctx         = MD5
INSTANCE_HASH(MD5)

-- | 'RIPEMD160' is a hash function designed by Hans Dobbertin, Antoon Bosselaers, and Bart Preneel, as a strengthened version of RIPEMD.
--   It produces message digests of 20 bytes (160 bits).
data RIPEMD160 = RIPEMD160 { ripemd160_ctx :: SecureMem }
instance NettleHashAlgorithm RIPEMD160 where
	nha_ctx_size    = Tagged c_ripemd160_ctx_size
	nha_block_size  = Tagged c_ripemd160_block_size
	nha_digest_size = Tagged c_ripemd160_digest_size
	nha_name        = Tagged "RIPEMD160"
	nha_init        = Tagged c_ripemd160_init
	nha_update      = Tagged c_ripemd160_update
	nha_digest      = Tagged c_ripemd160_digest
	nha_ctx         = ripemd160_ctx
	nha_Ctx         = RIPEMD160
INSTANCE_HASH(RIPEMD160)


-- | 'SHA1' is a hash function specified by NIST (The U.S. National Institute for Standards and Technology).
--   It produces message digests of 20 bytes (160 bits).
data SHA1 = SHA1 { sha1_ctx :: SecureMem }
instance NettleHashAlgorithm SHA1 where
	nha_ctx_size    = Tagged c_sha1_ctx_size
	nha_block_size  = Tagged c_sha1_block_size
	nha_digest_size = Tagged c_sha1_digest_size
	nha_name        = Tagged "SHA1"
	nha_init        = Tagged c_sha1_init
	nha_update      = Tagged c_sha1_update
	nha_digest      = Tagged c_sha1_digest
	nha_ctx         = sha1_ctx
	nha_Ctx         = SHA1
INSTANCE_HASH(SHA1)

-- | 'SHA224' is a member of the SHA2 family which outputs messages digests of 28 bytes (224 bits).
data SHA224 = SHA224 { sha224_ctx :: SecureMem }
instance NettleHashAlgorithm SHA224 where
	nha_ctx_size    = Tagged c_sha224_ctx_size
	nha_block_size  = Tagged c_sha224_block_size
	nha_digest_size = Tagged c_sha224_digest_size
	nha_name        = Tagged "SHA224"
	nha_init        = Tagged c_sha224_init
	nha_update      = Tagged c_sha224_update
	nha_digest      = Tagged c_sha224_digest
	nha_ctx         = sha224_ctx
	nha_Ctx         = SHA224
INSTANCE_HASH(SHA224)

-- | 'SHA256' is a member of the SHA2 family which outputs messages digests of 32 bytes (256 bits).
data SHA256 = SHA256 { sha256_ctx :: SecureMem }
instance NettleHashAlgorithm SHA256 where
	nha_ctx_size    = Tagged c_sha256_ctx_size
	nha_block_size  = Tagged c_sha256_block_size
	nha_digest_size = Tagged c_sha256_digest_size
	nha_name        = Tagged "SHA256"
	nha_init        = Tagged c_sha256_init
	nha_update      = Tagged c_sha256_update
	nha_digest      = Tagged c_sha256_digest
	nha_ctx         = sha256_ctx
	nha_Ctx         = SHA256
INSTANCE_HASH(SHA256)

-- | 'SHA384' is a member of the SHA2 family which outputs messages digests of 48 bytes (384 bits).
data SHA384 = SHA384 { sha384_ctx :: SecureMem }
instance NettleHashAlgorithm SHA384 where
	nha_ctx_size    = Tagged c_sha384_ctx_size
	nha_block_size  = Tagged c_sha384_block_size
	nha_digest_size = Tagged c_sha384_digest_size
	nha_name        = Tagged "SHA384"
	nha_init        = Tagged c_sha384_init
	nha_update      = Tagged c_sha384_update
	nha_digest      = Tagged c_sha384_digest
	nha_ctx         = sha384_ctx
	nha_Ctx         = SHA384
INSTANCE_HASH(SHA384)

-- | 'SHA512' is a member of the SHA2 family which outputs messages digests of 64 bytes (512 bits).
data SHA512 = SHA512 { sha512_ctx :: SecureMem }
instance NettleHashAlgorithm SHA512 where
	nha_ctx_size    = Tagged c_sha512_ctx_size
	nha_block_size  = Tagged c_sha512_block_size
	nha_digest_size = Tagged c_sha512_digest_size
	nha_name        = Tagged "SHA512"
	nha_init        = Tagged c_sha512_init
	nha_update      = Tagged c_sha512_update
	nha_digest      = Tagged c_sha512_digest
	nha_ctx         = sha512_ctx
	nha_Ctx         = SHA512
INSTANCE_HASH(SHA512)

-- | 'SHA3_224' is a member of the SHA3 family which outputs messages digests of 28 bytes (224 bits).
data SHA3_224 = SHA3_224 { sha3_224_ctx :: SecureMem }
instance NettleHashAlgorithm SHA3_224 where
	nha_ctx_size    = Tagged c_sha3_224_ctx_size
	nha_block_size  = Tagged c_sha3_224_block_size
	nha_digest_size = Tagged c_sha3_224_digest_size
	nha_name        = Tagged "SHA3-224"
	nha_init        = Tagged c_sha3_224_init
	nha_update      = Tagged c_sha3_224_update
	nha_digest      = Tagged c_sha3_224_digest
	nha_ctx         = sha3_224_ctx
	nha_Ctx         = SHA3_224
INSTANCE_HASH(SHA3_224)

-- | 'SHA3_256' is a member of the SHA3 family which outputs messages digests of 32 bytes (256 bits).
data SHA3_256 = SHA3_256 { sha3_256_ctx :: SecureMem }
instance NettleHashAlgorithm SHA3_256 where
	nha_ctx_size    = Tagged c_sha3_256_ctx_size
	nha_block_size  = Tagged c_sha3_256_block_size
	nha_digest_size = Tagged c_sha3_256_digest_size
	nha_name        = Tagged "SHA3-256"
	nha_init        = Tagged c_sha3_256_init
	nha_update      = Tagged c_sha3_256_update
	nha_digest      = Tagged c_sha3_256_digest
	nha_ctx         = sha3_256_ctx
	nha_Ctx         = SHA3_256
INSTANCE_HASH(SHA3_256)

-- | 'SHA3_384' is a member of the SHA3 family which outputs messages digests of 48 bytes (384 bits).
data SHA3_384 = SHA3_384 { sha3_384_ctx :: SecureMem }
instance NettleHashAlgorithm SHA3_384 where
	nha_ctx_size    = Tagged c_sha3_384_ctx_size
	nha_block_size  = Tagged c_sha3_384_block_size
	nha_digest_size = Tagged c_sha3_384_digest_size
	nha_name        = Tagged "SHA3-384"
	nha_init        = Tagged c_sha3_384_init
	nha_update      = Tagged c_sha3_384_update
	nha_digest      = Tagged c_sha3_384_digest
	nha_ctx         = sha3_384_ctx
	nha_Ctx         = SHA3_384
INSTANCE_HASH(SHA3_384)

-- | 'SHA3_512' is a member of the SHA3 family which outputs messages digests of 64 bytes (512 bits).
data SHA3_512 = SHA3_512 { sha3_512_ctx :: SecureMem }
instance NettleHashAlgorithm SHA3_512 where
	nha_ctx_size    = Tagged c_sha3_512_ctx_size
	nha_block_size  = Tagged c_sha3_512_block_size
	nha_digest_size = Tagged c_sha3_512_digest_size
	nha_name        = Tagged "SHA3-512"
	nha_init        = Tagged c_sha3_512_init
	nha_update      = Tagged c_sha3_512_update
	nha_digest      = Tagged c_sha3_512_digest
	nha_ctx         = sha3_512_ctx
	nha_Ctx         = SHA3_512
INSTANCE_HASH(SHA3_512)
