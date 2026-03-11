{-# LANGUAGE CPP, MultiParamTypeClasses #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.UMAC
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the UMAC algorithms supported by nettle:
--   <http://www.lysator.liu.se/~nisse/nettle/>
--
-----------------------------------------------------------------------------

module Crypto.Nettle.UMAC (
	  UMAC(..)
	, UMAC32
	, UMAC64
	, UMAC96
	, UMAC128

	, umacInitKeyedHash
	) where

import Data.SecureMem
import Data.Tagged
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Lazy as L
import Control.Applicative ((<$>))
import Data.List (foldl')

import Nettle.Utils
import Crypto.Nettle.KeyedHash
import Crypto.Nettle.Hash.ForeignImports

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

{-|
'UMAC' is a class of keyed hash algorithms that take an additional nonce.

Keys for 'UMAC' are always 16 bytes; there are different digest sizes: 4, 8, 12 and 16 bytes (32, 64, 96 and 128 bits),
and the variants are named after the digest length in bits.

On initialization the nonce is set to 0; each finalize returns a new state with an incremented nonce.
The nonce is interpreted as 16-byte (128-bit) big-endian integer (and for string shorter than 16 bytes padded with zeroes /on the left/; setting empty nonces is not allowed).
-}
class UMAC u where
	-- | digest size in bytes
	umacDigestSize :: Tagged u Int
	-- | umac name ("UMAC" ++ digest size in bits)
	umacName :: Tagged u String
	umacName = (("UMAC" ++) . show . (8*)) <$> umacDigestSize
	-- | initialize a new context from a @key@ with a zero @nonce@
	umacInit :: B.ByteString {- ^ @key@ argument -} -> u
	-- | set a @nonce@; can be called anytime before producing the digest
	umacSetNonce :: u -> B.ByteString {- ^ @nonce@ argument -} -> u
	-- | append @message@ data to be hashed
	umacUpdate :: u -> B.ByteString {- ^ @message@ argument -} -> u
	-- | append lazy @message@ data to be hashed
	umacUpdateLazy :: u -> L.ByteString {- ^ @message@ argument -} -> u
	umacUpdateLazy u = foldl' umacUpdate u . L.toChunks
	-- | produce a digest, and return a new state with incremented nonce
	umacFinalize :: u -> (B.ByteString, u)

-- make all (UMAC u) a (KeyedHashAlgorithm u u)
umacKHDigestSize :: UMAC u => Tagged u Int
umacKHDigestSize = umacDigestSize
umacKHName :: UMAC u => Tagged u String
umacKHName = umacName
umacKHInit :: UMAC u => B.ByteString -> u
umacKHInit = umacInit
umacKHUpdate :: UMAC u => u -> B.ByteString -> u
umacKHUpdate = umacUpdate
umacKHFinalize :: UMAC u => u -> B.ByteString
umacKHFinalize = fst . umacFinalize

{-|
The default 'KeyedHash' generated for UMAC 'KeyedHashAlgorithm' instances use a zero nonce; to set a different nonce you need to use this initialization function (or use the 'UMAC' interface).

Once the UMAC lives as 'KeyedHash' the nonce cannot be changed anymore, as 'KeyedHash' hides all internal state.
-}
umacInitKeyedHash :: (UMAC u, KeyedHashAlgorithm u) => B.ByteString {- ^ @key@ argument -} -> B.ByteString {- ^ @nonce@ argument -} -> Tagged u KeyedHash
umacInitKeyedHash key nonce = KeyedHash <$> flip umacSetNonce nonce <$> tagSelf (umacInit key)

class NettleUMAC u where
	nu_ctx_size :: Tagged u Int
	nu_digest_size :: Tagged u Int
	nu_set_key :: Tagged u (Ptr Word8 -> Ptr Word8 -> IO ())
	nu_set_nonce :: Tagged u (Ptr Word8 -> Word -> Ptr Word8 -> IO ())
	nu_update :: Tagged u (Ptr Word8 -> Word -> Ptr Word8 -> IO ())
	nu_digest :: Tagged u (Ptr Word8 -> Word -> Ptr Word8 -> IO ())
	nu_ctx :: u -> SecureMem
	nu_Ctx :: SecureMem -> u

nettleUmacDigestSize :: NettleUMAC u => Tagged u Int
nettleUmacDigestSize = nu_digest_size
nettleUmacInit :: NettleUMAC u => B.ByteString -> u
nettleUmacInit key = if B.length key /= 16 then error "wrong key length" else untag go where
	go :: NettleUMAC u => Tagged u u
	go = do
		size <- nu_ctx_size
		set_key <- nu_set_key
		return $ nu_Ctx $ unsafeCreateSecureMem size $ \ctxptr ->
			withByteStringPtr key $ \_ keyptr ->
			set_key ctxptr keyptr
nettleUmacSetNonce :: NettleUMAC u => u -> B.ByteString -> u
nettleUmacSetNonce c nonce = if B.length nonce < 1 || B.length nonce > 16 then error "invalid nonce length" else untag $ go c where
	go :: NettleUMAC u => u -> Tagged u u
	go ctx = do
		set_nonce <- nu_set_nonce
		return $ nu_Ctx $ unsafeDupablePerformIO $
			withSecureMemCopy (nu_ctx ctx) $ \ctxptr ->
			withByteStringPtr nonce $ \noncelen nonceptr ->
				set_nonce ctxptr noncelen nonceptr
nettleUmacUpdate :: NettleUMAC u => u -> B.ByteString -> u
nettleUmacUpdate c msg = untag $ go c where
	go :: NettleUMAC u => u -> Tagged u u
	go ctx = do
		update <- nu_update
		return $ nu_Ctx $ unsafeDupablePerformIO $
			withSecureMemCopy (nu_ctx ctx) $ \ctxptr ->
			withByteStringPtr msg $ \msglen msgptr ->
				update ctxptr msglen msgptr
nettleUmacUpdateLazy :: NettleUMAC u => u -> L.ByteString -> u
nettleUmacUpdateLazy c msg = untag $ go c where
	go :: NettleUMAC u => u -> Tagged u u
	go ctx = do
		update <- nu_update
		return $ nu_Ctx $ unsafeDupablePerformIO $
			withSecureMemCopy (nu_ctx ctx) $ \ctxptr ->
			forM_ (L.toChunks msg) $ \chunk ->
			withByteStringPtr chunk $ \chunklen chunkptr ->
				update ctxptr chunklen chunkptr
nettleUmacFinalize :: NettleUMAC u => u -> (B.ByteString, u)
nettleUmacFinalize c = untag $ go c where
	go :: NettleUMAC u => u -> Tagged u (B.ByteString, u)
	go ctx = do
		digestSize <- nu_digest_size
		digest <- nu_digest
		return $ unsafeDupablePerformIO $ do
			ctx' <- secureMemCopy (nu_ctx ctx)
			dig <- withSecureMemPtr ctx' $ \ctxptr ->
				B.create digestSize $ \digestptr ->
				digest ctxptr (fromIntegral digestSize) digestptr
			return (dig, nu_Ctx ctx')

#define INSTANCE_UMAC(Typ) \
instance UMAC Typ where \
	{ umacDigestSize = nettleUmacDigestSize \
	; umacInit       = nettleUmacInit \
	; umacSetNonce   = nettleUmacSetNonce \
	; umacUpdate     = nettleUmacUpdate \
	; umacUpdateLazy = nettleUmacUpdateLazy \
	; umacFinalize   = nettleUmacFinalize \
	} ; \
instance KeyedHashAlgorithm Typ where \
	{ implKeyedHashDigestSize = umacKHDigestSize \
	; implKeyedHashName       = umacKHName \
	; implKeyedHashInit       = umacKHInit \
	; implKeyedHashUpdate     = umacKHUpdate \
	; implKeyedHashFinalize   = umacKHFinalize \
	}


{-|
'UMAC32' is the 32-bit (4 byte) digest variant. See 'umacInitKeyedHash' for the 'KeyedHashAlgorithm' instance.
-}
newtype UMAC32 = UMAC32 { umac32_ctx :: SecureMem }
instance NettleUMAC UMAC32 where
	nu_ctx_size    = Tagged c_umac32_ctx_size
	nu_digest_size = Tagged c_umac32_digest_size
	nu_set_key     = Tagged c_umac32_set_key
	nu_set_nonce   = Tagged c_umac32_set_nonce
	nu_update      = Tagged c_umac32_update
	nu_digest      = Tagged c_umac32_digest
	nu_ctx         = umac32_ctx
	nu_Ctx         = UMAC32
INSTANCE_UMAC(UMAC32)

{-|
'UMAC64' is the 64-bit (8 byte) digest variant. See 'umacInitKeyedHash' for the 'KeyedHashAlgorithm' instance.
-}
newtype UMAC64 = UMAC64 { umac64_ctx :: SecureMem }
instance NettleUMAC UMAC64 where
	nu_ctx_size    = Tagged c_umac64_ctx_size
	nu_digest_size = Tagged c_umac64_digest_size
	nu_set_key     = Tagged c_umac64_set_key
	nu_set_nonce   = Tagged c_umac64_set_nonce
	nu_update      = Tagged c_umac64_update
	nu_digest      = Tagged c_umac64_digest
	nu_ctx         = umac64_ctx
	nu_Ctx         = UMAC64
INSTANCE_UMAC(UMAC64)

{-|
'UMAC96' is the 96-bit (12 byte) digest variant. See 'umacInitKeyedHash' for the 'KeyedHashAlgorithm' instance.
-}
newtype UMAC96 = UMAC96 { umac96_ctx :: SecureMem }
instance NettleUMAC UMAC96 where
	nu_ctx_size    = Tagged c_umac96_ctx_size
	nu_digest_size = Tagged c_umac96_digest_size
	nu_set_key     = Tagged c_umac96_set_key
	nu_set_nonce   = Tagged c_umac96_set_nonce
	nu_update      = Tagged c_umac96_update
	nu_digest      = Tagged c_umac96_digest
	nu_ctx         = umac96_ctx
	nu_Ctx         = UMAC96
INSTANCE_UMAC(UMAC96)

{-|
'UMAC128' is the 128-bit (16 byte) digest variant. See 'umacInitKeyedHash' for the 'KeyedHashAlgorithm' instance.
-}
newtype UMAC128 = UMAC128 { umac128_ctx :: SecureMem }
instance NettleUMAC UMAC128 where
	nu_ctx_size    = Tagged c_umac128_ctx_size
	nu_digest_size = Tagged c_umac128_digest_size
	nu_set_key     = Tagged c_umac128_set_key
	nu_set_nonce   = Tagged c_umac128_set_nonce
	nu_update      = Tagged c_umac128_update
	nu_digest      = Tagged c_umac128_digest
	nu_ctx         = umac128_ctx
	nu_Ctx         = UMAC128
INSTANCE_UMAC(UMAC128)
