{-# LANGUAGE CPP, MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.Ciphers
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports ciphers supported by nettle:
--   <http://www.lysator.liu.se/~nisse/nettle/>
--
-----------------------------------------------------------------------------

module Crypto.Nettle.Ciphers (
	-- * Block ciphers
	-- | Only block ciphers with a 128-bit 'blockSize' (16 bytes) support the XTS cipher mode.
	--
	--  For 'aeadInit' only 'AEAD_GCM' and 'AEAD_CCM' (with 'ccmInitTLS') is supported, and only if the the 'blockSize' is 16 bytes.
	--  In all other cases 'aeadInit' just returns 'Nothing'.

	-- ** AES
	  AES
	, AES128
	, AES192
	, AES256
	-- ** ARCTWO
	, ARCTWO
	, arctwoInitEKB
	, arctwoInitGutmann
	-- ** BLOWFISH
	, BLOWFISH
	-- ** Camellia
	, Camellia
	, Camellia128
	, Camellia192
	, Camellia256
	-- ** CAST-128
	, CAST128
	-- ** DES
	, DES
	-- ** DES3 (EDE)
	, DES_EDE3
	-- ** TWOFISH
	, TWOFISH
	-- ** SERPENT
	, SERPENT
	-- * Stream ciphers
	-- ** Nonce ciphers
	, StreamNonceCipher(..)
	, streamSetNonceWord64
	-- ** ARCFOUR
	, ARCFOUR
	-- ** ChaCha
	, CHACHA
	-- ** Salsa20
	, SALSA20
	, ESTREAM_SALSA20
	) where

import Crypto.Cipher.Types
import Crypto.Nettle.CCM

import Data.SecureMem
import qualified Data.ByteString as B
import Data.Word (Word64)
import Data.Bits
import Data.Tagged

import Crypto.Nettle.Ciphers.Internal
import Crypto.Nettle.Ciphers.ForeignImports
import Nettle.Utils

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

#define INSTANCE_CIPHER(Typ) \
instance Cipher Typ where \
	{ cipherInit = nettle_cipherInit \
	; cipherName = witness nc_cipherName \
	; cipherKeySize = witness nc_cipherKeySize \
	}
#define INSTANCE_BLOCKCIPHER(Typ) \
INSTANCE_CIPHER(Typ); \
instance BlockCipher Typ where \
	{ blockSize = witness nbc_blockSize \
	; ecbEncrypt = nettle_ecbEncrypt \
	; ecbDecrypt = nettle_ecbDecrypt \
	; cbcEncrypt = nettle_cbcEncrypt \
	; cbcDecrypt = nettle_cbcDecrypt \
	; cfbEncrypt = nettle_cfbEncrypt \
	; cfbDecrypt = nettle_cfbDecrypt \
	; ctrCombine = nettle_ctrCombine \
	; aeadInit AEAD_GCM = nettle_gcm_aeadInit \
	; aeadInit AEAD_CCM = ccmInitTLS \
	; aeadInit _        = \_ _ -> Nothing \
	} ; \
instance AEADModeImpl Typ NettleGCM where \
	{ aeadStateAppendHeader = nettle_gcm_aeadStateAppendHeader \
	; aeadStateEncrypt      = nettle_gcm_aeadStateEncrypt \
	; aeadStateDecrypt      = nettle_gcm_aeadStateDecrypt \
	; aeadStateFinalize     = nettle_gcm_aeadStateFinalize \
	}
#define INSTANCE_STREAMCIPHER(Typ) \
INSTANCE_CIPHER(Typ); \
instance StreamCipher Typ where \
	{ streamCombine = nettle_streamCombine \
	}
#define INSTANCE_STREAMNONCECIPHER(Typ) \
INSTANCE_STREAMCIPHER(Typ); \
instance StreamNonceCipher Typ where \
	{ streamSetNonce = nettle_streamSetNonce \
	; streamNonceSize = witness nsc_nonceSize \
	}
#define INSTANCE_BLOCKEDSTREAMCIPHER(Typ) \
INSTANCE_CIPHER(Typ); \
instance StreamCipher Typ where \
	{ streamCombine = nettle_blockedStreamCombine \
	}
#define INSTANCE_BLOCKEDSTREAMNONCECIPHER(Typ) \
INSTANCE_BLOCKEDSTREAMCIPHER(Typ); \
instance StreamNonceCipher Typ where \
	{ streamSetNonce = nettle_blockedStreamSetNonce \
	; streamNonceSize = witness nbsc_nonceSize \
	}

{-|
'AES' is the generic cipher context for the AES cipher, supporting key sizes
of 128, 196 and 256 bits (16, 24 and 32 bytes). The 'blockSize' is always 128 bits (16 bytes).

'aeadInit' only supports the 'AEAD_GCM' mode for now.
-}
newtype AES = AES SecureMem
instance NettleCipher AES where
	nc_cipherInit    = Tagged c_hs_aes_init
	nc_cipherName    = Tagged "AES"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,24,32]
	nc_ctx_size      = Tagged c_hs_aes_ctx_size
	nc_ctx   (AES c) = c
	nc_Ctx           = AES
instance NettleBlockCipher AES where
	nbc_blockSize          = Tagged 16
	nbc_ecb_encrypt        = Tagged c_hs_aes_encrypt
	nbc_ecb_decrypt        = Tagged c_hs_aes_decrypt
	nbc_fun_encrypt        = Tagged p_hs_aes_encrypt
	nbc_fun_decrypt        = Tagged p_hs_aes_decrypt

INSTANCE_BLOCKCIPHER(AES)

{-|
'AES128' provides the same interface as 'AES', but is restricted to 128-bit keys.
-}
newtype AES128 = AES128 SecureMem
instance NettleCipher AES128 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_aes128_init ctx key)
	nc_cipherName    = Tagged "AES-128"
	nc_cipherKeySize = Tagged $ KeySizeFixed 16
	nc_ctx_size      = Tagged c_hs_aes128_ctx_size
	nc_ctx (AES128 c) = c
	nc_Ctx            = AES128
instance NettleBlockCipher AES128 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_aes128_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_aes128_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_aes128_encrypt
	nbc_ecb_decrypt        = Tagged c_aes128_decrypt
	nbc_fun_encrypt        = Tagged p_aes128_encrypt
	nbc_fun_decrypt        = Tagged p_aes128_decrypt

INSTANCE_BLOCKCIPHER(AES128)


{-|
'AES192' provides the same interface as 'AES', but is restricted to 192-bit keys.
-}
newtype AES192 = AES192 SecureMem
instance NettleCipher AES192 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_aes192_init ctx key)
	nc_cipherName    = Tagged "AES-192"
	nc_cipherKeySize = Tagged $ KeySizeFixed 24
	nc_ctx_size      = Tagged c_hs_aes192_ctx_size
	nc_ctx  (AES192 c) = c
	nc_Ctx             = AES192
instance NettleBlockCipher AES192 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_aes192_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_aes192_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_aes192_encrypt
	nbc_ecb_decrypt        = Tagged c_aes192_decrypt
	nbc_fun_encrypt        = Tagged p_aes192_encrypt
	nbc_fun_decrypt        = Tagged p_aes192_decrypt

INSTANCE_BLOCKCIPHER(AES192)


{-|
'AES256' provides the same interface as 'AES', but is restricted to 256-bit keys.
-}
newtype AES256 = AES256 SecureMem
instance NettleCipher AES256 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_aes256_init ctx key)
	nc_cipherName    = Tagged "AES-256"
	nc_cipherKeySize = Tagged $ KeySizeFixed 32
	nc_ctx_size      = Tagged c_hs_aes256_ctx_size
	nc_ctx  (AES256 c) = c
	nc_Ctx             = AES256
instance NettleBlockCipher AES256 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_aes256_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_aes256_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_aes256_encrypt
	nbc_ecb_decrypt        = Tagged c_aes256_decrypt
	nbc_fun_encrypt        = Tagged p_aes256_encrypt
	nbc_fun_decrypt        = Tagged p_aes256_decrypt

INSTANCE_BLOCKCIPHER(AES256)


{-|
'ARCTWO' (also known as the trade marked name RC2) is a block cipher specified in RFC 2268.

The default 'cipherInit' uses @ekb = bit-length of the key@; 'arctwoInitEKB' allows to specify ekb manually.
'arctwoInitGutmann' uses @ekb = 1024@ (the maximum).

'ARCTWO' uses keysizes from 1 to 128 bytes, and uses a 'blockSize' of 64 bits (8 bytes).
-}
newtype ARCTWO = ARCTWO SecureMem
instance NettleCipher ARCTWO where
	nc_cipherInit    = Tagged c_arctwo_set_key
	nc_cipherName    = Tagged "ARCTWO"
	nc_cipherKeySize = Tagged $ KeySizeRange 1 128
	nc_ctx_size      = Tagged c_arctwo_ctx_size
	nc_ctx  (ARCTWO c) = c
	nc_Ctx             = ARCTWO
instance NettleBlockCipher ARCTWO where
	nbc_blockSize          = Tagged 8
	nbc_ecb_encrypt        = Tagged c_arctwo_encrypt
	nbc_ecb_decrypt        = Tagged c_arctwo_decrypt
	nbc_fun_encrypt        = Tagged p_arctwo_encrypt
	nbc_fun_decrypt        = Tagged p_arctwo_decrypt
INSTANCE_BLOCKCIPHER(ARCTWO)
{-|
Initialize cipher with an explicit @ekb@ value (valid values from 1 to 1024, 0 meaning the same as 1024).
-}
arctwoInitEKB :: Key ARCTWO -> Word -> ARCTWO
arctwoInitEKB k ekb = nettle_cipherInit' initfun k where
	initfun ctxptr ksize ptr = c_arctwo_set_key_ekb ctxptr ksize ptr ekb
{-|
Initialize cipher with @ekb = 1024@.
-}
arctwoInitGutmann :: Key ARCTWO -> ARCTWO
arctwoInitGutmann = nettle_cipherInit' c_arctwo_set_key_gutmann


{-|
'BLOWFISH' is a block cipher designed by Bruce Schneier.
It uses a 'blockSize' of 64 bits (8 bytes), and a variable key size from 64 to 448 bits (8 to 56 bytes).
-}
newtype BLOWFISH = BLOWFISH SecureMem
instance NettleCipher BLOWFISH where
	nc_cipherInit    = Tagged c_blowfish_set_key
	nc_cipherName    = Tagged "BLOWFISH"
	nc_cipherKeySize = Tagged $ KeySizeRange 1 128
	nc_ctx_size      = Tagged c_blowfish_ctx_size
	nc_ctx  (BLOWFISH c) = c
	nc_Ctx             = BLOWFISH
instance NettleBlockCipher BLOWFISH where
	nbc_blockSize          = Tagged 8
	nbc_ecb_encrypt        = Tagged c_blowfish_encrypt
	nbc_ecb_decrypt        = Tagged c_blowfish_decrypt
	nbc_fun_encrypt        = Tagged p_blowfish_encrypt
	nbc_fun_decrypt        = Tagged p_blowfish_decrypt
INSTANCE_BLOCKCIPHER(BLOWFISH)


{-|
Camellia is a block cipher developed by Mitsubishi and Nippon Telegraph and Telephone Corporation,
described in RFC3713, and recommended by some Japanese and European authorities as an alternative to AES.
The algorithm is patented (details see <http://www.lysator.liu.se/~nisse/nettle/nettle.html>).

Camellia uses a the same 'blockSize' and key sizes as 'AES'.

'aeadInit' only supports the 'AEAD_GCM' mode for now.
-}
newtype Camellia = Camellia SecureMem
instance NettleCipher Camellia where
	nc_cipherInit    = Tagged c_hs_camellia_init
	nc_cipherName    = Tagged "Camellia"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,24,32]
	nc_ctx_size      = Tagged c_hs_camellia_ctx_size
	nc_ctx     (Camellia c) = c
	nc_Ctx             = Camellia
instance NettleBlockCipher Camellia where
	nbc_blockSize          = Tagged 16
	nbc_ecb_encrypt        = Tagged c_hs_camellia_encrypt
	nbc_ecb_decrypt        = Tagged c_hs_camellia_decrypt
	nbc_fun_encrypt        = Tagged p_hs_camellia_encrypt
	nbc_fun_decrypt        = Tagged p_hs_camellia_decrypt

INSTANCE_BLOCKCIPHER(Camellia)

{-|
'Camellia128' provides the same interface as 'Camellia', but is restricted to 128-bit keys.
-}
newtype Camellia128 = Camellia128 SecureMem
instance NettleCipher Camellia128 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_camellia128_init ctx key)
	nc_cipherName    = Tagged "Camellia-128"
	nc_cipherKeySize = Tagged $ KeySizeFixed 16
	nc_ctx_size      = Tagged c_hs_camellia128_ctx_size
	nc_ctx  (Camellia128 c) = c
	nc_Ctx             = Camellia128
instance NettleBlockCipher Camellia128 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_camellia128_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_camellia128_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_camellia128_crypt
	nbc_ecb_decrypt        = Tagged c_camellia128_crypt
	nbc_fun_encrypt        = Tagged p_camellia128_crypt
	nbc_fun_decrypt        = Tagged p_camellia128_crypt

INSTANCE_BLOCKCIPHER(Camellia128)

{-|
'Camellia192' provides the same interface as 'Camellia', but is restricted to 192-bit keys.
-}
newtype Camellia192 = Camellia192 SecureMem
instance NettleCipher Camellia192 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_camellia192_init ctx key)
	nc_cipherName    = Tagged "Camellia-192"
	nc_cipherKeySize = Tagged $ KeySizeFixed 24
	nc_ctx_size      = Tagged c_hs_camellia192_ctx_size
	nc_ctx  (Camellia192 c) = c
	nc_Ctx             = Camellia192
instance NettleBlockCipher Camellia192 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_camellia192_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_camellia192_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_camellia192_crypt
	nbc_ecb_decrypt        = Tagged c_camellia192_crypt
	nbc_fun_encrypt        = Tagged p_camellia192_crypt
	nbc_fun_decrypt        = Tagged p_camellia192_crypt

INSTANCE_BLOCKCIPHER(Camellia192)

{-|
'Camellia256' provides the same interface as 'Camellia', but is restricted to 256-bit keys.
-}
newtype Camellia256 = Camellia256 SecureMem
instance NettleCipher Camellia256 where
	nc_cipherInit    = Tagged (\ctx _ key -> c_hs_camellia256_init ctx key)
	nc_cipherName    = Tagged "Camellia-256"
	nc_cipherKeySize = Tagged $ KeySizeFixed 32
	nc_ctx_size      = Tagged c_hs_camellia256_ctx_size
	nc_ctx  (Camellia256 c) = c
	nc_Ctx             = Camellia256
instance NettleBlockCipher Camellia256 where
	nbc_blockSize          = Tagged 16
	nbc_encrypt_ctx_offset = Tagged c_hs_camellia256_ctx_encrypt
	nbc_decrypt_ctx_offset = Tagged c_hs_camellia256_ctx_decrypt
	nbc_ecb_encrypt        = Tagged c_camellia256_crypt
	nbc_ecb_decrypt        = Tagged c_camellia256_crypt
	nbc_fun_encrypt        = Tagged p_camellia256_crypt
	nbc_fun_decrypt        = Tagged p_camellia256_crypt

INSTANCE_BLOCKCIPHER(Camellia256)

{-|
'CAST128' is a block cipher specified in RFC 2144. It uses a 64 bit (8 bytes) 'blockSize',
and a variable key size of 40 up to 128 bits (5 to 16 bytes).
-}
newtype CAST128 = CAST128 SecureMem
instance NettleCipher CAST128 where
	nc_cipherInit    = Tagged c_cast5_set_key
	nc_cipherName    = Tagged "CAST-128"
	nc_cipherKeySize = Tagged $ KeySizeRange 5 16
	nc_ctx_size      = Tagged c_cast128_ctx_size
	nc_ctx  (CAST128 c) = c
	nc_Ctx             = CAST128
instance NettleBlockCipher CAST128 where
	nbc_blockSize          = Tagged 8
	nbc_ecb_encrypt        = Tagged c_cast128_encrypt
	nbc_ecb_decrypt        = Tagged c_cast128_decrypt
	nbc_fun_encrypt        = Tagged p_cast128_encrypt
	nbc_fun_decrypt        = Tagged p_cast128_decrypt

INSTANCE_BLOCKCIPHER(CAST128)

{-|
'DES' is the old Data Encryption Standard, specified by NIST.
It uses a 'blockSize' of 64 bits (8 bytes), and a key size of 56 bits.

The key is given as 8 bytes, as one bit per byte is used as a parity bit.
The parity bit is ignored by this implementation.
-}
newtype DES = DES SecureMem
instance NettleCipher DES where
	nc_cipherInit    = Tagged $ \ctxptr _ -> c_des_set_key ctxptr
	nc_cipherName    = Tagged "DES"
	nc_cipherKeySize = Tagged $ KeySizeFixed 8
	nc_ctx_size      = Tagged c_des_ctx_size
	nc_ctx  (DES c) = c
	nc_Ctx             = DES
instance NettleBlockCipher DES where
	nbc_blockSize          = Tagged 8
	nbc_ecb_encrypt        = Tagged c_des_encrypt
	nbc_ecb_decrypt        = Tagged c_des_decrypt
	nbc_fun_encrypt        = Tagged p_des_encrypt
	nbc_fun_decrypt        = Tagged p_des_decrypt

INSTANCE_BLOCKCIPHER(DES)

{-|
'DES_EDE3' uses 3 'DES' keys @k1 || k2 || k3@.
Encryption first encrypts with k1, then decrypts with k2, then encrypts with k3.

The 'blockSize' is the same as for 'DES': 64 bits (8 bytes),
and the keys are simply concatenated, forming a 24 byte key string (with 168 bits actually getting used).
-}
newtype DES_EDE3 = DES_EDE3 SecureMem
instance NettleCipher DES_EDE3 where
	nc_cipherInit    = Tagged $ \ctxptr _ -> c_des3_set_key ctxptr
	nc_cipherName    = Tagged "DES-EDE3"
	nc_cipherKeySize = Tagged $ KeySizeFixed 24
	nc_ctx_size      = Tagged c_des3_ctx_size
	nc_ctx  (DES_EDE3 c) = c
	nc_Ctx             = DES_EDE3
instance NettleBlockCipher DES_EDE3 where
	nbc_blockSize          = Tagged 8
	nbc_ecb_encrypt        = Tagged c_des3_encrypt
	nbc_ecb_decrypt        = Tagged c_des3_decrypt
	nbc_fun_encrypt        = Tagged p_des3_encrypt
	nbc_fun_decrypt        = Tagged p_des3_decrypt

INSTANCE_BLOCKCIPHER(DES_EDE3)

{-|
'SERPENT' is one of the AES finalists, designed by Ross Anderson, Eli Biham and Lars Knudsen.

The 'blockSize' is 128 bits (16 bytes), and the valid key sizes are from 128 bits to 256 bits (16 to 32 bytes),
although smaller bits are just padded with zeroes.

'aeadInit' only supports the 'AEAD_GCM' mode for now.
-}
newtype SERPENT = SERPENT SecureMem
instance NettleCipher SERPENT where
	nc_cipherInit    = Tagged c_serpent_set_key
	nc_cipherName    = Tagged "SERPENT"
	nc_cipherKeySize = Tagged $ KeySizeRange 16 32
	nc_ctx_size      = Tagged c_serpent_ctx_size
	nc_ctx  (SERPENT c) = c
	nc_Ctx             = SERPENT
instance NettleBlockCipher SERPENT where
	nbc_blockSize          = Tagged 16
	nbc_ecb_encrypt        = Tagged c_serpent_encrypt
	nbc_ecb_decrypt        = Tagged c_serpent_decrypt
	nbc_fun_encrypt        = Tagged p_serpent_encrypt
	nbc_fun_decrypt        = Tagged p_serpent_decrypt
INSTANCE_BLOCKCIPHER(SERPENT)

{-|
'TWOFISH' is another AES finalist, designed by Bruce Schneier and others.

'TWOFISH' uses a the same 'blockSize' and key sizes as 'AES'.

'aeadInit' only supports the 'AEAD_GCM' mode for now.
-}
newtype TWOFISH = TWOFISH SecureMem
instance NettleCipher TWOFISH where
	nc_cipherInit    = Tagged c_twofish_set_key
	nc_cipherName    = Tagged "TWOFISH"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,24,32]
	nc_ctx_size      = Tagged c_twofish_ctx_size
	nc_ctx  (TWOFISH c) = c
	nc_Ctx             = TWOFISH
instance NettleBlockCipher TWOFISH where
	nbc_blockSize          = Tagged 16
	nbc_ecb_encrypt        = Tagged c_twofish_encrypt
	nbc_ecb_decrypt        = Tagged c_twofish_decrypt
	nbc_fun_encrypt        = Tagged p_twofish_encrypt
	nbc_fun_decrypt        = Tagged p_twofish_decrypt
INSTANCE_BLOCKCIPHER(TWOFISH)


{-|
'ARCFOUR' is a stream cipher, also known under the trade marked name RC4.

Valid key sizes are from 1 to 256 bytes.
-}
newtype ARCFOUR = ARCFOUR SecureMem
instance NettleCipher ARCFOUR where
	nc_cipherInit    = Tagged c_arcfour_set_key
	nc_cipherName    = Tagged "ARCFOUR"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,24,32]
	nc_ctx_size      = Tagged c_arcfour_ctx_size
	nc_ctx  (ARCFOUR c) = c
	nc_Ctx             = ARCFOUR
instance NettleStreamCipher ARCFOUR where
	nsc_streamCombine = Tagged c_arcfour_crypt
INSTANCE_STREAMCIPHER(ARCFOUR)


{-|
'StreamNonceCipher' are special stream ciphers that can encrypt many messages with the same key;
setting a nonce restarts the cipher.

A good value for the nonce is a message/packet counter. Usually a nonce should not be reused with the same key.
-}
class StreamCipher cipher => StreamNonceCipher cipher where
	streamNonceSize :: cipher -> KeySizeSpecifier
	streamSetNonce  :: cipher -> B.ByteString -> Maybe cipher

word64BE :: Word64 -> B.ByteString
word64BE value = B.pack $ _work (8::Int) [] value where
	_work 0 r _ = r
	_work n r v = let d = v `shiftR` 8; m = fromIntegral v :: Word8 in _work (n-1) (m:r) d

{-|
Sets a 'Word64' as 8-byte nonce (bigendian encoded)
-}
streamSetNonceWord64 :: StreamNonceCipher cipher => cipher -> Word64 -> Maybe cipher
streamSetNonceWord64 c nonce = streamSetNonce c $ word64BE nonce

-- set nonce to 0 on init
wrap_chacha_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
wrap_chacha_set_key ctxptr _ keyptr = do
	c_chacha_set_key ctxptr keyptr
	withByteStringPtr (B.replicate 8 0) $ \_ nonceptr ->
		c_chacha_set_nonce ctxptr nonceptr

-- check nonce length
wrap_chacha_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
wrap_chacha_set_nonce ctxptr ivlen ivptr = if ivlen == 8 then c_chacha_set_nonce ctxptr ivptr else error "Invalid nonce length"

{-|
'CHACHA' is a variant of the 'SALSA20' stream cipher, both designed by D. J. Bernstein.

Key size is 256 bits (32 bytes).

'CHACHA' works similar to 'SALSA20'; it could theoretically also support 128-bit keys, but there is no need for it as they share the same performance.

ChaCha uses a blocksize of 64 bytes internally; if crpyted input isn't aligned to 64 bytes it will
pad it with 0 and store the encrypted padding to xor with future input data.

Each message also requires a 8-byte ('Word64') nonce (which is initialized to 0; you can use a message sequence number).
Don't reuse a nonce with the same key.

Setting a nonce also resets the remaining padding data.
-}
newtype CHACHA = CHACHA (SecureMem, B.ByteString)
instance NettleCipher CHACHA where
	nc_cipherInit    = Tagged wrap_chacha_set_key
	nc_cipherName    = Tagged "ChaCha"
	nc_cipherKeySize = Tagged $ KeySizeFixed 32
	nc_ctx_size      = Tagged c_chacha_ctx_size
	nc_ctx (CHACHA (c, _)) = c
	nc_Ctx c           = CHACHA (c, B.empty)
instance NettleBlockedStreamCipher CHACHA where
	nbsc_blockSize     = Tagged 64
	nbsc_IncompleteState (CHACHA (c, _)) inc = CHACHA (c, inc)
	nbsc_incompleteState (CHACHA (_, inc)) = inc
	nbsc_streamCombine = Tagged c_chacha_crypt
	nbsc_nonceSize     = Tagged $ KeySizeFixed 8
	nbsc_setNonce      = Tagged $ Just wrap_chacha_set_nonce
INSTANCE_BLOCKEDSTREAMNONCECIPHER(CHACHA)

-- set nonce to 0 on init
wrap_salsa20_set_key :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
wrap_salsa20_set_key ctxptr keylen keyptr = do
	c_salsa20_set_key ctxptr keylen keyptr
	withByteStringPtr (B.replicate 8 0) $ \_ nonceptr ->
		c_salsa20_set_nonce ctxptr nonceptr

-- check nonce length
wrap_salsa20_set_nonce :: Ptr Word8 -> Word -> Ptr Word8 -> IO ()
wrap_salsa20_set_nonce ctxptr ivlen ivptr = if ivlen == 8 then c_salsa20_set_nonce ctxptr ivptr else error "Invalid nonce length"

{-|
'SALSA20' is a fairly recent stream cipher designed by D. J. Bernstein.

Valid key sizes are 128 and 256 bits (16 and 32 bytes).

Salsa20 uses a blocksize of 64 bytes internally; if crpyted input isn't aligned to 64 bytes it will
pad it with 0 and store the encrypted padding to xor with future input data.

Each message also requires a 8-byte ('Word64') nonce (which is initialized to 0; you can use a message sequence number).
Don't reuse a nonce with the same key.

Setting a nonce also resets the remaining padding data.
-}
newtype SALSA20 = SALSA20 (SecureMem, B.ByteString)
instance NettleCipher SALSA20 where
	nc_cipherInit    = Tagged wrap_salsa20_set_key
	nc_cipherName    = Tagged "Salsa20"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,32]
	nc_ctx_size      = Tagged c_salsa20_ctx_size
	nc_ctx (SALSA20 (c, _)) = c
	nc_Ctx c           = SALSA20 (c, B.empty)
instance NettleBlockedStreamCipher SALSA20 where
	nbsc_blockSize     = Tagged 64
	nbsc_IncompleteState (SALSA20 (c, _)) inc = SALSA20 (c, inc)
	nbsc_incompleteState (SALSA20 (_, inc)) = inc
	nbsc_streamCombine = Tagged c_salsa20_crypt
	nbsc_nonceSize     = Tagged $ KeySizeFixed 8
	nbsc_setNonce      = Tagged $ Just wrap_salsa20_set_nonce
INSTANCE_BLOCKEDSTREAMNONCECIPHER(SALSA20)


{-|
'ESTREAM_SALSA20' is the same as 'SALSA20', but uses only 12 instead of 20 rounds in mixing.
-}
newtype ESTREAM_SALSA20 = ESTREAM_SALSA20 (SecureMem, B.ByteString)
instance NettleCipher ESTREAM_SALSA20 where
	nc_cipherInit    = Tagged wrap_salsa20_set_key
	nc_cipherName    = Tagged "eSTREAM-Salsa20"
	nc_cipherKeySize = Tagged $ KeySizeEnum [16,32]
	nc_ctx_size      = Tagged c_salsa20_ctx_size
	nc_ctx (ESTREAM_SALSA20 (c, _)) = c
	nc_Ctx c           = ESTREAM_SALSA20 (c, B.empty)
instance NettleBlockedStreamCipher ESTREAM_SALSA20 where
	nbsc_blockSize     = Tagged 64
	nbsc_IncompleteState (ESTREAM_SALSA20 (c, _)) inc = ESTREAM_SALSA20 (c, inc)
	nbsc_incompleteState (ESTREAM_SALSA20 (_, inc)) = inc
	nbsc_streamCombine = Tagged c_salsa20r12_crypt
	nbsc_nonceSize     = Tagged $ KeySizeFixed 8
	nbsc_setNonce      = Tagged $ Just wrap_salsa20_set_nonce
INSTANCE_BLOCKEDSTREAMNONCECIPHER(ESTREAM_SALSA20)
