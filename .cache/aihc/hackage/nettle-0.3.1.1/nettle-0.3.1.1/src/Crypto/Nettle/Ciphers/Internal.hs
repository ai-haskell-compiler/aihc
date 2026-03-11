{-# OPTIONS_HADDOCK hide #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Crypto.Nettle.Ciphers.Internal
	( NettleCipher(..)
	, NettleBlockCipher(..)
	, NettleStreamCipher(..)
	, NettleBlockedStreamCipher(..)
	, NettleGCM
	, nettle_cipherInit
	, nettle_cipherInit'
	, nettle_ecbEncrypt
	, nettle_ecbDecrypt
	, nettle_cbcEncrypt
	, nettle_cbcDecrypt
	, nettle_cfbEncrypt
	, nettle_cfbDecrypt
	, nettle_ctrCombine
	, nettle_streamCombine
	, nettle_streamSetNonce
	, nettle_blockedStreamCombine
	, nettle_blockedStreamSetNonce
	, nettle_gcm_aeadInit
	, nettle_gcm_aeadStateAppendHeader
	, nettle_gcm_aeadStateEncrypt
	, nettle_gcm_aeadStateDecrypt
	, nettle_gcm_aeadStateFinalize
	) where

import Crypto.Cipher.Types as T

import Data.Tagged
import Data.Byteable (Byteable(..))
import Data.SecureMem
import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.Bits (xor)

import Nettle.Utils
import Crypto.Nettle.Ciphers.ForeignImports

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

class NettleCipher c where
	-- | pointer to new context, key length, (const) key pointer
	nc_cipherInit    :: Tagged c (Ptr Word8 -> Word -> Ptr Word8 -> IO())
	nc_cipherName    :: Tagged c String
	nc_cipherKeySize :: Tagged c T.KeySizeSpecifier
	nc_ctx_size      :: Tagged c Int
	nc_ctx           :: c -> SecureMem
	nc_Ctx           :: SecureMem -> c
class NettleCipher c => NettleBlockCipher c where
	nbc_blockSize          :: Tagged c Int
	nbc_encrypt_ctx_offset :: Tagged c (Ptr Word8 -> Ptr Word8)
	nbc_encrypt_ctx_offset = Tagged id
	nbc_decrypt_ctx_offset :: Tagged c (Ptr Word8 -> Ptr Word8)
	nbc_decrypt_ctx_offset = Tagged id
	nbc_ecb_encrypt        :: Tagged c NettleCryptFunc
	nbc_ecb_decrypt        :: Tagged c NettleCryptFunc
	nbc_fun_encrypt        :: Tagged c (FunPtr NettleCryptFunc)
	nbc_fun_decrypt        :: Tagged c (FunPtr NettleCryptFunc)
class NettleCipher c => NettleStreamCipher c where
	nsc_streamCombine      :: Tagged c NettleCryptFunc
	nsc_nonceSize          :: Tagged c T.KeySizeSpecifier
	nsc_nonceSize          = Tagged $ T.KeySizeEnum []
	nsc_setNonce           :: Tagged c (Maybe (Ptr Word8 -> Word -> Ptr Word8 -> IO ()))
	nsc_setNonce           = Tagged Nothing

-- stream cipher based on generating (large) blocks to XOR with input,
-- but don't keep incomplete blocks in the state, so we have to do that here
class NettleCipher c => NettleBlockedStreamCipher c where
	nbsc_blockSize          :: Tagged c Int
	-- set new incomplete state
	nbsc_IncompleteState    :: c -> B.ByteString -> c
	nbsc_incompleteState    :: c -> B.ByteString
	nbsc_streamCombine      :: Tagged c NettleCryptFunc
	nbsc_nonceSize          :: Tagged c T.KeySizeSpecifier
	nbsc_nonceSize          = Tagged $ T.KeySizeEnum []
	nbsc_setNonce           :: Tagged c (Maybe (Ptr Word8 -> Word -> Ptr Word8 -> IO ()))
	nbsc_setNonce           = Tagged Nothing

nettle_cipherInit :: NettleCipher c => Key c -> c
nettle_cipherInit k = let ctx = nettle_cipherInit' (nc_cipherInit `witness` ctx) k in ctx

nettle_cipherInit' :: NettleCipher c => (Ptr Word8 -> Word -> Ptr Word8 -> IO()) -> Key c -> c
nettle_cipherInit' f k = let ctx = nc_Ctx $ key_init f (nc_ctx_size `witness` ctx) k in ctx

assert_blockSize :: NettleBlockCipher c => c -> B.ByteString -> a -> a
assert_blockSize c src result = if 0 /= B.length src `mod` (nbc_blockSize `witness` c) then error "input not a multiple of blockSize" else result

nettle_ecbEncrypt :: NettleBlockCipher c => c -> B.ByteString -> B.ByteString
nettle_ecbEncrypt c    src = assert_blockSize c src $ c_run_crypt   (nbc_encrypt_ctx_offset `witness` c)               (nbc_ecb_encrypt `witness` c) (nc_ctx c) src
nettle_ecbDecrypt :: NettleBlockCipher c => c -> B.ByteString -> B.ByteString
nettle_ecbDecrypt c    src = assert_blockSize c src $ c_run_crypt   (nbc_decrypt_ctx_offset `witness` c)               (nbc_ecb_decrypt `witness` c) (nc_ctx c) src
nettle_cbcEncrypt :: NettleBlockCipher c => c -> IV c -> B.ByteString -> B.ByteString
nettle_cbcEncrypt c iv src = assert_blockSize c src $ blockmode_run (nbc_encrypt_ctx_offset `witness` c) c_cbc_encrypt (nbc_fun_encrypt `witness` c) (nc_ctx c) iv src
nettle_cbcDecrypt :: NettleBlockCipher c => c -> IV c -> B.ByteString -> B.ByteString
nettle_cbcDecrypt c iv src = assert_blockSize c src $ blockmode_run (nbc_decrypt_ctx_offset `witness` c) c_cbc_decrypt (nbc_fun_decrypt `witness` c) (nc_ctx c) iv src
nettle_cfbEncrypt :: NettleBlockCipher c => c -> IV c -> B.ByteString -> B.ByteString
nettle_cfbEncrypt c iv src = assert_blockSize c src $ blockmode_run (nbc_encrypt_ctx_offset `witness` c) c_cfb_encrypt (nbc_fun_encrypt `witness` c) (nc_ctx c) iv src
nettle_cfbDecrypt :: NettleBlockCipher c => c -> IV c -> B.ByteString -> B.ByteString
nettle_cfbDecrypt c iv src = assert_blockSize c src $ blockmode_run (nbc_encrypt_ctx_offset `witness` c) c_cfb_decrypt (nbc_fun_encrypt `witness` c) (nc_ctx c) iv src
nettle_ctrCombine :: NettleBlockCipher c => c -> IV c -> B.ByteString -> B.ByteString
nettle_ctrCombine c        =                          blockmode_run (nbc_encrypt_ctx_offset `witness` c) c_ctr_crypt   (nbc_fun_encrypt `witness` c) (nc_ctx c)

nettle_streamCombine :: NettleStreamCipher c => c -> B.ByteString -> (B.ByteString, c)
nettle_streamCombine c indata = let (r, c') = stream_crypt (nsc_streamCombine `witness` c) (nc_ctx c) indata in (r, nc_Ctx c')
nettle_streamSetNonce :: NettleStreamCipher c => c -> B.ByteString -> Maybe c
nettle_streamSetNonce c nonce = case nsc_setNonce `witness` c of
	Nothing -> Nothing
	Just setnonce -> unsafeDupablePerformIO $
		secureMemCopy (nc_ctx c) >>= \ctx' ->
		withSecureMemPtr ctx' $ \ctxptr ->
		withByteStringPtr nonce $ \noncelen nonceptr ->
		setnonce ctxptr noncelen nonceptr >>
		return (Just $ nc_Ctx ctx')

nettle_blockedStreamCombine :: NettleBlockedStreamCipher c => c -> B.ByteString -> (B.ByteString, c)
nettle_blockedStreamCombine c indata = if B.length indata == 0 then (indata, c) else
	let inc = nbsc_incompleteState c; blocksiz = nbsc_blockSize `witness` c in
	if B.length inc /= 0
		then let
			-- first xor remaining block, then combine the rest
			(i1, i2) = B.splitAt (B.length inc) indata
			(inc1, inc2) = B.splitAt (B.length indata) inc
			r1 = B.pack $ B.zipWith xor i1 inc1
			c' = if B.length inc2 == 0 then nc_Ctx $ nc_ctx c else nbsc_IncompleteState c inc2
			(r, c'') = nettle_blockedStreamCombine c' i2
			in (B.append r1 r, c'')
		else if B.length indata `mod` blocksiz /= 0
			then let
				padding = B.replicate (blocksiz - (B.length indata `mod` blocksiz)) 0
				(r', c') = stream_crypt (nbsc_streamCombine `witness` c) (nc_ctx c) (B.append indata padding)
				(r, inc') = B.splitAt (B.length indata) r'
				in (r, nbsc_IncompleteState (nc_Ctx c') inc')
			else
				let (r, c') = stream_crypt (nbsc_streamCombine `witness` c) (nc_ctx c) indata in (r, nc_Ctx c')
nettle_blockedStreamSetNonce :: NettleBlockedStreamCipher c => c -> B.ByteString -> Maybe c
nettle_blockedStreamSetNonce c nonce = case nbsc_setNonce `witness` c of
	Nothing -> Nothing
	Just setnonce -> unsafeDupablePerformIO $
		secureMemCopy (nc_ctx c) >>= \ctx' ->
		withSecureMemPtr ctx' $ \ctxptr ->
		withByteStringPtr nonce $ \noncelen nonceptr ->
		setnonce ctxptr noncelen nonceptr >>
		return (Just $ nc_Ctx ctx')


nettle_gcm_aeadInit              :: (NettleBlockCipher c, AEADModeImpl c NettleGCM, Byteable iv) => c -> iv -> Maybe (AEAD c)
nettle_gcm_aeadInit          c  iv = if nbc_blockSize `witness` c == 16 then Just $ AEAD c $ AEADState $ gcm_init (nbc_encrypt_ctx_offset `witness` c) (nbc_fun_encrypt `witness` c) (nc_ctx c) iv else Nothing
nettle_gcm_aeadStateAppendHeader :: t -> NettleGCM -> B.ByteString -> NettleGCM
nettle_gcm_aeadStateAppendHeader _ = gcm_update
nettle_gcm_aeadStateEncrypt      :: NettleBlockCipher c => c -> NettleGCM -> B.ByteString -> (B.ByteString, NettleGCM)
nettle_gcm_aeadStateEncrypt      c = gcm_crypt c_gcm_encrypt (nbc_encrypt_ctx_offset `witness` c) (nbc_fun_encrypt `witness` c) (nc_ctx c)
nettle_gcm_aeadStateDecrypt      :: NettleBlockCipher c => c -> NettleGCM -> B.ByteString -> (B.ByteString, NettleGCM)
nettle_gcm_aeadStateDecrypt      c = gcm_crypt c_gcm_decrypt (nbc_encrypt_ctx_offset `witness` c) (nbc_fun_encrypt `witness` c) (nc_ctx c)
nettle_gcm_aeadStateFinalize     :: NettleBlockCipher c => c -> NettleGCM -> Int -> AuthTag
nettle_gcm_aeadStateFinalize     c = gcm_digest              (nbc_encrypt_ctx_offset `witness` c) (nbc_fun_encrypt `witness` c) (nc_ctx c)



key_init
	:: ToSecureMem k
	=> (Ptr Word8 -> Word -> Ptr Word8 -> IO ())
	-> Int -> k -> SecureMem
key_init initfun size k = unsafeCreateSecureMem size $ \ctxptr ->
	withSecureMemPtrSz (toSecureMem k) $ \ksize kptr -> initfun ctxptr (fromIntegral ksize) kptr

-- run encryption/decryption with same length for in and output
c_run_crypt
	:: (Ptr Word8 -> Ptr Word8)
	-> NettleCryptFunc
	-> SecureMem -> B.ByteString -> B.ByteString
c_run_crypt ctxoffset cfun ctx indata = unsafeDupablePerformIO $ withSecureMemPtr ctx $ \ctxptr ->
	withByteStringPtr indata $ \indatalen indataptr ->
	B.create (B.length indata) $ \outptr ->
	cfun (ctxoffset ctxptr) indatalen outptr indataptr

blockmode_run
	:: (Byteable iv)
	=> (Ptr Word8 -> Ptr Word8)
	-> NettleBlockMode
	-> FunPtr NettleCryptFunc
	-> SecureMem -> iv -> B.ByteString -> B.ByteString
blockmode_run ctxoffset mode crypt ctx iv indata = unsafeDupablePerformIO $ withSecureMemPtr ctx $ \ctxptr ->
	withByteStringPtr indata $ \indatalen indataptr ->
	withSecureMemPtrSz (toSecureMem $ toBytes iv) $ \ivlen ivptr -> -- copy IV, may get modified
	B.create (B.length indata) $ \outptr ->
	mode (ctxoffset ctxptr) crypt (fromIntegral ivlen) ivptr indatalen outptr indataptr

data NettleGCM = NettleGCM !SecureMem !SecureMem

gcm_init
	:: (Byteable iv)
	=> (Ptr Word8 -> Ptr Word8)
	-> FunPtr NettleCryptFunc
	-> SecureMem -> iv -> NettleGCM
gcm_init encctxoffset encrypt encctx iv = unsafeDupablePerformIO $
	withBytePtr iv $ \ivptr ->
	withSecureMemPtr encctx $ \encctxptr -> do
	h <- createSecureMem c_gcm_key_size $ \hptr ->
		c_gcm_set_key hptr (encctxoffset encctxptr) encrypt
	withSecureMemPtr h $ \hptr -> do
	    ctx <- createSecureMem c_gcm_ctx_size $ \ctxptr ->
		c_gcm_set_iv ctxptr hptr (fromIntegral $ byteableLength iv) ivptr
	    return (NettleGCM ctx h)

-- independent of cipher
gcm_update
	:: NettleGCM -> B.ByteString -> NettleGCM
gcm_update (NettleGCM ctx h) indata = unsafeDupablePerformIO $
	secureMemCopy ctx >>= \ctx' ->
	withSecureMemPtr ctx' $ \ctxptr ->
	withSecureMemPtr h $ \hptr ->
	withByteStringPtr indata $ \indatalen indataptr ->
	c_gcm_update ctxptr hptr indatalen indataptr >>
	return (NettleGCM ctx' h)

gcm_crypt
	:: NettleGCMMode
	-> (Ptr Word8 -> Ptr Word8)
	-> FunPtr NettleCryptFunc
	-> SecureMem -> NettleGCM -> B.ByteString -> (B.ByteString, NettleGCM)
gcm_crypt mode encctxoffset encrypt encctx (NettleGCM ctx h) indata = unsafeDupablePerformIO $
	secureMemCopy ctx >>= \ctx' ->
	withSecureMemPtr ctx' $ \ctxptr ->
	withSecureMemPtr h $ \hptr ->
	withSecureMemPtr encctx $ \encctxptr ->
	withByteStringPtr indata $ \indatalen indataptr -> do
	outdata <- B.create (B.length indata) $ \outptr ->
		mode ctxptr hptr (encctxoffset encctxptr) encrypt indatalen outptr indataptr
	return (outdata, NettleGCM ctx' h)

gcm_digest
	:: (Ptr Word8 -> Ptr Word8)
	-> FunPtr NettleCryptFunc
	-> SecureMem -> NettleGCM -> Int -> AuthTag
gcm_digest encctxoffset encrypt encctx (NettleGCM ctx h) taglen = unsafeDupablePerformIO $
	secureMemCopy ctx >>= \ctx' ->
	withSecureMemPtr ctx' $ \ctxptr ->
	withSecureMemPtr h $ \hptr ->
	withSecureMemPtr encctx $ \encctxptr -> do
	tag <- B.create (fromIntegral taglen) $ \tagptr ->
		c_gcm_digest ctxptr hptr (encctxoffset encctxptr) encrypt (fromIntegral taglen) tagptr
	return $ AuthTag tag

stream_crypt
	:: NettleCryptFunc
	-> SecureMem -> B.ByteString -> (B.ByteString, SecureMem)
stream_crypt crypt ctx indata = unsafeDupablePerformIO $
	secureMemCopy ctx >>= \ctx' ->
	withSecureMemPtr ctx' $ \ctxptr ->
	withByteStringPtr indata $ \indatalen indataptr -> do
	outdata <- B.create (B.length indata) $ \outptr ->
		crypt ctxptr indatalen outptr indataptr
	return (outdata, ctx')
