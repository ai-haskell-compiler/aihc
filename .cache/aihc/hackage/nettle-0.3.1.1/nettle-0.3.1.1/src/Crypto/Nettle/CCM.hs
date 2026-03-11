{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.CCM
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- (This is not a binding to nettle; it is implemented in pure haskell)
--
-- This module adds CCM support to all 128-bit block ciphers:
--
-- @aeadInit AEAD_CCM = ccmInitTLS@
--
-- CCM uses 2 parameters t and q: t is the tag length (2,4,6,8,10,12,14,16) and q (2..8) is the
-- length in bytes that the length of the message is stored in (and the length of the
-- counter variable).
-- Maximum message length is @2^(8*q) - 1@.
--
-- CCM requires a nonce of length (15 - q). TLS uses CCM with @t = 16@ and @q = 3@,
-- and a nonce length of 12 (the first 4 bytes are fixed from the handshake, the other 8
-- usually represent the sequence counter).
--
-- CCM encrypts with a CTR mode, the start IV is based on the (t,q,nonce) parameters;
-- the tag is encrypted with counter value = 0, then the message follows.
--
-- Calculating the tag needs the message length first - so this implementation needs
-- to gather all data before calculating it.
--
-- In RFC 3610 @t@ is called @M@, and @q@ is called @L@.
-----------------------------------------------------------------------------

module Crypto.Nettle.CCM
	( ccmInit
	, ccmInitTLS
	) where


import Crypto.Cipher.Types
import qualified Data.ByteString as B
import Data.Byteable

import Nettle.Utils

-- internal functions are not camelCase on purpose
{-# ANN module "HLint: ignore Use camelCase" #-}

-- ccm needs a 128-bit block cipher

data CCM cipher
	= CCM_Header (Int, Int, B.ByteString) B.ByteString
	| CCM_Enc (Int, Int, B.ByteString) B.ByteString (IV cipher) B.ByteString
	| CCM_Dec (Int, Int, B.ByteString) B.ByteString (IV cipher) B.ByteString

{-|
Start a CCM encryption with specified tag length @t@, length @q@ of the message length field and a @15-q@ bytes long @nonce@.
Fails if any parameter is invalid or the block cipher doesn't use a 16-byte 'blockSize'.
-}
ccmInit
	:: (BlockCipher cipher, Byteable iv)
	=> Int    -- ^ tag length @t@
	-> Int    -- ^ length @q@ of the message length field
	-> cipher -- ^ cipher initialized with key
	-> iv     -- ^ @nonce@ with length @15-q@
	-> Maybe (AEAD cipher)
ccmInit t q cipher nonce = ccm_init t q cipher nonce >>= Just . AEAD cipher . AEADState

ccm_init :: (BlockCipher cipher, Byteable iv) => Int -> Int -> cipher -> iv -> Maybe (CCM cipher)
ccm_init t q cipher nonce = if valid then Just $ CCM_Header (t, q, toBytes nonce) B.empty else Nothing
	where
	valid = valid_cipher && valid_t && valid_q && valid_nonce
	valid_cipher = blockSize cipher == 16
	valid_t = t >= 4 && t <= 16 && even t
	valid_q = q >= 2 && q <= 8
	nonce_len = 15 - q
	valid_nonce = byteableLength nonce == fromIntegral nonce_len

{-|
Start a CCM encryption with specified tag length @t = 16@, length @q = 3@ for the message length field and a @8@ bytes long @nonce@.
Fails if any parameter is invalid or the block cipher doesn't use a 16-byte 'blockSize'.
This are the parameters used for TLS.
-}
ccmInitTLS
	:: (BlockCipher cipher, Byteable iv)
	=> cipher -- ^ cipher initialized with key
	-> iv     -- ^ 8 byte @nonce@
	-> Maybe (AEAD cipher)
ccmInitTLS = ccmInit 16 3


ccm_encodeAdditionalLength :: B.ByteString -> B.ByteString
ccm_encodeAdditionalLength s = B.append (encLen $ B.length s) s where
	encLen n
		| n == 0                       = B.empty
		| n < (2^(16::Int)-2^(8::Int)) = B.pack $ netEncode 2 n
		| n < (2^(32::Int))            = B.pack (0xff:0xfe:netEncode 4 n)
		| otherwise                    = B.pack (0xff:0xff:netEncode 8 n)

pad_zero :: Int -> B.ByteString -> B.ByteString
pad_zero l s = B.append s $ B.replicate (l - 1 - (B.length s - 1) `mod` l) 0

_makeIV :: BlockCipher cipher => B.ByteString -> IV cipher
_makeIV iv = let Just iv' = makeIV iv in iv'

ccm_start_iv :: BlockCipher cipher => (Int, Int, B.ByteString) -> IV cipher
ccm_start_iv (_, q, nonce) = _makeIV $ B.concat [B.singleton $ fromIntegral $ q - 1, nonce, B.replicate (q - 1) 0, B.singleton 1]

ccm_tag_iv :: BlockCipher cipher => (Int, Int, B.ByteString) -> IV cipher
ccm_tag_iv (_, q, nonce) = _makeIV $ B.concat [B.singleton $ fromIntegral $ q - 1, nonce, B.replicate q 0]

ccm_crypt :: BlockCipher cipher => cipher -> IV cipher -> B.ByteString -> (B.ByteString, IV cipher)
ccm_crypt key iv src = let
	blocks = (B.length src + 15) `div` 16
	dst = ctrCombine key iv src
	iv' = ivAdd iv blocks
	in (dst, iv')

ccm_tag :: BlockCipher cipher => cipher -> (Int, Int, B.ByteString) -> B.ByteString -> B.ByteString -> Int -> AuthTag
ccm_tag key (t, q, nonce) header msg taglen = let
	-- 64*(header != "") + 8*M' + L'
	auth_flags = (if B.length header > 0 then 64 else 0) + 4*(fromIntegral t - 2) + (fromIntegral q - 1)
	b0 = B.concat [B.singleton auth_flags, nonce, B.pack $ netEncode q $ B.length msg]
	blocks = B.concat [b0, pad_zero 16 $ ccm_encodeAdditionalLength header, pad_zero 16 msg]
	tag = fst $ ccm_crypt key (ccm_tag_iv (t, q, nonce)) $ B.drop (B.length blocks - 16) $ cbcEncrypt key nullIV blocks
	in AuthTag $ B.take taglen tag

instance BlockCipher cipher => AEADModeImpl cipher (CCM cipher) where
	aeadStateAppendHeader _ (CCM_Header (t, q, nonce) header) src = CCM_Header (t, q, nonce) $ B.append header src
	aeadStateAppendHeader _ _ _ = error "can't aeadStateAppendHeader anymore, already have real data"
	aeadStateEncrypt key (CCM_Header (t, q, nonce) header) src = aeadStateEncrypt key (CCM_Enc (t, q, nonce) header iv B.empty) src
		where iv = ccm_start_iv (t, q, nonce)
	aeadStateEncrypt key (CCM_Enc (t, q, nonce) header iv msg) src = let
		(dst, iv') = ccm_crypt key iv src
		in (dst, CCM_Enc (t, q, nonce) header iv' $ B.append msg src)
	aeadStateEncrypt _ _ _ = error "can't aeadStateEncrypt anymore, already is in decrypt mode"
	aeadStateDecrypt key (CCM_Header (t, q, nonce) header) src = aeadStateDecrypt key (CCM_Dec (t, q, nonce) header iv B.empty) src
		where iv = ccm_start_iv (t, q, nonce)
	aeadStateDecrypt key (CCM_Dec (t, q, nonce) header iv msg) src = let
		(dst, iv') = ccm_crypt key iv src
		in (dst, CCM_Enc (t, q, nonce) header iv' $ B.append msg dst)
	aeadStateDecrypt _ _ _ = error "can't aeadStateDecrypt anymore, already is in encrypt mode"
	aeadStateFinalize key (CCM_Header (t, q, nonce) header      ) taglen = ccm_tag key (t, q, nonce) header B.empty taglen
	aeadStateFinalize key (CCM_Enc    (t, q, nonce) header _ msg) taglen = ccm_tag key (t, q, nonce) header msg     taglen
	aeadStateFinalize key (CCM_Dec    (t, q, nonce) header _ msg) taglen = ccm_tag key (t, q, nonce) header msg     taglen
