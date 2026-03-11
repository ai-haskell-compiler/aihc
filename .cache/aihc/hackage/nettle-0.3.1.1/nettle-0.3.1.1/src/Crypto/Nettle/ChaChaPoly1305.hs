{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Crypto.Nettle.ChaChaPoly1305
-- Copyright   :  (c) 2013 Stefan Bühler
-- License     :  MIT-style (see the file COPYING)
-- 
-- Maintainer  :  stbuehler@web.de
-- Stability   :  experimental
-- Portability :  portable
--
-- This module exports the ChaCha-Poly1305 AEAD cipher supported by nettle:
--   <http://www.lysator.liu.se/~nisse/nettle/>
--
-- Both ChaCha (the underlying cipher) and Poly1305 (the keyed hash) were
-- designed by D. J. Bernstein.
--
-----------------------------------------------------------------------------

module Crypto.Nettle.ChaChaPoly1305 (
	-- * ChaCha-Poly1305
	--
	-- No streaming interface is provided, as this basically violates the
	-- spirit of the "AEAD-should-be-simple-to-use" concept - you only can
	-- use the decrypted data after it got successfully verified.

	  chaChaPoly1305Encrypt
	, chaChaPoly1305Decrypt
	) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import Data.SecureMem

import Crypto.Nettle.Ciphers.ForeignImports
import Nettle.Utils

{-|
Encrypt plain text and create a verification tag for the encrypted text and some additional data.
@key@ and @nonce@ must not be reused together.
The returned tag is 16 bytes long, but may be shortened for verification (losing security).
-}
chaChaPoly1305Encrypt
	:: B.ByteString                 -- ^ @key@ (must be 32 bytes)
	-> B.ByteString                 -- ^ @nonce@ (must be 12 bytes)
	-> B.ByteString                 -- ^ @aad@ additional data to be verified
	-> B.ByteString                 -- ^ @plain@ data to encrypt
	-> (B.ByteString, B.ByteString) -- ^ returns (@cipher@, @tag@) ciphertext and verification tag
chaChaPoly1305Encrypt key nonce aad plain = unsafeDupablePerformIO $ do
	ctx <- allocateSecureMem c_chacha_poly1305_ctx_size
	tag <- B.create 16 (\_ -> return ())
	cipher <- B.create (B.length plain) (\_ -> return ())
	withByteStringPtr plain $ \psize pptr ->
		withByteStringPtr aad $ \aadsize aadptr ->
		withByteStringPtr cipher $ \_ cipherptr ->
		withByteStringPtr tag $ \_ tagptr ->
		withSecureMemPtr ctx $ \ctxptr ->
		withSecureMemPtrSz (toSecureMem key) $ \ksize kptr -> if ksize /= 32 then error "Invalid key length" else
		withSecureMemPtrSz (toSecureMem nonce) $ \nsize nptr -> if nsize /= 12 then error "Invalid nonce length" else do
		c_chacha_poly1305_set_key ctxptr kptr
		c_chacha_poly1305_set_nonce ctxptr nptr
		c_chacha_poly1305_update ctxptr aadsize aadptr
		c_chacha_poly1305_encrypt ctxptr psize cipherptr pptr
		c_chacha_poly1305_digest ctxptr 16 tagptr
	return (cipher, tag)

{-|
Decrypt cipher text and verify a (possible shortened) tag for the encrypted text and some additional data.
@key@ and @nonce@ must not be reused together.
-}
chaChaPoly1305Decrypt :: B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> B.ByteString -> Maybe B.ByteString
chaChaPoly1305Decrypt key nonce aad cipher verifytag = unsafeDupablePerformIO $ do
	ctx <- allocateSecureMem c_chacha_poly1305_ctx_size
	tag <- B.create 16 (\_ -> return ())
	plain <- B.create (B.length cipher) (\_ -> return ())
	withByteStringPtr cipher $ \psize pptr ->
		withByteStringPtr aad $ \aadsize aadptr ->
		withByteStringPtr plain $ \_ plainptr ->
		withByteStringPtr tag $ \_ tagptr ->
		withSecureMemPtr ctx $ \ctxptr ->
		withSecureMemPtrSz (toSecureMem key) $ \ksize kptr -> if ksize /= 32 then error "Invalid key length" else
		withSecureMemPtrSz (toSecureMem nonce) $ \nsize nptr -> if nsize /= 12 then error "Invalid nonce length" else do
		c_chacha_poly1305_set_key ctxptr kptr
		c_chacha_poly1305_set_nonce ctxptr nptr
		c_chacha_poly1305_update ctxptr aadsize aadptr
		c_chacha_poly1305_decrypt ctxptr psize plainptr pptr
		c_chacha_poly1305_digest ctxptr 16 tagptr
	if B.take (B.length verifytag) tag == verifytag then return $ Just plain else return Nothing
