{-# LANGUAGE OverloadedStrings #-}

module Test (runTest) where

import Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import Test.Hspec

import Crypto.HPKE
import Crypto.HPKE.Internal

runTest
    :: Mode
    -> KEM_ID
    -> KDF_ID
    -> AEAD_ID
    -> Info
    -> ByteString -- pkEm
    -> ByteString -- skEm
    -> ByteString -- pkRm
    -> ByteString -- skRm
    -> ByteString -- skSm
    -> PSK
    -> PSK_ID
    -> CipherText
    -> CipherText
    -> Key
    -> Key
    -> Key
    -> IO ()
runTest mode kem_id kdf_id aead_id _info _pkEm _skEm _pkRm _skRm _skSm _psk _psk_id _ct0 _ct1 _sec0 _sec1 _sec2 = do
    (enc, ctxS) <-
        setupS
            defaultHPKEMap
            mode
            kem_id
            kdf_id
            aead_id
            (Just skEm) -- key provided
            mskSm -- auth
            pkRm
            info
            psk
            psk_id
    ctxR <-
        setupR
            defaultHPKEMap
            mode
            kem_id
            kdf_id
            aead_id
            skRm
            mskSm -- auth
            pkEm
            info
            psk
            psk_id
    enc `shouldBe` pkEm
    ct0' <- seal ctxS aad0 pt
    ct0' `shouldBe` ct0
    pt0 <- open ctxR aad0 ct0
    pt0 `shouldBe` pt
    ct1' <- seal ctxS aad1 pt
    ct1' `shouldBe` ct1
    pt1 <- open ctxR aad1 ct1
    pt1 `shouldBe` pt

    exportS ctxS exporter_context0 32 `shouldBe` sec0
    exportR ctxR exporter_context0 32 `shouldBe` sec0
    exportS ctxS exporter_context1 32 `shouldBe` sec1
    exportS ctxS exporter_context2 32 `shouldBe` sec2
  where
    info = B16.decodeLenient _info
    pkEm = EncodedPublicKey $ B16.decodeLenient _pkEm
    skEm = EncodedSecretKey $ B16.decodeLenient _skEm
    pkRm = EncodedPublicKey $ B16.decodeLenient _pkRm
    skRm = EncodedSecretKey $ B16.decodeLenient _skRm
    mskSm
        | _skSm == "" = Nothing
        | otherwise = Just $ EncodedSecretKey $ B16.decodeLenient _skSm
    psk = B16.decodeLenient _psk
    psk_id = B16.decodeLenient _psk_id
    ct0 = B16.decodeLenient _ct0
    ct1 = B16.decodeLenient _ct1
    sec0 = B16.decodeLenient _sec0
    sec1 = B16.decodeLenient _sec1
    sec2 = B16.decodeLenient _sec2

aad0 :: AAD
aad0 = "\x43\x6f\x75\x6e\x74\x2d\x30"
aad1 :: AAD
aad1 = "\x43\x6f\x75\x6e\x74\x2d\x31"
pt :: PlainText
pt = "Beauty is truth, truth beauty"
exporter_context0 :: Info
exporter_context0 = ""
exporter_context1 :: Info
exporter_context1 = "\x00"
exporter_context2 :: Info
exporter_context2 = "\x54\x65\x73\x74\x43\x6f\x6e\x74\x65\x78\x74"
