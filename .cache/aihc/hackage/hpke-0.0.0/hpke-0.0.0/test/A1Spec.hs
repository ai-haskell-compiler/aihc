{-# LANGUAGE OverloadedStrings #-}

module A1Spec where

import Crypto.HPKE
import Crypto.HPKE.Internal
import Data.ByteString ()
import Test.Hspec

import Test

kem_id :: KEM_ID
kem_id = DHKEM_X25519_HKDF_SHA256
kdf_id :: KDF_ID
kdf_id = HKDF_SHA256
aead_id :: AEAD_ID
aead_id = AES_128_GCM

spec :: Spec
spec = do
    describe "A.1. DHKEM(X25519, HKDF-SHA256), HKDF-SHA256, AES-128-GCM" $ do
        it "A.1.1. Base Setup Information" $ do
            runTest
                ModeBase
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "37fda3567bdbd628e88668c3c8d7e97d1d1253b6d4ea6d44c150f741f1bf4431"
                "52c4a758a802cd8b936eceea314432798d5baf2d7e9235dc084ab1b9cfa2f736"
                "3948cfe0ad1ddb695d780e59077195da6c56506b027329794ab02bca80815c4d"
                "4612c550263fc8ad58375df3f557aac531d26850903e55a9f23f21d8534e8ac8"
                ""
                ""
                ""
                "f938558b5d72f1a23810b4be2ab4f84331acc02fc97babc53a52ae8218a355a96d8770ac83d07bea87e13c512a"
                "af2d7e9ac9ae7e270f46ba1f975be53c09f8d875bdc8535458c2494e8a6eab251c03d0c22a56b8ca42c2063b84"
                "3853fe2b4035195a573ffc53856e77058e15d9ea064de3e59f4961d0095250ee"
                "2e8f0b54673c7029649d4eb9d5e33bf1872cf76d623ff164ac185da9e88c21a5"
                "e9e43065102c3836401bed8c3c3c75ae46be1639869391d62c61f1ec7af54931"

        it "A.1.2. PSK Setup Information" $ do
            runTest
                ModePsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "0ad0950d9fb9588e59690b74f1237ecdf1d775cd60be2eca57af5a4b0471c91b"
                "463426a9ffb42bb17dbe6044b9abd1d4e4d95f9041cef0e99d7824eef2b6f588"
                "9fed7e8c17387560e92cc6462a68049657246a09bfa8ade7aefe589672016366"
                "c5eb01eb457fe6c6f57577c5413b931550a162c71a03ac8d196babbd4e5ce0fd"
                ""
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "e52c6fed7f758d0cf7145689f21bc1be6ec9ea097fef4e959440012f4feb73fb611b946199e681f4cfc34db8ea"
                "49f3b19b28a9ea9f43e8c71204c00d4a490ee7f61387b6719db765e948123b45b61633ef059ba22cd62437c8ba"
                "dff17af354c8b41673567db6259fd6029967b4e1aad13023c2ae5df8f4f43bf6"
                "6a847261d8207fe596befb52928463881ab493da345b10e1dcc645e3b94e2d95"
                "8aff52b45a1be3a734bc7a41e20b4e055ad4c4d22104b0c20285a7c4302401cd"

        it "A.1.3. PSK Setup Information" $ do
            runTest
                ModeAuth
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "23fb952571a14a25e3d678140cd0e5eb47a0961bb18afcf85896e5453c312e76"
                "ff4442ef24fbc3c1ff86375b0be1e77e88a0de1e79b30896d73411c5ff4c3518"
                "1632d5c2f71c2b38d0a8fcc359355200caa8b1ffdf28618080466c909cb69b2e"
                "fdea67cf831f1ca98d8e27b1f6abeb5b7745e9d35348b80fa407ff6958f9137e"
                "dc4a146313cce60a278a5323d321f051c5707e9c45ba21a3479fecdf76fc69dd"
                ""
                ""
                "5fd92cc9d46dbf8943e72a07e42f363ed5f721212cd90bcfd072bfd9f44e06b80fd17824947496e21b680c141b"
                "d3736bb256c19bfa93d79e8f80b7971262cb7c887e35c26370cfed62254369a1b52e3d505b79dd699f002bc8ed"
                "28c70088017d70c896a8420f04702c5a321d9cbf0279fba899b59e51bac72c85"
                "25dfc004b0892be1888c3914977aa9c9bbaf2c7471708a49e1195af48a6f29ce"
                "5a0131813abc9a522cad678eb6bafaabc43389934adb8097d23c5ff68059eb64"
        it "A.1.4. PSK Setup Information" $ do
            runTest
                ModeAuthPsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "820818d3c23993492cc5623ab437a48a0a7ca3e9639c140fe1e33811eb844b7c"
                "14de82a5897b613616a00c39b87429df35bc2b426bcfd73febcb45e903490768"
                "1d11a3cd247ae48e901939659bd4d79b6b959e1f3e7d66663fbc9412dd4e0976"
                "cb29a95649dc5656c2d054c1aa0d3df0493155e9d5da6d7e344ed8b6a64a9423"
                "fc1c87d2f3832adb178b431fce2ac77c7ca2fd680f3406c77b5ecdf818b119f4"
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "a84c64df1e11d8fd11450039d4fe64ff0c8a99fca0bd72c2d4c3e0400bc14a40f27e45e141a24001697737533e"
                "4d19303b848f424fc3c3beca249b2c6de0a34083b8e909b6aa4c3688505c05ffe0c8f57a0a4c5ab9da127435d9"
                "08f7e20644bb9b8af54ad66d2067457c5f9fcb2a23d9f6cb4445c0797b330067"
                "52e51ff7d436557ced5265ff8b94ce69cf7583f49cdb374e6aad801fc063b010"
                "a30c20370c026bbea4dca51cb63761695132d342bae33a6a11527d3e7679436d"
