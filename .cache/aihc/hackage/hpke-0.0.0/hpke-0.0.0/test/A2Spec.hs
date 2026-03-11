{-# LANGUAGE OverloadedStrings #-}

module A2Spec where

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
aead_id = ChaCha20Poly1305

spec :: Spec
spec = do
    describe "A.2.  DHKEM(X25519, HKDF-SHA256), HKDF-SHA256, ChaCha20Poly1305" $ do
        it "A.2.1. Base Setup Information" $ do
            runTest
                ModeBase
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "1afa08d3dec047a643885163f1180476fa7ddb54c6a8029ea33f95796bf2ac4a"
                "f4ec9b33b792c372c1d2c2063507b684ef925b8c75a42dbcbf57d63ccd381600"
                "4310ee97d88cc1f088a5576c77ab0cf5c3ac797f3d95139c6c84b5429c59662a"
                "8057991eef8f1f1af18f4a9491d16a1ce333f695d4db8e38da75975c4478e0fb"
                ""
                ""
                ""
                "1c5250d8034ec2b784ba2cfd69dbdb8af406cfe3ff938e131f0def8c8b60b4db21993c62ce81883d2dd1b51a28"
                "6b53c051e4199c518de79594e1c4ab18b96f081549d45ce015be002090bb119e85285337cc95ba5f59992dc98c"
                "4bbd6243b8bb54cec311fac9df81841b6fd61f56538a775e7c80a9f40160606e"
                "8c1df14732580e5501b00f82b10a1647b40713191b7c1240ac80e2b68808ba69"
                "5acb09211139c43b3090489a9da433e8a30ee7188ba8b0a9a1ccf0c229283e53"
        it "A.2.2. PSK Setup Information" $ do
            runTest
                ModePsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "2261299c3f40a9afc133b969a97f05e95be2c514e54f3de26cbe5644ac735b04"
                "0c35fdf49df7aa01cd330049332c40411ebba36e0c718ebc3edf5845795f6321"
                "13640af826b722fc04feaa4de2f28fbd5ecc03623b317834e7ff4120dbe73062"
                "77d114e0212be51cb1d76fa99dd41cfd4d0166b08caa09074430a6c59ef17879"
                ""
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "4a177f9c0d6f15cfdf533fb65bf84aecdc6ab16b8b85b4cf65a370e07fc1d78d28fb073214525276f4a89608ff"
                "5c3cabae2f0b3e124d8d864c116fd8f20f3f56fda988c3573b40b09997fd6c769e77c8eda6cda4f947f5b704a8"
                "813c1bfc516c99076ae0f466671f0ba5ff244a41699f7b2417e4c59d46d39f40"
                "2745cf3d5bb65c333658732954ee7af49eb895ce77f8022873a62a13c94cb4e1"
                "ad40e3ae14f21c99bfdebc20ae14ab86f4ca2dc9a4799d200f43a25f99fa78ae"
        it "A.2.3. PSK Setup Information" $ do
            runTest
                ModeAuth
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "f7674cc8cd7baa5872d1f33dbaffe3314239f6197ddf5ded1746760bfc847e0e"
                "c94619e1af28971c8fa7957192b7e62a71ca2dcdde0a7cc4a8a9e741d600ab13"
                "1a478716d63cb2e16786ee93004486dc151e988b34b475043d3e0175bdb01c44"
                "3ca22a6d1cda1bb9480949ec5329d3bf0b080ca4c45879c95eddb55c70b80b82"
                "2def0cb58ffcf83d1062dd085c8aceca7f4c0c3fd05912d847b61f3e54121f05"
                ""
                ""
                "ab1a13c9d4f01a87ec3440dbd756e2677bd2ecf9df0ce7ed73869b98e00c09be111cb9fdf077347aeb88e61bdf"
                "3265c7807ffff7fdace21659a2c6ccffee52a26d270c76468ed74202a65478bfaedfff9c2b7634e24f10b71016"
                "070cffafd89b67b7f0eeb800235303a223e6ff9d1e774dce8eac585c8688c872"
                "2852e728568d40ddb0edde284d36a4359c56558bb2fb8837cd3d92e46a3a14a8"
                "1df39dc5dd60edcbf5f9ae804e15ada66e885b28ed7929116f768369a3f950ee"

        it "A.2.4. PSK Setup Information" $ do
            runTest
                ModeAuthPsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "656a2e00dc9990fd189e6e473459392df556e9a2758754a09db3f51179a3fc02"
                "5e6dd73e82b856339572b7245d3cbb073a7561c0bee52873490e305cbb710410"
                "a5099431c35c491ec62ca91df1525d6349cb8aa170c51f9581f8627be6334851"
                "7b36a42822e75bf3362dfabbe474b3016236408becb83b859a6909e22803cb0c"
                "90761c5b0a7ef0985ed66687ad708b921d9803d51637c8d1cb72d03ed0f64418"
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "9aa52e29274fc6172e38a4461361d2342585d3aeec67fb3b721ecd63f059577c7fe886be0ede01456ebc67d597"
                "59460bacdbe7a920ef2806a74937d5a691d6d5062d7daafcad7db7e4d8c649adffe575c1889c5c2e3a49af8e3e"
                "c23ebd4e7a0ad06a5dddf779f65004ce9481069ce0f0e6dd51a04539ddcbd5cd"
                "ed7ff5ca40a3d84561067ebc8e01702bc36cf1eb99d42a92004642b9dfaadd37"
                "d3bae066aa8da27d527d85c040f7dd6ccb60221c902ee36a82f70bcd62a60ee4"
