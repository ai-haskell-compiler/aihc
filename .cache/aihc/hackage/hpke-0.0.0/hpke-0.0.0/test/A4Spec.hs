{-# LANGUAGE OverloadedStrings #-}

module A4Spec where

import Crypto.HPKE
import Crypto.HPKE.Internal
import Data.ByteString ()
import Test.Hspec

import Test

kem_id :: KEM_ID
kem_id = DHKEM_P256_HKDF_SHA256
kdf_id :: KDF_ID
kdf_id = HKDF_SHA512
aead_id :: AEAD_ID
aead_id = AES_128_GCM

spec :: Spec
spec = do
    describe "A.4. DHKEM(P-256, HKDF-SHA256), HKDF-SHA512, AES-128-GCM" $ do
        it "A.4.1. Base Setup Information" $ do
            runTest
                ModeBase
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "0493ed86735bdfb978cc055c98b45695ad7ce61ce748f4dd63c525a3b8d53a15565c6897888070070c1579db1f86aaa56deb8297e64db7e8924e72866f9a472580"
                "2292bf14bb6e15b8c81a0f45b7a6e93e32d830e48cca702e0affcfb4d07e1b5c"
                "04085aa5b665dc3826f9650ccbcc471be268c8ada866422f739e2d531d4a8818a9466bc6b449357096232919ec4fe9070ccbac4aac30f4a1a53efcf7af90610edd"
                "3ac8530ad1b01885960fab38cf3cdc4f7aef121eaa239f222623614b4079fb38"
                ""
                ""
                ""
                "d3cf4984931484a080f74c1bb2a6782700dc1fef9abe8442e44a6f09044c88907200b332003543754eb51917ba"
                "d14414555a47269dfead9fbf26abb303365e40709a4ed16eaefe1f2070f1ddeb1bdd94d9e41186f124e0acc62d"
                "a32186b8946f61aeead1c093fe614945f85833b165b28c46bf271abf16b57208"
                "84998b304a0ea2f11809398755f0abd5f9d2c141d1822def79dd15c194803c2a"
                "93fb9411430b2cfa2cf0bed448c46922a5be9beff20e2e621df7e4655852edbc"
        it "A.4.2. PSK Setup Information" $ do
            runTest
                ModePsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04a307934180ad5287f95525fe5bc6244285d7273c15e061f0f2efb211c35057f3079f6e0abae200992610b25f48b63aacfcb669106ddee8aa023feed301901371"
                "a5901ff7d6931959c2755382ea40a4869b1dec3694ed3b009dda2d77dd488f18"
                "043f5266fba0742db649e1043102b8a5afd114465156719cea90373229aabdd84d7f45dabfc1f55664b888a7e86d594853a6cccdc9b189b57839cbbe3b90b55873"
                "bc6f0b5e22429e5ff47d5969003f3cae0f4fec50e23602e880038364f33b8522"
                ""
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "57624b6e320d4aba0afd11f548780772932f502e2ba2a8068676b2a0d3b5129a45b9faa88de39e8306da41d4cc"
                "159d6b4c24bacaf2f5049b7863536d8f3ffede76302dace42080820fa51925d4e1c72a64f87b14291a3057e00a"
                "8158bea21a6700d37022bb7802866edca30ebf2078273757b656ef7fc2e428cf"
                "6a348ba6e0e72bb3ef22479214a139ef8dac57be34509a61087a12565473da8d"
                "2f6d4f7a18ec48de1ef4469f596aada4afdf6d79b037ed3c07e0118f8723bffc"
        it "A.4.3. PSK Setup Information" $ do
            runTest
                ModeAuth
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04fec59fa9f76f5d0f6c1660bb179cb314ed97953c53a60ab38f8e6ace60fd59178084d0dd66e0f79172992d4ddb2e91172ce24949bcebfff158dcc417f2c6e9c6"
                "93cddd5288e7ef4884c8fe321d075df01501b993ff49ffab8184116f39b3c655"
                "04378bad519aab406e04d0e5608bcca809c02d6afd2272d4dd03e9357bd0eee8adf84c8deba3155c9cf9506d1d4c8bfefe3cf033a75716cc3cc07295100ec96276"
                "1ea4484be482bf25fdb2ed39e6a02ed9156b3e57dfb18dff82e4a048de990236"
                "02b266d66919f7b08f42ae0e7d97af4ca98b2dae3043bb7e0740ccadc1957579"
                ""
                ""
                "2480179d880b5f458154b8bfe3c7e8732332de84aabf06fc440f6b31f169e154157fa9eb44f2fa4d7b38a9236e"
                "10cd81e3a816d29942b602a92884348171a31cbd0f042c3057c65cd93c540943a5b05115bd520c09281061935b"
                "f03fbc82f321a0ab4840e487cb75d07aafd8e6f68485e4f7ff72b2f55ff24ad6"
                "1ce0cadec0a8f060f4b5070c8f8888dcdfefc2e35819df0cd559928a11ff0891"
                "70c405c707102fd0041ea716090753be47d68d238b111d542846bd0d84ba907c"
        it "A.4.4. PSK Setup Information" $ do
            runTest
                ModeAuthPsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04801740f4b1b35823f7fb2930eac2efc8c4893f34ba111c0bb976e3c7d5dc0aef5a7ef0bf4057949a140285f774f1efc53b3860936b92279a11b68395d898d138"
                "778f2254ae5d661d5c7fca8c4a7495a25bd13f26258e459159f3899df0de76c1"
                "04a4ca7af2fc2cce48edbf2f1700983e927743a4e85bb5035ad562043e25d9a111cbf6f7385fac55edc5c9d2ca6ed351a5643de95c36748e11dbec98730f4d43e9"
                "00510a70fde67af487c093234fc4215c1cdec09579c4b30cc8e48cb530414d0e"
                "d743b20821e6326f7a26684a4beed7088b35e392114480ca9f6c325079dcf10b"
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "840669634db51e28df54f189329c1b727fd303ae413f003020aff5e26276aaa910fc4296828cb9d862c2fd7d16"
                "d4680a48158d9a75fd09355878d6e33997a36ee01d4a8f22032b22373b795a941b7b9c5205ff99e0ff284beef4"
                "c8c917e137a616d3d4e4c9fcd9c50202f366cb0d37862376bc79f9b72e8a8db9"
                "33a5d4df232777008a06d0684f23bb891cfaef702f653c8601b6ad4d08dddddf"
                "bed80f2e54f1285895c4a3f3b3625e6206f78f1ed329a0cfb5864f7c139b3c6a"
