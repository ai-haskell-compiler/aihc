{-# LANGUAGE OverloadedStrings #-}

module A5Spec where

import Crypto.HPKE
import Crypto.HPKE.Internal
import Data.ByteString ()
import Test.Hspec

import Test

kem_id :: KEM_ID
kem_id = DHKEM_P256_HKDF_SHA256
kdf_id :: KDF_ID
kdf_id = HKDF_SHA256
aead_id :: AEAD_ID
aead_id = ChaCha20Poly1305

spec :: Spec
spec = do
    describe "A.5. DHKEM(P-256, HKDF-SHA256), HKDF-SHA256, ChaCha20Poly1305" $ do
        it "A.5.1. Base Setup Information" $ do
            runTest
                ModeBase
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04c07836a0206e04e31d8ae99bfd549380b072a1b1b82e563c935c095827824fc1559eac6fb9e3c70cd3193968994e7fe9781aa103f5b50e934b5b2f387e381291"
                "7550253e1147aae48839c1f8af80d2770fb7a4c763afe7d0afa7e0f42a5b3689"
                "04a697bffde9405c992883c5c439d6cc358170b51af72812333b015621dc0f40bad9bb726f68a5c013806a790ec716ab8669f84f6b694596c2987cf35baba2a006"
                "a4d1c55836aa30f9b3fbb6ac98d338c877c2867dd3a77396d13f68d3ab150d3b"
                ""
                ""
                ""
                "6469c41c5c81d3aa85432531ecf6460ec945bde1eb428cb2fedf7a29f5a685b4ccb0d057f03ea2952a27bb458b"
                "f1564199f7e0e110ec9c1bcdde332177fc35c1adf6e57f8d1df24022227ffa8716862dbda2b1dc546c9d114374"
                "9b13c510416ac977b553bf1741018809c246a695f45eff6d3b0356dbefe1e660"
                "6c8b7be3a20a5684edecb4253619d9051ce8583baf850e0cb53c402bdcaf8ebb"
                "477a50d804c7c51941f69b8e32fe8288386ee1a84905fe4938d58972f24ac938"
        it "A.5.2. PSK Setup Information" $ do
            runTest
                ModePsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04f336578b72ad7932fe867cc4d2d44a718a318037a0ec271163699cee653fa805c1fec955e562663e0c2061bb96a87d78892bff0cc0bad7906c2d998ebe1a7246"
                "7d6e4e006cee68af9b3fdd583a0ee8962df9d59fab029997ee3f456cbc857904"
                "041eb8f4f20ab72661af369ff3231a733672fa26f385ffb959fd1bae46bfda43ad55e2d573b880831381d9367417f554ce5b2134fbba5235b44db465feffc6189e"
                "12ecde2c8bc2d5d7ed2219c71f27e3943d92b344174436af833337c557c300b3"
                ""
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "21433eaff24d7706f3ed5b9b2e709b07230e2b11df1f2b1fe07b3c70d5948a53d6fa5c8bed194020bd9df0877b"
                "c74a764b4892072ea8c2c56b9bcd46c7f1e9ca8cb0a263f8b40c2ba59ac9c857033f176019562218769d3e0452"
                "530bbc2f68f078dccc89cc371b4f4ade372c9472bafe4601a8432cbb934f528d"
                "6e25075ddcc528c90ef9218f800ca3dfe1b8ff4042de5033133adb8bd54c401d"
                "6f6fbd0d1c7733f796461b3235a856cc34f676fe61ed509dfc18fa16efe6be78"
        it "A.5.3. PSK Setup Information" $ do
            runTest
                ModeAuth
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "040d5176aedba55bc41709261e9195c5146bb62d783031280775f32e507d79b5cbc5748b6be6359760c73cfe10ca19521af704ca6d91ff32fc0739527b9385d415"
                "085fd5d5e6ce6497c79df960cac93710006b76217d8bcfafbd2bb2c20ea03c42"
                "0444f6ee41818d9fe0f8265bffd016b7e2dd3964d610d0f7514244a60dbb7a11ece876bb110a97a2ac6a9542d7344bf7d2bd59345e3e75e497f7416cf38d296233"
                "3cb2c125b8c5a81d165a333048f5dcae29a2ab2072625adad66dbb0f48689af9"
                "39b19402e742d48d319d24d68e494daa4492817342e593285944830320912519"
                ""
                ""
                "25881f219935eec5ba70d7b421f13c35005734f3e4d959680270f55d71e2f5cb3bd2daced2770bf3d9d4916872"
                "653f0036e52a376f5d2dd85b3204b55455b7835c231255ae098d09ed138719b97185129786338ab6543f753193"
                "56c4d6c1d3a46c70fd8f4ecda5d27c70886e348efb51bd5edeaa39ff6ce34389"
                "d2d3e48ed76832b6b3f28fa84be5f11f09533c0e3c71825a34fb0f1320891b51"
                "eb0d312b6263995b4c7761e64b688c215ffd6043ff3bad2368c862784cbe6eff"
        it "A.5.4. PSK Setup Information" $ do
            runTest
                ModeAuthPsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "043539917ee26f8ae0aa5f784a387981b13de33124a3cde88b94672030183110f331400115855808244ff0c5b6ca6104483ac95724481d41bdcd9f15b430ad16f6"
                "11b7e4de2d919240616a31ab14944cced79bc2372108bb98f6792e3b645fe546"
                "04d383fd920c42d018b9d57fd73a01f1eee480008923f67d35169478e55d2e8817068daf62a06b10e0aad4a9e429fa7f904481be96b79a9c231a33e956c20b81b6"
                "c29fc577b7e74d525c0043f1c27540a1248e4f2c8d297298e99010a92e94865c"
                "53541bd995f874a67f8bfd8038afa67fd68876801f42ff47d0dc2a4deea067ae"
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "9eadfa0f954835e7e920ffe56dec6b31a046271cf71fdda55db72926e1d8fae94cc6280fcfabd8db71eaa65c05"
                "e357ad10d75240224d4095c9f6150a2ed2179c0f878e4f2db8ca95d365d174d059ff8c3eb38ea9a65cfc8eaeb8"
                "c52b4592cd33dd38b2a3613108ddda28dcf7f03d30f2a09703f758bfa8029c9a"
                "2f03bebc577e5729e148554991787222b5c2a02b77e9b1ac380541f710e5a318"
                "e01dd49e8bfc3d9216abc1be832f0418adf8b47a7b5a330a7436c31e33d765d7"
