{-# LANGUAGE OverloadedStrings #-}

module A3Spec where

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
aead_id = AES_128_GCM

spec :: Spec
spec = do
    describe "A.3. DHKEM(P-256, HKDF-SHA256), HKDF-SHA256, AES-128-GCM" $ do
        it "A.3.1. Base Setup Information" $ do
            runTest
                ModeBase
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04a92719c6195d5085104f469a8b9814d5838ff72b60501e2c4466e5e67b325ac98536d7b61a1af4b78e5b7f951c0900be863c403ce65c9bfcb9382657222d18c4"
                "4995788ef4b9d6132b249ce59a77281493eb39af373d236a1fe415cb0c2d7beb"
                "04fe8c19ce0905191ebc298a9245792531f26f0cece2460639e8bc39cb7f706a826a779b4cf969b8a0e539c7f62fb3d30ad6aa8f80e30f1d128aafd68a2ce72ea0"
                "f3ce7fdae57e1a310d87f1ebbde6f328be0a99cdbcadf4d6589cf29de4b8ffd2"
                ""
                ""
                ""
                "5ad590bb8baa577f8619db35a36311226a896e7342a6d836d8b7bcd2f20b6c7f9076ac232e3ab2523f39513434"
                "fa6f037b47fc21826b610172ca9637e82d6e5801eb31cbd3748271affd4ecb06646e0329cbdf3c3cd655b28e82"
                "5e9bc3d236e1911d95e65b576a8a86d478fb827e8bdfe77b741b289890490d4d"
                "6cff87658931bda83dc857e6353efe4987a201b849658d9b047aab4cf216e796"
                "d8f1ea7942adbba7412c6d431c62d01371ea476b823eb697e1f6e6cae1dab85a"
        it "A.3.2. PSK Setup Information" $ do
            runTest
                ModePsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "04305d35563527bce037773d79a13deabed0e8e7cde61eecee403496959e89e4d0ca701726696d1485137ccb5341b3c1c7aaee90a4a02449725e744b1193b53b5f"
                "57427244f6cc016cddf1c19c8973b4060aa13579b4c067fd5d93a5d74e32a90f"
                "040d97419ae99f13007a93996648b2674e5260a8ebd2b822e84899cd52d87446ea394ca76223b76639eccdf00e1967db10ade37db4e7db476261fcc8df97c5ffd1"
                "438d8bcef33b89e0e9ae5eb0957c353c25a94584b0dd59c991372a75b43cb661"
                ""
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "90c4deb5b75318530194e4bb62f890b019b1397bbf9d0d6eb918890e1fb2be1ac2603193b60a49c2126b75d0eb"
                "9e223384a3620f4a75b5a52f546b7262d8826dea18db5a365feb8b997180b22d72dc1287f7089a1073a7102c27"
                "a115a59bf4dd8dc49332d6a0093af8efca1bcbfd3627d850173f5c4a55d0c185"
                "4517eaede0669b16aac7c92d5762dd459c301fa10e02237cd5aeb9be969430c4"
                "164e02144d44b607a7722e58b0f4156e67c0c2874d74cf71da6ca48a4cbdc5e0"
        it "A.3.3. PSK Setup Information" $ do
            runTest
                ModeAuth
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "042224f3ea800f7ec55c03f29fc9865f6ee27004f818fcbdc6dc68932c1e52e15b79e264a98f2c535ef06745f3d308624414153b22c7332bc1e691cb4af4d53454"
                "6b8de0873aed0c1b2d09b8c7ed54cbf24fdf1dfc7a47fa501f918810642d7b91"
                "04423e363e1cd54ce7b7573110ac121399acbc9ed815fae03b72ffbd4c18b01836835c5a09513f28fc971b7266cfde2e96afe84bb0f266920e82c4f53b36e1a78d"
                "d929ab4be2e59f6954d6bedd93e638f02d4046cef21115b00cdda2acb2a4440e"
                "1120ac99fb1fccc1e8230502d245719d1b217fe20505c7648795139d177f0de9"
                ""
                ""
                "82ffc8c44760db691a07c5627e5fc2c08e7a86979ee79b494a17cc3405446ac2bdb8f265db4a099ed3289ffe19"
                "b0a705a54532c7b4f5907de51c13dffe1e08d55ee9ba59686114b05945494d96725b239468f1229e3966aa1250"
                "837e49c3ff629250c8d80d3c3fb957725ed481e59e2feb57afd9fe9a8c7c4497"
                "594213f9018d614b82007a7021c3135bda7b380da4acd9ab27165c508640dbda"
                "14fe634f95ca0d86e15247cca7de7ba9b73c9b9deb6437e1c832daf7291b79d5"

        it "A.3.4. PSK Setup Information" $ do
            runTest
                ModeAuthPsk
                kem_id
                kdf_id
                aead_id
                "4f6465206f6e2061204772656369616e2055726e"
                "046a1de3fc26a3d43f4e4ba97dbe24f7e99181136129c48fbe872d4743e2b131357ed4f29a7b317dc22509c7b00991ae990bf65f8b236700c82ab7c11a84511401"
                "36f771e411cf9cf72f0701ef2b991ce9743645b472e835fe234fb4d6eb2ff5a0"
                "04d824d7e897897c172ac8a9e862e4bd820133b8d090a9b188b8233a64dfbc5f725aa0aa52c8462ab7c9188f1c4872f0c99087a867e8a773a13df48a627058e1b3"
                "bdf4e2e587afdf0930644a0c45053889ebcadeca662d7c755a353d5b4e2a8394"
                "b0ed8721db6185435898650f7a677affce925aba7975a582653c4cb13c72d240"
                "0247fd33b913760fa1fa51e1892d9f307fbe65eb171e8132c2af18555a738b82"
                "456e6e796e20447572696e206172616e204d6f726961"
                "b9f36d58d9eb101629a3e5a7b63d2ee4af42b3644209ab37e0a272d44365407db8e655c72e4fa46f4ff81b9246"
                "51788c4e5d56276771032749d015d3eea651af0c7bb8e3da669effffed299ea1f641df621af65579c10fc09736"
                "595ce0eff405d4b3bb1d08308d70a4e77226ce11766e0a94c4fdb5d90025c978"
                "110472ee0ae328f57ef7332a9886a1992d2c45b9b8d5abc9424ff68630f7d38d"
                "18ee4d001a9d83a4c67e76f88dd747766576cac438723bad0700a910a4d717e6"
