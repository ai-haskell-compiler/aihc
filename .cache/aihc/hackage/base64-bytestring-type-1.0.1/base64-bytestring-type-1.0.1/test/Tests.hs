{-# LANGUAGE CPP #-}
module Main (main) where

import Test.Tasty
import Test.Tasty.QuickCheck (testProperty, (===))

import qualified Data.Aeson                           as A
import qualified Data.Binary                          as B
import qualified Data.ByteString                      as BS
import qualified Data.ByteString.Base64.Lazy.Type     as L
import qualified Data.ByteString.Base64.Type          as S
import qualified Data.ByteString.Base64.URL.Lazy.Type as UL
import qualified Data.ByteString.Base64.URL.Type      as US
import qualified Data.ByteString.Lazy                 as LBS

#ifdef MIN_VERSION_cereal
import qualified Data.Serialize as C
#endif

#ifdef MIN_VERSION_serialise
import qualified Codec.Serialise as CBOR
#endif

#ifdef MIN_VERSION_http_api_data
import qualified Web.HttpApiData as HTTP
#endif

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ aeson
  , binary
#ifdef MIN_VERSION_cereal
  , cereal
#endif
#ifdef MIN_VERSION_serialise
  , serialise
#endif
#ifdef MIN_VERSION_http_api_data
  , httpApiData
#endif
  ]

aeson :: TestTree
aeson = testGroup "aeson"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS  bs64 = A.decode (A.encode bs64) === Just (bs64 :: S.ByteString64)
    propL  bs64 = A.decode (A.encode bs64) === Just (bs64 :: L.ByteString64)
    propUL bs64 = A.decode (A.encode bs64) === Just (bs64 :: UL.ByteString64)
    propUS bs64 = A.decode (A.encode bs64) === Just (bs64 :: US.ByteString64)

binary :: TestTree
binary = testGroup "binary"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in B.decode (B.encode bs64) === bs64

    propL ws = let bs64 = L.makeByteString64 (LBS.pack ws)
               in B.decode (B.encode bs64) === bs64

    propUS ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in B.decode (B.encode bs64) === bs64

    propUL ws = let bs64 = UL.makeByteString64 (LBS.pack ws)
                in B.decode (B.encode bs64) === bs64

#ifdef MIN_VERSION_cereal
cereal :: TestTree
cereal = testGroup "cereal"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in C.decode (C.encode bs64) === Right bs64

    propL ws = let bs64 = L.makeByteString64 (LBS.pack ws)
               in C.decode (C.encode bs64) === Right bs64

    propUS ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in C.decode (C.encode bs64) === Right bs64

    propUL ws = let bs64 = UL.makeByteString64 (LBS.pack ws)
                in C.decode (C.encode bs64) === Right bs64
#endif

#ifdef MIN_VERSION_serialise
serialise :: TestTree
serialise = testGroup "serialise"
    [ testProperty "strict"     propS
    , testProperty "lazy"       propL
    , testProperty "strict url" propUL
    , testProperty "lazy url"   propUS
    ]
  where
    propS ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in CBOR.deserialiseOrFail (CBOR.serialise bs64) === Right bs64

    propL ws = let bs64 = L.makeByteString64 (LBS.pack ws)
               in CBOR.deserialiseOrFail (CBOR.serialise bs64) === Right bs64

    propUS ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in CBOR.deserialiseOrFail (CBOR.serialise bs64) === Right bs64

    propUL ws = let bs64 = UL.makeByteString64 (LBS.pack ws)
                in CBOR.deserialiseOrFail (CBOR.serialise bs64) === Right bs64
#endif

#ifdef MIN_VERSION_http_api_data
httpApiData :: TestTree
httpApiData = testGroup "http-api-data"
    [ testProperty "toUrlPiece"       propU
    , testProperty "toUrlPiece url"   propUU
    , testProperty "toHeader"         propH
    , testProperty "toHeader url"     propHU
    , testProperty "toQueryParam"     propQ
    , testProperty "toQueryParam url" propQU
    ]
  where
    propU ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in HTTP.parseUrlPiece (HTTP.toUrlPiece bs64) === Right bs64

    propUU ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in HTTP.parseUrlPiece (HTTP.toUrlPiece bs64) === Right bs64

    propH ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in HTTP.parseHeader (HTTP.toHeader bs64) === Right bs64

    propHU ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in HTTP.parseHeader (HTTP.toHeader bs64) === Right bs64

    propQ ws = let bs64 = S.makeByteString64 (BS.pack ws)
               in HTTP.parseQueryParam (HTTP.toQueryParam bs64) === Right bs64

    propQU ws = let bs64 = US.makeByteString64 (BS.pack ws)
                in HTTP.parseQueryParam (HTTP.toQueryParam bs64) === Right bs64
#endif
