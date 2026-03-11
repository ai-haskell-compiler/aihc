{-# LANGUAGE CPP #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}
{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

import Test.Tasty (TestTree, defaultMain, testGroup)
import Test.Tasty.QuickCheck

import Data.Word (Word8, Word32)
import Data.Either (isLeft, isRight)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Binary.Put as B
import qualified Data.Binary.Get as B
import qualified Data.Serialize.Put as C
import qualified Data.Serialize.Get as C
import Data.Serializer (Serializer)
import qualified Data.Serializer as S
import Data.Deserializer (Deserializer)
import qualified Data.Deserializer as D
import Control.Applicative ((<|>))

binaryBuilder = LBS.unpack . B.runPut . S.binarySerializer
cerealBuilder = LBS.unpack . C.runPutLazy . S.cerealSerializer

serializerTests ∷ Serializer s ⇒ String → (s → [Word8]) → TestTree
serializerTests name build =
  testGroup name
    [ testProperty "word32L" $
        build (S.word32L 0x12345678) == [0x78, 0x56, 0x34, 0x12]
    , testProperty "word32B" $
        build (S.word32B 0x12345678) == [0x12, 0x34, 0x56, 0x78]
    , testProperty "putL Word32" $
        build (S.putL (0x12345678 ∷ Word32)) == [0x78, 0x56, 0x34, 0x12]
    , testProperty "putB Word32" $
        build (S.putB (0x12345678 ∷ Word32)) == [0x12, 0x34, 0x56, 0x78]
    ]

binaryParser p = either (\(_, _, e) → Left e) (\(_, _, r) → Right r)
               . B.runGetOrFail (D.binaryDeserializer p)
               . LBS.pack
cerealParser p = C.runGet (D.cerealDeserializer p) . BS.pack

deserializerTests ∷ Deserializer μ
                  ⇒ String → (∀ α . μ α → [Word8] → Either String α) → TestTree
deserializerTests name parse =
  testGroup name
    [ testProperty "word32L" $
        parse D.word32L [0x12, 0x34, 0x56, 0x78] == Right 0x78563412
    , testProperty "word32B" $
        parse D.word32B [0x12, 0x34, 0x56, 0x78] == Right 0x12345678
    , testProperty "getL Word32" $
        parse D.getL [0x12, 0x34, 0x56, 0x78] == Right (0x78563412 ∷ Word32)
    , testProperty "getB Word32" $
        parse D.getB [0x12, 0x34, 0x56, 0x78] == Right (0x12345678 ∷ Word32)
    , testProperty "eof succeeds on empty input" $
        isRight (parse D.eof [])
    , testProperty "eof fails on non-empty input" $
        isLeft (parse D.eof [0x00])
    , testProperty "try and <|>" $
        isRight (parse (D.try (D.byte 0x01 >> D.bytes "\x02\x03") <|>
                        D.bytes "\x01\x03\x02")
                       [0x01, 0x03, 0x02])
    ]

main = defaultMain
     $ testGroup "Tests"
         [ testGroup "Serializers"
             [ serializerTests "Builder" S.buildBytes
             , serializerTests "Binary.Put" binaryBuilder
             , serializerTests "Cereal.Put" cerealBuilder
             ]
         , testGroup "Deserializers"
             [ deserializerTests "Binary.Get" binaryParser
             , deserializerTests "Cereal.Get" cerealParser
             ]
         ]
