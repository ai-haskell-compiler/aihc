{-# LANGUAGE TypeApplications #-}

module Main where

import qualified Codec.Serialise as Serialise
import Criterion.Main
import qualified Data.Avro as Avro
import qualified Data.Binary as Binary
import Data.ByteString.Lazy (fromStrict, toStrict)
import Data.ByteString.Short (fromShort, toShort)
import Data.MemPack
import qualified Data.Serialize as Cereal
import qualified Data.Store as Store
import qualified Flat as Flat

main :: IO ()
main = do
  defaultMain
    [ env (pure [1 :: Int .. 100000]) $ \xs ->
        bgroup
          "Pack"
          [ bgroup
              "ByteString"
              [ bench "MemPack" $ nf packByteString xs
              , bench "Store" $ nf Store.encode xs
              , bench "Flat" $ nf Flat.flat xs
              , bench "Cereal" $ nf Cereal.encode xs
              , bench "Binary" $ nf (toStrict . Binary.encode) xs
              , bench "Serialise" $ nf (toStrict . Serialise.serialise) xs
              , bench "Avro" $ nf (toStrict . Avro.encodeValue) xs
              ]
          ]
    , env (pure [1 :: Int .. 100000]) $ \xs ->
        bgroup
          "RoundTrip"
          [ bgroup
              "ByteString"
              [ bench "MemPack" $ nf (unpackError @[Int] . packByteString) xs
              , bench "Store" $ nf (Store.decodeEx @[Int] . Store.encode) xs
              , bench "Flat" $ nf (either (error . show) id . Flat.unflat @[Int] . Flat.flat) xs
              , bench "Cereal" $ nf (either (error . show) id . Cereal.decode @[Int] . Cereal.encode) xs
              , bench "Binary" $ nf (Binary.decode @[Int] . fromStrict . toStrict . Binary.encode) xs
              , bench "Serialise" $
                  nf (Serialise.deserialiseOrFail @[Int] . fromStrict . toStrict . Serialise.serialise) xs
              , bench "Avro" $
                  nf (Avro.decodeValue @[Int] . fromStrict . toStrict . Avro.encodeValue) xs
              ]
          , bgroup
              "ShortByteString"
              [ bench "MemPack" $ nf (unpackError @[Int] . packShortByteString) xs
              , bench "Store" $ nf (Store.decodeEx @[Int] . fromShort . toShort . Store.encode) xs
              , bench "Flat" $
                  nf (either (error . show) id . Flat.unflat @[Int] . fromShort . toShort . Flat.flat) xs
              , bench "Cereal" $
                  nf (either (error . show) id . Cereal.decode @[Int] . fromShort . toShort . Cereal.encode) xs
              , bench "Binary" $
                  nf
                    ( Binary.decode @[Int]
                        . fromStrict
                        . fromShort
                        . toShort
                        . toStrict
                        . Binary.encode
                    )
                    xs
              , bench "Serialise" $
                  nf
                    ( Serialise.deserialiseOrFail @[Int]
                        . fromStrict
                        . fromShort
                        . toShort
                        . toStrict
                        . Serialise.serialise
                    )
                    xs
              ]
          ]
    ]
