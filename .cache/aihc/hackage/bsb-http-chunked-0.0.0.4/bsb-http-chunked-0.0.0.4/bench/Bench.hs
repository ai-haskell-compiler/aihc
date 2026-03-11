{-# language DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
module Main where

import Gauge

import qualified Blaze.ByteString.Builder.HTTP as Blaze
import Data.ByteString.Builder.HTTP.Chunked

import Control.DeepSeq
import qualified Data.ByteString as S
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Builder.Extra as B
import qualified Data.ByteString.Lazy as L
import Data.Semigroup
import GHC.Generics

main :: IO ()
main = defaultMain
  [ benchEncode "clone village"
                cloneVillage
                (foldMap fromPerson)
  , benchEncode "100 4kB chunks"
                (S.replicate 4096 95)
                (stimes (100 :: Int) . B.byteString)
  , benchEncode "200kB strict bytestring"
                (S.replicate (200 * 1000) 95)
                B.byteString
  , benchEncode "1000 small chunks"
                "Hello"
                (stimes (1000 :: Int) . B.byteString)
  , benchEncode "1000 small chunks nocopy"
                "Hello"
                (stimes (1000 :: Int) . B.byteStringInsert)
  ]

-- Example adapted from
-- http://lambda-view.blogspot.de/2010/11/blaze-builder-library-faster.html

data Person = Person { pName :: String, pAge :: Int }
  deriving (Generic, NFData)

people :: [Person]
people = zipWith Person ["Haskell 98", "Switzerland", "λ-bot"] [12, 719, 7]

fromStringLen32le :: String -> B.Builder
fromStringLen32le cs =
  B.int32LE (fromIntegral $ length cs) <> B.stringUtf8 cs

fromPerson :: Person -> B.Builder
fromPerson p =
  fromStringLen32le (pName p) <> B.int32LE (fromIntegral $ pAge p)

cloneVillage :: [Person]
cloneVillage = take 10000 $ cycle $ people

-- Utils

benchEncode :: NFData input => String -> input -> (input -> B.Builder) -> Benchmark
benchEncode name input mkBuilder =
  env (return input) $ \input' -> bgroup name
    [ bench "bsbhc" $ nf (encode . mkBuilder) input'
    , bench "Blaze" $ nf (encodeBlaze . mkBuilder) input'
    ]

encode :: B.Builder -> L.ByteString
encode = B.toLazyByteString . chunkedTransferEncoding

encodeBlaze :: B.Builder -> L.ByteString
encodeBlaze = B.toLazyByteString . Blaze.chunkedTransferEncoding
