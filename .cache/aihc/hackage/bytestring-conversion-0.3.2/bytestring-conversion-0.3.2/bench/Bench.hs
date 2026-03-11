-- This Source Code Form is subject to the terms of the Mozilla Public
-- License, v. 2.0. If a copy of the MPL was not distributed with this
-- file, You can obtain one at http://mozilla.org/MPL/2.0/.

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Criterion
import Criterion.Main
import Data.ByteString.Conversion
import Data.Int
import Data.Monoid
import Data.Text.Encoding (encodeUtf8)
import Data.Word

import qualified Data.ByteString.Char8 as B
import qualified Data.Text             as T

main :: IO ()
main = defaultMain
    [ bgroup "bytestring-to : int"
        [ bench "1"  $ whnf toByteString (1 :: Int8)
        , bench "2"  $ whnf toByteString (19 :: Int16)
        , bench "4"  $ whnf toByteString (1234 :: Int32)
        , bench "8"  $ whnf toByteString (12345678 :: Int64)
        , bench "16" $ whnf toByteString (1234567812345678 :: Integer)
        ]
    , bgroup "pack . show : int"
        [ bench "1"  $ whnf (B.pack . show) (1 :: Int8)
        , bench "2"  $ whnf (B.pack . show) (19 :: Int16)
        , bench "4"  $ whnf (B.pack . show) (1234 :: Int32)
        , bench "8"  $ whnf (B.pack . show) (12345678 :: Int64)
        , bench "16" $ whnf (B.pack . show) (1234567812345678 :: Integer)
        ]
    , bgroup "bytestring-to : word"
        [ bench "1"  $ whnf toByteString (1 :: Word8)
        , bench "2"  $ whnf toByteString (19 :: Word16)
        , bench "4"  $ whnf toByteString (1234 :: Word32)
        , bench "8"  $ whnf toByteString (12345678 :: Word64)
        ]
    , bgroup "pack . show : word"
        [ bench "1"  $ whnf (B.pack . show) (1 :: Word8)
        , bench "2"  $ whnf (B.pack . show) (19 :: Word16)
        , bench "4"  $ whnf (B.pack . show) (1234 :: Word32)
        , bench "8"  $ whnf (B.pack . show) (12345678 :: Word64)
        ]
    , bgroup "bytestring-to : text"
        [ bench "26"  $ whnf toByteString txt26
        , bench "78"  $ whnf toByteString txt78
        ]
    , bgroup "encodeUtf8 : txt"
        [ bench "26"  $ whnf encodeUtf8 txt26
        , bench "78"  $ whnf encodeUtf8 txt78
        ]
    ]

txt26 :: T.Text
txt26 = T.pack $ ['a' .. 'z']

txt78 :: T.Text
txt78 = txt26 <> txt26 <> txt26
