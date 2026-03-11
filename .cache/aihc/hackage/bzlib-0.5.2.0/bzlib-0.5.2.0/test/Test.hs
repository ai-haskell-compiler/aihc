{-# LANGUAGE CPP #-}

module Main where

import Codec.Compression.BZip.Internal

import Test.Codec.Compression.BZip.Internal ()
import Test.Codec.Compression.BZip.Stream ()

import Test.QuickCheck
import Test.Tasty
import Test.Tasty.QuickCheck
import Utils ()

import Control.Monad


main :: IO ()
main = defaultMain $
  testGroup "bzip tests" [
    testGroup "property tests" [
      testProperty "decompress . compress = id (standard)" prop_decompress_after_compress
    ]
  ]


prop_decompress_after_compress :: CompressParams
                               -> DecompressParams
                               -> Property
prop_decompress_after_compress cp dp =
  decompressBufferSize dp > 0 && compressBufferSize cp > 0 ==>
  liftM2 (==) (decompress dp . compress cp) id
