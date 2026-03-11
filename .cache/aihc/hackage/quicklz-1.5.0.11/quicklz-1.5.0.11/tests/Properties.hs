{-# LANGUAGE ViewPatterns #-}
module Main (main) where

import qualified Data.ByteString as S
import Codec.Compression.QuickLZ

import Test.Framework (defaultMain, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)

main :: IO ()
main = defaultMain [ testGroup "simple" [
                       testProperty "compression identity" prop_compression_id
                     , testProperty "decompress is pure" prop_decompress_pure
                     , testProperty "compress is pure" prop_compress_pure
                     , testProperty "overlapping is correct" prop_overlap_id
                     ]
                   ]

prop_compression_id (S.pack -> xs) =
  xs == (decompress . compress) xs

prop_decompress_pure (S.pack -> xs) =
  let z  = compress xs
  in (decompress z) == (decompress z)

prop_compress_pure (S.pack -> xs) =
  (compress xs) == (compress xs)

prop_overlap_id (S.pack -> xs) =
  let z = compress xs
  in (decompress z) == (decompress' z)
