{-# LANGUAGE BangPatterns #-}
module Main where

import qualified Data.ByteString as BS
import           Data.ByteString (ByteString)
import           Data.Word
import           Criterion.Main

import           Data.Digest.CRC32C

main =
  defaultMain [
    env (pure (largeByteString (2^8) 42)) $ \bs ->
    bench "crc32c small (2^8)" $ whnf crc32c bs

  , env (pure (largeByteString (2^16) 42)) $ \bs ->
    bench "crc32c large (2^16)" $ whnf crc32c bs

  , env (pure (largeByteString (2^16+1) 42)) $ \bs ->
    bench "crc32c large (2^16+1)" $ whnf crc32c bs
  ]

largeByteString :: Int -> Word -> ByteString
largeByteString n seed =
    fst $ BS.unfoldrN n (Just . step) seed
  where
    -- C11 example linear congruential generator
    step :: Word -> (Word8, Word)
    step rng = (w8, rng')
      where
        !rng' = rng * 1103515245 + 12345
        !w8   = fromIntegral (rng' `div` 65536)

