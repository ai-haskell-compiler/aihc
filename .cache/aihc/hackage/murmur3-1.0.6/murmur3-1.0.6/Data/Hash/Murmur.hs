{-# LANGUAGE BangPatterns #-}

module Data.Hash.Murmur (murmur3) where

import Control.Monad (replicateM )

import Data.Serialize.Get 
    ( runGet
    , getWord32le
    )
import Data.Bits 
    ( shiftR
    , rotateL
    , xor
    )
import qualified Data.ByteString as BS 
    ( ByteString 
    , length 
    , drop
    , append
    , replicate
    )
import Data.List (foldl')
import Data.Word (Word32)

-- | MurmurHash3 (x86_32). For more details, see
-- <http://code.google.com/p/smhasher/source/browse/trunk/MurmurHash3.cpp>
murmur3 :: Word32         -- ^ Seed value
        -> BS.ByteString  -- ^ Strict bytestring data to hash
        -> Word32         -- ^ MurmurHash3 result
murmur3 nHashSeed bs = 
    h8
  where
    -- Block and tail sizes
    !nBlocks = BS.length bs `quot` 4
    !nTail   = BS.length bs `rem` 4
    -- Data objects
    Right blocks  = runGet (replicateM nBlocks getWord32le) bs
    bsTail  = BS.drop (nBlocks*4) bs `BS.append` BS.replicate (4-nTail) 0
    -- Body
    !h1   = foldl' mix nHashSeed blocks
    -- Tail
    Right !t1   = runGet getWord32le bsTail
    !t2   = t1 * c1
    !t3   = t2 `rotateL` 15
    !t4   = t3 * c2
    !h2   = h1 `xor` t4
    -- Finalization
    !h3   = h2 `xor` fromIntegral (BS.length bs)
    !h4   = h3 `xor` (h3 `shiftR` 16) 
    !h5   = h4 * 0x85ebca6b
    !h6   = h5 `xor` (h5 `shiftR` 13)
    !h7   = h6 * 0xc2b2ae35
    !h8   = h7 `xor` (h7 `shiftR` 16)
    -- Mix function
    mix !r1 !k1 = r4
      where
        !k2 = k1 * c1
        !k3 = k2 `rotateL` 15
        !k4 = k3 * c2
        !r2 = r1 `xor` k4
        !r3 = r2 `rotateL` 13
        !r4 = r3*5 + 0xe6546b64
    -- Constants
    c1 = 0xcc9e2d51 
    c2 = 0x1b873593 
