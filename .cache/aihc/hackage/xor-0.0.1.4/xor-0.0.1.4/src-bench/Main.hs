{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

{-# LANGUAGE CApiFFI           #-}

{-# OPTIONS_GHC -Wall  #-}

-- |
-- Copyright: © 2020  Herbert Valerio Riedel
-- SPDX-License-Identifier: GPL-2.0-or-later
--
module Main
    ( main
    , xor32ByteString'ref
    , xor32ByteString'v3
    , xor32ByteString'v4
    ) where

import           Control.Exception        (assert)
import           Control.Monad
import           Criterion.Main
import           Data.Bits
import qualified Data.ByteString          as BS
import           Data.ByteString.Internal as BS
import qualified Data.ByteString.Short    as SBS
import           Data.Word                (Word32, Word8)
import           Foreign.ForeignPtr
import           Foreign.Marshal.Utils    (copyBytes)
import           Foreign.Ptr
import           Foreign.Storable
import           GHC.ByteOrder            (ByteOrder (..), targetByteOrder)

import qualified Data.XOR                 as IUT

main :: IO ()
main = defaultMain benches

benches :: [Benchmark]
benches =
    [ doGroup "4k"   bs4k
    , doGroup "4k1"  bs4k1
    , doGroup "4k2"  bs4k2
    , doGroup "4k3"  bs4k3
    , doGroup "32k"  bs32k
    , doGroup "256k" bs256k
    ]
  where
    doGroup label bs = let sbs = SBS.toShort bs in bgroup label
      [ bench "REF"      $ whnf (xor32ByteString'ref msk)       bs
      , bench "IUT"      $ whnf (IUT.xor32StrictByteString msk) bs
      , bench "IUT/SBS"  $ whnf (IUT.xor32ShortByteString msk) sbs
      , bench "v3"       $ whnf (xor32ByteString'v3 msk)        bs
      , bench "v4"       $ whnf (xor32ByteString'v4 msk)        bs
      , bench "REF 8bit" $ whnf (xor8StrictByteString'ref msk8) bs
      , bench "IUT 8bit" $ whnf (IUT.xor8StrictByteString msk8) bs
      ]

    {-# NOINLINE bs32k #-}
    !bs4k = BS.replicate (4*1024) 0x55
    !bs4k1 = BS.replicate (4*1024+1) 0x55
    !bs4k2 = BS.replicate (4*1024+2) 0x55
    !bs4k3 = BS.replicate (4*1024+3) 0x55
    !bs32k = BS.replicate (32*1024) 0x55
    !bs256k = BS.replicate (256*1024) 0x55

    {-# NOINLINE msk #-}
    msk = 0x12345678

    msk8 = 0x42

----------------------------------------------------------------------------

-- reference impl

{-# NOINLINE xor32ByteString'ref #-}
xor32ByteString'ref :: Word32 -> BS.ByteString -> BS.ByteString
xor32ByteString'ref 0    = id
xor32ByteString'ref msk0 = snd . BS.mapAccumL go msk0
  where
    go :: Word32 -> Word8 -> (Word32,Word8)
    go msk b = let b'   = fromIntegral msk' `xor` b
                   msk' = rotateL msk 8
               in b' `seq` (msk',b')


{-# NOINLINE xor8StrictByteString'ref #-}
xor8StrictByteString'ref :: Word8 -> BS.ByteString -> BS.ByteString
xor8StrictByteString'ref 0    = id
xor8StrictByteString'ref msk0 = BS.map (xor msk0)

-- {-# NOINLINE xor32ByteString'v2 #-}
-- xor32ByteString'v2 :: Word32 -> BS.ByteString -> BS.ByteString
-- xor32ByteString'v2 msk0 = snd . BS.mapAccumL go mskstr
--   where
--     mskstr :: [Word8]
--     mskstr = cycle (map fromIntegral (tail (take 5 (iterate rotl8 msk0))))
--
--     rotl8 :: Word32 -> Word32
--     rotl8 = flip rotateL 8
--
--     go (x:xs) b = let !b' = xor x b in (xs,b')

{-# NOINLINE xor32ByteString'v3 #-}
xor32ByteString'v3 :: Word32 -> BS.ByteString -> BS.ByteString
xor32ByteString'v3 0 bs = bs
xor32ByteString'v3 _ bs | BS.null bs = bs
xor32ByteString'v3 msk0 (BS.PS x s l)
    = unsafeCreate l $ \p8 ->
        withForeignPtr x $ \f -> do
          copyBytes p8 (f `plusPtr` s) (fromIntegral l)
          let p32 = castPtr p8 :: Ptr Word32
              l32 = l `quot` 4
              p32end = p32 `plusPtr` (l32*4)
          unless (alignPtr p32 4 == p32) $ fail "bytestring allocation not aligned"
          xor32PtrAligned msk0 p32 (l32*4)
          _ <- xor32PtrNonAligned msk0 (castPtr p32end) (l - (l32*4))
          return ()

{-# NOINLINE xor32ByteString'v4 #-}
xor32ByteString'v4 :: Word32 -> BS.ByteString -> BS.ByteString
xor32ByteString'v4  0 bs = bs
xor32ByteString'v4  _ bs | BS.null bs = bs
xor32ByteString'v4 msk0 (BS.PS x s l)
    = unsafeCreate l $ \p8 ->
        withForeignPtr x $ \f -> do
          copyBytes p8 (f `plusPtr` s) (fromIntegral l)
          _ <- IUT.xor32CStringLen msk0 (castPtr p8,l)
          return ()

{-# INLINE xor32PtrNonAligned #-}
xor32PtrNonAligned :: Word32 -> Ptr Word8 -> Int -> IO Word32
xor32PtrNonAligned mask0 _ 0 = return mask0
xor32PtrNonAligned mask0 p0 n = go mask0 p0
  where
    p' = p0 `plusPtr` n
    go m p
      | p == p'   = return m
      | otherwise = do
          let m' = rotateL m 8
          xor8Ptr1 (fromIntegral m') p
          go m' (p `plusPtr` 1)

{-# INLINE xor32PtrAligned #-}
xor32PtrAligned :: Word32 -> Ptr Word32 -> Int -> IO ()
xor32PtrAligned _ _ 0 = return ()
xor32PtrAligned mask0be p0 n
  = assert (p0 == alignPtr p0 4 && n `rem` 4 == 0) $ go p0
  where
    p' = p0 `plusPtr` n
    go p
      | p == p'   = return ()
      | otherwise = do { xor32Ptr1 mask0 p; go (p `plusPtr` 4) }

    mask0 = case targetByteOrder of
              LittleEndian -> {- byteSwap32 -} mask0be
              BigEndian    -> mask0be

----------------------------------------------------------------------------

xor8Ptr1 :: Word8 -> Ptr Word8 -> IO ()
xor8Ptr1 msk ptr  = do { x <- peek ptr; poke ptr (xor msk x) }

-- xor16Ptr1 :: Word16 -> Ptr Word16 -> IO ()
-- xor16Ptr1 msk ptr = do { x <- peek ptr; poke ptr (xor msk x) }

xor32Ptr1 :: Word32 -> Ptr Word32 -> IO ()
xor32Ptr1 msk ptr = do { x <- peek ptr; poke ptr (xor msk x) }
