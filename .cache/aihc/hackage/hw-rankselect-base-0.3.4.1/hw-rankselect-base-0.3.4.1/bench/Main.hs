{-# LANGUAGE OverloadedStrings #-}

module Main where

import Criterion.Main
import Data.Bits.Pdep
import Data.Word
import HaskellWorks.Data.RankSelect.Base
import HaskellWorks.Data.RankSelect.Base.Internal

import qualified Data.Vector.Storable as DVS

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env (return ()) $ \_ -> bgroup "8-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x55 :: Word8)) 8)
    , bench "Select - Once" (whnf (select1  (0x55 :: Word8)) 8)
    ]
  , env (return ()) $ \_ -> bgroup "16-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x5555 :: Word16)) 16)
    , bench "Select - Once" (whnf (select1  (0x5555 :: Word16)) 16)
    ]
  , env (return ()) $ \_ -> bgroup "32-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x55555555 :: Word32)) 32)
    , bench "Select - Once" (whnf (select1  (0x55555555 :: Word32)) 32)
    ]
  , env (return ()) $ \_ -> bgroup "64-bit"
    [ bench "Rank - Once"   (whnf (rank1    (0x5555555555555555 :: Word64)) 64)
    , bench "Select - Once" (whnf (select1  (0x5555555555555555 :: Word64)) 64)
    ]
  , env (setupEnvVector 1000000) $ \bv -> bgroup "64-bit vector"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (rank1 bv)) [0, 1000..10000000])
    ]
  , env (return ()) $ \_ -> bgroup "64-bit"
    [ bench "Once: Rank1"               (whnf (rank1                  (0x5555555555555555 :: Word64)) 64)
    , bench "Once: Select1 Class"       (whnf (select1                (0x5555555555555555 :: Word64)) 64)
    , bench "Once: Select1 Function"    (whnf (select1Word64          (0x5555555555555555 :: Word64)) 64)
    , bench "Once: Select1 Broadword"   (whnf (select1Word64Broadword (0x5555555555555555 :: Word64)) 64)
    , bench "Once: Select1 Bmi2"        (whnf (select1Word64Bmi2      (0x5555555555555555 :: Word64)) 64)
    , bench "Once: Select1 Bmi2Base0"   (whnf (select1Word64Bmi2Base0 (0x5555555555555555 :: Word64)) 64)
    ]
  , env (return ()) $ \_ -> bgroup "32-bit"
    [ bench "Once: Rank1"               (whnf (rank1                  (0x55555555 :: Word32)) 32)
    , bench "Once: Select1 Class"       (whnf (select1                (0x55555555 :: Word32)) 32)
    , bench "Once: Select1 Function"    (whnf (select1Word32          (0x55555555 :: Word32)) 32)
    , bench "Once: Select1 Broadword"   (whnf (select1Word32Broadword (0x55555555 :: Word32)) 32)
    , bench "Once: Select1 Bmi2"        (whnf (select1Word32Bmi2      (0x55555555 :: Word32)) 32)
    ]
  ]

main :: IO ()
main = do
  putStrLn $ "Fast pdep enabled: " <> show fastPdepEnabled
  defaultMain benchRankSelect
