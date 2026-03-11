{-# LANGUAGE BangPatterns #-}

import           Criterion.Main
import qualified Crypto.Hash.MD5      as MD5
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as L

benchSize :: Int -> Benchmark
benchSize sz = bs `seq` bench msg (whnf MD5.hash bs)
  where
    bs = B.replicate sz 0
    msg = "bs-" ++ show sz

main :: IO ()
main = do
    let !lbs64x256  = L.fromChunks $ replicate 4  (B.replicate 64 0)
        !lbs64x4096 = L.fromChunks $ replicate 64 (B.replicate 64 0)
    defaultMain
        [ bgroup "cryptohash-md5"
          [ benchSize 0
          , benchSize 8
          , benchSize 32
          , benchSize 64
          , benchSize 128
          , benchSize 256
          , benchSize 1024
          , benchSize 4096
          , benchSize 8192
          , benchSize 16384
          , benchSize (128*1024)
          , benchSize (1024*1024)
          , benchSize (2*1024*1024)
          , benchSize (4*1024*1024)

          , L.length lbs64x256  `seq` bench "lbs64x256"  (whnf MD5.hashlazy lbs64x256)
          , L.length lbs64x4096 `seq` bench "lbs64x4096" (whnf MD5.hashlazy lbs64x4096)
          ]
        ]
