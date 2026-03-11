{-# LANGUAGE CPP #-}

module Main where

import Criterion.Main
import Data.Bits.Pdep
import Data.Bits.Pdep.Slow
import Data.Bits.Pext
import Data.Word

#if !MIN_VERSION_base(4,13,0)
import Data.Monoid         ((<>))
#endif

import qualified Data.Vector.Storable as DVS

{- HLINT ignore "Monoid law, left identity" -}

mkVector :: (Num w, DVS.Storable w) => w -> IO (DVS.Vector w)
mkVector w = return $ DVS.unfoldrN 1024 gen w
  where gen b = let a = b + 1 in Just (a, a)

benchPdep :: [Benchmark]
benchPdep =
  [ env (mkVector (0 :: Word64)) $ \_ -> bgroup "pdep-single"
    [ bench "pdep64"      (whnf (pdep       0) (63 :: Word64))
    , bench "pdep32"      (whnf (pdep       0) (63 :: Word32))
    ]
  , env (mkVector (0 :: Word64)) $ \_ -> bgroup "pdep-slow"
    [ bench "slowPdep64  8-bits"  (whnf (slowPdep  0x00000000000000ff) (0xff00000000000000 :: Word64))
    , bench "slowPdep64 16-bits"  (whnf (slowPdep  0x000000000000ffff) (0xffff000000000000 :: Word64))
    , bench "slowPdep64 32-bits"  (whnf (slowPdep  0x00000000ffffffff) (0xffffffff00000000 :: Word64))
    , bench "slowPdep64 48-bits"  (whnf (slowPdep  0x0000ffffffffffff) (0xffffffffffff0000 :: Word64))
    , bench "slowPdep64 64-bits"  (whnf (slowPdep  0xffffffffffffffff) (0xffffffffffffffff :: Word64))
    ]
  ]

benchPdepVector :: [Benchmark]
benchPdepVector =
  [ env (mkVector (0 :: Word64)) $ \v -> bgroup "pdep-vector"
    [ bench "pdep64"    (whnf (DVS.foldr (\a b -> pdep a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "pdep-vector"
    [ bench "pdep32"    (whnf (DVS.foldr (\a b -> pdep a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word64)) $ \v -> bgroup "pdep-vector"
    [ bench "pdep64"    (whnf (DVS.foldr (\a b -> pdep a 0 + b) 0) v)
    ]
  ]

benchPext :: [Benchmark]
benchPext =
  [ env (mkVector (0 :: Word64)) $ \_ -> bgroup "pext-single"
    [ bench "pext64"    (whnf (pext 0) (63 :: Word64))
    ]
  , env (mkVector (0 :: Word32)) $ \_ -> bgroup "pext-single"
    [ bench "pext32"    (whnf (pext 0) (63 :: Word64))
    ]
  ]

benchPextVector :: [Benchmark]
benchPextVector =
  [ env (mkVector (0 :: Word64)) $ \v -> bgroup "pext-vector"
    [ bench "pext64"    (whnf (DVS.foldr (\a b -> pext a 0 + b) 0) v)
    ]
  , env (mkVector (0 :: Word32)) $ \v -> bgroup "pext-vector"
    [ bench "pext32"    (whnf (DVS.foldr (\a b -> pext a 0 + b) 0) v)
    ]
  ]

benchAll :: [Benchmark]
benchAll = mempty
  <> benchPdep
  <> benchPext
  <> benchPdepVector
  <> benchPextVector

main :: IO ()
main = do
  putStrLn $ "Fast pdep enabled: " <> show fastPdepEnabled
  putStrLn $ "Fast pext enabled: " <> show fastPextEnabled
  defaultMain benchAll
