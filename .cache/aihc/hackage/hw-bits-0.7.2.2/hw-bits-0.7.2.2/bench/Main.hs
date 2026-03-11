module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Bits.Types.Broadword
import           HaskellWorks.Data.Bits.Types.Builtin

setupEnvVector :: Int -> IO (DVS.Vector Word64)
setupEnvVector n = return $ DVS.fromList (take n (cycle [maxBound, 0]))

benchPopCount1 :: [Benchmark]
benchPopCount1 =
  [ env (setupEnvVector 1000000) $ \bv -> bgroup "PopCount0"
    [ bench "Broadword" (nf (map (\n -> popCount0 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector (Broadword Word64))))) [0, 1000..100000])
    , bench "Builtin"   (nf (map (\n -> popCount0 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector (Builtin   Word64))))) [0, 1000..100000])
    , bench "Default"   (nf (map (\n -> popCount0 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector            Word64 )))) [0, 1000..100000])
    ]
  , env (setupEnvVector 1000000) $ \bv -> bgroup "PopCount1"
    [ bench "Broadword" (nf (map (\n -> popCount1 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector (Broadword Word64))))) [0, 1000..100000])
    , bench "Builtin"   (nf (map (\n -> popCount1 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector (Builtin   Word64))))) [0, 1000..100000])
    , bench "Default"   (nf (map (\n -> popCount1 (DVS.take n (DVS.unsafeCast bv :: DVS.Vector            Word64 )))) [0, 1000..100000])
    ]
  ]

main :: IO ()
main = defaultMain benchPopCount1
