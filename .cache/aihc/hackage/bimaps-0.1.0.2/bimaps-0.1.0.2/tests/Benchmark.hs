
--
--
-- TODO if 'benchLookup' has no explicit type, compilation fails under
-- ghc-8.0.1. Investigate!

module Main where

import           Control.Applicative ((<$>))
import           Control.DeepSeq
import           Criterion.Main
import           Data.Tuple (swap)
import qualified Data.HashMap.Strict as H
import qualified Data.Map.Strict as M
import qualified Data.Vector as VV
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Unboxed as VU
import           System.Random.MWC
import           Text.Printf

import qualified Data.Bijection.Class as B
import qualified Data.Bijection.HashMap as HS
import qualified Data.Bijection.Map as BM
import qualified Data.Bijection.Vector as BV
import qualified Data.Bijection.Vector.Storable as BS
import qualified Data.Bijection.Vector.Unboxed as BU


runLookupBench
  :: (BU.Dom r ~ BU.Dom l, BU.Cod r ~ BU.Cod l, BU.DomCod r,
      BU.DomCod l, VG.Vector v (BU.Dom l), Ord (BU.Cod l),
      Num (BU.Cod l)) =>
     v (BU.Dom r) -> BU.Bimap l r -> Benchmark
runLookupBench xs' z = bench s $ whnf allLR xs'
  where s = printf "%5d" (B.size z)
        lL k = B.lookupL z k
        lR k = B.lookupR z k
        allL xs = VV.foldl' f 0 . VV.map lL . VG.convert $ xs
        allR xs = VV.foldl' f 0 . VV.map lR . VG.convert $ xs
        allLR xs = allL xs + allR xs
        f k (Just (!x)) = max k x
        f k _           = k
{-# Inline runLookupBench #-}

benchLookup
  :: (BU.Dom r ~ BU.Dom l, BU.Cod r ~ BU.Cod l, BU.DomCod r,
      BU.DomCod l, VG.Vector v (BU.Dom l), Ord (BU.Cod l),
      Num (BU.Cod l)) =>
     v (BU.Dom r) -> BU.Bimap l r -> BU.Cod l
benchLookup xs z = allLR -- bench s $ whnf allLR xs'
  where lL k = B.lookupL z k
        lR k = B.lookupR z k
        allL = VV.foldl' f 0 . VV.map lL . VG.convert $ xs
        allR = VV.foldl' f 0 . VV.map lR . VG.convert $ xs
        allLR = allL + allR
        f k (Just (!x)) = max k x
        f k _           = k
{-# Inline benchLookup #-}

benchVU :: VU.Vector Int -> BU.Bimap (VU.Vector Int) (VU.Vector Int) -> Int
benchVU = benchLookup
{-# NOINLINE benchVU #-}

benchBM :: VU.Vector Int -> BM.Bimap (M.Map Int Int) (M.Map Int Int) -> Int
benchBM = benchLookup
{-# NOINLINE benchBM #-}

main :: IO ()
main = do
  lkup :: VU.Vector Int <- withSystemRandom . asGenIO $ \gen -> uniformVector gen 10
  inputs :: [[Int]] <- mapM (\l -> withSystemRandom . asGenIO $ \gen -> VU.toList <$> uniformVector gen l) [1, 5, 10, 50, 100, 1000] -- [1,10,100,1000,10000]
  let zVV :: [BV.Bimap (VV.Vector Int) (VV.Vector Int)] = map (\i -> B.fromList $ zip i i) inputs
  let zVU :: [BU.Bimap (VU.Vector Int) (VU.Vector Int)] = map (\i -> B.fromList $ zip i i) inputs
  let zVS :: [BS.Bimap (VS.Vector Int) (VS.Vector Int)] = map (\i -> B.fromList $ zip i i) inputs
  let zMS :: [BM.Bimap (M.Map Int Int) (M.Map Int Int)] = map (\i -> B.fromList $ zip i i) inputs
  let zHS :: [HS.Bimap (H.HashMap Int Int) (H.HashMap Int Int)] = map (\i -> B.fromList $ zip i i) inputs
  deepseq (lkup,inputs,zVV,zVU,zVS,zMS,zHS) `seq` defaultMain
    [ bgroup "5"
      [ bench "vector/ unboxed" $ whnf (benchVU lkup) (zVU !! 1)
      , bench "   map/  strict" $ whnf (benchBM lkup) (zMS !! 1)
      ]
    , bgroup "by type"
--      [ bgroup "vector/    boxed" (map (runLookupBench lkup) zVV)
--      , bgroup "vector/ storable" (map (runLookupBench lkup) zVS)
      [ bgroup "vector/  unboxed" (map (runLookupBench lkup) zVU)
      , bgroup "   map/   strict" (map (runLookupBench lkup) zMS)
      , bgroup "  hash/   strict" (map (runLookupBench lkup) zHS)
      ]
    ]

