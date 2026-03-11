{-# OPTIONS_GHC -fno-full-laziness #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}

import Control.Monad (forM_)
import Criterion.Main
import Criterion.Types
import Data.List (foldl')
import DataSketches.Quantiles.RelativeErrorQuantile
-- import Prometheus (register, summary, defaultQuantiles, observe, Info (Info))
import Control.Concurrent (withMVar, newMVar)

main :: IO ()
main = do
  outerSketch <- mkReqSketch 6 HighRanksAreAccurate
  -- let metric = summary (Info "adversarial_input" "woo") defaultQuantiles
  -- prometheusThing <- register metric
  skM <- newMVar =<< mkReqSketch 6 HighRanksAreAccurate
  -- mapM_ (update outerSketch) [1..10000]
  defaultMain
    [ bgroup "ReqSketch"
      [ bench "insert/1" $ perRunEnv (mkReqSketch 6 HighRanksAreAccurate) $ \sk -> do
          insert sk 1
      , bench "insert/10" $ perRunEnv (mkReqSketch 6 HighRanksAreAccurate) $ \sk -> do
          mapM_ (insert sk) [1..10]
      , bench "insert/100" $ perRunEnv (mkReqSketch 6 HighRanksAreAccurate) $ \sk -> do
          mapM_ (insert sk) [1..100]
      , bench "insert/1000" $ perRunEnv (mkReqSketch 6 HighRanksAreAccurate) $ \sk -> do
          mapM_ (insert sk) [1..1000]
      , bench "insert/10000" $ perRunEnv (mkReqSketch 6 HighRanksAreAccurate) $ \sk -> do
          mapM_ (insert sk) [1..10000]
      , bench "insert/existing" $ whnfIO $ insert outerSketch 1
      , bench "insert/mvar" $ whnfIO $ withMVar skM (`insert` 1)
      ]
    , bgroup "DoubleBuffer"
      [ -- bench "sort" $ 
      ]
    -- , bgroup "Prometheus"
    --   [ bench "insert/existing" $ whnfIO $
    --       observe prometheusThing 1
    --   ]
    ]