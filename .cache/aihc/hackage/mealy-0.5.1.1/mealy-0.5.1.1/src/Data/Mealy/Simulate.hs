{-# LANGUAGE NoImplicitPrelude #-}

-- | simulation to support testing of Mealy's using mwc-probability
module Data.Mealy.Simulate
  ( rvs,
    rvsp,
    create,
  )
where

import Control.Monad.Primitive (PrimState)
import NumHask.Prelude hiding (fold)
import System.Random.MWC.Probability

-- $setup
-- >>> :set -XFlexibleContexts
-- >>> import Data.Mealy
-- >>> gen <- create

-- | rvs creates a list of standard normal random variates.
--
-- >>> import Data.Mealy
-- >>> import Data.Mealy.Simulate
-- >>> gen <- create
-- >>> rvs gen 3
-- [1.8005943761746166e-2,0.36444481359059255,-1.2939898115295387]
--
-- >>> rs <- rvs gen 10000
-- >>> fold (ma 1) rs
-- 1.29805301109162e-2
--
-- >>> fold (std 1) rs
-- 1.0126527176272948
rvs :: Gen (PrimState IO) -> Int -> IO [Double]
rvs gen n = samples n standardNormal gen

-- | rvsPair generates a list of correlated random variate tuples
--
-- >>> rvsp gen 3 0.8
-- [(1.8005943761746166e-2,7.074509906249835e-2),(0.36444481359059255,-0.7073208451897444),(-1.2939898115295387,-0.643930709405127)]
--
-- >>> rsp <- rvsp gen 10000 0.8
-- >>> fold (corr (ma 1) (std 1)) rsp
-- 0.8050112742986588
rvsp :: Gen (PrimState IO) -> Int -> Double -> IO [(Double, Double)]
rvsp gen n c = do
  s0 <- rvs gen n
  s1 <- rvs gen n
  let s1' = zipWith (\x y -> c * x + sqrt (1 - c * c) * y) s0 s1
  pure $ zip s0 s1'
