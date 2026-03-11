{-# LANGUAGE NoImplicitPrelude #-}
module Synthesizer.Plain.Filter.Delay (
   phaser,
   plane,

   -- for testing
   propAll,
   ) where

import qualified Synthesizer.Plain.Filter.NonRecursive as FiltNR
import qualified Synthesizer.Plain.Displacement as Syn
import qualified Synthesizer.Plain.Control as Ctrl
import qualified Synthesizer.Plain.Noise   as Noise
import System.Random (randomRs, mkStdGen, )

import qualified Algebra.Module    as Module
import qualified Algebra.RealField as RealField

import qualified Synthesizer.Plain.Interpolation as Interpolation

import qualified Synthesizer.Plain.Filter.Delay.ST    as DelayST
import qualified Synthesizer.Plain.Filter.Delay.List  as DelayList
import qualified Synthesizer.Plain.Filter.Delay.Block as DelayBlock

import NumericPrelude.Numeric
import NumericPrelude.Base


phaser :: (Module.C a v, RealField.C a) => a -> [a] -> [v] -> [v]
phaser maxDelay ts xs =
   FiltNR.amplifyVector (0.5 `asTypeOf` head ts)
      (Syn.mix xs
          (DelayBlock.modulated Interpolation.constant (ceiling maxDelay) ts xs))


plane :: Double -> [Double]
plane sampleRate =
   let maxDelay = 500
   in  phaser
          maxDelay
          (map (maxDelay-)
               (Ctrl.exponential2 (10*sampleRate) maxDelay))
          Noise.white


-- move to test suite ***
propSingle :: Interpolation.T Double Double -> [Bool]
propSingle ip =
   let maxDelay = (5::Int)
       xs = randomRs (-1,1) (mkStdGen 1037)
       ts = take 20 (randomRs (0, fromIntegral maxDelay) (mkStdGen 2330))
       pm0 = DelayST.modulated      ip maxDelay ts xs
       pm1 = DelayList.modulatedRev ip maxDelay ts xs
       pm2 = DelayList.modulated    ip maxDelay ts xs
       pm3 = DelayBlock.modulated   ip maxDelay ts xs
       approx x y = abs (x-y) < 1e-10
       -- equal as = and (zipWith (==) as (tail as))
       -- equal [pm0, pm1 {-, pm2-}]
   in  [pm0==pm1, pm2==pm3, and (zipWith approx pm1 pm2)]

{- |
The test for constant interpolation will fail,
due to different point of views in forward and backward interpolation.
-}
propAll :: [[Bool]]
propAll =
   map propSingle $
      Interpolation.constant :
      Interpolation.linear :
      Interpolation.cubic :
      []
