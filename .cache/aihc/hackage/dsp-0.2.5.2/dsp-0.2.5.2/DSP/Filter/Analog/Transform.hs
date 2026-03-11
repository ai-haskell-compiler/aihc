-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Filter.Analog.Transform
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Analog prototype filter transforms
---
-- Reference: R&G, pg 258; P&M, pg 698
--
-----------------------------------------------------------------------------

module DSP.Filter.Analog.Transform (
   a_lp2lp, a_lp2hp, a_lp2bp, a_lp2bs,
   substitute, propSubstituteRecip, propSubstituteAlt,
  ) where

import Polynomial.Basic

-- Normalizes a filter

normalize :: ([Double],[Double]) -> ([Double],[Double])
normalize (num,den) = (num',den')
    where a0 = last den
          num' = map (/ a0) num
          den' = map (/ a0) den

-- | Lowpass to lowpass: @s --> s\/wc@

a_lp2lp :: Double -- ^ wc
        -> ([Double],[Double]) -- ^ (b,a)
        -> ([Double],[Double]) -- ^ (b',a')

a_lp2lp wu (num,den) = normalize (num',den')
    where num' = polysubst [ 0, 1/wu ] num
          den' = polysubst [ 0, 1/wu ] den

-- | Lowpass to highpass: @s --> wc\/s@

a_lp2hp :: Double -- ^ wc
        -> ([Double],[Double]) -- ^ (b,a)
        -> ([Double],[Double]) -- ^ (b',a')

a_lp2hp wu (num,den) = normalize (num',den')
    where nn   = length num
          nd   = length den
          n    = max nn nd
          num' = polysubst [ 0, 1/wu ] $ reverse $ num ++ replicate (n-nn) 0
          den' = polysubst [ 0, 1/wu ] $ reverse $ den ++ replicate (n-nd) 0


-- | Lowpass to bandpass: @s --> (s^2 + wl*wu) \/ (s(wu-wl))@

a_lp2bp :: Double -- ^ wl
        -> Double -- ^ wu
        -> ([Double],[Double]) -- ^ (b,a)
        -> ([Double],[Double]) -- ^ (b',a')

a_lp2bp wl wu = substitute ([ wl*wu, 0, 1 ], [ 0, wu-wl ])


-- | Lowpass to bandstop: @s --> (s(wu-wl)) \/ (s^2 + wl*wu)@

a_lp2bs :: Double -- ^ wl
        -> Double -- ^ wu
        -> ([Double],[Double]) -- ^ (b,a)
        -> ([Double],[Double]) -- ^ (b',a')

a_lp2bs wl wu = substitute ([ 0, wu-wl ], [ wu*wl, 0, 1 ])



substitute ::
   ([Double],[Double]) -> ([Double],[Double]) -> ([Double],[Double])
substitute (nsub,dsub) (num,den) = normalize (num',den')
    where num' = polyPolySubst nsub $ weightedPowers $ num
          den' = polyPolySubst nsub $ weightedPowers $ den
          weightedPowers = flip (zipWith polyscale) dsubPowers
          dsubPowers = reverse $ take m $ iterate (polymult dsub) [1]
          m = max (length num) (length den)

substituteAlt ::
   ([Double],[Double]) -> ([Double],[Double]) -> ([Double],[Double])
substituteAlt (nsub,dsub) (num,den) = normalize (num',den')
    where m    = max (length num - 1) (length den - 1)
          num' = step3 $ step2 (0::Int) $ step1 m $ num
          den' = step3 $ step2 (0::Int) $ step1 m $ den
          step1 _ []     = []
          step1 n (x:xs) = map (x*) (polypow dsub n) : step1 (n-1) xs
          step2 _ []     = []
          step2 n (x:xs) = polymult (polypow nsub n) x : step2 (n+1) xs
          step3 x = foldr polyadd [0] x


propSubstituteRecip :: ([Double],[Double]) -> ([Double],[Double]) -> Bool
propSubstituteRecip (nsub,dsub) (num,den) =
   let (x,y) =  substitute (nsub,dsub) (num,den)
   in  (y,x) == substitute (dsub,nsub) (den,num)


propSubstituteAlt :: ([Double],[Double]) -> ([Double],[Double]) -> Bool
propSubstituteAlt q p   =   substitute q p == substituteAlt q p
