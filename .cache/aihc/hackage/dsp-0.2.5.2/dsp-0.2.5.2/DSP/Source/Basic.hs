-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Source.Basic
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic signals
--
-----------------------------------------------------------------------------

module DSP.Source.Basic where

-- | all zeros

zeros :: (Num a) => [a]
zeros = repeat 0

-- | single impulse

impulse :: (Num a) => [a]
impulse = 1 : zeros

-- | unit step

step :: (Num a) => [a]
step = repeat 1

-- | ramp

ramp :: (Num a) => [a]
ramp = iterate (1+) 0
