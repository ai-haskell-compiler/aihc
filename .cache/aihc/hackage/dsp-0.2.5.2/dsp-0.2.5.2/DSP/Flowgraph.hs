-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Flowgraph
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Flowgraph functions
--
-- DO NOT USE YET
--
-----------------------------------------------------------------------------

module DSP.Flowgraph where

-----------------------------------------------------------------------------

-- | Cascade of functions, eg
--
-- @cascade [ f1, f2, f3 ] x == (f3 . f2 . f1) x@

cascade :: Num a => [[a] -> [a]] -- ^ [f_n(x)]
	-> [a] -- ^ x[n]
	-> [a] -- ^ y[n]

cascade []     = id
cascade (f:fs) = cascade fs . f

-----------------------------------------------------------------------------

-- | Gain node
--
-- @y[n] = a * x[n]@

gain :: Num a => a -- ^ a
     -> [a] -- ^ x[n]
     -> [a] -- ^ y[n]

gain x = map (x*)

-----------------------------------------------------------------------------

-- | Bias node
--
-- @y[n] = x[n] + a@

bias :: Num a => a -- ^ a
     -> [a] -- ^ x[n]
     -> [a] -- ^ y[n]

bias x = map (x+)

-----------------------------------------------------------------------------

-- | Adder node
--
-- @z[n] = x[n] + y[n]@

adder :: Num a => [a] -- ^ x[n]
      -> [a] -- ^ y[n]
      -> [a] -- ^ z[n]

adder = zipWith (+)
