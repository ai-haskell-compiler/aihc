-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Estimation.Spectral.MA
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- This module contains one algorithm for MA parameter estimation.  It
-- is taken from Steven M. Kay, _Modern Spectral Estimation: Theory and
-- Application_, which is one of the standard texts on the subject.  When
-- possible, variable conventions are the same in the code as they are
-- found in the text.
--
-----------------------------------------------------------------------------


module DSP.Estimation.Spectral.MA (ma_durbin) where

import Data.Array
import Data.Complex

import DSP.Estimation.Spectral.AR

-- * Functions

-------------------------------------------------------------------------------
-- ma_durbin x q l
-------------------------------------------------------------------------------

-- Section 8.4 in Kay

-- | Computes an MA(q) model estimate from x using the Durbin's method
-- where l is the order of the AR process used in the algorithm

ma_durbin :: (Ix a, Integral a, RealFloat b) => Array a (Complex b)  -- ^ x
          -> a                        -- ^ q
          -> a                        -- ^ l
          -> (Array a (Complex b), b) -- ^ (a,rho)

ma_durbin x q l = (b, sig2)
    where (b,_)       = ar_yw a' q
 	  a'          = array (0,l) ((0,1) : [ (i, a''!i) | i <- [1..l] ])
          (a'', sig2) = ar_yw x l
