-----------------------------------------------------------------------------
-- |
-- Module      :  Polynomial.Roots
-- Copyright   :  (c) 1998 Numeric Quest Inc., All rights reserved
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Root finder using Laguerre's method
--
-----------------------------------------------------------------------------

-- This file was sucked out of the Wayback Machine at www.archive.org.
-- This was orginally a HTML files containing literate Haskell. It has
-- been modified to use the Polynomial library, and Haddock style comments
-- have been added.  As much as the original formatting has been retained
-- as possible. --mpd

-- Original comments are below

{-
	Literate Haskell module <i>Roots.lhs</i>

	Jan Skibinski, <a href="http://www.numeric-quest.com/news/">
	Numeric Quest Inc.</a>, Huntsville, Ontario, Canada

	1998.09.05, last modified 1998.09.24

	This module implements <i>Laguerre's</i> method for finding complex
	roots of polynomials. According to [1], it <i> is by far the most
	straightforward of these sure-fire methods. It does require that you
	perform complex arithmetic (even while converging to real roots), but
	it is guaranteed to converge to a root from any starting point. In
	some instances the complex arithmetic is no disadvantage, since the
	polynomial itself may have complex coefficients. </i>

	[1] Numerical Recipes in Pascal, W.H. Press, B.P. Flannery,
	S.A. Teukolsky, W.T. Vetterling, Cambridge University Press,
	ISBN 0-521-37516-9

	See also some other variations of the same book by the same authors:
	Numerical Recipes in C, Fortran, etc. I just happen to own [1], although
	I have never programmed in Pascal. :-)	

	Example

	To solve the equation

	x^2 - 3 x + 2 = 0

	form the list of coefficients [2, -3, 1] (notice the reverse
	order of coefficients) and execute

	roots 1.0e-6 300 [2,-3, 1]
	-- where
	--     1.0e-6 is a required accuracy
	--     300 is a count of permitted iterations
	--     (You set it to small number just in case you
	--	do not trust the algorithm. But if you do,
	--	then set it to something big, say 300)

	The answer is [2.0 :+ 0.0, 1.0 :+ 0.0]; that is, both roots are
	real and equal to 2 and 1:

	x^2 - 3 x + 2 = (x - 2) (x - 1) = 0
-}

module Polynomial.Roots (roots) where

import Data.Complex

import Polynomial.Basic

-- * Functions

-- | Root finder using Laguerre's method

roots :: RealFloat a => a           -- ^ epsilon
                     -> Int         -- ^ iteration limit
                     -> [Complex a] -- ^ the polynomial
                     -> [Complex a] -- ^ the roots
roots eps0 count0 as0 =
	--
	-- List of complex roots of a polynomial
	-- a0 + a1*x + a2*x^2...
	-- represented by the list as=[a0,a1,a2...]
	-- where
	--     eps is a desired accuracy
	--     count is a maximum count of iterations allowed
	-- Require: list 'as' must have at least two elements
	--     and the last element must not be zero
	roots' eps0 count0 as0 []
	where
	    roots' eps count as xs
	        | length as <= 2  = x:xs
	        | otherwise       =
                 roots' eps count (deflate x bs [last as]) (x:xs)
	        where
	            x  = laguerre eps count as 0
	            bs = drop 1 (reverse (drop 1 as))
	            deflate z bs' cs
	                | bs' == []  = cs
		        | otherwise  =
                         deflate z (tail bs') (((head bs')+z*(head cs)):cs)


laguerre :: RealFloat a => a -> Int -> [Complex a] -> Complex a -> Complex a
laguerre eps0 count as0 x0
	--
	-- One of the roots of the polynomial 'as',
	-- where
	--    eps is a desired accuracy
	--    count is a maximum count of iterations allowed
	--    x is initial guess of the root
	-- This method is due to Laguerre.
	--
	| count <= 0	              = x0
	| magnitude (x0 - x0') < eps0 = x0'
	| otherwise                   = laguerre eps0 (count - 1) as0 x0'
	where x0'    = laguerre2 eps0 as0 as0' as0'' x0
	      as0'   = polyderiv as0
	      as0''  = polyderiv as0'
	      laguerre2 eps as as' as'' x
	        -- One iteration step
	        | magnitude b < eps           = x
	        | magnitude gp < magnitude gm =
		    if gm == 0 then x - 1 else x - n/gm
	        | otherwise                   =
		    if gp == 0 then x - 1 else x - n/gp
	        where gp    = g + delta
		      gm    = g - delta
		      g     = d/b
		      delta = sqrt ((n-1)*(n*h - g2))
		      h     = g2 - f/b
		      b     = polyeval as x
		      d     = polyeval as' x
		      f     = polyeval as'' x
		      g2    = g^(2::Int)
		      n     = fromIntegral (length as)

-- Original Copyright Notice

-----------------------------------------------------------------------------
--
-- Copyright:
--
--	(C) 1998 Numeric Quest Inc., All rights reserved
--
-- Email:
--
--      jans@numeric-quest.com
--
-- License:
--
--	GNU General Public License, GPL
--
-----------------------------------------------------------------------------
