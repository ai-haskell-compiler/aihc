module Roots where

import Data.Complex
import Data.List(genericLength)

roots :: RealFloat a => a -> Int -> [Complex a] -> [Complex a]
roots eps count as =
      --
      -- List of complex roots of a polynomial
      -- a0 + a1*x + a2*x^2...
      -- represented by the list as=[a0,a1,a2...]
      -- where
      --     eps is a desired accuracy
      --     count is a maximum count of iterations allowed
      -- Require: list 'as' must have at least two elements
      --     and the last element must not be zero
      roots' eps count as []
      where
          roots' epr cnt cs xs
              | length cs <= 2  = x:xs
              | otherwise       =
                  roots' epr cnt (deflate x bs [last cs]) (x:xs)
              where
                  x  = laguerre epr cnt as 0
                  bs = drop 1 $ reverse $ drop 1 cs
                  deflate z es fs
                      | es == []  = fs
                      | otherwise =
                          deflate z (tail fs) (((head fs)+z*(head es)):es)


laguerre :: RealFloat a => a -> Int -> [Complex a] -> Complex a -> Complex a
laguerre eps count as x
      --
      -- One of the roots of the polynomial 'as',
      -- where
      --    eps is a desired accuracy
      --    count is a maximum count of iterations allowed
      --    x is initial guess of the root
      -- This method is due to Laguerre.
      --
      | count <= 0               = x
      | magnitude (x - x') < eps = x'
      | otherwise                = laguerre eps (count - 1) as x'
      where
          x'     = laguerre2 eps as as' as'' x
          as'    = polynomial_derivative as
          as''   = polynomial_derivative as'
          laguerre2 epr bs bs' bs'' y
              -- One iteration step
              | magnitude b < epr           = y
              | magnitude gp < magnitude gm =
                  if gm == 0 then y - 1 else y - n/gm
              | otherwise                   =
                  if gp == 0 then y - 1 else y - n/gp
              where
                  gp    = g + delta
                  gm    = g - delta
                  g     = d/b
                  delta = sqrt ((n-1)*(n*h - g2))
                  h     = g2 - f/b
                  b     = polynomial_value bs y
                  d     = polynomial_value bs' y
                  f     = polynomial_value bs'' y
                  g2    = g^(2::Int)
                  n     = genericLength bs

polynomial_value :: Num a => [a] -> a -> a
polynomial_value as x =
      --
      -- Value of polynomial a0 + a1 x  + a2 x^2 ...
      -- evaluated for 'x',
      -- where 'as' is a list [a0,a1,a2...]
      --
      foldr (u x) 0 as
      where
          u y a b = a + b*y

polynomial_derivative :: Num a => [a] -> [a]
polynomial_derivative as =
      --
      -- List of coefficients for derivative of polynomial
      -- a0 + a1 x + a2 x^2 ...
      --
      zipWith (*) (iterate (1+) 1) (drop 1 as)

-----------------------------------------------------------------------------
--
-- Copyright:
--
--      (C) 1998 Numeric Quest Inc., All rights reserved
--
-- Email:
--
--      jans@numeric-quest.com
--
-- License:
--
--      GNU General Public License, GPL
--
-----------------------------------------------------------------------------
