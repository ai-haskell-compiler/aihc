-----------------------------------------------------------------------------
-- |
-- Module      :  Polynomial.Basic
-- Copyright   :  (c) Matthew Donadio 2002
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Simple module for handling polynomials.
--
-----------------------------------------------------------------------------

-- TODO: We should really create a datatype for polynomials...

-- TODO: Should polydiv return the quotient and the remainder as a tuple?

module Polynomial.Basic where

-- * Types

-- | Polynomials are lists of numbers:
-- [ a0, a1, ... , an ] == an*x^n + ... + a1*x + a0
-- and negative exponents are currently verboten.

-- * Functions

-- | Evaluate a polynomial using Horner's method.

polyeval :: Num a => [a] -> a -> a
polyeval []     _ = 0
polyeval (p:ps) x = p + x * polyeval ps x

-- | Add two polynomials

polyadd :: Num a => [a] -> [a] -> [a]
polyadd [] ys          = ys
polyadd xs []          = xs
polyadd (x:xs) (y:ys)  = (x+y) : polyadd xs ys

polyAddScalar :: Num a => a -> [a] -> [a]
polyAddScalar c [] = [c]
polyAddScalar c (x:xs) = (c+x):xs

-- | Subtract two polynomials

polysub :: Num a => [a] -> [a] -> [a]
polysub [] ys          = map negate ys
polysub xs []          = xs
polysub (x:xs) (y:ys)  = (x-y) : polysub xs ys

-- | Scale a polynomial

polyscale :: Num a => a -> [a] -> [a]
polyscale a x = map (a*) x

-- | Multiply two polynomials

polymult :: Num a => [a] -> [a] -> [a]
polymult ys =
   foldr (\x acc -> polyadd (polyscale x ys) (0 : acc)) []

polymultAlt :: Num a => [a] -> [a] -> [a]
polymultAlt _  []     = []
polymultAlt ys (x:xs) = polyadd (polyscale x ys) (0 : polymult ys xs)

-- | Divide two polynomials

polydiv :: Fractional a => [a] -> [a] -> [a]
polydiv x0 y0 = reverse $ polydiv' (reverse x0) (reverse y0)
    where polydiv' (x:xs) y
             | length (x:xs) < length y = []
             | otherwise = z : (polydiv' (tail (polysub (x:xs) (polymult [z] y))) y)
                where z = x / head y
          polydiv' [] _ = []

-- | Modulus of two polynomials (remainder of division)

polymod :: Fractional a => [a] -> [a] -> [a]
polymod x0 y0 = reverse $ polymod' (reverse x0) (reverse y0)
    where polymod' (x:xs) y
             | length (x:xs) < length y = (x:xs)
             | otherwise = polymod' (tail (polysub (x:xs) (polymult [z] y))) y
                where z = x / head y
          polymod' [] _ = []

-- | Raise a polynomial to a non-negative integer power

polypow :: (Num a, Integral b) => [a] -> b -> [a]
polypow _ 0 = [ 1 ]
polypow x 1 = x
polypow x n | even n    = xSqr
            | otherwise = polymult x xSqr
    where xSqr = polymult x2 x2
          x2   = polypow x (n `div` 2)

-- | Polynomial substitution y(n) = x(w(n))

polysubst :: Num a => [a] -> [a] -> [a]
polysubst ws = foldr (\x -> polyAddScalar x . polymult ws) []

polysubstAlt :: Num a => [a] -> [a] -> [a]
polysubstAlt w0 x0 = foldr polyadd [0] (polysubst' 0 w0 x0)
    where polysubst' _ _ []     = []
          polysubst' n w (x:xs) = polyscale x (polypow w (n::Int)) : polysubst' (n+1) w xs

{- |
Polynomial substitution @y(n) = x(w(n))@
where the coefficients of @x@ are also polynomials.
-}
polyPolySubst :: Num a => [a] -> [[a]] -> [a]
polyPolySubst ws = foldr (\x -> polyadd x . polymult ws) []

-- | Polynomial derivative

polyderiv :: Num a => [a] -> [a]
polyderiv [] = []
polyderiv (_:xs0) = polyderiv' 1 xs0
    where polyderiv' _ []     = []
          polyderiv' n (x:xs) = n * x : polyderiv' (n+1) xs

-- | Polynomial integration

polyinteg :: Fractional a => [a] -> a -> [a]
polyinteg x0 c = c : polyinteg' 1 x0
    where polyinteg' _ []     = []
          polyinteg' n (x:xs) = x / n : polyinteg' (n+1) xs

-- | Convert roots to a polynomial

roots2poly :: Num a => [a] -> [a]
roots2poly []     = [1]
roots2poly (r:rs) = polymult [-r, 1] (roots2poly rs)
