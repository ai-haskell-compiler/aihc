{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{- |
Copyright   :  (c) Henning Thielemann 2008
License     :  GPL

Maintainer  :  synthesizer@henning-thielemann.de
Stability   :  provisional
Portability :  requires multi-parameter type classes
-}
module Synthesizer.Plain.Filter.Recursive.AllpassPoly where

import qualified Algebra.Module                as Module
import qualified Algebra.RealTranscendental    as RealTrans
import qualified Algebra.Transcendental        as Trans
import qualified Algebra.Field                 as Field
import qualified Algebra.ZeroTestable          as ZeroTestable

import Number.Complex (cis,(+:),real,imag)
import qualified Number.Complex as Complex
import Orthogonals(Scalar,one_ket_solution)

import qualified Prelude as P
import NumericPrelude.Numeric
import NumericPrelude.Base



newtype Parameter a = Parameter [a]
   deriving Show

{- | Compute coefficients for an allpass that shifts low frequencies
     by approximately the shift you want.
     To achieve this we solve a linear least squares problem,
     where low frequencies are more weighted than high ones.
     The output is a list of coefficients for an arbitrary order allpass. -}
shiftParam :: (Scalar a, P.Fractional a, Trans.C a) =>
   Int -> a -> a -> Parameter a
shiftParam order weight phase =
    let {- construct matrix for normal equations -}
        normalVector = map negate
           (map (scalarProdScrewExp weight order phase 0) [1..order])
        normalMatrix = map (\j ->
            map (scalarProdScrewExp weight order phase j) [1..order]) [1..order]
    in  Parameter (one_ket_solution normalMatrix normalVector)

{-
  GNUPlot.plotFunc (GNUPlot.linearScale 500 (0,1)) ((fwrap (-pi,pi)).(makePhase (shiftParam 6 (-6) (-pi/2::Double))))
-}
makePhase :: (RealTrans.C a, ZeroTestable.C a) => Parameter a -> a -> a
makePhase (Parameter ks) frequency =
    let omega  = 2*pi * frequency
        omegas = iterate (omega+) omega
        denom = 1+sum (zipWith (\k w -> k*cos w +: k*sin w) ks omegas)
    in  2 * Complex.phase denom - omega*(fromIntegral (length ks))

{- integrate (0,2*pi) (\omega -> exp (r*omega) * screwProd order phase k j omega) -}
scalarProdScrewExp :: Trans.C a => a -> Int -> a -> Int -> Int -> a
scalarProdScrewExp r order phase k j =
    let (intCos,intSin) = integrateScrewExp r (k+j-order)
    in  2 * (fst (integrateScrewExp r (k-j)) -
              (cos phase * intCos + sin phase * intSin))

screwProd :: Trans.C a => Int -> a -> Int -> Int -> a -> a
screwProd order phase k j omega =
    let z0 = cis (fromIntegral k * omega) -
                       cis phase * cis (fromIntegral (order-k) * omega)
        z1 = cis (fromIntegral j * omega) -
                       cis phase * cis (fromIntegral (order-j) * omega)
    in  real z0 * real z1 + imag z0 * imag z1

{- integrate (0,2*pi) (\omega -> (exp (r*omega) +: 0) * cis (k*omega)) -}
integrateScrewExp :: Trans.C a => a -> Int -> (a,a)
integrateScrewExp r kInt =
    let k = fromIntegral kInt
        q = (exp (2*pi*r) - 1) / (r^2 + k^2)
    in  (r*q, -k*q)

{- Should be moved to NumericPrelude.Numeric -}
integrateNum :: (Field.C a, Module.C a v) => Int -> (a,a) -> (a->v) -> v
integrateNum n (lo,hi) f =
    let xs = map (\k -> lo + (hi-lo) * fromIntegral k / fromIntegral n)
                 [1..(n-1)]
    in  ((hi-lo) / fromIntegral n) *>
        (foldl (+) ((1/2 `asTypeOf` lo) *> (f lo + f hi))
               (map f xs))
