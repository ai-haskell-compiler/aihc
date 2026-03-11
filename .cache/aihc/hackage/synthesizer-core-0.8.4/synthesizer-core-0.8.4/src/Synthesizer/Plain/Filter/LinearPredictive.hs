module Synthesizer.Plain.Filter.LinearPredictive where

import Synthesizer.Plain.Analysis (scalarProduct)

import qualified Data.List.Match as ListMatch
import qualified Data.List as List

import qualified Algebra.Field    as Field

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- |
Determine optimal filter coefficients and residue by adaptive approximation.
The number of initial filter coefficients is used as filter order.
-}
approxCoefficients :: Field.C a =>
   a -> [a] -> [a] -> [(a,[a])]
approxCoefficients k mask0 xs =
   let infixes = map (ListMatch.take mask0) (List.tails xs)
       targets = ListMatch.drop mask0 xs
   in  scanl
          (\(_,mask) (infx,target) ->
              let residue = target - scalarProduct mask infx
                  norm2 = scalarProduct infx infx
              in  (residue,
                   mask + map ((k*residue/norm2)*) infx))
          (zero,mask0) (zip infixes targets)
{-
mapM print $ take 20 $ drop 2000 $ approxCoefficients (1::Double) [0,0,0,0.1] (iterate (1+) 100)


mapM print $ take 20 $ drop 10000 $ approxCoefficients (0.2::Double) [0.1,0] (map sin (iterate (0.03+) 0))

must yield coefficients [-1, 2*cos(0.03::Double)]
-}
