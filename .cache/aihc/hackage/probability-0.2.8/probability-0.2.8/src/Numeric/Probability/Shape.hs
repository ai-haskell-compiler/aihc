{- |
Collection of some shapes of distribution.
-}
module Numeric.Probability.Shape where

{- |
A shape is a mapping from the interval @[0,1]@ to non-negative numbers.
They need not to be normalized (sum up to 1)
because this is done by subsequent steps.
(It would also be impossible to normalize the function in a way
that each discretization is normalized as well.)
-}
type T prob = prob -> prob


linear :: Fractional prob => T prob
linear = id

uniform :: Fractional prob => T prob
uniform = const 1

negExp :: Floating prob => T prob
negExp x = exp (-x)

normal :: Floating prob => T prob
normal = normalCurve 0.5 0.5

normalCurve :: Floating prob =>
   prob -> prob -> prob -> prob
normalCurve mean dev x =
   let u = (x - mean) / dev
   in  exp (-1/2 * u^(2::Int)) / sqrt (2 * pi)
