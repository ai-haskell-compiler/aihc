module Combinatorics.BellNumbers where

import Combinatorics (binomials, )
import Combinatorics.Utility (scalarProduct, )
import qualified PowerSeries


{- $setup
>>> import qualified Combinatorics.BellNumbers as Bell
>>> import Test.Utility (equalFuncList)
-}


{- |
List of Bell numbers computed with the recursive formula
given in Wurzel 2004-06, page 136
-}
bellRec :: Num a => [a]
bellRec =
   1 : map (scalarProduct bellRec) binomials

{- |
prop> equalFuncList (\k -> round (Bell.bellSeries (fromInteger k) :: Double)) (Bell.bellRec :: [Integer]) 20
-}
bellSeries :: (Floating a, Enum a) => Int -> a
bellSeries n =
   scalarProduct
      (map (^n) [0..])
      (take 30 PowerSeries.derivativeCoefficients)
     / exp 1
