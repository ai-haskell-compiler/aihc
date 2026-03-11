{- |
Interpolation basis functions represented with a minimum of required nodes.
-}
module Numeric.Interpolation.Basis.Compact (
   linear, hermite1, cubicLinear, cubicParabola,
   ) where

import qualified Numeric.Interpolation.NodeList as Nodes
import Numeric.Interpolation.Private.Basis (
   parabolaBasisDerivativeRight,
   parabolaBasisDerivativeCenter,
   parabolaBasisDerivativeLeft,
   )
import Numeric.Interpolation.Private.List (
   mapAdjacentMaybe3,
   mapAdjacentMaybe5,
   )

import Control.Monad (liftM, liftM2)

import Data.Maybe (catMaybes)


generic :: ny -> ny -> [x] -> [Nodes.T x ny]
generic nz ny =
   mapAdjacentMaybe3
      (\l n r ->
          Nodes.Node (n,ny)
             (maybe Nodes.Interval (flip Nodes.singleton nz) l)
             (maybe Nodes.Interval (flip Nodes.singleton nz) r))


linear :: (Num b) => [a] -> [Nodes.T a b]
linear = generic 0 1

hermite1 :: (Num b) => [a] -> [Nodes.T a (b, b)]
hermite1 xs =
   concat $
   zipWith (\f df -> [f,df])
      (generic (0,0) (1,0) xs)
      (generic (0,0) (0,1) xs)




cubicAutoGeneric ::
   (Num b) =>
   (a -> a -> a -> b) ->
   (a -> a -> a -> b) ->
   (a -> a -> a -> b) ->
   [a] -> [Nodes.T a (b, b)]
cubicAutoGeneric dl dn dr =
   mapAdjacentMaybe5
      (\ml2 ml1 n mr1 mr2 ->
         let node x y y' = (x, (y,y'))
         in  Nodes.fromList $ catMaybes $
             liftM (\l2 -> node l2 0 0) ml2 :
             liftM2 (\l2 l1 -> node l1 0 (dl l2 l1 n)) ml2 ml1 :
             liftM2 (\l1 r1 -> node n  1 (dn l1 n r1)) ml1 mr1 :
             liftM2 (\r1 r2 -> node r1 0 (dr n r1 r2)) mr1 mr2 :
             liftM (\r2 -> node r2 0 0) mr2 :
             [])


{- |
Cubic interpolation
where the derivative at a node is set to the slope of the two adjacent nodes.
-}
cubicLinear :: (Fractional a) => [a] -> [Nodes.T a (a, a)]
cubicLinear =
   cubicAutoGeneric
      (\ll _l n -> recip $ n-ll)
      (\_l _n _r -> 0)
      (\n _r rr -> recip $ n-rr)


{- |
Cubic interpolation
where the derivative at a node is set to the slope of the parabola
through the current and the two adjacent nodes.
-}
cubicParabola :: (Fractional a) => [a] -> [Nodes.T a (a, a)]
cubicParabola =
   cubicAutoGeneric
      parabolaBasisDerivativeRight
      parabolaBasisDerivativeCenter
      parabolaBasisDerivativeLeft


{- |
Experimental interpolation
which is mean of 'cubicLinear' and 'cubicParabola'.
The result looks reasonable, too.
-}
_cubicMean :: (Fractional a) => [a] -> [Nodes.T a (a, a)]
_cubicMean =
   cubicAutoGeneric
      (\ll l n -> (parabolaBasisDerivativeRight ll l n + recip (n-ll))/2)
      (\l n r -> parabolaBasisDerivativeCenter l n r / 2)
      (\n r rr -> (parabolaBasisDerivativeLeft n r rr + recip (n-rr))/2)
