module Numeric.Interpolation.Piece (
   Piece.T,
   linear,
   hermite1,
   ) where

import qualified Numeric.Interpolation.Private.Piece as Piece


{- $setup
>>> import qualified Numeric.Interpolation.Piece as Piece
>>> import qualified Numeric.Interpolation.Private.Piece as PiecePriv
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>>
>>> forAllDistinctPoints ::
>>>    (Show a, QC.Arbitrary a, QC.Testable prop) =>
>>>    ((Rational, a) -> (Rational, a) -> prop) -> QC.Property
>>> forAllDistinctPoints f =
>>>    QC.forAll QC.arbitrary $ \p1@(x1,_) ->
>>>    QC.forAll QC.arbitrary $ \p2@(x2,_) ->
>>>       x1/=x2  ==>  f p1 p2
-}

{- |
prop> forAllDistinctPoints $ \p1 p2 x -> Piece.linear p1 p2 x == Piece.linear p2 p1 x
-}
linear :: (Fractional a) => Piece.T a a a
linear = Piece.linear

{- |
Hermite interpolation with one derivative per node.
That is, the interpolating polynomial is cubic.

prop> forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == Piece.hermite1 p2 p1 x
prop> forAllDistinctPoints $ \p1@(x1,y1) p2@(x2,y2) x -> Piece.linear p1 p2 x == let slope = (y2-y1)/(x2-x1) in Piece.hermite1 (x1, (y1,slope)) (x2, (y2,slope)) x
prop> forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == PiecePriv.hermite1 p1 p2 x
-}
hermite1 :: (Fractional a) => Piece.T a a (a, a)
hermite1 (x0,(y0,dy0)) (x1,(y1,dy1)) x =
   let d = (y1-y0) / dx10
       dx0 = x-x0
       dx1 = x1-x
       dx10 = x1-x0
   in  (y0*dx1 + y1*dx0 +
        ((dy0-d) * dx1 - (dy1-d) * dx0) * dx0 * dx1 / dx10)
          / dx10
