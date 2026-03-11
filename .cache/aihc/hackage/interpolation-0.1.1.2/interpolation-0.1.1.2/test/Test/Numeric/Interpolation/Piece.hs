-- Do not edit! Automatically created with doctest-extract from src/Numeric/Interpolation/Piece.hs
{-# LINE 10 "src/Numeric/Interpolation/Piece.hs" #-}

module Test.Numeric.Interpolation.Piece where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 11 "src/Numeric/Interpolation/Piece.hs" #-}
import     qualified Numeric.Interpolation.Piece as Piece
import     qualified Numeric.Interpolation.Private.Piece as PiecePriv
import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((==>))

forAllDistinctPoints     ::
       (Show a, QC.Arbitrary a, QC.Testable prop) =>
       ((Rational, a) -> (Rational, a) -> prop) -> QC.Property
forAllDistinctPoints     f =
       QC.forAll QC.arbitrary $ \p1@(x1,_) ->
       QC.forAll QC.arbitrary $ \p2@(x2,_) ->
          x1/=x2  ==>  f p1 p2

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.Interpolation.Piece:26: "
{-# LINE 26 "src/Numeric/Interpolation/Piece.hs" #-}
 DocTest.property
{-# LINE 26 "src/Numeric/Interpolation/Piece.hs" #-}
     (forAllDistinctPoints $ \p1 p2 x -> Piece.linear p1 p2 x == Piece.linear p2 p1 x)
 DocTest.printPrefix "Numeric.Interpolation.Piece:35: "
{-# LINE 35 "src/Numeric/Interpolation/Piece.hs" #-}
 DocTest.property
{-# LINE 35 "src/Numeric/Interpolation/Piece.hs" #-}
     (forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == Piece.hermite1 p2 p1 x)
 DocTest.printPrefix "Numeric.Interpolation.Piece:36: "
{-# LINE 36 "src/Numeric/Interpolation/Piece.hs" #-}
 DocTest.property
{-# LINE 36 "src/Numeric/Interpolation/Piece.hs" #-}
     (forAllDistinctPoints $ \p1@(x1,y1) p2@(x2,y2) x -> Piece.linear p1 p2 x == let slope = (y2-y1)/(x2-x1) in Piece.hermite1 (x1, (y1,slope)) (x2, (y2,slope)) x)
 DocTest.printPrefix "Numeric.Interpolation.Piece:37: "
{-# LINE 37 "src/Numeric/Interpolation/Piece.hs" #-}
 DocTest.property
{-# LINE 37 "src/Numeric/Interpolation/Piece.hs" #-}
     (forAllDistinctPoints $ \p1 p2 x -> Piece.hermite1 p1 p2 x == PiecePriv.hermite1 p1 p2 x)
