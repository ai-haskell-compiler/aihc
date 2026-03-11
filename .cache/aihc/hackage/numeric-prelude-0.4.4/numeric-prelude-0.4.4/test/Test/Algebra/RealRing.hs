-- Do not edit! Automatically created with doctest-extract from src/Algebra/RealRing.hs
{-# LINE 38 "src/Algebra/RealRing.hs" #-}

module Test.Algebra.RealRing where

import qualified Test.DocTest.Driver as DocTest

{-# LINE 39 "src/Algebra/RealRing.hs" #-}
import     qualified Algebra.RealRing as RealRing
import     Data.Tuple.HT (mapFst)
import     NumericPrelude.Numeric as NP
import     NumericPrelude.Base
import     Prelude ()

infix     4 =~=

(=~=)     :: (Eq b) => (a -> b) -> (a -> b) -> a -> Bool
(f     =~= g) x = f x == g x

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Algebra.RealRing:134: "
{-# LINE 134 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 134 "src/Algebra/RealRing.hs" #-}
         (\x -> (x::Rational) == (uncurry (+) $ mapFst fromInteger $ splitFraction x))
 DocTest.printPrefix "Algebra.RealRing:135: "
{-# LINE 135 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 135 "src/Algebra/RealRing.hs" #-}
         (\x -> uncurry (==) $ mapFst (((x::Double)-) . fromInteger) $ splitFraction x)
 DocTest.printPrefix "Algebra.RealRing:136: "
{-# LINE 136 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 136 "src/Algebra/RealRing.hs" #-}
         (\x -> uncurry (==) $ mapFst (((x::Rational)-) . fromInteger) $ splitFraction x)
 DocTest.printPrefix "Algebra.RealRing:137: "
{-# LINE 137 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 137 "src/Algebra/RealRing.hs" #-}
         (\x -> splitFraction x == (floor (x::Double) :: Integer, fraction x))
 DocTest.printPrefix "Algebra.RealRing:138: "
{-# LINE 138 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 138 "src/Algebra/RealRing.hs" #-}
         (\x -> splitFraction x == (floor (x::Rational) :: Integer, fraction x))
 DocTest.printPrefix "Algebra.RealRing:142: "
{-# LINE 142 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 142 "src/Algebra/RealRing.hs" #-}
         (\x -> let y = fraction (x::Double) in 0<=y && y<1)
 DocTest.printPrefix "Algebra.RealRing:143: "
{-# LINE 143 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 143 "src/Algebra/RealRing.hs" #-}
         (\x -> let y = fraction (x::Rational) in 0<=y && y<1)
 DocTest.printPrefix "Algebra.RealRing:147: "
{-# LINE 147 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 147 "src/Algebra/RealRing.hs" #-}
         (\x -> ceiling (-x) == negate (floor (x::Double) :: Integer))
 DocTest.printPrefix "Algebra.RealRing:148: "
{-# LINE 148 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 148 "src/Algebra/RealRing.hs" #-}
         (\x -> ceiling (-x) == negate (floor (x::Rational) :: Integer))
 DocTest.printPrefix "Algebra.RealRing:564: "
{-# LINE 564 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 564 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericFloor =~= (NP.floor :: Double -> Integer))
 DocTest.printPrefix "Algebra.RealRing:565: "
{-# LINE 565 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 565 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericFloor =~= (NP.floor :: Rational -> Integer))
 DocTest.printPrefix "Algebra.RealRing:574: "
{-# LINE 574 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 574 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericCeiling =~= (NP.ceiling :: Double -> Integer))
 DocTest.printPrefix "Algebra.RealRing:575: "
{-# LINE 575 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 575 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericCeiling =~= (NP.ceiling :: Rational -> Integer))
 DocTest.printPrefix "Algebra.RealRing:584: "
{-# LINE 584 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 584 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericTruncate =~= (NP.truncate :: Double -> Integer))
 DocTest.printPrefix "Algebra.RealRing:585: "
{-# LINE 585 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 585 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericTruncate =~= (NP.truncate :: Rational -> Integer))
 DocTest.printPrefix "Algebra.RealRing:594: "
{-# LINE 594 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 594 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericRound =~= (NP.round :: Double -> Integer))
 DocTest.printPrefix "Algebra.RealRing:595: "
{-# LINE 595 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 595 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericRound =~= (NP.round :: Rational -> Integer))
 DocTest.printPrefix "Algebra.RealRing:604: "
{-# LINE 604 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 604 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericFraction =~= (NP.fraction :: Double -> Double))
 DocTest.printPrefix "Algebra.RealRing:605: "
{-# LINE 605 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 605 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericFraction =~= (NP.fraction :: Rational -> Rational))
 DocTest.printPrefix "Algebra.RealRing:614: "
{-# LINE 614 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 614 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericSplitFraction =~= (NP.splitFraction :: Double -> (Integer,Double)))
 DocTest.printPrefix "Algebra.RealRing:615: "
{-# LINE 615 "src/Algebra/RealRing.hs" #-}
 DocTest.property
{-# LINE 615 "src/Algebra/RealRing.hs" #-}
     (RealRing.genericSplitFraction =~= (NP.splitFraction :: Rational -> (Integer,Rational)))
