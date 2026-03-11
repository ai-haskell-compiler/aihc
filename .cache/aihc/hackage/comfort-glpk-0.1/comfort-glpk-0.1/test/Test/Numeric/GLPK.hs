-- Do not edit! Automatically created with doctest-extract from src/Numeric/GLPK.hs
{-# LINE 67 "src/Numeric/GLPK.hs" #-}

module Test.Numeric.GLPK where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 68 "src/Numeric/GLPK.hs" #-}
import     qualified Numeric.LinearProgramming.Test as TestLP
import     qualified Numeric.GLPK as LP
import     Numeric.GLPK ((.*), (<=.), (==.))

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape

import     Data.Tuple.HT (mapPair, mapSnd)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((===), (.&&.), (.||.))

type     X = Shape.Element
type     PairShape = Shape.NestedTuple Shape.TupleIndex (X,X)
type     TripletShape = Shape.NestedTuple Shape.TupleIndex (X,X,X)

pairShape     :: PairShape
pairShape     = Shape.static

tripletShape     :: TripletShape
tripletShape     = Shape.static

round3     :: Double -> Double
round3     x = fromInteger (round (1000*x)) / 1000

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.GLPK:99: "
{-# LINE 99 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 99 "src/Numeric/GLPK.hs" #-}
     (\target -> case Shape.indexTupleFromShape pairShape of (pos,neg) -> case mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[1.*pos, (-1).*neg] ==. target] (LP.Minimize, Array.fromTuple (1,1) :: Array.Array PairShape Double) of (Right (LP.Optimal,(absol,(posResult,negResult)))) -> QC.property (TestLP.approxReal 0.001 absol (abs target)) .&&. (posResult === 0 .||. negResult === 0); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:100: "
{-# LINE 100 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 100 "src/Numeric/GLPK.hs" #-}
     (\(QC.Positive posWeight) (QC.Positive negWeight) target -> case Shape.indexTupleFromShape pairShape of (pos,neg) -> case mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[1.*pos, (-1).*neg] ==. target] (LP.Minimize, Array.fromTuple (posWeight,negWeight) :: Array.Array PairShape Double) of (Right (LP.Optimal,(absol,(posResult,negResult)))) -> QC.property (absol>=0) .&&. (posResult === 0 .||. negResult === 0); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:101: "
{-# LINE 101 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 101 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, _) -> True; _ -> False)
 DocTest.printPrefix "Numeric.GLPK:102: "
{-# LINE 102 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 102 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> TestLP.checkFeasibility 0.1 bounds constrs sol; _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:103: "
{-# LINE 103 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 103 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> TestLP.checkFeasibility 0.1 bounds constrs $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:104: "
{-# LINE 104 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 104 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (opt,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> let val = TestLP.scalarProduct obj $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin) in (case dir of LP.Minimize -> opt-0.01 <= val; LP.Maximize -> opt+0.01 >= val); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:96: "
{-# LINE 96 "src/Numeric/GLPK.hs" #-}
 DocTest.example
{-# LINE 96 "src/Numeric/GLPK.hs" #-}
   (case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double))
  [ExpectedLine [LineChunk "Right (Optimal,(28.0,(5.0,0.0,4.0)))"]]
 DocTest.printPrefix "Numeric.GLPK:117: "
{-# LINE 117 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 117 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (LP.simplex bounds constrs (dir,obj), LP.exact bounds constrs (dir,obj)) of (Right (LP.Optimal, (optSimplex,_)), Right (LP.Optimal, (optExact,_))) -> TestLP.approx "optimum" 0.001 optSimplex optExact; _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:118: "
{-# LINE 118 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 118 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> TestLP.checkFeasibility 0.1 bounds constrs sol; _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:119: "
{-# LINE 119 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 119 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> TestLP.checkFeasibility 0.01 bounds constrs $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:120: "
{-# LINE 120 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 120 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (opt,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> let val = TestLP.scalarProduct obj $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin) in (case dir of LP.Minimize -> opt-0.01 <= val; LP.Maximize -> opt+0.01 >= val); _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:114: "
{-# LINE 114 "src/Numeric/GLPK.hs" #-}
 DocTest.example
{-# LINE 114 "src/Numeric/GLPK.hs" #-}
   (case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapSnd Array.toTuple) <$> LP.exact [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double))
  [ExpectedLine [LineChunk "Right (Optimal,(28.0,(5.0,0.0,4.0)))"]]
 DocTest.printPrefix "Numeric.GLPK:147: "
{-# LINE 147 "src/Numeric/GLPK.hs" #-}
 DocTest.property
{-# LINE 147 "src/Numeric/GLPK.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (LP.simplex bounds constrs (dir,obj), LP.interior bounds constrs (dir,obj)) of (Right (LP.Optimal, (optSimplex,_)), Right (LP.Optimal, (optExact,_))) -> TestLP.approx "optimum" 0.001 optSimplex optExact; _ -> QC.property False)
 DocTest.printPrefix "Numeric.GLPK:144: "
{-# LINE 144 "src/Numeric/GLPK.hs" #-}
 DocTest.example
{-# LINE 144 "src/Numeric/GLPK.hs" #-}
   (case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapPair (round3, Array.toTuple . Array.map round3)) <$> LP.interior [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double))
  [ExpectedLine [LineChunk "Right (Optimal,(28.0,(5.0,0.0,4.0)))"]]
