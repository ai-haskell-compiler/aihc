-- Do not edit! Automatically created with doctest-extract from src/Numeric/COINOR/CLP.hs
{-# LINE 46 "src/Numeric/COINOR/CLP.hs" #-}

module Test.Numeric.COINOR.CLP where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 47 "src/Numeric/COINOR/CLP.hs" #-}
import     qualified Numeric.COINOR.CLP as LP
import     qualified Numeric.LinearProgramming.Test as TestLP
import     Numeric.COINOR.CLP
       (PlusMinusOne(..), (.*), (==.), (<=.), (>=.), (>=<.))

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape

import     Data.Either.HT (mapRight)
import     Data.Tuple.HT (mapSnd)

import     qualified Test.QuickCheck as QC
import     Test.QuickCheck ((===), (.&&.), (.||.))

type     X = Shape.Element
type     PairShape = Shape.NestedTuple Shape.TupleIndex (X,X)
type     TripletShape = Shape.NestedTuple Shape.TupleIndex (X,X,X)

pairShape     :: PairShape
pairShape     = Shape.static

tripletShape     :: TripletShape
tripletShape     = Shape.static

approxReal     :: (Ord a, Num a) => a -> a -> a -> Bool
approxReal     tol x y = abs (x-y) <= tol

genMethod     :: QC.Gen (String, LP.Method)
genMethod     = QC.elements $
       ("dual", LP.dual) :
       ("primal", LP.primal) :
       ("initialSolve", LP.initialSolve) :
       ("initialDualSolve", LP.initialDualSolve) :
       ("initialPrimalSolve", LP.initialPrimalSolve) :
       ("initialBarrierSolve", LP.initialBarrierSolve) :
       -- let tests fail
       -- ("initialBarrierNoCrossSolve", LP.initialBarrierNoCrossSolve) :
       []

forAllMethod     ::
       (QC.Testable prop) => (LP.Method -> prop) -> QC.Property
forAllMethod     prop = QC.forAllShow genMethod fst (prop . snd)

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.COINOR.CLP:172: "
{-# LINE 172 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.example(
{-# LINE 172 "src/Numeric/COINOR/CLP.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.dual []
            [[2.*x, 1.*y] <=. 10, [1.*y, (5::Double).*z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
  )
  [ExpectedLine [LineChunk "Right (28.0,(5.0,0.0,4.0))"]]
 DocTest.printPrefix "Numeric.COINOR.CLP:183: "
{-# LINE 183 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.example(
{-# LINE 183 "src/Numeric/COINOR/CLP.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, (-1).*y] <=. 10, [(-1).*y, (1::Double).*z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
  )
  [ExpectedLine [LineChunk "Right (116.0,(22.0,12.0,32.0))"]]
 DocTest.printPrefix "Numeric.COINOR.CLP:194: "
{-# LINE 194 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.example(
{-# LINE 194 "src/Numeric/COINOR/CLP.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[PlusOne .* x, MinusOne .* y] <=. 10,
             [MinusOne .* y, PlusOne .* z] <=. 20]
            (LP.Maximize, Array.fromTuple (4,-3,2)
               :: Array.Array TripletShape Double)
  )
  [ExpectedLine [LineChunk "Right (116.0,(22.0,12.0,32.0))"]]
 DocTest.printPrefix "Numeric.COINOR.CLP:206: "
{-# LINE 206 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.example(
{-# LINE 206 "src/Numeric/COINOR/CLP.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, 1.*y] <=. 10, [1.*y, (-1::Double).*z] >=. 20]
            (LP.Maximize, Array.fromTuple (4,3,2)
               :: Array.Array TripletShape Double)
  )
  [ExpectedLine [LineChunk "Left PrimalInfeasible"]]
 DocTest.printPrefix "Numeric.COINOR.CLP:217: "
{-# LINE 217 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.example(
{-# LINE 217 "src/Numeric/COINOR/CLP.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.simplex LP.primal [y >=<. (-12,12)]
            [[1.*x, 1.*y] <=. 10, [1.*y, (1::Double).*z] >=. 20]
            (LP.Maximize, Array.fromTuple (4,3,2)
               :: Array.Array TripletShape Double)
  )
  [ExpectedLine [LineChunk "Left DualInfeasible"]]
 DocTest.printPrefix "Numeric.COINOR.CLP:228: "
{-# LINE 228 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 228 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method (QC.Positive posWeight) (QC.Positive negWeight) target ->
   case Shape.indexTupleFromShape pairShape of
      (pos,neg) ->
         case mapSnd Array.toTuple <$>
               LP.simplex method []
                  [[1.*pos, (-1::Double).*neg] ==. target]
                  (LP.Minimize, Array.fromTuple (posWeight,negWeight)
                     :: Array.Array PairShape Double) of
            Left _ -> QC.property False
            Right (absol,(posResult,negResult)) ->
               QC.property (absol>=0)
               .&&.
               (posResult === 0 .||. negResult === 0)
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:243: "
{-# LINE 243 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 243 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method target ->
   case Shape.indexTupleFromShape pairShape of
      (pos,neg) ->
         case mapSnd Array.toTuple <$>
               LP.simplex method []
                  [[1.*pos, (-1::Double).*neg] ==. target]
                  (LP.Minimize, Array.fromTuple (1,1)
                     :: Array.Array PairShape Double) of
            Left _ -> QC.property False
            Right (absol,(posResult,negResult)) ->
               QC.counterexample (show(absol,(posResult,negResult))) $
               QC.property (approxReal 0.001 absol (abs target))
               .&&.
               (posResult === 0 .||. negResult === 0)
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:260: "
{-# LINE 260 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 260 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> False
      Right _ -> True
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:269: "
{-# LINE 269 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 269 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (_,sol) -> TestLP.checkFeasibility 0.1 bounds constrs sol
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:278: "
{-# LINE 278 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 278 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (_,sol) ->
         QC.forAll (QC.choose (0,1)) $ \lambda ->
         TestLP.checkFeasibility 0.1 bounds constrs $
         TestLP.affineCombination lambda sol (Array.map fromIntegral origin)
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:290: "
{-# LINE 290 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 290 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case LP.simplex method bounds constrs (dir,obj) of
      Left _ -> QC.property False
      Right (opt,sol) ->
         QC.forAll (QC.choose (0,1)) $ \lambda ->
            let val = TestLP.scalarProduct obj $
                        TestLP.affineCombination lambda sol (Array.map fromIntegral origin)
            in case dir of
                  LP.Minimize -> opt-0.01 <= val
                  LP.Maximize -> opt+0.01 >= val
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:305: "
{-# LINE 305 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 305 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllBoundedProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \dirObjA ->
   QC.forAll (TestLP.genObjective origin) $ \dirObjB ->
   let solA = LP.simplex method bounds constrs dirObjA in
   let solB = LP.simplex method bounds constrs dirObjB in
   QC.counterexample (show (mapRight fst solA, mapRight fst solB)) $
   case (solA, solB) of
      (Right _, Right _) -> True
      (Left _, Left _) -> True
      _ -> False
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:319: "
{-# LINE 319 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 319 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(_dir,obj) ->
   case (LP.simplex method bounds constrs (LP.Minimize,obj),
         LP.simplex method bounds constrs (LP.Maximize,obj)) of
      (Right (optMin,_), Right (optMax,_)) ->
         QC.counterexample (show (optMin, optMax)) $ optMin <= optMax + 0.01
      _ -> QC.property False
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:330: "
{-# LINE 330 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 330 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds allConstrs ->
   QC.forAll (QC.sublistOf allConstrs) $ \someConstrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case (LP.simplex method bounds allConstrs (dir,obj),
         LP.simplex method bounds someConstrs (dir,obj)) of
      (Right (optAll,_), Right (optSome,_)) ->
         QC.counterexample (show (optAll, optSome)) $
         case dir of
            LP.Minimize -> optAll >= optSome-0.01
            LP.Maximize -> optAll <= optSome+0.01
      _ -> QC.property False
  )
 DocTest.printPrefix "Numeric.COINOR.CLP:345: "
{-# LINE 345 "src/Numeric/COINOR/CLP.hs" #-}
 DocTest.property(
{-# LINE 345 "src/Numeric/COINOR/CLP.hs" #-}
        
   forAllMethod $ \methodA ->
   forAllMethod $ \methodB ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \dirObj ->
   case (LP.simplex methodA bounds constrs dirObj,
         LP.simplex methodB bounds constrs dirObj) of
      (Right (optA,_), Right (optB,_)) ->
         QC.counterexample (show (optA, optB)) $
         approxReal 0.01 optA optB
      _ -> QC.property False
  )
