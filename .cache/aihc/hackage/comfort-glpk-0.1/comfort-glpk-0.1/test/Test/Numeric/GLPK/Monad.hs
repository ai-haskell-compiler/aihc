-- Do not edit! Automatically created with doctest-extract from src/Numeric/GLPK/Monad.hs
{-# LINE 42 "src/Numeric/GLPK/Monad.hs" #-}

{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XTypeOperators #-}
module Test.Numeric.GLPK.Monad where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 45 "src/Numeric/GLPK/Monad.hs" #-}
import     qualified Numeric.LinearProgramming.Monad as LPMonad
import     qualified Numeric.LinearProgramming.Test as TestLP
import     qualified Numeric.GLPK.Monad as LP
import     qualified Numeric.GLPK as GLPK
import     Test.Numeric.GLPK.Utility
       (traverseLag, traverse_Lag, approxSuccession)
import     Test.Numeric.GLPK (TripletShape, tripletShape)
import     Numeric.GLPK (Bounds, Constraints, Objective, (.*), (<=.))

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.NonEmpty as NonEmpty
import     Data.Array.Comfort.Storable (Array)
import     Data.Traversable (Traversable)
import     Data.Foldable (Foldable)

import     qualified Control.Monad.Trans.Except as ME

import     Data.Tuple.HT (mapSnd)

import     qualified Test.QuickCheck as QC


runSuccessive     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Foldable t) =>
       sh ->
       Bounds ix ->
       (Constraints ix, (GLPK.Direction, Objective sh)) ->
       t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
       Either GLPK.FailureType ()
runSuccessive     shape bounds (constrs,dirObj) objs =
       LP.run shape bounds $ ME.runExceptT $ do
          (_solType, (opt, _xs)) <- ME.ExceptT $ LP.simplex constrs dirObj
          traverse_Lag opt
             (\prevResult (newConstr, dirObjI) -> do
                 (_solType, (optI, _xs)) <-
                    ME.ExceptT $ LP.simplex (newConstr prevResult) dirObjI
                 return optI)
             objs

solveSuccessiveWarm     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
       sh ->
       Bounds ix ->
       (Constraints ix, (GLPK.Direction, Objective sh)) ->
       t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
       Either GLPK.FailureType
          (NonEmpty.T t (GLPK.SolutionType, (Double, Array sh Double)))
solveSuccessiveWarm     shape bounds (constrs,dirObj) objs =
       LP.run shape bounds $ ME.runExceptT $ do
          result <- ME.ExceptT $ LP.simplex constrs dirObj
          NonEmpty.Cons result <$>
             traverseLag result
                (\(_solType, (prevOpt, _xs)) (newConstr, dirObjI) ->
                    ME.ExceptT $ LP.simplex (newConstr prevOpt) dirObjI)
                objs

solveSuccessiveGen     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
       sh ->
       Bounds ix ->
       (Constraints ix, (GLPK.Direction, Objective sh)) ->
       t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
       Either GLPK.FailureType
          (NonEmpty.T t (GLPK.SolutionType, (Double, Array sh Double)))
solveSuccessiveGen     shape bounds (constrs,dirObj) objs =
       LPMonad.run shape bounds $ ME.runExceptT $ do
          result <- ME.ExceptT $ LPMonad.lift GLPK.simplex constrs dirObj
          NonEmpty.Cons result <$>
             traverseLag result
                (\(_solType, (prevOpt, _xs)) (newConstr, dirObjI) ->
                    ME.ExceptT $
                       LPMonad.lift GLPK.simplex (newConstr prevOpt) dirObjI)
                objs

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.GLPK.Monad:143: "
{-# LINE 143 "src/Numeric/GLPK/Monad.hs" #-}
 DocTest.property
{-# LINE 143 "src/Numeric/GLPK/Monad.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (GLPK.simplex bounds constrs (dir,obj), LP.run (Array.shape origin) bounds $ LP.simplex constrs (dir,obj)) of (Right (GLPK.Optimal, (optA,_)), Right (GLPK.Optimal, (optB,_))) -> TestLP.approxReal 0.1 optA optB; _ -> False)
 DocTest.printPrefix "Numeric.GLPK.Monad:145: "
{-# LINE 145 "src/Numeric/GLPK/Monad.hs" #-}
 DocTest.property
{-# LINE 145 "src/Numeric/GLPK/Monad.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> TestLP.forAllObjectives origin $ \objs_ -> case TestLP.successiveObjectives origin 0.01 objs_ of (dirObj, objs) -> either (\msg -> QC.counterexample (show msg) False) (const $ QC.property True) $ runSuccessive (Array.shape origin) bounds (constrs,dirObj) objs)
 DocTest.printPrefix "Numeric.GLPK.Monad:147: "
{-# LINE 147 "src/Numeric/GLPK/Monad.hs" #-}
 DocTest.property
{-# LINE 147 "src/Numeric/GLPK/Monad.hs" #-}
     (TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> TestLP.forAllObjectives origin $ \objs_ -> case TestLP.successiveObjectives origin 0.01 objs_ of (dirObj, objs) -> approxSuccession 0.01 (solveSuccessiveWarm (Array.shape origin) bounds (constrs,dirObj) objs) (solveSuccessiveGen (Array.shape origin) bounds (constrs,dirObj) objs))
 DocTest.printPrefix "Numeric.GLPK.Monad:140: "
{-# LINE 140 "src/Numeric/GLPK/Monad.hs" #-}
 DocTest.example
{-# LINE 140 "src/Numeric/GLPK/Monad.hs" #-}
   (case Shape.indexTupleFromShape tripletShape of (x,y,z) -> mapSnd (mapSnd Array.toTuple) <$> LP.run tripletShape [] (LP.simplex [[2.*x, 1.*y] <=. 10, [1.*y, (5::Double).*z] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double)))
  [ExpectedLine [LineChunk "Right (Optimal,(28.0,(5.0,0.0,4.0)))"]]
