-- Do not edit! Automatically created with doctest-extract from src/Numeric/COINOR/CLP/Monad.hs
{-# LINE 43 "src/Numeric/COINOR/CLP/Monad.hs" #-}

{-# OPTIONS_GHC -XTypeFamilies #-}
{-# OPTIONS_GHC -XTypeOperators #-}
module Test.Numeric.COINOR.CLP.Monad where

import Test.DocTest.Base
import qualified Test.DocTest.Driver as DocTest

{-# LINE 46 "src/Numeric/COINOR/CLP/Monad.hs" #-}
import     qualified Numeric.COINOR.CLP.Monad as LP
import     qualified Numeric.COINOR.CLP as CLP
import     Test.Numeric.COINOR.CLP.Utility (traverse_Lag, traverseLag)
import     Test.Numeric.COINOR.CLP (TripletShape, tripletShape, forAllMethod)
import     Numeric.COINOR.CLP (Direction, (.*), (<=.))

import     qualified Numeric.LinearProgramming.Monad as LPMonad
import     qualified Numeric.LinearProgramming.Test as TestLP
import     Numeric.LinearProgramming.Common (Bounds, Objective)

import     qualified Data.Array.Comfort.Storable as Array
import     qualified Data.Array.Comfort.Shape as Shape
import     qualified Data.NonEmpty as NonEmpty
import     Data.Array.Comfort.Storable (Array)
import     Data.Traversable (Traversable)
import     Data.Foldable (Foldable)

import     qualified Control.Monad.Trans.Except as ME

import     qualified Data.List.HT as ListHT
import     Data.Tuple.HT (mapSnd)

import     Foreign.Storable (Storable)

import     qualified Test.QuickCheck as QC


type     Constraints ix = CLP.Constraints Double ix


approxSuccession     ::
       (Shape.C sh, Show sh, Show a, Ord a, Num a, Storable a) =>
       a ->
       Either CLP.FailureType (NonEmpty.T [] (a, Array sh a)) ->
       Either CLP.FailureType (NonEmpty.T [] (a, Array sh a)) ->
       QC.Property
approxSuccession     tol x y =
       QC.counterexample (show x) $
       QC.counterexample (show y) $
       case (x,y) of
          (Left sx, Left sy) -> sx==sy
          (Right (NonEmpty.Cons xh xs), Right (NonEmpty.Cons yh ys)) ->
             let equalSol (optX, _) (optY, _) = TestLP.approxReal tol optX optY
             in equalSol xh yh  &&  ListHT.equalWith equalSol xs ys
          _ -> False


runSuccessive     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Foldable t) =>
       CLP.Method ->
       sh ->
       Bounds ix ->
       (Constraints ix, (Direction, Objective sh)) ->
       t (Double -> Constraints ix, (Direction, Objective sh)) ->
       Either CLP.FailureType ()
runSuccessive     method shape bounds (constrs,dirObj) objs =
       LP.run shape bounds $ ME.runExceptT $ do
          (opt, _xs) <- ME.ExceptT $ LP.simplex method constrs dirObj
          traverse_Lag opt
             (\prevResult (newConstr, dirObjI) -> do
                 (optI, _xs) <-
                    ME.ExceptT $
                       LP.simplex method (newConstr prevResult) dirObjI
                 return optI)
             objs

solveSuccessiveWarm     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
       CLP.Method ->
       sh ->
       Bounds ix ->
       (Constraints ix, (Direction, Objective sh)) ->
       t (Double -> Constraints ix, (Direction, Objective sh)) ->
       Either CLP.FailureType (NonEmpty.T t (Double, Array sh Double))
solveSuccessiveWarm     method shape bounds (constrs,dirObj) objs =
       LP.run shape bounds $ ME.runExceptT $ do
          result <- ME.ExceptT $ LP.simplex method constrs dirObj
          NonEmpty.Cons result <$>
             traverseLag result
                (\(prevOpt, _xs) (newConstr, dirObjI) ->
                    ME.ExceptT $ LP.simplex method (newConstr prevOpt) dirObjI)
                objs

solveSuccessiveGen     ::
       (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
       CLP.Method ->
       sh ->
       Bounds ix ->
       (Constraints ix, (Direction, Objective sh)) ->
       t (Double -> Constraints ix, (Direction, Objective sh)) ->
       Either CLP.FailureType (NonEmpty.T t (Double, Array sh Double))
solveSuccessiveGen     method shape bounds (constrs,dirObj) objs =
       LPMonad.run shape bounds $ ME.runExceptT $ do
          result <-
             ME.ExceptT $ LPMonad.lift (CLP.simplex method) constrs dirObj
          NonEmpty.Cons result <$>
             traverseLag result
                (\(prevOpt, _xs) (newConstr, dirObjI) ->
                    ME.ExceptT $
                       LPMonad.lift (CLP.simplex method)
                          (newConstr prevOpt) dirObjI)
                objs

test :: DocTest.T ()
test = do
 DocTest.printPrefix "Numeric.COINOR.CLP.Monad:179: "
{-# LINE 179 "src/Numeric/COINOR/CLP/Monad.hs" #-}
 DocTest.example(
{-# LINE 179 "src/Numeric/COINOR/CLP/Monad.hs" #-}
      
   case Shape.indexTupleFromShape tripletShape of
      (x,y,z) ->
         mapSnd Array.toTuple <$>
         LP.run tripletShape []
            (LP.simplex LP.dual
               [[2.*x, 1.*y] <=. 10, [1.*y, (5::Double).*z] <=. 20]
               (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double))
  )
  [ExpectedLine [LineChunk "Right (28.0,(5.0,0.0,4.0))"]]
 DocTest.printPrefix "Numeric.COINOR.CLP.Monad:190: "
{-# LINE 190 "src/Numeric/COINOR/CLP/Monad.hs" #-}
 DocTest.property(
{-# LINE 190 "src/Numeric/COINOR/CLP/Monad.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   QC.forAll (TestLP.genObjective origin) $ \(dir,obj) ->
   case (CLP.simplex method bounds constrs (dir,obj),
         LP.run (Array.shape origin) bounds $
            LP.simplex method constrs (dir,obj)) of
      (Right (optA,_), Right (optB,_)) ->
         TestLP.approxReal 0.1 optA optB; _ -> False
  )
 DocTest.printPrefix "Numeric.COINOR.CLP.Monad:202: "
{-# LINE 202 "src/Numeric/COINOR/CLP/Monad.hs" #-}
 DocTest.property(
{-# LINE 202 "src/Numeric/COINOR/CLP/Monad.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   TestLP.forAllObjectives origin $ \objs_ ->
   case TestLP.successiveObjectives origin 0.01 objs_ of
      (dirObj, objs) ->
         either (\msg -> QC.counterexample (show msg) False) (const $ QC.property True) $
         runSuccessive method (Array.shape origin) bounds (constrs,dirObj) objs
  )
 DocTest.printPrefix "Numeric.COINOR.CLP.Monad:213: "
{-# LINE 213 "src/Numeric/COINOR/CLP/Monad.hs" #-}
 DocTest.property(
{-# LINE 213 "src/Numeric/COINOR/CLP/Monad.hs" #-}
        
   forAllMethod $ \method ->
   TestLP.forAllOrigin $ \origin ->
   TestLP.forAllProblem origin $ \bounds constrs ->
   TestLP.forAllObjectives origin $ \objs_ ->
   case TestLP.successiveObjectives origin 0.01 objs_ of
      (dirObj, objs) ->
         approxSuccession 0.01
            (solveSuccessiveWarm method (Array.shape origin) bounds (constrs,dirObj) objs)
            (solveSuccessiveGen method (Array.shape origin) bounds (constrs,dirObj) objs)
  )
