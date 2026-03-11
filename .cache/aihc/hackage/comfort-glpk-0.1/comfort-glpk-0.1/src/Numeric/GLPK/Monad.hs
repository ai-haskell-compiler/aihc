{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{- |
The monadic interface to GLPK allows to optimize
with respect to multiple objectives, successively.

It is not intended to provide a general imperative interface to GLPK
that manipulates GLPK's state by setters and getters.
-}
module Numeric.GLPK.Monad (
   T,
   run,
   simplex,
   exact,
   Direction(..),
   ) where

import qualified Math.Programming.Glpk.Header as FFI
import qualified Numeric.GLPK.Debug as Debug
import Numeric.GLPK.Private
         (Constraints, Result,
          allocaArray, pokeElem, columnIndex, prepareBounds, storeBounds,
          setDirection, setObjective, peekSimplexSolution)
import Numeric.GLPK (Bounds, Direction(..), Objective)
import Numeric.LinearProgramming.Common (Term(Term))

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Foldable (for_)

import qualified Control.Monad.Trans.Reader as MR
import Control.Monad.IO.Class (liftIO)
import Control.Monad (void, when)
import Control.Exception (bracket)

import System.IO.Unsafe (unsafePerformIO)

import Foreign.Ptr (Ptr, nullPtr)


{- $setup
>>> :set -XTypeFamilies
>>> :set -XTypeOperators
>>> import qualified Numeric.LinearProgramming.Monad as LPMonad
>>> import qualified Numeric.LinearProgramming.Test as TestLP
>>> import qualified Numeric.GLPK.Monad as LP
>>> import qualified Numeric.GLPK as GLPK
>>> import Test.Numeric.GLPK.Utility
>>>    (traverseLag, traverse_Lag, approxSuccession)
>>> import Test.Numeric.GLPK (TripletShape, tripletShape)
>>> import Numeric.GLPK (Bounds, Constraints, Objective, (.*), (<=.))
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.NonEmpty as NonEmpty
>>> import Data.Array.Comfort.Storable (Array)
>>> import Data.Traversable (Traversable)
>>> import Data.Foldable (Foldable)
>>>
>>> import qualified Control.Monad.Trans.Except as ME
>>>
>>> import Data.Tuple.HT (mapSnd)
>>>
>>> import qualified Test.QuickCheck as QC
>>>
>>>
>>> runSuccessive ::
>>>    (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Foldable t) =>
>>>    sh ->
>>>    Bounds ix ->
>>>    (Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    Either GLPK.FailureType ()
>>> runSuccessive shape bounds (constrs,dirObj) objs =
>>>    LP.run shape bounds $ ME.runExceptT $ do
>>>       (_solType, (opt, _xs)) <- ME.ExceptT $ LP.simplex constrs dirObj
>>>       traverse_Lag opt
>>>          (\prevResult (newConstr, dirObjI) -> do
>>>              (_solType, (optI, _xs)) <-
>>>                 ME.ExceptT $ LP.simplex (newConstr prevResult) dirObjI
>>>              return optI)
>>>          objs
>>>
>>> solveSuccessiveWarm ::
>>>    (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
>>>    sh ->
>>>    Bounds ix ->
>>>    (Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    Either GLPK.FailureType
>>>       (NonEmpty.T t (GLPK.SolutionType, (Double, Array sh Double)))
>>> solveSuccessiveWarm shape bounds (constrs,dirObj) objs =
>>>    LP.run shape bounds $ ME.runExceptT $ do
>>>       result <- ME.ExceptT $ LP.simplex constrs dirObj
>>>       NonEmpty.Cons result <$>
>>>          traverseLag result
>>>             (\(_solType, (prevOpt, _xs)) (newConstr, dirObjI) ->
>>>                 ME.ExceptT $ LP.simplex (newConstr prevOpt) dirObjI)
>>>             objs
>>>
>>> solveSuccessiveGen ::
>>>    (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix, Traversable t) =>
>>>    sh ->
>>>    Bounds ix ->
>>>    (Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    t (Double -> Constraints ix, (GLPK.Direction, Objective sh)) ->
>>>    Either GLPK.FailureType
>>>       (NonEmpty.T t (GLPK.SolutionType, (Double, Array sh Double)))
>>> solveSuccessiveGen shape bounds (constrs,dirObj) objs =
>>>    LPMonad.run shape bounds $ ME.runExceptT $ do
>>>       result <- ME.ExceptT $ LPMonad.lift GLPK.simplex constrs dirObj
>>>       NonEmpty.Cons result <$>
>>>          traverseLag result
>>>             (\(_solType, (prevOpt, _xs)) (newConstr, dirObjI) ->
>>>                 ME.ExceptT $
>>>                    LPMonad.lift GLPK.simplex (newConstr prevOpt) dirObjI)
>>>             objs
-}


newtype T sh a = Cons (MR.ReaderT (sh, Ptr FFI.Problem) IO a)
   deriving (Functor, Applicative, Monad)

run ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   sh -> Bounds ix -> T sh a -> a
run shape bounds (Cons act) =
   unsafePerformIO $
   bracket FFI.glp_create_prob FFI.glp_delete_prob $ \lp -> do
      Debug.initLog
      storeBounds lp shape bounds
      liftIO $ MR.runReaderT act (shape, lp)


{- |
Add new constraints to an existing problem
and solve with a new direction and objective.

>>> case Shape.indexTupleFromShape tripletShape of (x,y,z) -> mapSnd (mapSnd Array.toTuple) <$> LP.run tripletShape [] (LP.simplex [[2.*x, 1.*y] <=. 10, [1.*y, (5::Double).*z] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double))
Right (Optimal,(28.0,(5.0,0.0,4.0)))

prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (GLPK.simplex bounds constrs (dir,obj), LP.run (Array.shape origin) bounds $ LP.simplex constrs (dir,obj)) of (Right (GLPK.Optimal, (optA,_)), Right (GLPK.Optimal, (optB,_))) -> TestLP.approxReal 0.1 optA optB; _ -> False

prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> TestLP.forAllObjectives origin $ \objs_ -> case TestLP.successiveObjectives origin 0.01 objs_ of (dirObj, objs) -> either (\msg -> QC.counterexample (show msg) False) (const $ QC.property True) $ runSuccessive (Array.shape origin) bounds (constrs,dirObj) objs

prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> TestLP.forAllObjectives origin $ \objs_ -> case TestLP.successiveObjectives origin 0.01 objs_ of (dirObj, objs) -> approxSuccession 0.01 (solveSuccessiveWarm (Array.shape origin) bounds (constrs,dirObj) objs) (solveSuccessiveGen (Array.shape origin) bounds (constrs,dirObj) objs)
-}
simplex ::
   (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Constraints ix ->
   (Direction, Objective sh) -> T sh (Result sh)
simplex = solve (flip FFI.glp_simplex nullPtr)

exact ::
   (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Constraints ix ->
   (Direction, Objective sh) -> T sh (Result sh)
exact = solve (flip FFI.glp_exact nullPtr)

solve ::
   (Eq sh, Shape.Indexed sh, Shape.Index sh ~ ix) =>
   (Ptr FFI.Problem -> IO FFI.GlpkSimplexStatus) ->
   Constraints ix ->
   (Direction, Objective sh) ->
   T sh (Result sh)
solve method constrs (dir,obj) = Cons $ do
   (shape, lp) <- MR.ask
   when (shape /= Array.shape obj) $
      error "GLPK.Monad.solve: objective shape mismatch"

   liftIO $ do
      setDirection lp dir
      setObjective lp obj
      newRow <- FFI.glp_add_rows lp $ fromIntegral $ length constrs
      for_ (zip [newRow..] (map prepareBounds constrs)) $
            \(row, (terms,(bnd,lo,up))) -> do
         FFI.glp_set_row_bnds lp row bnd lo up
         let numTerms = length terms
         allocaArray numTerms $ \indicesPtr ->
            allocaArray numTerms $ \coeffsPtr -> do
            for_ (zip [1..] terms) $
               \(k, Term c x) -> do
                  pokeElem indicesPtr k (columnIndex shape x)
                  pokeElem coeffsPtr k (realToFrac c)
            FFI.glp_set_mat_row lp row
               (fromIntegral numTerms) indicesPtr coeffsPtr
      void $ method lp
      peekSimplexSolution shape lp
