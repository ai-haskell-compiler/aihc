{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{- |
The following LP problem

maximize @4 x_1 - 3 x_2 + 2 x_3@ subject to

@2 x_1 + x_2 <= 10@

@x_2 + 5 x_3 <= 20@

and

@x_i >= 0@

is used as an example in the doctest comments.


By default all indeterminates are non-negative.
A given bound for a variable completely replaces the default,
so @0 <= x_i <= b@ must be explicitly given as @i >=<. (0,b)@.
Multiple bounds for a variable are not allowed,
instead of @[i >=. a, i <=. b]@ use @i >=<. (a,b)@.
-}
module Numeric.GLPK (
   Term,
   Bound(..),
   Inequality(..),
   LP.free, (LP.<=.), (LP.>=.), (LP.==.), (LP.>=<.),
   FailureType(..),
   SolutionType(..),
   Result,
   Constraints,
   Direction(..),
   Objective,
   Bounds,
   (.*),
   LP.objectiveFromTerms,
   simplex,
   exact,
   interior,
   ) where

import qualified Math.Programming.Glpk.Header as FFI
import qualified Numeric.GLPK.Debug as Debug
import qualified Numeric.LinearProgramming.Common as LP
import Numeric.GLPK.Private
import Numeric.LinearProgramming.Common
         (Bound(..), Inequality(Inequality),
          Bounds, Direction(..), Objective, (.*))

import qualified Data.Array.Comfort.Storable.Mutable as Mutable
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Foldable (for_)

import Control.Monad (void)
import Control.Applicative (liftA2)
import Control.Exception (bracket)

import System.IO.Unsafe (unsafePerformIO)

import qualified Foreign
import Foreign.Ptr (nullPtr)


{- $setup
>>> import qualified Numeric.LinearProgramming.Test as TestLP
>>> import qualified Numeric.GLPK as LP
>>> import Numeric.GLPK ((.*), (<=.), (==.))
>>>
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>>
>>> import Data.Tuple.HT (mapPair, mapSnd)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((===), (.&&.), (.||.))
>>>
>>> type X = Shape.Element
>>> type PairShape = Shape.NestedTuple Shape.TupleIndex (X,X)
>>> type TripletShape = Shape.NestedTuple Shape.TupleIndex (X,X,X)
>>>
>>> pairShape :: PairShape
>>> pairShape = Shape.static
>>>
>>> tripletShape :: TripletShape
>>> tripletShape = Shape.static
>>>
>>> round3 :: Double -> Double
>>> round3 x = fromInteger (round (1000*x)) / 1000
-}


{- |
>>> case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double)
Right (Optimal,(28.0,(5.0,0.0,4.0)))

prop> \target -> case Shape.indexTupleFromShape pairShape of (pos,neg) -> case mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[1.*pos, (-1).*neg] ==. target] (LP.Minimize, Array.fromTuple (1,1) :: Array.Array PairShape Double) of (Right (LP.Optimal,(absol,(posResult,negResult)))) -> QC.property (TestLP.approxReal 0.001 absol (abs target)) .&&. (posResult === 0 .||. negResult === 0); _ -> QC.property False
prop> \(QC.Positive posWeight) (QC.Positive negWeight) target -> case Shape.indexTupleFromShape pairShape of (pos,neg) -> case mapSnd (mapSnd Array.toTuple) <$> LP.simplex [] [[1.*pos, (-1).*neg] ==. target] (LP.Minimize, Array.fromTuple (posWeight,negWeight) :: Array.Array PairShape Double) of (Right (LP.Optimal,(absol,(posResult,negResult)))) -> QC.property (absol>=0) .&&. (posResult === 0 .||. negResult === 0); _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, _) -> True; _ -> False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> TestLP.checkFeasibility 0.1 bounds constrs sol; _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> TestLP.checkFeasibility 0.1 bounds constrs $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin); _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.simplex bounds constrs (dir,obj) of Right (LP.Optimal, (opt,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> let val = TestLP.scalarProduct obj $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin) in (case dir of LP.Minimize -> opt-0.01 <= val; LP.Maximize -> opt+0.01 >= val); _ -> QC.property False
-}
simplex ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> Result sh
simplex = solve (flip FFI.glp_simplex nullPtr)


{- |
>>> case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapSnd Array.toTuple) <$> LP.exact [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double)
Right (Optimal,(28.0,(5.0,0.0,4.0)))

prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (LP.simplex bounds constrs (dir,obj), LP.exact bounds constrs (dir,obj)) of (Right (LP.Optimal, (optSimplex,_)), Right (LP.Optimal, (optExact,_))) -> TestLP.approx "optimum" 0.001 optSimplex optExact; _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> TestLP.checkFeasibility 0.1 bounds constrs sol; _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (_,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> TestLP.checkFeasibility 0.01 bounds constrs $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin); _ -> QC.property False
prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case LP.exact bounds constrs (dir,obj) of Right (LP.Optimal, (opt,sol)) -> QC.forAll (QC.choose (0,1)) $ \lambda -> let val = TestLP.scalarProduct obj $ TestLP.affineCombination lambda sol (Array.map fromIntegral origin) in (case dir of LP.Minimize -> opt-0.01 <= val; LP.Maximize -> opt+0.01 >= val); _ -> QC.property False
-}
exact ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> Result sh
exact = solve (flip FFI.glp_exact nullPtr)


{-# INLINE solve #-}
solve ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   (Foreign.Ptr FFI.Problem -> IO FFI.GlpkSimplexStatus) ->
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> Result sh
solve solver bounds constrs (dir,obj) = unsafePerformIO $
   bracket FFI.glp_create_prob FFI.glp_delete_prob $ \lp -> do
   storeProblem bounds constrs (dir,obj) lp
   void $ solver lp
   peekSimplexSolution (Array.shape obj) lp



{- |
>>> case Shape.indexTupleFromShape tripletShape of (x1,x2,x3) -> mapSnd (mapPair (round3, Array.toTuple . Array.map round3)) <$> LP.interior [] [[2.*x1, 1.*x2] <=. 10, [1.*x2, 5.*x3] <=. 20] (LP.Maximize, Array.fromTuple (4,-3,2) :: Array.Array TripletShape Double)
Right (Optimal,(28.0,(5.0,0.0,4.0)))

prop> TestLP.forAllOrigin $ \origin -> TestLP.forAllProblem origin $ \bounds constrs -> QC.forAll (TestLP.genObjective origin) $ \(dir,obj) -> case (LP.simplex bounds constrs (dir,obj), LP.interior bounds constrs (dir,obj)) of (Right (LP.Optimal, (optSimplex,_)), Right (LP.Optimal, (optExact,_))) -> TestLP.approx "optimum" 0.001 optSimplex optExact; _ -> QC.property False
-}
interior ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> Result sh
interior bounds constrs (dir,obj) = unsafePerformIO $
   bracket FFI.glp_create_prob FFI.glp_delete_prob $ \lp -> do
   storeProblem bounds constrs (dir,obj) lp
   void $ FFI.glp_interior lp nullPtr
   let examine =
         liftA2 (,)
            (realToFrac <$> FFI.glp_ipt_obj_val lp)
            (readGLPArray (Array.shape obj) $ \arr ix ->
               Mutable.write arr ix . realToFrac
                  =<< FFI.glp_ipt_col_prim lp (deferredColumnIndex ix))
   status <- FFI.glp_ipt_status lp
   either (return . Left) (\typ -> Right . (,) typ <$> examine) $
      analyzeStatus status


storeProblem ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Bounds ix -> Constraints ix ->
   (Direction, Objective sh) -> Foreign.Ptr FFI.Problem -> IO ()
storeProblem bounds constrs (dir,obj) lp = do
   Debug.initLog
   let shape = Array.shape obj
   setDirection lp dir
   firstRow <- FFI.glp_add_rows lp $ fromIntegral $ length constrs
   for_ (zip [firstRow..] $
      map prepareBounds constrs) $ \(row,(_x,(bnd,lo,up))) ->
      FFI.glp_set_row_bnds lp row bnd lo up
   storeBounds lp shape bounds
   setObjective lp obj
   let numTerms = length $ concatMap (fst . prepareBounds) constrs
   allocaArray numTerms $ \ia ->
      allocaArray numTerms $ \ja ->
      allocaArray numTerms $ \ar -> do
      for_ (zip [1..] $ concat $
            zipWith (map . (,)) [firstRow..] $
            map (fst . prepareBounds) constrs) $
         \(k, (row, LP.Term c x)) -> do
            pokeElem ia k row
            pokeElem ja k (columnIndex shape x)
            pokeElem ar k (realToFrac c)
      FFI.glp_load_matrix lp (fromIntegral numTerms) ia ja ar
