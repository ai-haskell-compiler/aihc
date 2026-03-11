{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.GLPK.Private where

import qualified Numeric.LinearProgramming.Common as LP
import Numeric.LinearProgramming.Common
         (Bound(..), Inequality(Inequality),
          Bounds, Direction(..), Objective)

import qualified Math.Programming.Glpk.Header as FFI

import qualified Data.Array.Comfort.Storable.Mutable as Mutable
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)
import Data.Maybe (fromMaybe)
import Data.Foldable (for_)

import Control.Applicative (liftA2)
import Control.DeepSeq (NFData, rnf)

import qualified Foreign
import Foreign.C.Types (CDouble)


type Term = LP.Term Double

type Constraints ix = LP.Constraints Double ix

data FailureType =
     Undefined
   | NoFeasible
   | Unbounded
   deriving (Eq, Show)

data SolutionType =
     Feasible
   | Infeasible
   | Optimal
   deriving (Eq, Show)

instance NFData FailureType where
    rnf NoFeasible = ()
    rnf _ = ()

instance NFData SolutionType where
    rnf Optimal = ()
    rnf _ = ()

type Result sh =
      Either FailureType (SolutionType, (Double, Array sh Double))


{- |
@libglpk@ considers (Between x x) an error. @glpsol@ does not.
In handwritten problems, (Between x x) might indicate a mistake.
In automatically generated problems it will certainly not.
-}
canonicalizeBounds :: Inequality a -> Inequality a
canonicalizeBounds (Inequality x bnd) =
   Inequality x $
   case bnd of
      Between lo up -> if lo == up then Equal lo else bnd
      _ -> bnd

prepareBoundsFFI ::
   Inequality a -> (a, (FFI.GlpkConstraintType, CDouble, CDouble))
prepareBoundsFFI (Inequality x bnd) =
   (,) x $
   (\(bndType,lo,up) -> (bndType, realToFrac lo, realToFrac up)) $
   case bnd of
      LessEqual up    -> (FFI.glpkLT,      0,  up)
      GreaterEqual lo -> (FFI.glpkGT,      lo, 0)
      Between lo up   -> (FFI.glpkBounded, lo, up)
      Equal y         -> (FFI.glpkFixed,   y,  y)
      Free            -> (FFI.glpkFree,    0,  0)

prepareBounds ::
   Inequality a -> (a, (FFI.GlpkConstraintType, CDouble, CDouble))
prepareBounds = prepareBoundsFFI . canonicalizeBounds

storeBounds ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   Foreign.Ptr FFI.Problem -> sh -> Bounds ix -> IO ()
storeBounds lp shape bounds = do
   _firstCol <- FFI.glp_add_cols lp $ fromIntegral $ Shape.size shape
   for_ (Shape.indices $ Shape.Deferred shape) $ \x ->
      FFI.glp_set_col_bnds lp (deferredColumnIndex x) FFI.glpkGT 0 0
   for_ (map prepareBounds bounds) $ \(x,(bnd,lo,up)) ->
      FFI.glp_set_col_bnds lp (columnIndex shape x) bnd lo up



columnIndex :: (Shape.Indexed sh, Shape.Index sh ~ ix) => sh -> ix -> FFI.Column
columnIndex shape var = 1 + fromIntegral (Shape.offset shape var)

deferredColumnIndex :: Shape.DeferredIndex ix -> FFI.Column
deferredColumnIndex (Shape.DeferredIndex var) = 1 + fromIntegral var

allocaArray :: (Foreign.Storable a) => Int -> (FFI.GlpkArray a -> IO b) -> IO b
allocaArray n f = Foreign.allocaArray (n+1) $ f . FFI.GlpkArray

pokeElem :: (Foreign.Storable a) => FFI.GlpkArray a -> Int -> a -> IO ()
pokeElem (FFI.GlpkArray ptr) k a = Foreign.pokeElemOff ptr k a



setDirection :: Foreign.Ptr FFI.Problem -> Direction -> IO ()
setDirection lp dir =
   FFI.glp_set_obj_dir lp $
      case dir of
         Minimize -> FFI.glpkMin
         Maximize -> FFI.glpkMax

setObjective ::
   (Shape.Indexed sh) => Foreign.Ptr FFI.Problem -> Objective sh -> IO ()
setObjective lp obj =
   for_ (Array.toAssociations obj) $ \(x,c) ->
      FFI.glp_set_obj_coef lp (columnIndex (Array.shape obj) x) (realToFrac c)

{-# INLINE readGLPArray #-}
readGLPArray ::
   (Shape.C sh, Foreign.Storable a, Num a) =>
   sh ->
   (Mutable.Array IO (Shape.Deferred sh) a ->
    Shape.DeferredIndex sh -> IO ()) ->
   IO (Array sh a)
readGLPArray shape act = do
   let defShape = Shape.Deferred shape
   arr <- Mutable.new defShape 0
   for_ (Shape.indices defShape) (act arr)
   Array.reshape shape <$> Mutable.freeze arr

analyzeStatus :: FFI.GlpkSolutionStatus -> Either FailureType SolutionType
analyzeStatus status =
   fromMaybe (error "glpk-solver: solution type unknown") $ lookup status $
      (FFI.glpkUndefined,  Left Undefined) :
      (FFI.glpkFeasible,   Right Feasible) :
      (FFI.glpkInfeasible, Right Infeasible) :
      (FFI.glpkNoFeasible, Left NoFeasible) :
      (FFI.glpkOptimal,    Right Optimal) :
      (FFI.glpkUnbounded,  Left Unbounded) :
      []


peekSimplexSolution ::
   (Shape.C sh) => sh -> Foreign.Ptr FFI.Problem -> IO (Result sh)
peekSimplexSolution shape lp = do
   let examine =
         liftA2 (,)
            (realToFrac <$> FFI.glp_get_obj_val lp)
            (readGLPArray shape $ \arr ix ->
               Mutable.write arr ix . realToFrac
                  =<< FFI.glp_get_col_prim lp (deferredColumnIndex ix))
   status <- FFI.glp_get_status lp
   either (return . Left) (\typ -> Right . (,) typ <$> examine) $
      analyzeStatus status
