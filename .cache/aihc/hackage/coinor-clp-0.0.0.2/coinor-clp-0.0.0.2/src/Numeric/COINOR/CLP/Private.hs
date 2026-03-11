{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.COINOR.CLP.Private where

import qualified Numeric.COINOR.CLP.FFI as FFI
import Numeric.LinearProgramming.Common
         (Term(..), Bound(..), Inequality(Inequality),
          Bounds, Constraints, Direction(..))

import qualified Data.Array.Comfort.Boxed as BoxedArray
import qualified Data.Array.Comfort.Storable.Unchecked.Monadic as ArrayMonadic
import qualified Data.Array.Comfort.Storable.Unchecked as ArrayUnchecked
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable (Array)
import Data.Foldable (for_)
import Data.Tuple.HT (mapPair)

import qualified Control.Monad.Trans.Cont as MC
import qualified Control.Applicative.HT as AppHT
import qualified Control.Functor.HT as FuncHT
import Control.Functor.HT (void)

import Foreign.Storable (pokeElemOff, peekElemOff)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.C.Types (CDouble, CInt, CBool)



withBuffer :: Array sh a -> MC.ContT r IO (Ptr a)
withBuffer arr =
   MC.ContT $ withForeignPtr (ArrayUnchecked.buffer arr)

runContT :: MC.ContT a IO a -> IO a
runContT act = MC.runContT act return



false, true :: CBool
false = toEnum $ fromEnum False
true  = toEnum $ fromEnum True

positiveInfinity, negativeInfinity :: CDouble
positiveInfinity =  1/0
negativeInfinity = -1/0

prepareBounds :: Inequality a -> (a, (CDouble, CDouble))
prepareBounds (Inequality x bnd) =
   (,) x $
   case bnd of
      LessEqual up    -> (negativeInfinity, realToFrac up)
      GreaterEqual lo -> (realToFrac lo,    positiveInfinity)
      Between lo up   -> (realToFrac lo,    realToFrac up)
      Equal y         -> (realToFrac y,     realToFrac y)
      Free            -> (negativeInfinity, positiveInfinity)

prepareColumnBoundsArrays ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   sh -> Bounds ix -> (Array sh CDouble, Array sh CDouble)
prepareColumnBoundsArrays shape =
   mapPair (Array.fromBoxed, Array.fromBoxed) .
   FuncHT.unzip .
   BoxedArray.fromAssociations (0, positiveInfinity) shape .
   map prepareBounds


type ShapeInt = Shape.ZeroBased Int

prepareRowBoundsArrays ::
   Bounds ix -> (Array ShapeInt CDouble, Array ShapeInt CDouble)
prepareRowBoundsArrays constrs =
   let shape = Shape.ZeroBased $ length constrs in
   mapPair (Array.fromList shape, Array.fromList shape) $
   unzip $ map (snd . prepareBounds) constrs

storeBounds ::
   (Array sh CDouble, Array sh CDouble) ->
   MC.ContT r IO (Ptr CDouble, Ptr CDouble)
storeBounds = AppHT.mapPair (withBuffer, withBuffer)


prepareConstraints ::
   (Shape.Indexed sh, Shape.Index sh ~ ix) =>
   sh -> Constraints Double ix ->
   (Array ShapeInt CDouble, Array ShapeInt CInt, Array ShapeInt FFI.BigIndex)
prepareConstraints shape constrs =
   let {-
       It seems that LP solvers generally do not expect zero coefficients,
       although that is not document.
       https://hydra.nixos.org/build/239790474/nixlog/2
       https://list.coin-or.org/pipermail/clp/2023-November/001805.html
       -}
       constrsNonZero = map (fmap (filter (\(Term c _x) -> c/=0))) constrs
       rowStarts =
         Array.vectorFromList $ scanl (+) 0 $
         map (\(Inequality terms _bnd) -> fromIntegral $ length terms)
            constrsNonZero
       shapeOffset = Shape.offset shape
       coefficients =
         concatMap (\(Inequality terms _bnd) -> terms) constrsNonZero
       indexArr =
         Array.vectorFromList $
         map (\(Term _ ix) -> fromIntegral $ shapeOffset ix) coefficients
       coefficientArr =
         Array.vectorFromList $
         map (\(Term c _) -> realToFrac c) coefficients
   in (coefficientArr, indexArr, rowStarts)

storeConstraints ::
   (Array ShapeInt CDouble, Array ShapeInt CInt, Array ShapeInt FFI.BigIndex) ->
   MC.ContT r IO (Ptr CDouble, Ptr CInt, Ptr FFI.BigIndex)
storeConstraints (coefficients, indices, rowStarts) =
   AppHT.lift3 (,,)
      (withBuffer coefficients)
      (withBuffer indices)
      (withBuffer rowStarts)


setOptimizationDirection :: Ptr FFI.Simplex -> Direction -> IO ()
setOptimizationDirection lp dir =
   FFI.setOptimizationDirection lp $
      case dir of Minimize -> 1; Maximize -> -1


newtype Method = Method {runMethod :: Ptr FFI.Simplex -> IO ()}

dual, primal :: Method
dual = Method $ \lp -> void $ FFI.dual lp 0
primal = Method $ \lp -> void $ FFI.primal lp 0

initialSolve, initialDualSolve, initialPrimalSolve,
   initialBarrierSolve, initialBarrierNoCrossSolve :: Method
initialSolve = Method $ void . FFI.initialSolve
initialDualSolve = Method $ void . FFI.initialDualSolve
initialPrimalSolve = Method $ void . FFI.initialPrimalSolve
initialBarrierSolve = Method $ void . FFI.initialBarrierSolve
initialBarrierNoCrossSolve = Method $ void . FFI.initialBarrierNoCrossSolve


data FailureType =
     PrimalInfeasible
   | DualInfeasible
   | StoppedOnIterations
   | StoppedDueToErrors
   deriving (Eq, Show)

type Result sh = Either FailureType (Double, Array sh Double)

examineStatus :: (Shape.C sh) => sh -> Ptr FFI.Simplex -> IO (Result sh)
examineStatus shape lp = do
   status <- FFI.status lp
   case status of
      0 -> do
         objVal <- FFI.objectiveValue lp
         optVec <-
            ArrayMonadic.unsafeCreateWithSize shape $ \size arrPtr -> do
               optVecPtr <- FFI.getColSolution lp
               for_ (take size [0..]) $ \k ->
                  pokeElemOff arrPtr k . realToFrac
                     =<< peekElemOff optVecPtr k
         return $ Right (realToFrac objVal, optVec)
      1 -> return $ Left PrimalInfeasible
      2 -> return $ Left DualInfeasible
      3 -> return $ Left StoppedOnIterations
      _ -> return $ Left StoppedDueToErrors
