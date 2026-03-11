{-# LANGUAGE ForeignFunctionInterface #-}
module Numeric.COINOR.CLP.FFI where

import qualified Foreign.C.Types as C
import Foreign.C.String (CString)
import Foreign.Ptr (Ptr)

import Data.Int (Int32)


type CDouble = C.CDouble
type CInt = C.CInt
type CBool = C.CBool


#include "Clp_C_Interface.h"

type BigIndex = #{type CoinBigIndex}

data CoinPackedMatrix = CoinPackedMatrix
data PlusMinusOneMatrix = PlusMinusOneMatrix
data PackedMatrix = PackedMatrix
data MatrixBase = MatrixBase
data Simplex = Simplex


foreign import ccall "Clp_newPlusMinusOneMatrix"
   newPlusMinusOneMatrix ::
      CInt -> CInt -> CBool -> Ptr CInt ->
      Ptr BigIndex -> Ptr BigIndex ->
      IO (Ptr PlusMinusOneMatrix)

foreign import ccall "Clp_deletePlusMinusOneMatrix"
   deletePlusMinusOneMatrix ::
      Ptr PlusMinusOneMatrix -> IO ()

foreign import ccall "Clp_loadProblemFromMatrix" 
   loadProblemFromMatrix ::
      Ptr Simplex ->
      Ptr matrix ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      IO ()


foreign import ccall "Clp_newCoinPackedMatrix"
   newCoinPackedMatrix ::
      CBool -> CInt -> CInt -> BigIndex ->
      Ptr CDouble -> Ptr CInt -> Ptr BigIndex -> Ptr CInt ->
      IO (Ptr CoinPackedMatrix)

foreign import ccall "Clp_deleteCoinPackedMatrix"
   deleteCoinPackedMatrix ::
      Ptr CoinPackedMatrix -> IO ()

foreign import ccall "Clp_loadProblemFromCoinMatrix" 
   loadProblemFromCoinMatrix ::
      Ptr Simplex ->
      Ptr CoinPackedMatrix ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      IO ()


foreign import ccall "Clp_newModel"
   newModel :: IO (Ptr Simplex)

foreign import ccall "Clp_deleteModel"
   deleteModel :: Ptr Simplex -> IO ()

foreign import ccall "Clp_loadProblem"
   loadProblem ::
      Ptr Simplex ->
      CInt -> CInt ->
      Ptr BigIndex -> Ptr CInt ->
      Ptr CDouble ->
      Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      Ptr CDouble -> Ptr CDouble ->
      IO ()

foreign import ccall "Clp_setOptimizationDirection"
   setOptimizationDirection :: Ptr Simplex -> CDouble -> IO ()

foreign import ccall "Clp_chgObjCoefficients"
   chgObjCoefficients :: Ptr Simplex -> Ptr CDouble -> IO ()

foreign import ccall "Clp_addRows"
   addRows ::
      Ptr Simplex -> CInt -> Ptr CDouble -> Ptr CDouble ->
      Ptr BigIndex -> Ptr CInt -> Ptr CDouble -> IO ()

foreign import ccall "Clp_addColumns"
   addColumns ::
      Ptr Simplex -> CInt -> Ptr CDouble -> Ptr CDouble ->
      Ptr CDouble ->
      Ptr BigIndex -> Ptr CInt -> Ptr CDouble -> IO ()


foreign import ccall "Clp_initialSolve"
   initialSolve :: Ptr Simplex -> IO CInt
foreign import ccall "Clp_initialDualSolve"
   initialDualSolve :: Ptr Simplex -> IO CInt
foreign import ccall "Clp_initialPrimalSolve"
   initialPrimalSolve :: Ptr Simplex -> IO CInt
foreign import ccall "Clp_initialBarrierSolve"
   initialBarrierSolve :: Ptr Simplex -> IO CInt
foreign import ccall "Clp_initialBarrierNoCrossSolve"
   initialBarrierNoCrossSolve :: Ptr Simplex -> IO CInt

foreign import ccall "Clp_dual"
   dual :: Ptr Simplex -> CInt -> IO CInt

foreign import ccall "Clp_primal"
   primal :: Ptr Simplex -> CInt -> IO CInt


foreign import ccall "Clp_objectiveValue"
   objectiveValue :: Ptr Simplex -> IO CDouble

foreign import ccall "Clp_getColSolution"
   getColSolution :: Ptr Simplex -> IO (Ptr CDouble)


foreign import ccall "Clp_status"
   status :: Ptr Simplex -> IO CInt
foreign import ccall "Clp_secondaryStatus"
   secondaryStatus :: Ptr Simplex -> IO CInt
{- Are there a numerical difficulties? -}
foreign import ccall "Clp_isAbandoned"
   isAbandoned :: Ptr Simplex -> IO CInt
{- Is optimality proven? -}
foreign import ccall "Clp_isProvenOptimal"
   isProvenOptimal :: Ptr Simplex -> IO CInt
{- Is primal infeasiblity proven? -}
foreign import ccall "Clp_isProvenPrimalInfeasible"
   isProvenPrimalInfeasible :: Ptr Simplex -> IO CInt
{- Is dual infeasiblity proven? -}
foreign import ccall "Clp_isProvenDualInfeasible"
   isProvenDualInfeasible :: Ptr Simplex -> IO CInt
{- Is the given primal objective limit reached? -}
foreign import ccall "Clp_isPrimalObjectiveLimitReached"
   isPrimalObjectiveLimitReached :: Ptr Simplex -> IO CInt
{- Is the given dual objective limit reached? -}
foreign import ccall "Clp_isDualObjectiveLimitReached"
   isDualObjectiveLimitReached :: Ptr Simplex -> IO CInt
{- Iteration limit reached? -}
foreign import ccall "Clp_isIterationLimitReached"
   isIterationLimitReached :: Ptr Simplex -> IO CInt


foreign import ccall "Clp_setLogLevel"
   setLogLevel :: Ptr Simplex -> CInt -> IO ()

foreign import ccall "Clp_dumpMatrix"
   dumpMatrix :: Ptr Simplex -> IO ()


newtype CoinorBool = CoinorBool CInt

foreign import ccall "Clp_readMps"
   readMps :: Ptr Simplex -> CString -> CoinorBool -> CoinorBool -> IO CInt

foreign import ccall "Clp_writeMps"
   writeMps :: Ptr Simplex -> CString -> CInt -> CInt -> CDouble -> IO CInt
