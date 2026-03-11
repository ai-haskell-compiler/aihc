-- | A basic interface between Vectors and the fftw library.
module Numeric.FFT.Vector.Base(
            -- * Transforms
            Transform(..),
            planOfType,
            PlanType(..),
            plan,
            run,
            -- * multi-demensional Transforms
            TransformND(..),
            planOfTypeND,
            planND,
            runND,
            -- * Plans
            Plan(),
            planInputSize,
            planOutputSize,
            execute,
            executeM,
            withPlanner,
            -- * Unsafe C stuff
            CFlags,
            CPlan,
            -- * Normalization helpers
            Scalable(..),
            modifyInput,
            modifyOutput,
            constMultOutput,
            multC,
            unsafeModify,
            ) where

import qualified Data.Vector.Storable as VS
import qualified Data.Vector.Storable.Mutable as MS
import Data.Vector.Generic as V hiding (forM_)
import Data.Vector.Generic.Mutable as M hiding (forM_, unsafeModify)
import Data.List as L
import Control.Concurrent.MVar
import Control.Monad.Primitive (RealWorld,PrimMonad(..), PrimBase,
            unsafePrimToPrim, unsafePrimToIO)
import Control.Monad(forM_)
import Foreign (Storable(..), Ptr, FunPtr,
                ForeignPtr, withForeignPtr, newForeignPtr)
import Foreign.C (CInt(..), CUInt, CSize(..))
import Data.Bits ( (.|.) )
import Data.Complex(Complex(..))
import Foreign.Storable.Complex()
import System.IO.Unsafe (unsafePerformIO)



#include <fftw3.h>

---------------------
-- Creating FFTW plans

-- First, the Transform flags:
data PlanType = Estimate | Measure | Patient | Exhaustive
data Preservation = PreserveInput | DestroyInput

type CFlags = CUInt

-- | Marshal the Transform flags for use by fftw.
planInitFlags :: PlanType -> Preservation -> CFlags
planInitFlags pt pr = planTypeInt .|. preservationInt
  where
    planTypeInt = case pt of
                    Estimate -> #const FFTW_ESTIMATE
                    Measure -> #const FFTW_MEASURE
                    Patient -> #const FFTW_PATIENT
                    Exhaustive -> #const FFTW_EXHAUSTIVE
    preservationInt = case pr of
                    PreserveInput -> #const FFTW_PRESERVE_INPUT
                    DestroyInput -> #const FFTW_DESTROY_INPUT

newtype CPlan = CPlan {unCPlan :: ForeignPtr CPlan}

withPlan :: CPlan -> (Ptr CPlan -> IO a) -> IO a
withPlan = withForeignPtr . unCPlan

foreign import ccall unsafe fftw_execute :: Ptr CPlan -> IO ()
foreign import ccall "&" fftw_destroy_plan :: FunPtr (Ptr CPlan -> IO ())

newPlan :: Ptr CPlan -> IO CPlan
newPlan = fmap CPlan . newForeignPtr fftw_destroy_plan

----------------------------------------
-- vector-fftw plans

-- | A 'Plan' can be used to run an @fftw@ algorithm for a specific input/output size.
data Plan a b = Plan {
                    planInput :: {-# UNPACK #-} !(VS.MVector RealWorld a),
                    planOutput :: {-# UNPACK #-} !(VS.MVector RealWorld b),
                    planExecute :: IO ()
                }

-- | The (only) valid input size for this plan.
planInputSize :: Storable a => Plan a b -> Int
planInputSize = MS.length . planInput

-- | The (only) valid output size for this plan.
planOutputSize :: Storable b => Plan a b -> Int
planOutputSize = MS.length . planOutput

-- | Run a plan on the given 'Vector'.
--
-- If @'planInputSize' p /= length v@, then calling
-- @execute p v@ will throw an exception.
execute :: (Vector v a, Vector v b, Storable a, Storable b)
            => Plan a b -> v a -> v b
execute Plan{..} = \v -> -- fudge the arity to make sure it's always inlined
    if n /= V.length v
        then error $ "execute: size mismatch; expected " L.++ show n
                    L.++ ", got " L.++ show (V.length v)
        else unsafePerformIO $ do
                        forM_ [0..n-1] $ \k -> M.unsafeWrite planInput k
                                                $ V.unsafeIndex v k
                        planExecute
                        v' <- unsafeNew m
                        forM_ [0..m-1] $ \k -> M.unsafeRead planOutput k
                                                >>= M.unsafeWrite v' k
                        V.unsafeFreeze v'
  where
    n = MS.length planInput
    m = MS.length planOutput
{-# INLINE execute #-}

-- TODO: decide whether this is actually unsafe.
-- | Run a plan on the given mutable vectors.  The same vector may be used for both
-- input and output.
--
-- If @'planInputSize' p \/= length vIn@ or @'planOutputSize' p \/= length vOut@,
-- then calling @unsafeExecuteM p vIn vOut@ will throw an exception.
executeM :: forall m v a b .
        (PrimBase m, MVector v a, MVector v b, Storable a, Storable b)
            => Plan a b -- ^ The plan to run.
            -> v (PrimState m) a  -- ^ The input vector.
                    -> v (PrimState m) b -- ^ The output vector.
                    -> m ()
executeM Plan{..} = \vIn vOut ->
    if n /= M.length vIn || m /= M.length vOut
        then error $ "executeM: size mismatch; expected " L.++ show (n,m)
                    L.++ ", got " L.++ show (M.length vIn, M.length vOut)
        else unsafePrimToPrim $ act vIn vOut
  where
    n = MS.length planInput
    m = MS.length planOutput

    act :: v (PrimState m) a -> v (PrimState m) b -> IO ()
    act vIn vOut = do
            forM_ [0..n-1] $ \k -> unsafePrimToIO (M.unsafeRead vIn k :: m a)
                                    >>= M.unsafeWrite planInput k
            unsafePrimToPrim planExecute
            forM_ [0..n-1] $ \k -> M.unsafeRead planOutput k
                                    >>= unsafePrimToIO . (M.unsafeWrite vOut k
                                                            :: b -> m ())
{-# INLINE executeM #-}

------------------
-- Malloc/free of fftw array

foreign import ccall unsafe fftw_malloc :: CSize -> IO (Ptr a)
foreign import ccall "&" fftw_free :: FunPtr (Ptr a -> IO ())

newFFTVector :: forall a . Storable a => Int -> IO (MS.MVector RealWorld a)
newFFTVector n = do
    p <- fftw_malloc $ toEnum $ n * sizeOf (undefined :: a)
    fp <- newForeignPtr fftw_free p
    return $ MS.MVector n fp
{-# INLINE newFFTVector #-}


-----------------------
-- Transforms: methods of plan creation.

-- | A transform which may be applied to vectors of different sizes.
data Transform a b = Transform {
                        inputSize :: Int -> Int,
                        outputSize :: Int -> Int,
                        creationSizeFromInput :: Int -> Int,
                        makePlan :: CInt -> Ptr a -> Ptr b -> CFlags -> IO (Ptr CPlan),
                        normalization :: Int -> Plan a b -> Plan a b
                    }

-- | Create a 'Plan' of a specific size for this transform.
planOfType :: (Storable a, Storable b) => PlanType
                                -> Transform a b -> Int -> Plan a b
planOfType ptype Transform{..} n
  | m_in <= 0 || m_out <= 0 = error "Can't (yet) plan for empty arrays!"
  | otherwise  = unsafePerformIO $ do
    planInput <- newFFTVector m_in
    planOutput <- newFFTVector m_out
    MS.unsafeWith planInput $ \inP -> MS.unsafeWith planOutput $ \outP -> do
        pPlan <- makePlan (toEnum n) inP outP $ planInitFlags ptype DestroyInput
        cPlan <- newPlan pPlan
        -- Use unsafeWith here to ensure that the Storable MVectors' ForeignPtrs
        -- aren't released too soon:
        let planExecute = MS.unsafeWith planInput $ \_ ->
                            MS.unsafeWith planOutput $ \_ ->
                                withPlan cPlan fftw_execute
        return $ normalization n $ Plan {..}
  where
    m_in = inputSize n
    m_out = outputSize n
{-# INLINE planOfType #-}

-- | Create a 'Plan' of a specific size.  This function is equivalent to
-- @'planOfType' 'Estimate'@.
plan :: (Storable a, Storable b) => Transform a b -> Int -> Plan a b
plan = planOfType Estimate
{-# INLINE plan #-}

-- | Create and run a 'Plan' for the given transform.
run :: (Vector v a, Vector v b, Storable a, Storable b)
            => Transform a b -> v a -> v b
run p = \v -> execute
            (planOfType Estimate p $ creationSizeFromInput p $ V.length v)
            v
{-# INLINE run #-}

-----------------------
-- TransformND: methods of plan creation for multi-dimensional plans.

-- | A transform which may be applied to vectors of different sizes.
--
-- @since 0.2
data TransformND a b = TransformND {
                        inputSizeND :: Int -> Int,
                        outputSizeND :: Int -> Int,
                        creationSizeFromInputND :: Int -> Int,
                        makePlanND :: CInt -> Ptr CInt -> Ptr a -> Ptr b -> CFlags -> IO (Ptr CPlan),
                        normalizationND :: VS.Vector Int -> Plan a b -> Plan a b
                    }

-- | Create a 'Plan' of a specific size for this transform.
-- 'dims' must have rank greater or equal to 1
--
-- @since 0.2
planOfTypeND :: (Storable a, Storable b) => PlanType
                                -> TransformND a b -> VS.Vector Int -> Plan a b
planOfTypeND ptype TransformND{..} dims
  | m_in <= 0 || m_out <= 0 = error "Can't (yet) plan for empty arrays!"
  | otherwise  = unsafePerformIO $ do
    mdims <- unsafeThaw $ V.map toEnum dims
    planInput <- newFFTVector m_in
    planOutput <- newFFTVector m_out
    MS.unsafeWith mdims $ \dimsP -> MS.unsafeWith planInput $ \inP -> MS.unsafeWith planOutput $ \outP -> do
        pPlan <- makePlanND (toEnum $ V.length dims) dimsP inP outP $ planInitFlags ptype DestroyInput
        cPlan <- newPlan pPlan
        -- Use unsafeWith here to ensure that the Storable MVectors' ForeignPtrs
        -- aren't released too soon:
        let planExecute = MS.unsafeWith planInput $ \_ ->
                                MS.unsafeWith planOutput $ \_ ->
                                withPlan cPlan fftw_execute
        return $ normalizationND dims $ Plan {..}
  where
    m = V.product $ V.init dims
    m_in = m * inputSizeND (V.last dims)
    m_out = m * outputSizeND (V.last dims)
{-# INLINE planOfTypeND #-}

-- | Create a 'Plan' of a specific size.  This function is equivalent to
-- @'planOfType' 'Estimate'@.
--
-- @since 0.2
planND :: (Storable a, Storable b) => TransformND a b -> VS.Vector Int -> Plan a b
planND = planOfTypeND Estimate
{-# INLINE planND #-}

-- | Create and run a 'Plan' for the given transform.
--
-- @since 0.2
runND :: (Vector v a, Vector v b, Storable a, Storable b)
            => TransformND a b -> VS.Vector Int ->  v a -> v b
runND p = \dims v ->
  let creationSize = V.init dims `V.snoc` creationSizeFromInputND p (V.last dims) in
    execute
    (planOfTypeND Estimate p creationSize)
    v
{-# INLINE runND #-}

---------------------------
-- For scaling input/output:

class Scalable a where
    scaleByD :: Double -> a -> a

instance Scalable Double where
    scaleByD = (*)
    {-# INLINE scaleByD #-}

instance Scalable (Complex Double) where
    scaleByD s (x:+y) = s*x :+ s*y
    {-# INLINE scaleByD #-}


{-# INLINE modifyInput #-}
modifyInput :: (MS.MVector RealWorld a -> IO ()) -> Plan a b -> Plan a b
modifyInput f p@Plan{..} = p {planExecute = f planInput >> planExecute}

{-# INLINE modifyOutput #-}
modifyOutput :: (MS.MVector RealWorld b -> IO ()) -> Plan a b -> Plan a b
modifyOutput f p@Plan{..} = p {planExecute = planExecute >> f planOutput}

{-# INLINE constMultOutput #-}
constMultOutput :: (Storable b, Scalable b) => Double -> Plan a b -> Plan a b
constMultOutput !s = modifyOutput (multC s)

{-# INLINE multC #-}
multC :: (Storable a, Scalable a) => Double -> MS.MVector RealWorld a -> IO ()
multC !s v = forM_ [0..n-1] $ \k -> unsafeModify v k (scaleByD s)
  where !n = MS.length v

-- | Helper function; seems like it should be in the vector package...
{-# INLINE unsafeModify #-}
unsafeModify :: (Storable a)
                => MS.MVector RealWorld a -> Int -> (a -> a) -> IO ()
unsafeModify v k f = MS.unsafeRead v k >>= MS.unsafeWrite v k . f

plannerLock :: MVar ()
plannerLock = unsafePerformIO $ newMVar ()
{-# NOINLINE plannerLock #-}

-- | Calls to the FFTW planner are non-reentrant. Here we take a mutex to
-- ensure thread safety.
withPlanner :: IO a -> IO a
withPlanner action = do
    takeMVar plannerLock
    res <- action
    putMVar plannerLock ()
    return res
