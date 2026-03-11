{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Math.FFT.Base (
   module Math.FFT.Base,
   module Math.FFT.FFI,
   ) where

import Math.FFT.FFI

import qualified Foreign.C.Types as C
import Foreign.C.String (withCString, peekCString)
import Foreign.Marshal.Array (copyArray, withArray, withArrayLen)
import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr, nullPtr)
import Foreign.Storable (Storable)
import Foreign.Storable.Complex ()
import System.IO.Unsafe (unsafePerformIO)

import Control.Arrow (second)
import Control.Monad (when)
import Control.Applicative ((<$>))
import Control.Exception (assert, finally)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)

import Data.Array.CArray
          (CArray, withCArray, unsafeForeignPtrToCArray,
           Ix, Shapable, shape, rank, size, rangeSize)
import Data.Array.CArray.Base (mallocForeignPtrArrayAligned, mapCArrayInPlace)
import Data.Ix.Shapable (shapeToStride, sBounds)
import Data.Complex (Complex)
import Data.Bits (Bits, complement, (.&.), (.|.))
import Data.List (nub, (\\))
import Data.Typeable ()


-- | Our API is polymorphic over the real data type.  FFTW, at least in
-- principle, supports single precision 'Float', double precision 'Double' and
-- long double 'CLDouble' (presumable?).
class (Storable a, RealFloat a) => FFTWReal a where
    plan_guru_dft   :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex a)
                    -> Ptr (Complex a) -> FFTWSign -> FFTWFlag -> IO Plan
    plan_guru_dft_r2c :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr a
                      -> Ptr (Complex a) -> FFTWFlag -> IO Plan
    plan_guru_dft_c2r :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr (Complex a)
                      -> Ptr a -> FFTWFlag -> IO Plan
    plan_guru_r2r :: C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> Ptr a
                  -> Ptr a -> Ptr FFTWKind -> FFTWFlag -> IO Plan

-- | Using this instance requires linking with @-lfftw3f@.
instance FFTWReal Float where
    plan_guru_dft = cf_plan_guru_dft
    plan_guru_dft_r2c = cf_plan_guru_dft_r2c
    plan_guru_dft_c2r = cf_plan_guru_dft_c2r
    plan_guru_r2r = cf_plan_guru_r2r

-- | Using this instance requires linking with @-lfftw3@.
instance FFTWReal Double where
    plan_guru_dft = c_plan_guru_dft
    plan_guru_dft_r2c = c_plan_guru_dft_r2c
    plan_guru_dft_c2r = c_plan_guru_dft_c2r
    plan_guru_r2r = c_plan_guru_r2r

-- | This lock must be taken during /planning/ of any transform.  The FFTW
-- library is not thread-safe in the planning phase.  Thankfully, the lock is
-- not needed during the execute phase.
lock :: MVar ()
lock = unsafePerformIO $ newMVar ()
{-# NOINLINE lock #-}

withLock :: IO a -> IO a
withLock = withMVar lock . const

-- | The 'Flag' type is used to influence the kind of plans which are created.
-- To specify multiple flags, use a bitwise '.|.'.
newtype Flag = Flag { unFlag :: FFTWFlag }
    deriving (Eq, Show, Num, Bits)

-- | Default flag.  For most transforms, this is equivalent to setting 'measure'
-- and 'preserveInput'.  The exceptions are complex to real and half-complex to
-- real transforms.
nullFlag :: Flag
nullFlag = Flag 0

--
-- Algorithm restriction flags
--

-- | Allows FFTW to overwrite the input array with arbitrary data; this can
-- sometimes allow more efficient algorithms to be employed.
--
-- Setting this flag implies that two memory allocations will be done, one for
-- work space, and one for the result.  When 'estimate' is not set, we will be
-- doing two memory allocations anyway, so we set this flag as well (since we
-- don't retain the work array anyway).
destroyInput :: Flag
destroyInput = Flag c_destroy_input

-- | 'preserveInput' specifies that an out-of-place transform must not change
-- its input array. This is ordinarily the default, except for complex to real
-- transforms for which 'destroyInput' is the default. In the latter cases,
-- passing 'preserveInput' will attempt to use algorithms that do not destroy
-- the input, at the expense of worse performance; for multi-dimensional complex
-- to real transforms, however, no input-preserving algorithms are implemented
-- so the Haskell bindings will set 'destroyInput' and do a transform with two
-- memory allocations.
preserveInput :: Flag
preserveInput = Flag c_preserve_input

-- | Instruct FFTW not to generate a plan which uses SIMD instructions, even if
-- the memory you are planning with is aligned.  This should only be needed if
-- you are using the guru interface and want to reuse a plan with memory that
-- may be unaligned (i.e. you constructed the 'CArray' with
-- 'unsafeForeignPtrToCArray').
unaligned :: Flag
unaligned = Flag c_unaligned

-- | The header claims that this flag is documented, but in reality, it is not.
-- I don't know what it does and it is here only for completeness.
conserveMemory :: Flag
conserveMemory = Flag c_conserve_memory

--
-- Planning rigor flags
--

-- | 'estimate' specifies that, instead of actual measurements of different
-- algorithms, a simple heuristic is used to pick a (probably sub-optimal) plan
-- quickly. With this flag, the input/output arrays are not overwritten during
-- planning.
--
-- This is the only planner flag for which a single memory allocation is possible.
estimate :: Flag
estimate = Flag c_estimate

-- | 'measure' tells FFTW to find an optimized plan by actually computing
-- several FFTs and measuring their execution time. Depending on your machine,
-- this can take some time (often a few seconds). 'measure' is the default
-- planning option.
measure :: Flag
measure = Flag c_measure

-- | 'patient' is like 'measure', but considers a wider range of algorithms and
-- often produces a "more optimal" plan (especially for large transforms), but
-- at the expense of several times longer planning time (especially for large
-- transforms).
patient :: Flag
patient = Flag c_patient

-- | 'exhaustive' is like 'patient' but considers an even wider range of
-- algorithms, including many that we think are unlikely to be fast, to
-- produce the most optimal plan but with a substantially increased planning
-- time.
exhaustive :: Flag
exhaustive = Flag c_exhaustive

-- | Determine which direction of DFT to execute.
data Sign = DFTForward | DFTBackward
    deriving (Eq,Show)

unSign :: Sign -> FFTWSign
unSign DFTForward = c_forward
unSign DFTBackward = c_backward

-- | Real to Real transform kinds.
data Kind = R2HC | HC2R                             -- half-complex transforms
          | DHT                                     -- discrete Hartley transformm
          | REDFT00 | REDFT10 | REDFT01 | REDFT11   -- discrete cosine transforms
          | RODFT00 | RODFT01 | RODFT10 | RODFT11   -- discrete sine transforms
    deriving (Eq,Show)

unKind :: Kind -> FFTWKind
unKind k = case k of
               R2HC -> c_r2hc
               HC2R -> c_hc2r
               DHT -> c_dht
               REDFT00 -> c_redft00
               REDFT10 -> c_redft10
               REDFT01 -> c_redft01
               REDFT11 -> c_redft11
               RODFT00 -> c_rodft00
               RODFT01 -> c_rodft01
               RODFT10 -> c_rodft10
               RODFT11 -> c_rodft11

-- | Tuple of transform dimensions and non-transform dimensions of the array.
type TSpec = ([IODim],[IODim])

-- | Types of transforms.  Used to control 'dftShape'.
data DFT = CC | RC | CR | CRO | RR
    deriving (Eq, Show)

-- | Verify that a plan is valid.  Throws an exception if not.
check :: Plan -> IO ()
check p = when (p == nullPtr) . ioError $ userError "invalid plan"

-- | Confirm that the plan is valid, then execute the transform.
execute :: Plan -> IO ()
execute p = check p >> c_execute p

-- | In-place normalization outside of IO.  You must be able to prove that no
-- reference to the original can be retained.
unsafeNormalize :: (Ix i, Shapable i, Fractional e, Storable e)
                   => [Int] -> CArray i e -> CArray i e
unsafeNormalize tdims a = mapCArrayInPlace (* s) a
    where s = 1 / fromIntegral (product $ map (shape a !!) tdims)

-- | Normalized general complex DFT
dftG :: (FFTWReal r, Ix i, Shapable i) => Sign -> Flag -> [Int] -> CArray i (Complex r) -> CArray i (Complex r)
dftG s f tdims ain = case s of 
    DFTForward -> dftGU s f tdims ain
    DFTBackward -> unsafeNormalize tdims (dftGU s f tdims ain)

-- | Normalized general complex to real DFT where the last transformed dimension
-- is logically even.
dftCRG :: (FFTWReal r, Ix i, Shapable i) => Flag -> [Int] -> CArray i (Complex r) -> CArray i r
dftCRG f tdims ain = unsafeNormalize tdims (dftCRGU f tdims ain)

-- | Normalized general complex to real DFT where the last transformed dimension
-- is logicall odd.
dftCROG :: (FFTWReal r, Ix i, Shapable i) => Flag -> [Int] -> CArray i (Complex r) -> CArray i r
dftCROG f tdims ain = unsafeNormalize tdims (dftCROGU f tdims ain)

-- | Multi-dimensional forward DFT.
dftN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i (Complex r) -> CArray i (Complex r)
dftN = dftG DFTForward estimate
-- | Multi-dimensional inverse DFT.
idftN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i (Complex r) -> CArray i (Complex r)
idftN = dftG DFTBackward estimate
-- | Multi-dimensional forward DFT of real data.
dftRCN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i (Complex r)
dftRCN = dftRCG estimate
-- | Multi-dimensional inverse DFT of Hermitian-symmetric data (where only the
-- non-negative frequencies are given).
dftCRN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i (Complex r) -> CArray i r
dftCRN = dftCRG estimate
-- | Multi-dimensional inverse DFT of Hermitian-symmetric data (where only the
-- non-negative frequencies are given) and the last transformed dimension is
-- logically odd.
dftCRON :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i (Complex r) -> CArray i r
dftCRON = dftCROG estimate

fzr :: b -> [a] -> [(a,b)]
fzr = flip zip . repeat
drr :: (FFTWReal r, Ix i, Shapable i) => Kind -> [Int] -> CArray i r -> CArray i r
drr = (dftRRN .) . fzr

-- | Multi-dimensional real to real transform.  The result is not normalized.
dftRRN :: (FFTWReal r, Ix i, Shapable i) => [(Int,Kind)] -> CArray i r -> CArray i r
dftRRN = dftRRG estimate

--
-- The following do the same type of transform in each dimension specified.
--
-- | Multi-dimensional real to half-complex transform.  The result is not normalized.
dftRHN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dftRHN = drr R2HC
-- | Multi-dimensional half-complex to real transform.  The result is not normalized.
dftHRN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dftHRN = drr HC2R
-- | Multi-dimensional Discrete Hartley Transform.  The result is not normalized.
dhtN :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dhtN = drr DHT
-- | Multi-dimensional Type 1 discrete cosine transform.
dct1N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dct1N = drr REDFT00
-- | Multi-dimensional Type 2 discrete cosine transform.  This is commonly known
-- as /the/ DCT.
dct2N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dct2N = drr REDFT10
-- | Multi-dimensional Type 3 discrete cosine transform.  This is commonly known
-- as /the/ inverse DCT.  The result is not normalized.
dct3N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dct3N = drr REDFT01
-- | Multi-dimensional Type 4 discrete cosine transform.
dct4N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dct4N = drr REDFT11
-- | Multi-dimensional Type 1 discrete sine transform.
dst1N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dst1N = drr RODFT00
-- | Multi-dimensional Type 2 discrete sine transform.
dst2N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dst2N = drr RODFT10
-- | Multi-dimensional Type 3 discrete sine transform.
dst3N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dst3N = drr RODFT01
-- | Multi-dimensional Type 4 discrete sine transform.
dst4N :: (FFTWReal r, Ix i, Shapable i) => [Int] -> CArray i r -> CArray i r
dst4N = drr RODFT11

--
-- Transform in the first dimension only.
--

-- | 1-dimensional complex DFT.
dft :: (FFTWReal r, Ix i, Shapable i) => CArray i (Complex r) -> CArray i (Complex r)
dft    = dftN    [0]
-- | 1-dimensional complex inverse DFT.  Inverse of 'dft'.
idft :: (FFTWReal r, Ix i, Shapable i) => CArray i (Complex r) -> CArray i (Complex r)
idft   = idftN   [0]
-- | 1-dimensional real to complex DFT.
dftRC :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i (Complex r)
dftRC  = dftRCN  [0]
-- | 1-dimensional complex to real DFT with logically even dimension.  Inverse of 'dftRC'.
dftCR :: (FFTWReal r, Ix i, Shapable i) => CArray i (Complex r) -> CArray i r
dftCR  = dftCRN  [0]
-- | 1-dimensional complex to real DFT with logically odd dimension.  Inverse of 'dftRC'.
dftCRO :: (FFTWReal r, Ix i, Shapable i) => CArray i (Complex r) -> CArray i r
dftCRO = dftCRON [0]
-- | 1-dimensional real to half-complex DFT.
dftRH :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dftRH  = dftRHN  [0]
-- | 1-dimensional half-complex to real DFT.  Inverse of 'dftRH' after normalization.
dftHR :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dftHR  = dftHRN  [0]
-- | 1-dimensional Discrete Hartley Transform.  Self-inverse after normalization.
dht :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dht    = dhtN    [0]
-- | 1-dimensional Type 1 discrete cosine transform.
dct1 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dct1   = dct1N   [0]
-- | 1-dimensional Type 2 discrete cosine transform.  This is commonly known as /the/ DCT.
dct2 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dct2   = dct2N   [0]
-- | 1-dimensional Type 3 discrete cosine transform.  This is commonly known as /the/ inverse DCT.
dct3 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dct3   = dct3N   [0]
-- | 1-dimensional Type 4 discrete cosine transform.
dct4 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dct4   = dct4N   [0]
-- | 1-dimensional Type 1 discrete sine transform.
dst1 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dst1   = dst1N   [0]
-- | 1-dimensional Type 2 discrete sine transform.
dst2 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dst2   = dst2N   [0]
-- | 1-dimensional Type 3 discrete sine transform.
dst3 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dst3   = dst3N   [0]
-- | 1-dimensional Type 4 discrete sine transform.
dst4 :: (FFTWReal r, Ix i, Shapable i) => CArray i r -> CArray i r
dst4   = dst4N   [0]

-- Check if a flag is set.
infix 7 `has`
has :: Flag -> Flag -> Bool
a `has` b = a .&. b == b

-- | Try to transform a CArray with only one memory allocation (for the result).
-- If we can find a way to prove that FFTW already has a sufficiently good plan
-- for this transform size and the input will not be overwritten, then we could
-- call have a version of this that does not require 'estimate'.  Since this is
-- not currently the case, we require 'estimate' to be set.  Note that we do not
-- check for the 'preserveInput' flag here.  This is because the default is to
-- preserve input for all but the C->R and HC->R transforms.  Therefore, this
-- function must not be called for those transforms, unless 'preserveInput' is
-- set.
{-# NOINLINE transformCArray #-}
transformCArray :: (Ix i, Storable a, Storable b)
                   => Flag -> CArray i a -> (i,i) -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan) -> CArray i b
transformCArray f a lu planner = if f `has` estimate
                                 && not (any (f `has`) [patient, exhaustive])
                                 then go else transformCArray' f a lu planner
    where go = unsafePerformIO $ do
              ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
              withCArray a $ \ip ->
                  withForeignPtr ofp $ \op -> do
                      p <- withLock $ planner (unFlag f) ip op
                      execute p
              unsafeForeignPtrToCArray ofp lu

-- | Transform a CArray with two memory allocations.  This is entirely safe with
-- all transforms, but it must allocate a temporary array to do the planning in.
{-# NOINLINE transformCArray' #-}
transformCArray' :: (Ix i, Storable a, Storable b)
                    => Flag -> CArray i a -> (i,i) -> (FFTWFlag -> Ptr a -> Ptr b -> IO Plan) -> CArray i b
transformCArray' f a lu planner = unsafePerformIO $ do
    ofp <- mallocForeignPtrArrayAligned (rangeSize lu)
    wfp <- mallocForeignPtrArrayAligned sz
    withCArray a $ \ip ->
        withForeignPtr ofp $ \op ->
            withForeignPtr wfp $ \wp -> do
                p <- withLock $ planner (unFlag f') wp op
                copyArray wp ip sz
                execute p
    unsafeForeignPtrToCArray ofp lu
    where sz = size a
          f' = f .&. complement preserveInput .|. destroyInput

-- | All the logic for determining shape of resulting array, and how to do the transform.
dftShape :: (Ix i, Shapable i, Storable e)
             => DFT -> [Int] -> CArray i e -> ((i,i),TSpec)
dftShape t tdims a = assert valid (oBounds,tspec)
    where shp = shape a
          rnk = rank a
          strides = shapeToStride shp
          valid = not (null tdims) && 0 <= minimum tdims
                  && maximum tdims < rnk && nub tdims == tdims
          tspec = (map (d !!) tdims, map (d !!) ([0 .. rnk - 1] \\ tdims))
              where d = zipWith3 IODim lShape strides oStrides
          oShape = adjust f ldim shp -- Physical shape of the output array
              where f = case t of
                            RC  -> (\n -> n `div` 2 + 1)
                            CR  -> (\n -> (n - 1) * 2)
                            CRO -> (\n -> (n - 1) * 2 + 1)
                            _   -> id
          lShape = adjust f ldim shp -- Logical shape of the output array
              where f = case t of
                            CR  -> (\n -> (n - 1) * 2)
                            CRO -> (\n -> (n - 1) * 2 + 1)
                            _   -> id
          oBounds = sBounds oShape
          oStrides = shapeToStride oShape
          ldim = last tdims

-- | A simple helper.
withTSpec :: TSpec -> (C.CInt -> Ptr IODim -> C.CInt -> Ptr IODim -> IO a) -> IO a
withTSpec (dims,dims') f = withArrayLen dims $ \r ds ->
                           withArrayLen dims' $ \hr hds ->
                           f (fromIntegral r) ds (fromIntegral hr) hds

-- | A generally useful list utility
adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = uncurry (++) . second (\(x:xs) -> f x : xs) . splitAt i

-- | Complex to Complex DFT, un-normalized.
dftGU :: (FFTWReal r, Ix i, Shapable i) => Sign -> Flag -> [Int] -> CArray i (Complex r) -> CArray i (Complex r)
dftGU s f tdims ain = transformCArray f ain bds go
    where go f' ip op = withTSpec tspec $ \r ds hr hds ->
                        plan_guru_dft r ds hr hds ip op (unSign s) f'
          (bds,tspec) = dftShape CC tdims ain

-- | Real to Complex DFT.
dftRCG :: (FFTWReal r, Ix i, Shapable i) => Flag -> [Int] -> CArray i r -> CArray i (Complex r)
dftRCG f tdims ain = transformCArray f ain bds go
    where go f' ip op = withTSpec tspec $ \r ds hr hds ->
                        plan_guru_dft_r2c r ds hr hds ip op f'
          (bds,tspec) = dftShape RC tdims ain

-- | Complex to Real DFT.  The first argument determines whether the last
-- transformed dimension is logically odd or even.  'True' implies the dimension
-- is odd.
dftCRG_ :: (FFTWReal r, Ix i, Shapable i) => Bool -> Flag -> [Int] -> CArray i (Complex r) -> CArray i r
dftCRG_ isOdd f tdims ain = tCArr f ain bds go
    where go f' ip op = withTSpec tspec $ \r ds hr hds ->
                        plan_guru_dft_c2r r ds hr hds ip op f'
          (bds,tspec) = dftShape (if isOdd then CRO else CR) tdims ain
          tCArr = if length tdims == 1 && f `has` preserveInput
                  -- A multi-dimensional C->R transform destroys its input.
                  -- Also, a one-dimensional transform is faster if it can
                  -- destroy input.
                  then transformCArray
                  else transformCArray'

-- | Complex to Real DFT where last transformed dimension is logically even.
dftCRGU :: (FFTWReal r, Ix i, Shapable i) => Flag -> [Int] -> CArray i (Complex r) -> CArray i r
dftCRGU = dftCRG_ False

-- | Complex to Real DFT where last transformed dimension is logically odd.
dftCROGU :: (FFTWReal r, Ix i, Shapable i) => Flag -> [Int] -> CArray i (Complex r) -> CArray i r
dftCROGU = dftCRG_ True

-- | Real to Real transforms.
dftRRG :: (FFTWReal r, Ix i, Shapable i) => Flag -> [(Int,Kind)] -> CArray i r -> CArray i r
dftRRG f tk ain = tCArr f ain bds go
    where go f' ip op = withTSpec tspec $ \r ds hr hds ->
                        withArray (map unKind ks) $ \pk ->
                            plan_guru_r2r r ds hr hds ip op pk f'
          (bds,tspec) = dftShape RR tdims ain
          (tdims,ks) = unzip tk
          tCArr = if any (== HC2R) ks && not (f `has` preserveInput)
                  then transformCArray'
                  else transformCArray

-- | Queries the FFTW cache.  The 'String' can be written to a file so the
-- wisdom can be reused on a subsequent run.
exportWisdomString :: IO String
exportWisdomString = do
    pc <- c_export_wisdom_string
    peekCString pc `finally` c_free pc

-- | Add wisdom to the FFTW cache.  Returns 'True' if it is successful.
importWisdomString :: String -> IO Bool
importWisdomString str =
    (==1) <$> withCString str c_import_wisdom_string

-- | Tries to import wisdom from a global source, typically @/etc/fftw/wisdom@.
-- Returns 'True' if it was successful.
importWisdomSystem :: IO Bool
importWisdomSystem = (==1) <$> c_import_wisdom_system
