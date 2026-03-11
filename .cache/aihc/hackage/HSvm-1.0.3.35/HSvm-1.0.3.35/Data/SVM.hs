-- |
-- This module provides a safe bindings to libsvm functions and structures with implicit memory handling.
module Data.SVM
  ( Vector,
    Problem,
    KernelType (..),
    Algorithm (..),
    ExtraParam (..),
    Model,
    train,
    train',
    crossValidate,
    crossValidate',
    loadModel,
    saveModel,
    predict,
    withPrintFn,
    CSvmPrintFn,
  )
where

import Control.Exception
import Control.Monad (when)
import Data.IntMap (IntMap, toList)
import qualified Data.IntMap as M
import Data.SVM.Raw
  ( CSvmModel,
    CSvmNode (..),
    CSvmParameter,
    CSvmPrintFn,
    CSvmProblem (..),
    c_clone_model_support_vectors,
    c_svm_check_parameter,
    c_svm_cross_validation,
    c_svm_destroy_model,
    c_svm_load_model,
    c_svm_predict,
    c_svm_save_model,
    c_svm_set_print_string_function,
    c_svm_train,
    createSvmPrintFnPtr,
    defaultCParam,
  )
import qualified Data.SVM.Raw as R
import Foreign.C.String (newCString, peekCString)
import Foreign.ForeignPtr
  ( ForeignPtr,
    newForeignPtr,
    withForeignPtr,
  )
import Foreign.Marshal.Alloc (alloca, free, malloc)
import Foreign.Marshal.Array
  ( allocaArray,
    newArray,
    newArray0,
    peekArray,
    withArray0,
  )
import Foreign.Ptr (Ptr, freeHaskellFunPtr, nullPtr)
import Foreign.Storable (peek, poke)

-- | Vector type provides a sparse implementation of vector. It uses IntMap as underlying implementation.
type Vector = IntMap Double

-- | SVM problem is a list of maps from training vectors to 1.0 or -1.0
type Problem = [(Double, Vector)]

-- | 'Model' is a wrapper over foreign pointer to 'CSvmModel'
newtype Model = Model (ForeignPtr CSvmModel)

-- | Kernel function for SVM algorithm.
data KernelType
  = -- | Linear kernel function, i.e. dot product
    Linear
  | -- | Gaussian radial basis function with parameter 'gamma'
    RBF {gamma :: Double}
  | -- | Sigmoid kernel function
    Sigmoid {gamma :: Double, coef0 :: Double}
  | -- | Inhomogeneous polynomial function
    Poly {gamma :: Double, coef0 :: Double, degree :: Int}

-- | SVM Algorithm with parameters
data Algorithm
  = -- | c-SVC algorithm
    CSvc {c :: Double}
  | -- | nu-SVC algorithm
    NuSvc {nu :: Double}
  | -- | nu-SVR algorithm
    NuSvr {nu :: Double, c :: Double}
  | -- | eps-SVR algorithm
    EpsilonSvr {epsilon :: Double, c :: Double}
  | -- | One class SVM
    OneClassSvm {nu :: Double}

-- | Extra parameters of SVM implementation
data ExtraParam = ExtraParam
  { cacheSize :: Double,
    shrinking :: Int,
    probability :: Int
  }

-- | Default extra parameters of SVM implamentation
defaultExtra :: ExtraParam
defaultExtra = ExtraParam {cacheSize = 1000, shrinking = 1, probability = 0}

mergeKernel :: KernelType -> CSvmParameter -> CSvmParameter
mergeKernel Linear p = p {R.kernel_type = R.linear}
mergeKernel (RBF g) p =
  p
    { R.kernel_type = R.rbf,
      R.gamma = realToFrac g
    }
mergeKernel (Sigmoid g cf) p =
  p
    { R.kernel_type = R.sigmoid,
      R.gamma = realToFrac g,
      R.coef0 = realToFrac cf
    }
mergeKernel (Poly g cf d) p =
  p
    { R.kernel_type = R.poly,
      R.gamma = realToFrac g,
      R.coef0 = realToFrac cf,
      R.degree = fromIntegral d
    }

mergeAlgo :: Algorithm -> CSvmParameter -> CSvmParameter
mergeAlgo (CSvc cf) p =
  p
    { R.svm_type = R.cSvc,
      R.c = realToFrac cf
    }
mergeAlgo (NuSvc n) p =
  p
    { R.svm_type = R.nuSvc,
      R.nu = realToFrac n
    }
mergeAlgo (NuSvr n cf) p =
  p
    { R.svm_type = R.nuSvr,
      R.nu = realToFrac n,
      R.c = realToFrac cf
    }
mergeAlgo (EpsilonSvr e cf) p =
  p
    { R.svm_type = R.epsilonSvr,
      R.eps = realToFrac e,
      R.c = realToFrac cf
    }
mergeAlgo (OneClassSvm n) p =
  p
    { R.svm_type = R.oneClass,
      R.nu = realToFrac n
    }

mergeExtra :: ExtraParam -> CSvmParameter -> CSvmParameter
mergeExtra (ExtraParam cf s pr) p =
  p
    { R.cache_size = realToFrac cf,
      R.shrinking = fromIntegral s,
      R.probability = fromIntegral pr
    }

-------------------------------------------------------------------------------

convertToNodeArray :: Vector -> [CSvmNode]
convertToNodeArray = map convertNode . toList . M.filter (/= 0)
  where
    convertNode (key, val) = CSvmNode (fromIntegral key) (realToFrac val)

endMarker :: CSvmNode
endMarker = CSvmNode (-1) 0.0

newCSvmNodeArray :: Vector -> IO (Ptr CSvmNode)
newCSvmNodeArray v = newArray0 endMarker (convertToNodeArray v)

withCSvmNodeArray :: Vector -> (Ptr CSvmNode -> IO a) -> IO a
withCSvmNodeArray v = withArray0 endMarker (convertToNodeArray v)

newCSvmProblem :: Problem -> IO (Ptr CSvmProblem)
newCSvmProblem lvs = do
  nodePtrList <- mapM (newCSvmNodeArray . snd) lvs
  nodePtrPtr <- newArray nodePtrList
  labelPtr <- newArray (map (realToFrac . fst) lvs)
  let z = fromIntegral . length $ lvs
  ptr <- malloc
  poke ptr $ CSvmProblem z labelPtr nodePtrPtr
  return ptr

freeCSVmProblem :: Ptr CSvmProblem -> IO ()
freeCSVmProblem ptr = do
  prob <- peek ptr
  free $ y prob
  vecList <- peekArray (fromIntegral $ l prob) (x prob)
  mapM_ free vecList
  free $ x prob
  free ptr

withProblem :: Problem -> (Ptr CSvmProblem -> IO a) -> IO a
withProblem prob = bracket (newCSvmProblem prob) freeCSVmProblem

---

withParam ::
  ExtraParam ->
  Algorithm ->
  KernelType ->
  (Ptr CSvmParameter -> IO a) ->
  IO a
withParam extra algo kern f =
  let merge = mergeAlgo algo . mergeKernel kern . mergeExtra extra
      param = merge defaultCParam
   in alloca $ \paramPtr -> poke paramPtr param >> f paramPtr

checkParam :: Ptr CSvmProblem -> Ptr CSvmParameter -> IO ()
checkParam probPtr paramPtr = do
  let errStr = c_svm_check_parameter probPtr paramPtr
  when (errStr /= nullPtr) $ peekCString errStr >>= error . ("svm: " ++)

--

-- | Like 'train' but with extra parameters
train' :: ExtraParam -> Algorithm -> KernelType -> Problem -> IO Model
train' extra algo kern prob =
  withProblem prob $ \probPtr ->
    withParam extra algo kern $ \paramPtr -> do
      checkParam probPtr paramPtr
      modelPtr <- c_svm_train probPtr paramPtr
      _ <- c_clone_model_support_vectors modelPtr
      modelForeignPtr <- newForeignPtr c_svm_destroy_model modelPtr
      return $ Model modelForeignPtr

-- | The 'train' function allows training a 'Model' starting from a 'Problem'
-- by specifying an 'Algorithm' and a 'KernelType'
train :: Algorithm -> KernelType -> Problem -> IO Model
train = train' defaultExtra

-- | Like 'crossvalidate' but with extra parameters
crossValidate' ::
  ExtraParam ->
  Algorithm ->
  KernelType ->
  Problem ->
  Int ->
  IO [Double]
crossValidate' extra algo kern prob nFold =
  withProblem prob $ \probPtr ->
    withParam extra algo kern $ \paramPtr -> do
      probLen <- (fromIntegral . R.l) `fmap` peek probPtr
      allocaArray probLen $ \targetPtr -> do
        -- (length prob is inefficient)
        checkParam probPtr paramPtr
        let c_nFold = fromIntegral nFold
        c_svm_cross_validation probPtr paramPtr c_nFold targetPtr
        map realToFrac `fmap` peekArray probLen targetPtr

-- | Stratified cross validation
crossValidate :: Algorithm -> KernelType -> Problem -> Int -> IO [Double]
crossValidate = crossValidate' defaultExtra

-----------------------------------------------------------------------

-- | Save model to the file
saveModel :: Model -> FilePath -> IO ()
saveModel (Model modelForeignPtr) path =
  withForeignPtr modelForeignPtr $ \modelPtr -> do
    pathString <- newCString path
    ret <- c_svm_save_model pathString modelPtr
    when (ret /= 0) $ error $ "svm: error saving the model:" ++ show ret

-- | Load model from the file
loadModel :: FilePath -> IO Model
loadModel path = do
  modelPtr <- c_svm_load_model =<< newCString path
  Model `fmap` newForeignPtr c_svm_destroy_model modelPtr

-- | Predict a value for 'Vector' by using 'Model'
predict :: Model -> Vector -> IO Double
predict (Model modelForeignPtr) vector = action
  where
    action :: IO Double
    action = withForeignPtr modelForeignPtr $ \modelPtr ->
      withCSvmNodeArray vector (fmap realToFrac . c_svm_predict modelPtr)

-- | Wrapper to change the libsvm output reporting function.
--
--  libsvm by default writes some statistics to stdout. If you don't
--  want any output from libsvm, you can do e.g.:
--
--  >>> withPrintFn (\_ -> return ()) $ train (NuSvc 0.25) (RBF 1) feats
withPrintFn :: CSvmPrintFn -> IO a -> IO a
withPrintFn printfn body =
  bracket
    ( do
        c_printfn <- createSvmPrintFnPtr printfn
        c_svm_set_print_string_function c_printfn
        return c_printfn
    )
    freeHaskellFunPtr
    (const body)
