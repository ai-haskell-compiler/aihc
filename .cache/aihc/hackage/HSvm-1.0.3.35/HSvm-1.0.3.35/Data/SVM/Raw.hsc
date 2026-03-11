{-|This is a module with raw bindings to libsvm
-}

{-# LANGUAGE ForeignFunctionInterface, GeneralizedNewtypeDeriving, 
             EmptyDataDecls #-}

#include "svm.h"
#include <stddef.h>

#if __GLASGOW_HASKELL__ < 800
#let alignment t = "%lu", (unsigned long)offsetof(struct {char x__; t (y__); }, y__)
#endif

module Data.SVM.Raw where

import Foreign.Storable (Storable(..), peekByteOff, pokeByteOff)
import Foreign.C.Types (CDouble (..), CInt (..))
import Foreign.C.String (CString)
import Foreign.Ptr(nullPtr, Ptr, FunPtr)
import Foreign.ForeignPtr (FinalizerPtr)

data CSvmNode = CSvmNode { 
    index:: CInt,
    value:: CDouble 
}

instance Storable CSvmNode where
    sizeOf _ = #size struct svm_node
    alignment _ = #alignment struct svm_node
    peek ptr = do idx <- (#peek struct svm_node, index) ptr
                  val <- (#peek struct svm_node, value) ptr
                  return $ CSvmNode idx val
    poke ptr (CSvmNode i v) = do (#poke struct svm_node, index) ptr i
                                 (#poke struct svm_node, value) ptr v

data CSvmProblem = CSvmProblem {
    l:: CInt,
    y:: Ptr CDouble,
    x:: Ptr (Ptr CSvmNode)
}       

instance Storable CSvmProblem where
    sizeOf _ = #size struct svm_problem
    alignment _ = #alignment struct svm_problem
    peek ptr = do lp <- (#peek struct svm_problem, l) ptr
                  yp <- (#peek struct svm_problem, y) ptr
                  xp <- (#peek struct svm_problem, x) ptr
                  return $ CSvmProblem lp yp xp
    poke ptr (CSvmProblem lp yp xp) = do (#poke struct svm_problem, l) ptr lp
                                         (#poke struct svm_problem, y) ptr yp
                                         (#poke struct svm_problem, x) ptr xp


newtype CSvmType = CSvmType {unCSvmType :: CInt}
                   deriving (Storable, Show)
#enum CSvmType, CSvmType, C_SVC, NU_SVC, ONE_CLASS, EPSILON_SVR, NU_SVR

newtype CKernelType = CKernelType {unCKernelType :: CInt} 
                      deriving (Storable, Show)
#enum CKernelType, CKernelType, LINEAR, POLY, RBF, SIGMOID, PRECOMPUTED

data CSvmParameter = CSvmParameter {
    svm_type     :: CSvmType,
    kernel_type  :: CKernelType,
    degree       :: CInt,
    gamma        :: CDouble,
    coef0        :: CDouble,
    cache_size   :: CDouble,
    eps          :: CDouble,
    c            :: CDouble,
    nr_weight    :: CInt,
    weight_label :: Ptr CInt,
    weight       :: Ptr CDouble,
    nu           :: CDouble,
    p            :: CDouble,
    shrinking    :: CInt,
    probability  :: CInt
} deriving Show

defaultCParam :: CSvmParameter
defaultCParam = CSvmParameter cSvc rbf 3 0 0 100 1e-3 1 
                              0 nullPtr nullPtr 0.5 0.1 1 0

instance Storable CSvmParameter where
    sizeOf _ = #size struct svm_parameter
    alignment _ = #alignment struct svm_parameter
    peek ptr = do svm_type_p     <- (#peek struct svm_parameter, svm_type) ptr
                  kernel_type_p  <- (#peek struct svm_parameter, kernel_type) ptr
                  degree_p       <- (#peek struct svm_parameter, degree) ptr
                  gamma_p        <- (#peek struct svm_parameter, gamma) ptr
                  coef0_p        <- (#peek struct svm_parameter, coef0) ptr
                  cache_size_p   <- (#peek struct svm_parameter, cache_size) ptr
                  eps_p          <- (#peek struct svm_parameter, eps) ptr
                  c_p            <- (#peek struct svm_parameter, C) ptr
                  nr_weight_p    <- (#peek struct svm_parameter, nr_weight) ptr
                  weight_label_p <- (#peek struct svm_parameter, weight_label) ptr
                  weight_p       <- (#peek struct svm_parameter, weight) ptr
                  nu_p           <- (#peek struct svm_parameter, nu) ptr
                  p_p            <- (#peek struct svm_parameter, p) ptr
                  shrinking_p    <- (#peek struct svm_parameter, degree) ptr
                  probability_p  <- (#peek struct svm_parameter, probability) ptr
                  return $ CSvmParameter svm_type_p kernel_type_p degree_p      
                                gamma_p coef0_p cache_size_p eps_p c_p nr_weight_p
                                weight_label_p weight_p nu_p p_p shrinking_p probability_p
    poke ptr (CSvmParameter svm_type_p kernel_type_p degree_p
                           gamma_p coef0_p cache_size_p eps_p c_p nr_weight_p
                           weight_label_p weight_p nu_p p_p shrinking_p probability_p) =
           do (#poke struct svm_parameter, svm_type) ptr svm_type_p
              (#poke struct svm_parameter, kernel_type) ptr kernel_type_p
              (#poke struct svm_parameter, degree) ptr degree_p
              (#poke struct svm_parameter, gamma) ptr gamma_p
              (#poke struct svm_parameter, coef0) ptr coef0_p
              (#poke struct svm_parameter, cache_size) ptr cache_size_p
              (#poke struct svm_parameter, eps) ptr eps_p
              (#poke struct svm_parameter, C) ptr c_p
              (#poke struct svm_parameter, nr_weight) ptr nr_weight_p
              (#poke struct svm_parameter, weight_label) ptr weight_label_p
              (#poke struct svm_parameter, weight) ptr weight_p
              (#poke struct svm_parameter, nu) ptr nu_p
              (#poke struct svm_parameter, p) ptr p_p
              (#poke struct svm_parameter, shrinking) ptr shrinking_p
              (#poke struct svm_parameter, probability) ptr probability_p

-- |Managed type for struct svm_model.
data CSvmModel

foreign import ccall unsafe "svm.h svm_train" c_svm_train :: Ptr CSvmProblem -> Ptr CSvmParameter -> IO (Ptr CSvmModel)
                        
foreign import ccall unsafe "svm.h svm_cross_validation" c_svm_cross_validation:: Ptr CSvmProblem -> Ptr CSvmParameter -> CInt -> Ptr CDouble -> IO () 

foreign import ccall unsafe "svm.h svm_predict" c_svm_predict :: Ptr CSvmModel -> Ptr CSvmNode -> IO CDouble

foreign import ccall unsafe "svm.h svm_save_model" c_svm_save_model :: CString -> Ptr CSvmModel -> IO CInt

foreign import ccall unsafe "svm.h svm_load_model" c_svm_load_model :: CString -> IO (Ptr CSvmModel)
                        
foreign import ccall unsafe "svm.h svm_check_parameter" c_svm_check_parameter :: Ptr CSvmProblem -> Ptr CSvmParameter -> CString

foreign import ccall unsafe "svm.h &svm_destroy_model" c_svm_destroy_model :: FinalizerPtr CSvmModel

foreign import ccall unsafe "svm.h clone_model_support_vectors" c_clone_model_support_vectors :: Ptr CSvmModel -> IO CInt

type CSvmPrintFn = CString -> IO ()

foreign import ccall unsafe "svm.h svm_set_print_string_function" c_svm_set_print_string_function :: FunPtr CSvmPrintFn -> IO ()
foreign import ccall unsafe "wrapper" createSvmPrintFnPtr :: CSvmPrintFn -> IO (FunPtr CSvmPrintFn)
