-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Fitting.Linear
-- Copyright   :  (c) A. V. H. McPhail 2010, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL linear regression functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Fitting.Linear (
                                   linear, linear_w, linear_est,
                                   multifit, multifit_w, multifit_est
                             ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

--import Numeric.LinearAlgebra.Linear

--import Control.Monad(when)

import Foreign
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
import Foreign.C.Types(CInt(..))
--import Foreign.C.String(newCString,peekCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

--import Prelude hiding(reverse)

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = applyRaw a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

-- | fits the model Y = C X
linear :: Vector Double    -- ^ x data
       -> Vector Double    -- ^ y data
       -> (Double,Double,Double,Double,Double,Double) -- ^ (c_0,c_1,cov_00,cov_01,cov_11,chi_sq)
linear x y = unsafePerformIO $ do
           alloca $ \c0 ->
               alloca $ \c1 ->
                   alloca $ \chi_sq -> 
                       alloca $ \cov00 -> 
                           alloca $ \cov01 -> 
                               alloca $ \cov11 -> do
                                                  (x # y # id) (fitting_linear c0 c1 chi_sq cov00 cov01 cov11) #| "linear"
                                                  c0' <- peek c0
                                                  c1' <- peek c1
                                                  cov00' <- peek cov00
                                                  cov01' <- peek cov01
                                                  cov11' <- peek cov11
                                                  chi_sq' <- peek chi_sq
                                                  return (c0',c1',cov00',cov01',cov11',chi_sq')


-----------------------------------------------------------------------------

foreign import ccall "fitting-aux.h linear" fitting_linear :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | fits the model Y = C X, with x data weighted
linear_w :: Vector Double    -- ^ x data
         -> Vector Double    -- ^ x weights
         -> Vector Double    -- ^ y data
         -> (Double,Double,Double,Double,Double,Double) -- ^ (c_0,c_1,cov_00,cov_01,cov_11,chi_sq)
linear_w x w y = unsafePerformIO $ do
             alloca $ \c0 ->
                 alloca $ \c1 ->
                     alloca $ \chi_sq ->
                       alloca $ \cov00 -> 
                           alloca $ \cov01 -> 
                               alloca $ \cov11 -> do
                                                  (x # w # y # id) (fitting_linear_w c0 c1 chi_sq cov00 cov01 cov11) #| "linear_w"
                                                  c0' <- peek c0
                                                  c1' <- peek c1
                                                  cov00' <- peek cov00
                                                  cov01' <- peek cov01
                                                  cov11' <- peek cov11
                                                  chi_sq' <- peek chi_sq
                                                  return (c0',c1',cov00',cov01',cov11',chi_sq')

-----------------------------------------------------------------------------

foreign import ccall "fitting-aux.h linear_weighted" fitting_linear_w :: Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | computes the fitted function and standard deviation at the input point
linear_est :: Double          -- ^ x data point
           -> Double          -- ^ c0
           -> Double          -- ^ c1
           -> Double          -- ^ cov00
           -> Double          -- ^ cov01
           -> Double          -- ^ cov11
           -> (Double,Double) -- ^ (y,error)
linear_est x c0 c1 cov00 cov01 cov11 = unsafePerformIO $ do
                                       alloca $ \y ->
                                           alloca $ \e -> do
                                               check "linear_est" $ fitting_linear_est x c0 c1 cov00 cov01 cov11 y e
                                               y' <- peek y
                                               e' <- peek e
                                               return (y',e')

-----------------------------------------------------------------------------

foreign import ccall "fitting-aux.h linear_estimate" fitting_linear_est :: Double -> Double -> Double -> Double -> Double -> Double -> Ptr Double -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | fit the model Y = C X, with design matrix X
-- |    X is a design matrix X_{ij} = x_j(i) with i observations and p predictors 
-- |      a polynomial would be X_{ij} = x_i^j
-- |      a fourier series would be X_{ij} = sin (\omega_j x_i)
multifit :: Matrix Double          -- ^ design matrix (X)
         -> Vector Double          -- ^ observations
         -> (Vector Double,Matrix Double,Double)   -- ^ (coefficients,covariance,chi_sq)
multifit x y = unsafePerformIO $ do
               let p = cols x
               cov <- createMatrix RowMajor p p
               c <- createVector p
               alloca$ \chi_sq -> do
                   (cmat x # y # c # cov # id) (fitting_multifit chi_sq) #| "multifit"
                   chi_sq' <- peek chi_sq
                   return (c,cov,chi_sq')

-----------------------------------------------------------------------------

foreign import ccall "fitting_aux.h multifit" fitting_multifit :: Ptr Double -> CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | fit the model Y = C X, with design matrix X, and x weighted
multifit_w :: Matrix Double          -- ^ design matrix (X)
           -> Vector Double          -- ^ weights 
           -> Vector Double          -- ^ observations
           -> (Vector Double,Matrix Double,Double)   -- ^ (coefficients,covariance,chi_sq)
multifit_w x w y = unsafePerformIO $ do
                   let p = cols x
                   cov <- createMatrix RowMajor p p
                   c <- createVector p
                   alloca$ \chi_sq -> do
                       (cmat x # w # y # c # cov # id) (fitting_multifit_w chi_sq) #| "multifit"
                       chi_sq' <- peek chi_sq
                       return (c,cov,chi_sq')

-----------------------------------------------------------------------------

foreign import ccall "fitting_aux.h multifit_weighted" fitting_multifit_w :: Ptr Double -> CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | computes the fitted function and standard deviation at the input point
multifit_est :: Vector Double     -- ^ input point
             -> Vector Double     -- ^ the coefficients
             -> Matrix Double     -- ^ the covariance matrix
             -> (Double,Double)   -- ^ (y,y_error)
multifit_est x c cov = unsafePerformIO $ do
                       alloca $ \y ->
                           alloca $ \e -> do
                               (x # c # cov # id) (fitting_multifit_est y e) #| "multifit_estimate"
                               y' <- peek y
                               e' <- peek e
                               return (y',e')

-----------------------------------------------------------------------------

foreign import ccall "fitting_aux.h multifit_estimate" fitting_multifit_est :: Ptr Double -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------
