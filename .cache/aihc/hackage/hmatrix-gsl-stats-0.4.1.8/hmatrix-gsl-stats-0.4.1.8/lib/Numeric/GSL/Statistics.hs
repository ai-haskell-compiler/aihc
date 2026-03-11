{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Statistics
-- Copyright   :  (c) A. V. H. McPhail 2010, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL statistics functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Statistics (
                               mean
                              , variance,variance_m,variance_pm
                              , stddev,stddev_m,stddev_pm
                              , tot_sumsq,tot_sumsq_m
                              , absdev, absdev_m
                              , skew, skew_m_sd
                              , kurtosis, kurtosis_m_sd
                              --
                              , mean_w
                              , variance_w,variance_w_m,variance_w_pm
                              , stddev_w,stddev_w_m,stddev_w_pm
                              , tot_sumsq_w,tot_sumsq_w_m
                              , absdev_w, absdev_w_m
                              , skew_w, skew_w_m_sd
                              , kurtosis_w, kurtosis_w_m_sd
                              --
                              , lag1auto
                              , covariance, covariance_m
                              , correlation
                              --
                              , median, quantile
                ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

--import Numeric.GSL.Vector
--import Numeric.LinearAlgebra.Instances()
--import Numeric.LinearAlgebra.Linear(Linear(..))

import Foreign
import Foreign.C.Types(CInt(..))
--import Foreign.Marshal.Alloc(alloca)

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = apply a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

type PD = Ptr Double

-----------------------------------------------------------------------------

getD1 :: (Ptr Double -> CInt -> Ptr Double -> IO CInt) -> String -> Vector Double -> Double
getD1 f s v = unsafePerformIO $ do
                alloca $ \r -> do
                   (v # id) (f r) #| s
                   r' <- peek r
                   return r'

getD2 :: (Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt) -> String -> Vector Double -> Vector Double -> Double
getD2 f s v w = unsafePerformIO $ do
                   alloca $ \r -> do
                      (v # w # id) (f r) #| s
                      r' <- peek r
                      return r'

-----------------------------------------------------------------------------

-- | the mean of the elements of a vector
mean :: Vector Double -> Double
mean = getD1 statistics_mean "mean"

-- | the sample variance
variance :: Vector Double -> Double
variance = getD1 statistics_variance "variance"

-- | the sample variance given the precomputed mean
variance_m :: Double -> Vector Double -> Double
variance_m m = getD1 (statistics_variance_m m) "variance_m"

-- | the population variance given the a priori mean
variance_pm :: Double -> Vector Double -> Double
variance_pm m = getD1 (statistics_var_with_fixed_m m) "variance_pm"

-- | the sample standard deviation
stddev :: Vector Double -> Double
stddev = getD1 statistics_stddev "stddev"

-- | the sample standard deviation given the precomputed mean
stddev_m :: Double -> Vector Double -> Double
stddev_m m = getD1 (statistics_stddev_m m) "stddev_m"

-- | the population standard deviation given the a priori mean
stddev_pm :: Double -> Vector Double -> Double
stddev_pm m = getD1 (statistics_stddev_with_fixed_m m) "stddev_pm"

-- | the total sum of squares about the mean
tot_sumsq :: Vector Double -> Double
tot_sumsq = getD1 statistics_tot_sumsq "tot_sumsq"

-- | the total sum of squares about the precomputed mean
tot_sumsq_m :: Double -> Vector Double -> Double
tot_sumsq_m m = getD1 (statistics_tot_sumsq_m m) "totsumsq_m"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h mean" statistics_mean :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h variance" statistics_variance :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h variance_m" statistics_variance_m :: Double -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h var_with_fixed_m" statistics_var_with_fixed_m :: Double -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h stddev" statistics_stddev :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h stddev_m" statistics_stddev_m :: Double -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h stddev_with_fixed_m" statistics_stddev_with_fixed_m :: Double -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h tot_sumsq" statistics_tot_sumsq :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h tot_sumsq_m" statistics_tot_sumsq_m :: Double -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the absolute deviation from the mean
absdev :: Vector Double -> Double
absdev = getD1 statistics_absdev "absdev"

-- | the absolute deviation from the precomputed mean
absdev_m :: Double -> Vector Double -> Double
absdev_m m = getD1 (statistics_absdev_m m) "absdev_m"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h absdev" statistics_absdev :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h absdev_m" statistics_absdev_m :: Double -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the skewness of the data (asymmetry of tails)
skew :: Vector Double -> Double
skew = getD1 statistics_skew "skew"

-- | the skewness of the data (asymmetry of tails) with precomputed mean and sd
skew_m_sd :: Double -> Double -> Vector Double -> Double
skew_m_sd m sd = getD1 (statistics_skew_m_sd m sd) "skew_m_sd"

-- | the kurtosis of the data (sharpness of peak relative to width)
kurtosis :: Vector Double -> Double
kurtosis = getD1 statistics_kurtosis "kurtosis"

-- | the kurtosis of the data (sharpness of peak relative to width) with precomputed mean and sd
kurtosis_m_sd :: Double -> Double -> Vector Double -> Double
kurtosis_m_sd m sd = getD1 (statistics_kurtosis_m_sd m sd) "kurtosis_m_sd"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h skew" statistics_skew :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h skew_m_sd" statistics_skew_m_sd :: Double -> Double -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h kurtosis" statistics_kurtosis :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h kurtosis_m_sd" statistics_kurtosis_m_sd :: Double -> Double -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the lag-1 autocorrelation of the data
lag1auto :: Vector Double -> Double
lag1auto = getD1 statistics_lag1auto "lag1auto"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h lag1_autocorrelation" statistics_lag1auto :: PD -> CInt -> PD -> IO CInt

----------------------------------------------------------------------------

-- | the covariance of two datasets of the same length
covariance :: Vector Double -> Vector Double -> Double
covariance = getD2 statistics_covariance "covariance"

-- | the covariance of two datasets of the same length
covariance_m :: Double -> Double -> Vector Double -> Vector Double -> Double
covariance_m m1 m2 = getD2 (statistics_covariance_m m1 m2) "covariance_m"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h covariance" statistics_covariance :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h covariance_m" statistics_covariance_m :: Double -> Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the Pearson correlation of two datasets of the same length
correlation :: Vector Double -> Vector Double -> Double
correlation = getD2 statistics_correlation "correlation"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h correlation" statistics_correlation :: PD -> CInt -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the weighted mean of the elements of a vector
mean_w :: Vector Double -- ^ weights
       -> Vector Double -- ^ dataset
       -> Double
mean_w = getD2 statistics_w_mean "w_mean"

-- | the weighted sample variance
variance_w :: Vector Double -> Vector Double -> Double
variance_w = getD2 statistics_w_variance "w_variance"

-- | the weighted sample variance given the precomputed mean
variance_w_m :: Double -> Vector Double -> Vector Double -> Double
variance_w_m m = getD2 (statistics_w_variance_m m) "w_variance_m"

-- | the weighted population variance given the a priori mean
variance_w_pm :: Double -> Vector Double -> Vector Double -> Double
variance_w_pm m = getD2 (statistics_w_var_with_fixed_m m) "w_variance_pm"

-- | the weighted sample standard deviation
stddev_w :: Vector Double -> Vector Double -> Double
stddev_w = getD2 statistics_w_stddev "w_stddev"

-- | the weighted sample standard deviation given the precomputed mean
stddev_w_m :: Double -> Vector Double -> Vector Double -> Double
stddev_w_m m = getD2 (statistics_w_stddev_m m) "w_stddev_m"

-- | the weighted population standard deviation given the a priori mean
stddev_w_pm :: Double -> Vector Double -> Vector Double -> Double
stddev_w_pm m = getD2 (statistics_w_stddev_with_fixed_m m) "w_stddev_pm"

-- | the weighted total sum of squares about the mean
tot_sumsq_w :: Vector Double -> Vector Double -> Double
tot_sumsq_w = getD2 statistics_w_tot_sumsq "w_tot_sumsq"

-- | the weighted total sum of squares about the precomputed mean
tot_sumsq_w_m :: Double -> Vector Double -> Vector Double -> Double
tot_sumsq_w_m m = getD2 (statistics_w_tot_sumsq_m m) "w_totsumsq_m"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h w_mean" statistics_w_mean :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_variance" statistics_w_variance :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_variance_m" statistics_w_variance_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_var_with_fixed_m" statistics_w_var_with_fixed_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_stddev" statistics_w_stddev :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_stddev_m" statistics_w_stddev_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_stddev_with_fixed_m" statistics_w_stddev_with_fixed_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_tot_sumsq" statistics_w_tot_sumsq :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_tot_sumsq_m" statistics_w_tot_sumsq_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the weighted absolute deviation from the mean
absdev_w :: Vector Double -> Vector Double -> Double
absdev_w = getD2 statistics_w_absdev "w_absdev"

-- | the weighted absolute deviation from the precomputed mean
absdev_w_m :: Double -> Vector Double -> Vector Double -> Double
absdev_w_m m = getD2 (statistics_w_absdev_m m) "w_absdev_m"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h w_absdev" statistics_w_absdev :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_absdev_m" statistics_w_absdev_m :: Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the weighted skewness of the data (asymmetry of tails)
skew_w :: Vector Double -> Vector Double -> Double
skew_w = getD2 statistics_w_skew "w_skew"

-- | the weighted skewness of the data (asymmetry of tails) with precomputed mean and sd
skew_w_m_sd :: Double -> Double -> Vector Double -> Vector Double -> Double
skew_w_m_sd m sd = getD2 (statistics_w_skew_m_sd m sd) "w_skew_m_sd"

-- | the weighted kurtosis of the data (sharpness of peak relative to width)
kurtosis_w :: Vector Double -> Vector Double -> Double
kurtosis_w = getD2 statistics_w_kurtosis "w_kurtosis"

-- | the weighted kurtosis of the data (sharpness of peak relative to width) with precomputed mean and sd
kurtosis_w_m_sd :: Double -> Double -> Vector Double -> Vector Double -> Double
kurtosis_w_m_sd m sd = getD2 (statistics_w_kurtosis_m_sd m sd) "w_kurtosis_m_sd"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h w_skew" statistics_w_skew :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_skew_m_sd" statistics_w_skew_m_sd :: Double -> Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_kurtosis" statistics_w_kurtosis :: PD -> CInt -> PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h w_kurtosis_m_sd" statistics_w_kurtosis_m_sd :: Double -> Double -> PD -> CInt -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------

-- | the median value of the dataset, which must be sorted
median :: Vector Double -> Double
median = getD1 statistics_median "median"

-- | the quantile value of the dataset, which must be sorted
quantile :: Double        -- ^ the desired quantile from [0..1]
         -> Vector Double -- ^ the dataset
         -> Double
quantile f = getD1 (statistics_quantile f) "quantile"

-----------------------------------------------------------------------------

foreign import ccall "statistics-aux.h median" statistics_median :: PD -> CInt -> PD -> IO CInt
foreign import ccall "statistics-aux.h quantile" statistics_quantile :: Double -> PD -> CInt -> PD -> IO CInt

-----------------------------------------------------------------------------


