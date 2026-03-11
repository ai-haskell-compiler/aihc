{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Continuous
-- Copyright   :  (c) A. V. H. McPhail 2010, 2015, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL continuous random distribution functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Continuous (
                                 ZeroParamDist(..), OneParamDist(..)
                                , TwoParamDist(..), ThreeParamDist(..)
                                , MultiParamDist(..)
                                , BivariateDist(..)
                                , DistFunc(..)
                                , random_0p,random_0p_s,random_0p_v,density_0p
                                , random_1p,random_1p_s,random_1p_v,density_1p
                                , random_2p,random_2p_s,random_2p_v,density_2p
                                , random_3p,random_3p_s,random_3p_v,density_3p
                                , random_mp,random_mp_s,density_mp
                                , random_biv,random_biv_s
                                , random_biv_v,density_biv
                                , spherical_vector 
                             ) where

-----------------------------------------------------------------------------


import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

--import Control.Monad(when)

import Foreign hiding(shift)
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
--import Foreign.C.Types(CInt,CChar)
import Foreign.C.Types(CInt(..))
--import Foreign.C.String(newCString,peekCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

--import Prelude hiding(reverse)

import Numeric.GSL.Distribution.Common
import Numeric.GSL.Distribution.Internal

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = applyRaw a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

data ZeroParamDist = Landau
                   deriving Enum

data OneParamDist = Gaussian    -- ^ standard deviation
                  | Exponential -- ^ mean
                  | Laplace     -- ^ width
                  | Cauchy      -- ^ scale
                  | Rayleigh    -- ^ standard deviation
                  | ChiSq       -- ^ degrees of freedom
                  | TDist       -- ^ degrees of freedom
                  | Logistic    -- ^ scale
                    deriving Enum

data TwoParamDist = GaussianTail -- ^ limit, standard deviation
                  | ExpPower     -- ^ scale, exponent
                  | RayleighTail -- ^ lower limit, standard deviation
                  | Levy         -- ^ scale, exponent
                  | Gamma        -- ^ par1, par2
                  | Uniform      -- ^ lower, upper
                  | Lognormal    -- ^ offset, standard deviation
                  | FDist        -- ^ degrees of freedom, degrees of freedom
                  | Beta         -- ^ parameter a, parameter b
                  | Pareto       -- ^ exponent, scale
                  | Weibull      -- ^ scale, exponent
                  | GumbellI     -- ^ A, B
                  | GumbellII    -- ^ A, B
                    deriving Enum

data ThreeParamDist = LevySkew    -- ^ scale, exponent, skewness
                    deriving Enum

data MultiParamDist = Dirichlet   -- ^ size, alpha
                    deriving Enum

data BivariateDist = BiGaussian  -- ^ standard deviation, standard deviation, correlation coefficient
                    deriving Enum

-----------------------------------------------------------------------------

fromei x = fromIntegral (fromEnum x) :: CInt

-----------------------------------------------------------------------------

-- | draw a sample from a zero parameter distribution
random_0p :: ZeroParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ result
random_0p d s = unsafePerformIO $ distribution_random_zero_param (fromIntegral s) (fromei d)            

-- | draw a sample from a zero parameter distribution
random_0p_s :: RNG             -- ^ the random number generator
            -> ZeroParamDist   -- ^ distribution type
            -> IO Double          -- ^ result
random_0p_s (RNG rng) d = withForeignPtr rng $ \r -> distribution_random_zero_param_s r (fromei d)            

-- | draw samples from a zero parameter distribution
random_0p_v :: ZeroParamDist    -- ^ distribution type
            -> Int             -- ^ random seed
            -> Int             -- ^ number of samples
            -> Vector Double   -- ^ result
random_0p_v  d s l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_random_zero_param_v (fromIntegral s) (fromei d)) #| "random_0p_v"
   return r

-- | probability of a variate take a value outside the argument
density_0p :: ZeroParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ value
                -> Double         -- ^ result
density_0p d f x = unsafePerformIO $ do
                                     case d of
                                            Landau -> density_only f d x
    where density_only f' d' x' = if f' /= Density
                                     then error "distribution has no CDF"
                                     else distribution_dist_zero_param (fromei f') (fromei d') x'

foreign import ccall "distribution-aux.h random0" distribution_random_zero_param :: CInt -> CInt -> IO Double
foreign import ccall "distribution-aux.h random0_s" distribution_random_zero_param_s :: RNGHandle -> CInt -> IO Double
foreign import ccall "distribution-aux.h random0_v" distribution_random_zero_param_v :: CInt -> CInt -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random0_dist" distribution_dist_zero_param :: CInt -> CInt -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a one parameter distribution
random_1p :: OneParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter
          -> Double          -- ^ result
random_1p d s p = unsafePerformIO $
                  alloca $ \r -> do
                      check "random1p" $ distribution_random_one_param (fromIntegral s) (fromei d) p r
                      r' <- peek r
                      return r'

-- | draw a sample from a one parameter distribution
random_1p_s :: RNG             -- ^ the random number generator
            -> OneParamDist    -- ^ distribution type
            -> Double          -- ^ parameter
            -> IO Double       -- ^ result
random_1p_s (RNG rng) d p = alloca $ \r -> do
  check "random_1p_s" $ withForeignPtr rng $ \rg -> distribution_random_one_param_s rg (fromei d) p r
  r' <- peek r
  return r'

-- | draw samples from a one parameter distribution
random_1p_v :: OneParamDist    -- ^ distribution type
            -> Int             -- ^ random seed
            -> Double          -- ^ parameter
            -> Int             -- ^ number of samples
            -> Vector Double   -- ^ result
random_1p_v d s p l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_random_one_param_v (fromIntegral s) (fromei d) p) #| "random_1p_v"
   return r

-- | probability of a variate take a value outside the argument
density_1p :: OneParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter
                -> Double         -- ^ value
                -> Double  -- ^ result
density_1p d f p x = unsafePerformIO $ distribution_dist_one_param (fromei f) (fromei d) x p

foreign import ccall "distribution-aux.h random1" distribution_random_one_param :: CInt -> CInt -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random1_s" distribution_random_one_param_s :: RNGHandle -> CInt -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random1_v" distribution_random_one_param_v :: CInt -> CInt -> Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random1_dist" distribution_dist_one_param :: CInt -> CInt -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a two parameter distribution
random_2p :: TwoParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ result
random_2p d s p1 p2  = unsafePerformIO $
                       alloca $ \r -> do
                           check "random2p" $ distribution_random_two_param (fromIntegral s) (fromei d) p1 p2 r
                           r' <- peek r
                           return r'

-- | draw a sample from a two parameter distribution
random_2p_s :: RNG             -- ^ the random number generator
            -> TwoParamDist    -- ^ distribution type
            -> Double          -- ^ parameter 1
            -> Double          -- ^ parameter 2
            -> IO Double       -- ^ result
random_2p_s (RNG rng) d p1 p2 = alloca $ \r -> do
  check "random_2p_s" $ withForeignPtr rng $ \rg -> distribution_random_two_param_s rg (fromei d) p1 p2 r
  r' <- peek r
  return r'

-- | draw samples from a two parameter distribution
random_2p_v :: TwoParamDist    -- ^ distribution type
            -> Int             -- ^ random seed
            -> Double          -- ^ parameter 1
            -> Double          -- ^ parameter 2
            -> Int             -- ^ number of samples
            -> Vector Double   -- ^ result
random_2p_v d s p1 p2 l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_random_two_param_v (fromIntegral s) (fromei d) p1 p2) #| "random_2p_v"
   return r

-- | probability of a variate take a value outside the argument
density_2p :: TwoParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter 1
                -> Double         -- ^ parameter 2
                -> Double         -- ^ value
                -> Double         -- ^ result
density_2p d f p1 p2 x = unsafePerformIO $ do
                          case d of
                                 GaussianTail -> density_only f d p1 p2 x
                                 ExpPower     -> no_inverse f d p1 p2 x
                                 RayleighTail -> density_only f d p1 p2 x
                                 Levy         -> error "no PDF or CDF for Levy"
                                 _            -> distribution_dist_two_param (fromei f) (fromei d) x p1 p2
    where density_only f' d' p1' p2' x' = if f' /= Density
                                            then error "distribution has no CDF"
                                            else distribution_dist_two_param (fromei f') (fromei d') x' p1' p2'
          no_inverse f' d' p1' p2' x' = if (f' == LowInv || f' == UppInv)
                                          then error "distribution has no inverse CDF"
                                          else distribution_dist_two_param (fromei f') (fromei d') x' p1' p2'

foreign import ccall "distribution-aux.h random2" distribution_random_two_param :: CInt -> CInt -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random2_s" distribution_random_two_param_s :: RNGHandle -> CInt -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random2_v" distribution_random_two_param_v :: CInt -> CInt -> Double -> Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random2_dist" distribution_dist_two_param :: CInt -> CInt -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a three parameter distribution
random_3p :: ThreeParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ parameter 3
          -> Double          -- ^ result
random_3p d s p1 p2 p3 = unsafePerformIO $
                         alloca $ \r -> do
                                 check "random_3p" $ distribution_random_three_param (fromIntegral s) (fromei d) p1 p2 p3 r
                                 r' <- peek r
                                 return r'

-- | draw a sample from a three parameter distribution
random_3p_s :: RNG             -- ^ the random number generator
            -> ThreeParamDist  -- ^ distribution type
            -> Double          -- ^ parameter 1
            -> Double          -- ^ parameter 2
            -> Double          -- ^ parameter 3
            -> IO Double       -- ^ result
random_3p_s (RNG rng) d p1 p2 p3 = alloca $ \r -> do
  check "random_3p_s" $ withForeignPtr rng $ \rg -> distribution_random_three_param_s rg (fromei d) p1 p2 p3 r           
  r' <- peek r
  return r'

-- | draw samples from a three parameter distribution
random_3p_v :: ThreeParamDist  -- ^ distribution type
            -> Int             -- ^ random seed
            -> Double          -- ^ parameter 1
            -> Double          -- ^ parameter 2
            -> Double          -- ^ parameter 3
            -> Int             -- ^ number of samples
            -> Vector Double   -- ^ result
random_3p_v d s p1 p2 p3 l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_random_three_param_v (fromIntegral s) (fromei d) p1 p2 p3) #| "random_3p_v"
   return r

-- | probability of a variate take a value outside the argument
density_3p :: ThreeParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter 1
                -> Double         -- ^ parameter 2
                -> Double         -- ^ parameter 3
                -> Double         -- ^ value
                -> Double         -- ^ result
density_3p d _ _ _ _ _ = unsafePerformIO $ do
                            case d of
                                 LevySkew -> error "Levy skew has no PDF or CDF"
                                 _        -> error "unknown 3 parameter distribution"

foreign import ccall "distribution-aux.h random3" distribution_random_three_param :: CInt -> CInt -> Double -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random3_s" distribution_random_three_param_s :: RNGHandle -> CInt -> Double -> Double -> Double -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random3_v" distribution_random_three_param_v :: CInt -> CInt -> Double -> Double -> Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random3_dist" distribution_dist_three_param :: CInt -> CInt -> Double -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a multi parameter distribution
random_mp :: MultiParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Vector Double   -- ^ parameters
          -> Vector Double   -- ^ result
random_mp d s p = unsafePerformIO $ do
                  r <- createVector $ size p
                  (p # r # id) (distribution_random_multi_param (fromIntegral s) (fromei d)) #| "random_mp"
                  return r

-- | draw a sample from a multi parameter distribution
random_mp_s :: RNG                -- ^ the random number generator
            -> MultiParamDist     -- ^ distribution type
            -> Vector Double      -- ^ parameters
            -> IO (Vector Double) -- ^ result
random_mp_s (RNG rng) d p = do
  let l = size p
  r <- createVector l
  withForeignPtr rng $ \rg -> (p # r # id) (distribution_random_multi_param_s rg (fromei d)) #| "random_mp_s"
  return r
  
-- | probability of a variate take a value outside the argument
density_mp :: MultiParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Vector Double  -- ^ parameters
                -> Vector Double  -- ^ values
                -> Double         -- ^ result
density_mp d f p q = unsafePerformIO $ do
                            case d of
                                 Dirichlet -> density_only f d p q
    where density_only f' d' p' q' = if f' /= Density
                                              then error "distribution has no CDF"
                                              else alloca $ \r -> do
                                                                  (p' # q' # id) (distribution_dist_multi_param (fromei f') (fromei d') r) #| "density_mp"
                                                                  r' <- peek r
                                                                  return r'

foreign import ccall "distribution-aux.h random_mp" distribution_random_multi_param :: CInt -> CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random_mp_s" distribution_random_multi_param_s :: RNGHandle -> CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random_mp_dist" distribution_dist_multi_param :: CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------

-- | draw a sample from a bivariate distribution
random_biv :: BivariateDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Double          -- ^ parameter 2
          -> Double          -- ^ parameter 3
          -> (Double,Double) -- ^ result
random_biv d s p1 p2 p3 = unsafePerformIO $
                         alloca $ \r1 ->
                             alloca $ \r2 -> do
                                 distribution_random_bivariate (fromIntegral s) (fromei d) p1 p2 p3 r1 r2
                                 r1' <- peek r1
                                 r2' <- peek r2
                                 return (r1',r2')

-- | draw a sample from a bivariate distribution
random_biv_s :: RNG                -- ^ the random number generator
            -> BivariateDist      -- ^ distribution type
            -> Double          -- ^ parameter 1
            -> Double          -- ^ parameter 2
            -> Double          -- ^ parameter 3
            -> IO (Double,Double) -- ^ result
random_biv_s (RNG rng) d p1 p2 p3 = do
  alloca $ \r1 ->
    alloca $ \r2 -> do
      withForeignPtr rng $ \r -> distribution_random_bivariate_s r (fromei d) p1 p2 p3 r1 r2
      r1' <- peek r1
      r2' <- peek r2
      return (r1',r2')
  
-- | draw a sample from a bivariate distribution
random_biv_v :: BivariateDist  -- ^ distribution type
             -> Int             -- ^ random seed
             -> Double          -- ^ parameter 1
             -> Double          -- ^ parameter 2
             -> Double          -- ^ parameter 3
             -> Int             -- ^ number of samples
             -> (Vector Double,Vector Double) -- ^ result
random_biv_v d s p1 p2 p3 l = unsafePerformIO $ do
   r1 <- createVector l
   r2 <- createVector l
   (r1 # r2 # id) (distribution_random_bivariate_v (fromIntegral s) (fromei d) p1 p2 p3) #| "random_biv_v"
   return (r1,r2)

-- | probability of a variate take a value outside the argument
density_biv :: BivariateDist      -- ^ density type
                -> DistFunc        -- ^ distribution function type
                -> Double          -- ^ parameter 1
                -> Double          -- ^ parameter 2
                -> Double          -- ^ parameter 3
                -> (Double,Double) -- ^ value
                -> Double          -- ^ result
density_biv d f p1 p2 p3 (x,y) = unsafePerformIO $ do
                            case d of
                                 BiGaussian -> density_only f d p1 p2 p3 x y
    where density_only f' d' p1' p2' p3' x' y' = if f' /= Density
                                                 then error "distribution has no CDF"
                                                 else distribution_dist_bivariate (fromei f') (fromei d') x' y' p1' p2' p3'

foreign import ccall "distribution-aux.h random_biv" distribution_random_bivariate :: CInt -> CInt -> Double -> Double -> Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall "distribution-aux.h random_biv_s" distribution_random_bivariate_s :: RNGHandle -> CInt -> Double -> Double -> Double -> Ptr Double -> Ptr Double -> IO ()
foreign import ccall "distribution-aux.h random_biv_v" distribution_random_bivariate_v :: CInt -> CInt -> Double -> Double -> Double -> CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt
foreign import ccall "distribution-aux.h random_biv_dist" distribution_dist_bivariate :: CInt -> CInt -> Double -> Double -> Double -> Double -> Double -> IO Double

-----------------------------------------------------------------------------

-- | returns a normalised random direction vector from a multivariate gaussian distribution
spherical_vector :: Int           -- ^ seed
                 -> Int           -- ^ vector size
                 -> Vector Double -- result
spherical_vector s vs = unsafePerformIO $ do
                        r <- createVector vs
                        (r # id) (distribution_spherical_vector (fromIntegral s)) #| "spherical_vector"
                        return r

foreign import ccall "distribution-aux.h spherical_vector" distribution_spherical_vector :: CInt -> CInt -> Ptr Double -> IO CInt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
