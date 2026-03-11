{-# LANGUAGE TypeFamilies #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Discrete
-- Copyright   :  (c) A. V. H. McPhail 2010, 2015, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL discrete random distribution functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Discrete (
                                 OneParamDist(..)
                                , TwoParamDist(..), ThreeParamDist(..)
                                , MultiParamDist(..)
                                , DistFunc(..)
                                , random_1p, random_1p_v, density_1p
                                , random_2p, random_2p_v, density_2p
                                , random_3p, random_3p_v, density_3p
                                , random_mp, density_mp
                             ) where

-----------------------------------------------------------------------------

import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

import Data.Vector.Storable

--import Control.Monad(when)

import Foreign hiding(shift)
--import Foreign.ForeignPtr
--import Foreign.Marshal.Alloc(alloca)
--import Foreign.C.Types(CInt,CUInt,CChar)
import Foreign.C.Types(CInt(..),CUInt(..))
--import Foreign.C.String(newCString,peekCString)

--import GHC.ForeignPtr           (mallocPlainForeignPtrBytes)

--import GHC.Base
--import GHC.IOBase

import Prelude hiding(map)

import Numeric.GSL.Distribution.Common
import Numeric.GSL.Distribution.Internal

import System.IO.Unsafe(unsafePerformIO)

-----------------------------------------------------------------------------

infixr 1 #
a # b = applyRaw a b
{-# INLINE (#) #-}

-----------------------------------------------------------------------------

data OneParamDist = Poisson     -- ^ mean
                  | Bernoulli   -- ^ probability
                  | Geometric   -- ^ probability
                  | Logarithmic -- ^ probability
                    deriving Enum

data TwoParamDist = Binomial     -- ^ probability, successes
                  | NegBinomial  -- ^ probability, successes
                  | Pascal       -- ^ probability, n
                    deriving Enum

data ThreeParamDist = HyperGeometric    -- ^ number type 1, number type 2, samples
                    deriving Enum

data MultiParamDist = Multinomial   -- ^ trials, probabilities
                    deriving Enum

-----------------------------------------------------------------------------

fromei x = fromIntegral (fromEnum x) :: CInt

-----------------------------------------------------------------------------

-- | draw a sample from a one parameter distribution
random_1p :: OneParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter
          -> Word32          -- ^ result
random_1p d s p = unsafePerformIO $
                  alloca $ \r -> do
                      check "random1p" $ distribution_discrete_one_param (fromIntegral s) (fromei d) p r
                      r' <- peek r
                      return $ fromIntegral r'

-- | draw a sample from a one parameter distribution
random_1p_s :: RNG           -- ^ RNG
            -> OneParamDist    -- ^ distribution type
            -> Double          -- ^ parameter
            -> IO Word32       -- ^ result
random_1p_s (RNG rng) d p = do
  alloca $ \r ->
    withForeignPtr rng $ \rg -> do
      check "random1p_s" $ distribution_discrete_one_param_s rg (fromei d) p r
      r' <- peek r
      return $ fromIntegral r'

-- | draw samples from a one parameter distribution
random_1p_v :: OneParamDist    -- ^ distribution type
            -> Int             -- ^ random seed
            -> Double          -- ^ parameter
            -> Int             -- ^ number of samples
            -> Vector Word32   -- ^ result
random_1p_v d s p l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_discrete_one_param_v (fromIntegral s) (fromei d) p) #| "random_1p_v"
   return (map fromIntegral r)

-- | probability of a variate take a value outside the argument
density_1p :: OneParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter
                -> Word32         -- ^ value
                -> Double         -- ^ result
density_1p d f p x = unsafePerformIO $ do
                                       case d of 
                                              Poisson     -> no_inverse f d p x
                                              Geometric   -> no_inverse f d p x
                                              Logarithmic -> pdf_only f d p x
                                              Bernoulli   -> pdf_only f d p x
                                              _           -> distribution_dist_one_param (fromei f) (fromei d) (fromIntegral x) p
    where pdf_only f' d' p' x' = if f' /= Density
                                       then error "no CDF"
                                       else distribution_dist_one_param (fromei f') (fromei d') (fromIntegral x') p'
          no_inverse f' d' p' x' = if (f' == LowInv || f' == UppInv)
                                       then error "No inverse CDF"
                                       else distribution_dist_one_param (fromei f') (fromei d') (fromIntegral x') p'

foreign import ccall "distribution-aux.h discrete1" distribution_discrete_one_param :: CInt -> CInt -> Double -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete1_s" distribution_discrete_one_param_s :: RNGHandle -> CInt -> Double -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete1_v" distribution_discrete_one_param_v :: CInt -> CInt -> Double -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete1_dist" distribution_dist_one_param :: CInt -> CInt -> CUInt -> Double -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a two parameter distribution
random_2p :: TwoParamDist    -- ^ distribution type
          -> Int             -- ^ random seed
          -> Double          -- ^ parameter 1
          -> Word32          -- ^ parameter 2
          -> Word32          -- ^ result
random_2p d s p1 p2  = unsafePerformIO $
                       alloca $ \r -> do
                           check "random2p" $ distribution_discrete_two_param (fromIntegral s) (fromei d) p1 (fromIntegral p2) r
                           r' <- peek r
                           return $ fromIntegral r'

-- | draw a sample from a two parameter distribution
random_2p_s :: RNG           -- ^ RNG
            -> TwoParamDist    -- ^ distribution type
            -> Double          -- ^ parameter 1
            -> Word32          -- ^ parameter 2
            -> IO Word32       -- ^ result
random_2p_s (RNG rng) d p1 p2  = alloca $ \r ->
  withForeignPtr rng $ \rg -> do
    check "random2p_s" $ distribution_discrete_two_param_s rg (fromei d) p1 (fromIntegral p2) r
    r' <- peek r
    return $ fromIntegral r'

-- | draw samples from a two parameter distribution
random_2p_v :: TwoParamDist    -- ^ distribution type
            -> Int             -- ^ random seed
            -> Double          -- ^ parameter 1
            -> Word32          -- ^ parameter 2
            -> Int             -- ^ number of samples
            -> Vector Word32   -- ^ result
random_2p_v d s p1 p2 l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_discrete_two_param_v (fromIntegral s) (fromei d) p1 (fromIntegral p2)) #| "random_2p_v"
   return (map fromIntegral r)

-- | probability of a variate take a value outside the argument
density_2p :: TwoParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Double         -- ^ parameter 1
                -> Word32         -- ^ parameter 2
                -> Word32         -- ^ value
                -> Double         -- ^ result
density_2p d f p1 p2 x = unsafePerformIO $ do
                          case d of
                                 _            -> no_inverse f d p1 p2 x
    where no_inverse f' d' p1' p2' x' = if (f' == LowInv || f' == UppInv)
                                          then error "distribution has no inverse CDF"
                                          else distribution_dist_two_param (fromei f') (fromei d') (fromIntegral x') p1' (fromIntegral p2')

foreign import ccall "distribution-aux.h discrete2" distribution_discrete_two_param :: CInt -> CInt -> Double -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete2_s" distribution_discrete_two_param_s :: RNGHandle -> CInt -> Double -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete2_v" distribution_discrete_two_param_v :: CInt -> CInt -> Double -> CUInt -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete2_dist" distribution_dist_two_param :: CInt -> CInt -> CUInt -> Double -> CUInt -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a three parameter distribution
random_3p :: ThreeParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Word32          -- ^ parameter 1
          -> Word32          -- ^ parameter 2
          -> Word32          -- ^ parameter 3
          -> Word32          -- ^ result
random_3p d s p1 p2 p3 = unsafePerformIO $
                         alloca $ \r -> do
                                 check "random_3p" $ distribution_discrete_three_param (fromIntegral s) (fromei d) (fromIntegral p1) (fromIntegral p2) (fromIntegral p3) r
                                 r' <- peek r
                                 return $ fromIntegral r'

-- | draw a sample from a three parameter distribution
random_3p_s :: RNG           -- ^ RNG
            -> ThreeParamDist  -- ^ distribution type
            -> Word32          -- ^ parameter 1
            -> Word32          -- ^ parameter 2
            -> Word32          -- ^ parameter 3
            -> IO Word32       -- ^ result
random_3p_s (RNG rng) d p1 p2 p3 = alloca $ \r ->
  withForeignPtr rng $ \rg -> do
    check "random_3p_s" $ distribution_discrete_three_param_s rg (fromei d) (fromIntegral p1) (fromIntegral p2) (fromIntegral p3) r
    r' <- peek r
    return $ fromIntegral r'

-- | draw samples from a three parameter distribution
random_3p_v :: ThreeParamDist  -- ^ distribution type
            -> Int             -- ^ random seed
            -> Word32          -- ^ parameter 1
            -> Word32          -- ^ parameter 2
            -> Word32          -- ^ parameter 3
            -> Int             -- ^ number of samples
            -> Vector Word32   -- ^ result
random_3p_v d s p1 p2 p3 l = unsafePerformIO $ do
   r <- createVector l
   (r # id) (distribution_discrete_three_param_v (fromIntegral s) (fromei d) (fromIntegral p1) (fromIntegral p2) (fromIntegral p3)) #| "random_3p_v"
   return (map fromIntegral r)

-- | probability of a variate take a value outside the argument
density_3p :: ThreeParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Word32         -- ^ parameter 1
                -> Word32         -- ^ parameter 2
                -> Word32         -- ^ parameter 3
                -> Word32         -- ^ value
                -> Double         -- ^ result
density_3p d f p1 p2 p3 x = unsafePerformIO $ do
                            case d of
                                 HyperGeometric -> no_inverse f d p1 p2 p3 x
    where no_inverse f' d' p1' p2' p3' x' = if (f' == LowInv || f' == UppInv)
                                       then error "No inverse CDF"
                                       else distribution_dist_three_param (fromei f') (fromei d') (fromIntegral x') (fromIntegral p1') (fromIntegral p2') (fromIntegral p3')

foreign import ccall "distribution-aux.h discrete3" distribution_discrete_three_param :: CInt -> CInt -> CUInt -> CUInt -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete3_s" distribution_discrete_three_param_s :: RNGHandle -> CInt -> CUInt -> CUInt -> CUInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete3_v" distribution_discrete_three_param_v :: CInt -> CInt -> CUInt -> CUInt -> CUInt -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete3_dist" distribution_dist_three_param :: CInt -> CInt -> CUInt -> CUInt -> CUInt -> CUInt -> IO Double

-----------------------------------------------------------------------------

-- | draw a sample from a multi-parameter distribution
random_mp :: MultiParamDist  -- ^ distribution type
          -> Int             -- ^ random seed
          -> Word32          -- ^ trials
          -> Vector Double   -- ^ parameters
          -> Vector Word32   -- ^ result
random_mp d s t p = unsafePerformIO $ do
                    r <- createVector $ size p
                    (p # r # id) (distribution_discrete_multi_param (fromIntegral s) (fromei d) (fromIntegral t)) #| "random_mp"
                    return $ map (\x -> (fromIntegral x) :: Word32) r

-- | draw a sample from a multi-parameter distribution
random_mp_s :: RNG           -- ^ RNG
            -> MultiParamDist  -- ^ distribution type
            -> Word32          -- ^ trials
            -> Vector Double   -- ^ parameters
            -> IO (Vector Word32) -- ^ result
random_mp_s (RNG rng) d t p = withForeignPtr rng $ \rg -> do
  r <- createVector $ size p
  (p # r # id) (distribution_discrete_multi_param_s rg (fromei d) (fromIntegral t)) #| "random_mp_s"
  return $ map (\x -> (fromIntegral x) :: Word32) r

-- | probability of a variate take a value outside the argument
density_mp :: MultiParamDist   -- ^ density type
                -> DistFunc       -- ^ distribution function type
                -> Vector Double  -- ^ parameters
                -> Vector Word32  -- ^ values
                -> Double         -- ^ result
density_mp d f p q = unsafePerformIO $ do
                     case d of
                            Multinomial -> density_only f d p q
    where density_only f' d' p' q' = if f' /= Density
                                     then error "distribution has no CDF"
                                     else alloca $ \r -> do
                                                         (p' # (map (\x -> (fromIntegral x) :: CUInt) q') # id) (distribution_dist_multi_param (fromei f') (fromei d') r) #| "density_mp"
                                                         r' <- peek r
                                                         return r'

foreign import ccall "distribution-aux.h discrete_mp" distribution_discrete_multi_param :: CInt -> CInt -> CUInt -> CInt -> Ptr Double -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete_mp_s" distribution_discrete_multi_param_s :: RNGHandle -> CInt -> CUInt -> CInt -> Ptr Double -> CInt -> Ptr CUInt -> IO CInt
foreign import ccall "distribution-aux.h discrete_mp_dist" distribution_dist_multi_param :: CInt -> CInt -> Ptr Double -> CInt -> Ptr Double -> CInt -> Ptr CUInt -> IO CInt

-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
-----------------------------------------------------------------------------
