-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Distribution.Internal
-- Copyright   :  (c) A. V. H. McPhail 2015
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL common data types for distributions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Distribution.Internal (
                                RNGHandle
                                ,RNG(..)
                                ,newRNG,seedRNG
                             ) where

-----------------------------------------------------------------------------

import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.C.Types(CULong(..))

-----------------------------------------------------------------------------

data GSL_RNG
type RNGHandle = Ptr GSL_RNG
data RNG = RNG { gsl_rng ::  {-# UNPACK #-} !(ForeignPtr GSL_RNG) }

-----------------------------------------------------------------------------

-- | create a random number generator (RNG) object
newRNG :: IO RNG
newRNG = do
  rng <- rng_new
  rng' <- newForeignPtr rng_free rng
  return $ RNG rng'

-- | assign a new seed to an RNG
seedRNG :: RNG -> Int -> IO ()
seedRNG (RNG rng) s = withForeignPtr rng $ \r -> rng_seed r (fromIntegral s)

-----------------------------------------------------------------------------

foreign import ccall "distribution-aux.h new_rng" rng_new :: IO RNGHandle
foreign import ccall "gsl_rng.h gsl_rng_set" rng_seed :: RNGHandle -> CULong -> IO ()
foreign import ccall "gsl_rng.h &gsl_rng_free" rng_free :: FunPtr (RNGHandle -> IO ())

-----------------------------------------------------------------------------
