-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      : Math.NumberTheory.Prime.Count
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
--
-- This module provides a high-level, polymorphic interface to the primecount
-- library. For a lower-level interface, see
-- "Math.NumberTheory.Prime.Count.FFI#".
module Math.NumberTheory.Prime.Count
  ( primePi,
    primePiMaxBound,
    nthPrime,
    nthPrimeMaxBound,
    primePhi,
    getNumPrimecountThreads,
    setNumPrimecountThreads,
    primecountVersion,
  )
where

import Data.Int (Int64)
import Foreign.C.String (CString, peekCString, withCString)
import Foreign.C.Types (CSize)
import Foreign.Marshal.Array (allocaArray)
import Math.NumberTheory.Prime.Count.FFI
import System.IO.Unsafe (unsafePerformIO)
import Text.Read (readMaybe)

-- | The number of primes less than or equal to @n@. Throws an error if @n@
--   is larger than 'primePiMaxBound'. Also might throw an error if there's not
--   enough memory available to compute the result, which can happen even if @n@
--   is smaller than @primePiMaxBound@.
primePi :: (Integral a) => a -> a
primePi n
  | n < 0 = 0
  | n' <= bound = fromIntegral (primecount_pi (fromInteger n'))
  | n' <= primePiMaxBound = fromInteger (primePiStr n')
  | otherwise = error "primePi: input larger than primePiMaxBound"
  where
    bound :: Integer
    bound = toInteger (maxBound :: Int64)

    n' :: Integer
    n' = toInteger n

-- | The largest input supported by 'primePi'.
--
-- * 64-bit CPUs: @10^31@
-- * 32-bit CPUs: @2^63 - 1@
primePiMaxBound :: Integer
primePiMaxBound = read $ unsafePerformIO $ peekCString primecount_get_max_x
{-# NOINLINE primePiMaxBound #-}

primePiStr :: Integer -> Integer
primePiStr n = unsafePerformIO $ do
  withCString (show n) $ \nString -> do
    let len = 32 :: CSize
    allocaArray (fromIntegral len) $ \(res :: CString) -> do
      ret <- primecount_pi_str nString res len
      if ret < 0 || ret > fromIntegral len
        then error "primePi: call to primecount_pi_str failed"
        else do
          answer <- peekCString res
          maybe
            (error "primePi: couldn't parse result of primecount_pi_str")
            pure
            (readMaybe answer)
{-# NOINLINE primePiStr #-}

-- | The nth prime, starting at @nthPrime 1 == 2@.
--
--    * Throws an error if the input is less than 1.
--    * Throws an error if the input is larger than 'nthPrimeMaxBound'.
nthPrime :: (Integral a) => a -> a
nthPrime n
  | n < 1 = error "nthPrime: n must be >= 1"
  | n' > bound = error "nthPrime: answer cannot be packed into a 64-bit int"
  | otherwise = fromIntegral (primecount_nth_prime (fromInteger n'))
  where
    bound :: Integer
    bound = 216289611853439384

    n' :: Integer
    n' = toInteger n

-- | The largest input supported by 'nthPrime', equal to
--   @primePi ('maxBound' :: 'Int64') == 216289611853439384@.
nthPrimeMaxBound :: Integer
nthPrimeMaxBound = 216289611853439384

-- | @primePhi n a@ counts the number of positive integers @<= n@ that are not
--    divisible by any of the first @a@ primes. Throws an error if @n@ is larger
--    than @'maxBound' :: 'Int64'@.
primePhi :: (Integral a) => a -> a -> a
primePhi n a
  | n <= 0 = 0
  | a <= 0 = n
  | n' > bound = error "primePhi: input cannot be packed into a 64-bit int"
  | otherwise = fromIntegral (primecount_phi (fromInteger n') (fromInteger a'))
  where
    bound :: Integer
    bound = toInteger (maxBound :: Int64)

    n' :: Integer
    n' = toInteger n

    a' :: Integer
    a' = min bound (toInteger a)

-- | Get the currently set number of threads used by @libprimecount@.
getNumPrimecountThreads :: IO Int
getNumPrimecountThreads = primecount_get_num_threads

-- | Set the number of threads used by @libprimecount@. If the input is not
--   positive, the thread count is set to 1. If the input is greater than the
--   number of CPUs available, the thread count is set to the number of CPUs
--   available.
setNumPrimecountThreads :: Int -> IO ()
setNumPrimecountThreads = primecount_set_num_threads

-- | Get the @libprimecount@ version number, in the form @"i.j"@
primecountVersion :: String
primecountVersion = unsafePerformIO (peekCString primecount_version)
{-# NOINLINE primecountVersion #-}
