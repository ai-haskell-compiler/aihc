-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause

-- |
-- Module      : Math.NumberTheory.Prime.Count.FFI
-- License     : BSD-3-Clause
-- Maintainer  : libraries@mail.preetham.io
-- Stability   : experimental
--
-- This module provides direct access to the C API of the primecount library.
-- It's recommended that you use the higher-level interface in
-- "Math.NumberTheory.Prime.Count#".
--
-- Documentation adapted from the [C API reference]
-- (https://github.com/kimwalisch/primecount/blob/master/doc/libprimecount.md#c-api-reference)
-- and [@primecount.h@]
-- (https://github.com/kimwalisch/primecount/blob/master/include/primecount.h).
module Math.NumberTheory.Prime.Count.FFI
  ( primecount_pi,
    primecount_pi_str,
    primecount_nth_prime,
    primecount_phi,
    primecount_get_max_x,
    primecount_get_num_threads,
    primecount_set_num_threads,
    primecount_version,
  )
where

import Data.Int (Int64)
import Foreign.C.String (CString)
import Foreign.C.Types (CSize (..))

-- | Count the number of primes @<= x@.
foreign import ccall unsafe "primecount_pi"
  primecount_pi :: Int64 -> Int64

-- | Count the number of primes <= x (supports 128-bit).
foreign import ccall unsafe "primecount_pi_str"
  primecount_pi_str :: CString -> CString -> CSize -> IO Int

-- | Find the @n@th prime e.g.: @primecount_nth_prime 25 == 97@.
foreign import ccall unsafe "primecount_nth_prime"
  primecount_nth_prime :: Int64 -> Int64

-- | @primecount_phi x a@ counts the numbers @<= x@ that are not divisible by
--   any of the first @a@ primes.
foreign import ccall unsafe "primecount_phi"
  primecount_phi :: Int64 -> Int64 -> Int64

-- | @primecount_get_max_x@ is the largest number supported by
--   'primecount_pi_str'.
--
-- * 64-bit CPUs: @10^31@
-- * 32-bit CPUs: @2^63 - 1@
foreign import ccall unsafe "primecount_get_max_x"
  primecount_get_max_x :: CString

-- | Get the currently set number of threads used by @libprimecount@.
foreign import ccall unsafe "primecount_get_num_threads"
  primecount_get_num_threads :: IO Int

-- | Set the number of threads used by @libprimecount@.
foreign import ccall unsafe "primecount_set_num_threads"
  primecount_set_num_threads :: Int -> IO ()

-- | Get the @libprimecount@ version number, in the form @"i.j"@.
foreign import ccall unsafe "primecount_version"
  primecount_version :: CString
