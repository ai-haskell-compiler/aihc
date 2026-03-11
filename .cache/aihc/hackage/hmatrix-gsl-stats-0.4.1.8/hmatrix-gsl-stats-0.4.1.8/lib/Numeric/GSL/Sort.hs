{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Numeric.GSL.Sort
-- Copyright   :  (c) A. V. H. McPhail 2010, 2015, 2016
-- License     :  BSD3
--
-- Maintainer  :  haskell.vivian.mcphail <at> gmail <dot> com
-- Stability   :  provisional
-- Portability :  uses ffi
--
-- GSL sorting functions
--
-- <http://www.gnu.org/software/gsl/manual/>
--
-----------------------------------------------------------------------------

module Numeric.GSL.Sort (
                         sort
                         ) where


import Numeric.LinearAlgebra.Data
import Numeric.LinearAlgebra.Devel

import Foreign
import Foreign.C.Types(CInt(..))

import System.IO.Unsafe(unsafePerformIO)

infixr 1 #
a # b = apply a b
{-# INLINE (#) #-}

-- | sort the elements of a vector into ascending order
sort :: Vector Double -> Vector Double
sort v = unsafePerformIO $ do
  r <- createVector (size v)
  (v # r # id) sort_sort #| "sort"
  return r

foreign import ccall "sort-aux.h sort" sort_sort :: CInt -> Ptr Double -> CInt -> Ptr Double -> IO CInt

