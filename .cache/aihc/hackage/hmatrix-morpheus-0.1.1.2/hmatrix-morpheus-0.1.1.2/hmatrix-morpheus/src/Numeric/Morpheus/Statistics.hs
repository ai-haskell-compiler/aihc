{-|
Module: Numeric.Morpheus.Statistics
Description: Statistics Functions
Copyright: (c) Alexander Ignatyev, 2017
License: BSD-3
Stability: experimental
Portability: POSIX
-}

module Numeric.Morpheus.Statistics
(
  mean
  , stddev_m
  , stddev
  , columnMean
  , rowMean
  , columnStddev_m
  , rowStddev_m
)

where

import Numeric.Morpheus.Utils(morpheusLayout)

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign.C.Types
import Foreign.Ptr(Ptr)
import Foreign.Storable

{- morpheus_mean -}
foreign import ccall unsafe "morpheus_mean"
  c_morpheus_mean :: CInt -> Ptr R -> IO R

-- | Calculates mean (average).
mean :: Vector R -> R
mean x = unsafePerformIO $ do
    apply x id c_morpheus_mean


{- morpheus_stddev_m -}
foreign import ccall unsafe "morpheus_stddev"
  c_morpheus_stddev_m :: R -> CInt -> Ptr R -> IO R

-- | Calculates sample standard deviation using given mean value.
stddev_m :: R -> Vector R -> R
stddev_m m x = unsafePerformIO $ do
    apply x id (c_morpheus_stddev_m m)


{- morpheus_stddev -}
foreign import ccall unsafe "morpheus_stddev"
  c_morpheus_stddev :: CInt -> Ptr R -> IO R

-- | Calculates sample standard deviation.
stddev :: Vector R -> R
stddev x = unsafePerformIO $ do
    apply x id c_morpheus_stddev


{- morpheus_column_mean -}
foreign import ccall unsafe "morpheus_column_mean"
  c_morpheus_column_mean :: CInt -> CInt -> CInt -> Ptr R -> Ptr R -> IO ()


call_morpheus_column_mean :: CInt -> CInt -> CInt -> CInt -> Ptr R
                          -> CInt -> Ptr R
                          -> IO ()
call_morpheus_column_mean rows cols xRow xCol mat _ means = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_mean layout rows cols mat means


-- | Calculates mean (average) for every column.
columnMean :: Matrix R -> Vector R
columnMean m = unsafePerformIO $ do
    v <- createVector (cols m)
    apply m (apply v id) call_morpheus_column_mean
    return v


{- morpheus_row_mean -}
foreign import ccall unsafe "morpheus_row_mean"
  c_morpheus_row_mean :: CInt -> CInt -> CInt -> Ptr R -> Ptr R -> IO ()


call_morpheus_row_mean :: CInt -> CInt -> CInt -> CInt -> Ptr R
                       -> CInt -> Ptr R
                       -> IO ()
call_morpheus_row_mean rows cols xRow xCol mat _ means = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_mean layout rows cols mat means


-- | Calculates mean (average) for every row.
rowMean :: Matrix R -> Vector R
rowMean m = unsafePerformIO $ do
    v <- createVector (rows m)
    apply m (apply v id) call_morpheus_row_mean
    return v


{- morpheus_column_stddev_m -}
foreign import ccall unsafe "morpheus_column_stddev_m"
  c_morpheus_column_stddev_m :: CInt -> CInt -> CInt -> Ptr R -> Ptr R -> Ptr R -> IO ()


call_morpheus_column_stddev_m :: CInt -> Ptr R
                              -> CInt -> CInt -> CInt -> CInt -> Ptr R
                              -> CInt -> Ptr R
                              -> IO ()
call_morpheus_column_stddev_m _ means rows cols xRow xCol mat _ stddevs = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_stddev_m layout rows cols means mat stddevs


-- | Calculates sample standard deviation using given mean value for every column.
columnStddev_m :: Vector R -> Matrix R -> Vector R
columnStddev_m means m = unsafePerformIO $ do
    v <- createVector (cols m)
    apply means (apply m (apply v id)) call_morpheus_column_stddev_m
    return v


{- morpheus_row_stddev_m -}
foreign import ccall unsafe "morpheus_row_stddev_m"
  c_morpheus_row_stddev_m :: CInt -> CInt -> CInt -> Ptr R -> Ptr R -> Ptr R -> IO ()


call_morpheus_row_stddev_m :: CInt -> Ptr R
                              -> CInt -> CInt -> CInt -> CInt -> Ptr R
                              -> CInt -> Ptr R
                              -> IO ()
call_morpheus_row_stddev_m _ means rows cols xRow xCol mat _ stddevs = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_stddev_m layout rows cols means mat stddevs


-- | Calculates sample standard deviation using given mean value for every row.
rowStddev_m :: Vector R -> Matrix R -> Vector R
rowStddev_m means m = unsafePerformIO $ do
    v <- createVector (rows m)
    apply means (apply m (apply v id)) call_morpheus_row_stddev_m
    return v
