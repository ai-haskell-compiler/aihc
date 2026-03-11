{-|
Module: Numeric.Morpheus.MatrixReduce
Description: Matrix reduce by column/row operations.
Copyright: (c) Alexander Ignatyev, 2017
License: BSD-3
Stability: experimental
Portability: POSIX
-}

module Numeric.Morpheus.MatrixReduce
(
  columnPredicate
  , rowPredicate
  , columnSum
  , rowSum
  , columnMaxIndex
  , columnMinIndex
  , rowMaxIndex
  , rowMinIndex
)

where

import Numeric.Morpheus.Utils(morpheusLayout)

import Numeric.LinearAlgebra
import Numeric.LinearAlgebra.Devel
import System.IO.Unsafe(unsafePerformIO)
import Foreign
import Foreign.C.Types
import Foreign.Ptr(Ptr)


type Predicate = R -> R -> R
foreign import ccall "wrapper" mkPredicate :: Predicate -> IO (FunPtr Predicate)


{- morpheus_column_predicate -}
foreign import ccall safe "morpheus_column_predicate"
  c_morpheus_column_predicate :: FunPtr Predicate -> CInt -> CInt -> CInt
                              -> Ptr R -> Ptr R -> IO ()


call_morpheus_column_predicate :: FunPtr Predicate
                               -> CInt -> CInt -> CInt -> CInt -> Ptr R
                               -> CInt -> Ptr R
                               -> IO ()
call_morpheus_column_predicate f rows cols xRow xCol matPtr _ vecPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_predicate f layout rows cols matPtr vecPtr


-- | Scan every column of the given matrix.
-- Predicate takes and accumulator and next value of the column, returns new accumulator.
-- Returns accumulator values for every column.
columnPredicate :: (R -> R -> R) -> Matrix R -> Vector R
columnPredicate f m = unsafePerformIO $ do
  v <- createVector (cols m)
  fpred <- mkPredicate f
  apply m (apply v id) (call_morpheus_column_predicate fpred)
  return v


{- morpheus_row_predicate -}
foreign import ccall safe "morpheus_row_predicate"
  c_morpheus_row_predicate :: FunPtr Predicate -> CInt -> CInt -> CInt
                           -> Ptr R -> Ptr R -> IO ()


call_morpheus_row_predicate :: FunPtr Predicate
                            -> CInt -> CInt -> CInt -> CInt -> Ptr R
                            -> CInt -> Ptr R
                            -> IO ()
call_morpheus_row_predicate f rows cols xRow xCol matPtr _ vecPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_predicate f layout rows cols matPtr vecPtr


-- | Scan every row of the given matrix.
-- Predicate takes and accumulator and next value of the row, returns new accumulator.
-- Returns accumulator values for every row.
rowPredicate :: (R -> R -> R) -> Matrix R -> Vector R
rowPredicate f m = unsafePerformIO $ do
  v <- createVector (rows m)
  fpred <- mkPredicate f
  apply m (apply v id) (call_morpheus_row_predicate fpred)
  return v


{- morpheus_column_sum -}
foreign import ccall unsafe "morpheus_column_sum"
  c_morpheus_column_sum :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_column_sum :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                         -> CInt -> Ptr Double
                         -> IO ()
call_morpheus_column_sum rows cols xRow xCol matPtr _ vecPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_sum layout rows cols matPtr vecPtr


-- | Calculates sums of elements of every column of the given matrix
columnSum :: Matrix Double -> Vector Double
columnSum m = unsafePerformIO $ do
    v <- createVector (cols m)
    apply m (apply v id) call_morpheus_column_sum
    return v


{- morpheus_row_sum -}
foreign import ccall unsafe "morpheus_row_sum"
  c_morpheus_row_sum :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> IO ()


call_morpheus_row_sum :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                      -> CInt -> Ptr Double
                      -> IO ()
call_morpheus_row_sum rows cols xRow xCol matPtr _ vecPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_sum layout rows cols matPtr vecPtr


-- | Calculates sums of elements of every row of the given matrix
rowSum :: Matrix Double -> Vector Double
rowSum m = unsafePerformIO $ do
    v <- createVector (rows m)
    apply m (apply v id) call_morpheus_row_sum
    return v


{- morpheus_column_max_index -}
foreign import ccall unsafe "morpheus_column_max_index"
  c_morpheus_column_max_index :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> IO ()


call_morpheus_column_max_index :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                                  -> CInt -> Ptr Double
                                  -> CInt -> Ptr CInt
                                  -> IO ()
call_morpheus_column_max_index rows cols xRow xCol matPtr _ vecPtr _ idxPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_max_index layout rows cols matPtr vecPtr idxPtr


-- | Finds maximum values and their indices of every column of the given matrix
columnMaxIndex :: Matrix Double -> (Vector R, Vector I)
columnMaxIndex m = unsafePerformIO $ do
    v <- createVector (cols m)
    i <- createVector (cols m)
    apply m (apply v (apply i id)) call_morpheus_column_max_index
    return (v, i)


{- morpheus_column_min_index -}
foreign import ccall unsafe "morpheus_column_min_index"
  c_morpheus_column_min_index :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> IO ()


call_morpheus_column_min_index :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                                  -> CInt -> Ptr Double
                                  -> CInt -> Ptr CInt
                                  -> IO ()
call_morpheus_column_min_index rows cols xRow xCol matPtr _ vecPtr _ idxPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_column_min_index layout rows cols matPtr vecPtr idxPtr


-- | Finds minimum values and their indices of every column of the given matrix
columnMinIndex :: Matrix Double -> (Vector R, Vector I)
columnMinIndex m = unsafePerformIO $ do
    v <- createVector (cols m)
    i <- createVector (cols m)
    apply m (apply v (apply i id)) call_morpheus_column_min_index
    return (v, i)


{- morpheus_row_max_index -}
foreign import ccall unsafe "morpheus_row_max_index"
  c_morpheus_row_max_index :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> IO ()


call_morpheus_row_max_index :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> CInt -> Ptr Double
                               -> CInt -> Ptr CInt
                               -> IO ()
call_morpheus_row_max_index rows cols xRow xCol matPtr _ vecPtr _ idxPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_max_index layout rows cols matPtr vecPtr idxPtr


-- | Finds maximum values and their indices of every row of the given matrix
rowMaxIndex :: Matrix Double -> (Vector R, Vector I)
rowMaxIndex m = unsafePerformIO $ do
    v <- createVector (rows m)
    i <- createVector (rows m)
    apply m (apply v (apply i id)) call_morpheus_row_max_index
    return (v, i)


{- morpheus_row_min_index -}
foreign import ccall unsafe "morpheus_row_min_index"
  c_morpheus_row_min_index :: CInt -> CInt -> CInt -> Ptr Double -> Ptr Double -> Ptr CInt -> IO ()


call_morpheus_row_min_index :: CInt -> CInt -> CInt -> CInt -> Ptr Double
                               -> CInt -> Ptr Double
                               -> CInt -> Ptr CInt
                               -> IO ()
call_morpheus_row_min_index rows cols xRow xCol matPtr _ vecPtr _ idxPtr = do
  let layout = morpheusLayout xCol cols
  c_morpheus_row_min_index layout rows cols matPtr vecPtr idxPtr


-- | Finds minimum values and their indices of every row of the given matrix
rowMinIndex :: Matrix Double -> (Vector R, Vector I)
rowMinIndex m = unsafePerformIO $ do
    v <- createVector (rows m)
    i <- createVector (rows m)
    apply m (apply v (apply i id)) call_morpheus_row_min_index
    return (v, i)
