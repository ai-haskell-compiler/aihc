-- |
-- Module      :  Data.Algorithm.Assignment
-- Copyright   :  © 2024–present Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- A solution to the assignment problem.
module Data.Algorithm.Assignment
  ( assign,
  )
where

import Control.Monad (forM_, void, when)
import Control.Monad.Fix (fix)
import Control.Monad.ST (ST, runST)
import Data.Array (Array)
import Data.Array.Base qualified as A
import Data.Array.ST (STUArray)
import Data.Array.ST qualified as ST
import Data.STRef (modifySTRef', newSTRef, readSTRef, writeSTRef)

type CostMatrix s = STUArray s (Int, Int) Int

type MarkMatrix s = STUArray s (Int, Int) Char

type CoverageVector s = STUArray s Int Bool

-- | \(\mathcal{O}(n^4)\). Assign elements from two collections to each
-- other so that the total cost is minimal. The cost of each combination is
-- given the by the first argument and it can be negative. If any of the
-- collections is empty the result is the empty list. The sizes of the
-- collections need not to match. Finally, there is no guarantees on the
-- order of elements in the returned list of pairs.
--
-- See: <https://en.wikipedia.org/wiki/Hungarian_algorithm#Matrix_interpretation>
assign ::
  -- | How to calculate the cost
  (a -> b -> Int) ->
  -- | The first collection
  [a] ->
  -- | The second collection
  [b] ->
  -- | The resulting optimal assignment (no guarantees about order)
  [(a, b)]
assign _ [] _ = []
assign _ _ [] = []
assign cost as bs = runST $ do
  let length_a = length as
      length_b = length bs
      aMinBound = 0
      aMaxBound = length_a - 1
      bMinBound = 0
      bMaxBound = length_b - 1
      abMaxBound = max aMaxBound bMaxBound
      asArray = A.listArray (aMinBound, aMaxBound) as
      bsArray = A.listArray (bMinBound, bMaxBound) bs
      matrixBounds = ((aMinBound, bMinBound), (abMaxBound, abMaxBound))
  c <- ST.newArray matrixBounds 0
  m <- ST.newArray matrixBounds noMark
  aCoverage <- ST.newArray (aMinBound, abMaxBound) False
  bCoverage <- ST.newArray (bMinBound, abMaxBound) False
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j ->
      ST.writeArray c (i, j) (cost (asArray A.! i) (bsArray A.! j))
  if aMaxBound - aMinBound >= bMaxBound - bMinBound
    then normalizePerB c
    else normalizePerA c
  starZeros c m aCoverage bCoverage
  fix $ \recurse0 -> do
    done <- coverZeros m aCoverage bCoverage
    if done
      then recoverResults m asArray bsArray
      else fix $ \recurse1 -> do
        r <- primeUncoveredZero c m aCoverage bCoverage
        case r of
          Nothing -> do
            adjustCosts c aCoverage bCoverage
            recurse1
          Just z0 -> do
            adjustMarks m z0
            clearCoverage aCoverage bCoverage
            recurse0
{-# INLINEABLE assign #-}

normalizePerA :: CostMatrix s -> ST s ()
normalizePerA c = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds c
  countFromTo aMinBound aMaxBound $ \i -> do
    minValueRef <- newSTRef maxBound
    countFromTo bMinBound bMaxBound $ \j ->
      ST.readArray c (i, j) >>= modifySTRef' minValueRef . min
    minValue <- readSTRef minValueRef
    when (minValue /= 0) $ do
      countFromTo bMinBound bMaxBound $ \j -> do
        ST.modifyArray' c (i, j) (subtract minValue)
{-# INLINE normalizePerA #-}

normalizePerB :: CostMatrix s -> ST s ()
normalizePerB c = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds c
  countFromTo bMinBound bMaxBound $ \j -> do
    minValueRef <- newSTRef maxBound
    countFromTo aMinBound aMaxBound $ \i ->
      ST.readArray c (i, j) >>= modifySTRef' minValueRef . min
    minValue <- readSTRef minValueRef
    when (minValue /= 0) $ do
      countFromTo aMinBound aMaxBound $ \i -> do
        ST.modifyArray' c (i, j) (subtract minValue)
{-# INLINE normalizePerB #-}

starZeros ::
  CostMatrix s ->
  MarkMatrix s ->
  CoverageVector s ->
  CoverageVector s ->
  ST s ()
starZeros c m aCoverage bCoverage = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds c
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      x <- ST.readArray c (i, j)
      when (x == 0) $ do
        aCovered <- ST.readArray aCoverage i
        bCovered <- ST.readArray bCoverage j
        when (not aCovered && not bCovered) $ do
          ST.writeArray m (i, j) starMark
          ST.writeArray aCoverage i True
          ST.writeArray bCoverage j True
  clearCoverage aCoverage bCoverage
{-# INLINE starZeros #-}

coverZeros ::
  MarkMatrix s ->
  CoverageVector s ->
  CoverageVector s ->
  ST s Bool
coverZeros m _aCoverage bCoverage = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds m
  nRef <- newSTRef 0
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      x <- ST.readArray m (i, j)
      bCovered <- ST.readArray bCoverage j
      when (x == starMark && not bCovered) $ do
        ST.writeArray bCoverage j True
        modifySTRef' nRef (+ 1)
  n <- readSTRef nRef
  return (n > aMaxBound)
{-# INLINE coverZeros #-}

recoverResults ::
  MarkMatrix s ->
  Array Int a ->
  Array Int b ->
  ST s [(a, b)]
recoverResults m as bs = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds m
  resultRef <- newSTRef []
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      x <- ST.readArray m (i, j)
      when (x == starMark) $ do
        case (,) <$> (as A.!? i) <*> (bs A.!? j) of
          Nothing -> return ()
          Just (a, b) -> modifySTRef' resultRef ((a, b) :)
  readSTRef resultRef
{-# INLINE recoverResults #-}

primeUncoveredZero ::
  CostMatrix s ->
  MarkMatrix s ->
  CoverageVector s ->
  CoverageVector s ->
  ST s (Maybe (Int, Int))
primeUncoveredZero c m aCoverage bCoverage = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds m
  primedRef <- newSTRef Nothing
  void . countFromTo' aMinBound aMaxBound $ \i ->
    countFromTo' bMinBound bMaxBound $ \j -> do
      x <- ST.readArray c (i, j)
      if x == 0
        then do
          aCovered <- ST.readArray aCoverage i
          bCovered <- ST.readArray bCoverage j
          if not aCovered && not bCovered
            then False <$ writeSTRef primedRef (Just (i, j))
            else return True
        else return True
  r <- readSTRef primedRef
  case r of
    Nothing -> return Nothing
    Just (i, j) -> do
      ST.writeArray m (i, j) primeMark
      mj' <- findInA m starMark i
      case mj' of
        Nothing -> return (Just (i, j))
        Just j' -> do
          ST.writeArray aCoverage i True
          ST.writeArray bCoverage j' False
          primeUncoveredZero c m aCoverage bCoverage
{-# INLINEABLE primeUncoveredZero #-}

adjustMarks :: MarkMatrix s -> (Int, Int) -> ST s ()
adjustMarks m z0 = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds m
  let go (_, j) acc = do
        mi' <- findInB m starMark j
        case mi' of
          Nothing -> return acc
          Just i' -> do
            mj' <- findInA m primeMark i'
            case mj' of
              Nothing -> error "Data.Algorithm.Assignment.adjustMarks"
              Just j' -> go (i', j') ((i', j) : (i', j') : acc)
  path <- go z0 [z0]
  forM_ path $ \(i, j) -> do
    let adjust x =
          if x == starMark
            then noMark
            else starMark
    ST.modifyArray' m (i, j) adjust
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      let resetPrime x =
            if x == primeMark
              then noMark
              else x
      ST.modifyArray' m (i, j) resetPrime
{-# INLINE adjustMarks #-}

adjustCosts ::
  CostMatrix s ->
  CoverageVector s ->
  CoverageVector s ->
  ST s ()
adjustCosts c aCoverage bCoverage = do
  ((aMinBound, bMinBound), (aMaxBound, bMaxBound)) <- ST.getBounds c
  minUncoveredValueRef <- newSTRef maxBound
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      aCovered <- ST.readArray aCoverage i
      bCovered <- ST.readArray bCoverage j
      when (not aCovered && not bCovered) $ do
        ST.readArray c (i, j) >>= modifySTRef' minUncoveredValueRef . min
  minUncoveredValue <- readSTRef minUncoveredValueRef
  countFromTo aMinBound aMaxBound $ \i ->
    countFromTo bMinBound bMaxBound $ \j -> do
      aCovered <- ST.readArray aCoverage i
      bCovered <- ST.readArray bCoverage j
      if not aCovered && not bCovered
        then ST.modifyArray c (i, j) (subtract minUncoveredValue)
        else
          when (aCovered && bCovered) $
            ST.modifyArray c (i, j) (+ minUncoveredValue)
{-# INLINE adjustCosts #-}

clearCoverage ::
  CoverageVector s ->
  CoverageVector s ->
  ST s ()
clearCoverage aCoverage bCoverage = do
  let clearOne v = do
        (from, to) <- ST.getBounds v
        countFromTo from to $ \i ->
          ST.writeArray v i False
  clearOne aCoverage
  clearOne bCoverage
{-# INLINE clearCoverage #-}

findInA ::
  MarkMatrix s ->
  Char ->
  Int ->
  ST s (Maybe Int)
findInA m mark i = do
  ((_aMinBound, bMinBound), (_aMaxBound, bMaxBound)) <- ST.getBounds m
  starredRef <- newSTRef Nothing
  void . countFromTo' bMinBound bMaxBound $ \j -> do
    x <- ST.readArray m (i, j)
    if x == mark
      then False <$ writeSTRef starredRef (Just j)
      else return True
  readSTRef starredRef
{-# INLINE findInA #-}

findInB ::
  MarkMatrix s ->
  Char ->
  Int ->
  ST s (Maybe Int)
findInB m mark j = do
  ((aMinBound, _bMinBound), (aMaxBound, _bMaxBound)) <- ST.getBounds m
  starredRef <- newSTRef Nothing
  void . countFromTo' aMinBound aMaxBound $ \i -> do
    x <- ST.readArray m (i, j)
    if x == mark
      then False <$ writeSTRef starredRef (Just i)
      else return True
  readSTRef starredRef
{-# INLINE findInB #-}

countFromTo :: Int -> Int -> (Int -> ST s ()) -> ST s ()
countFromTo start end action = go start
  where
    go !n = when (n <= end) $ do
      action n
      go (n + 1)
{-# INLINE countFromTo #-}

countFromTo' :: Int -> Int -> (Int -> ST s Bool) -> ST s Bool
countFromTo' start end action = go start
  where
    go !n =
      if n <= end
        then do
          r <- action n
          if r then go (n + 1) else return False
        else return True
{-# INLINE countFromTo' #-}

noMark, starMark, primeMark :: Char
noMark = 'n'
starMark = 's'
primeMark = 'p'
