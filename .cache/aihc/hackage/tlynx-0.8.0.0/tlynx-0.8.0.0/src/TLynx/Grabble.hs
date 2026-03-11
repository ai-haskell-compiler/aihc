-- |
-- Module      :  TLynx.Grabble
-- Description :  Grabble a list
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Creation date: Mon Sep  6 09:39:16 2021.
module TLynx.Grabble
  ( grabble,
  )
where

import Control.Monad
import Control.Monad.ST
import Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as M
import System.Random.Stateful

-- | @grabble xs m n@ is /O(m*n')/, where @n' = min n (length xs)@. Choose @n'@
-- elements from @xs@, without replacement, and that @m@ times.
grabble :: (StatefulGen g m) => [a] -> Int -> Int -> g -> m [[a]]
grabble xs m n gen = do
  swapss <- replicateM m $
    forM [0 .. min (l - 1) n] $ \i -> do
      j <- uniformRM (i, l) gen
      return (i, j)
  return $ map (V.toList . V.take n . swapElems (V.fromList xs)) swapss
  where
    l = length xs - 1

swapElems :: Vector a -> [(Int, Int)] -> Vector a
swapElems xs swaps = runST $ do
  mxs <- V.unsafeThaw xs
  mapM_ (uncurry $ M.unsafeSwap mxs) swaps
  V.unsafeFreeze mxs
