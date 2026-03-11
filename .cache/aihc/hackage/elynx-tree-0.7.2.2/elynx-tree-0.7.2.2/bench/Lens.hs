-- |
-- Module      :  Lens
-- Description :  Benchmark lens operations
-- Copyright   :  2021 Dominik Schrempf
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Tue Nov  3 14:05:53 2020.
module Lens
  ( sumWithGetter,
    sumWithSetter,
    sumWithAccessorFunction,
    sumWithModifyFunction,
  )
where

import Data.Foldable
import ELynx.Tree.Length
import Lens.Micro

len :: HasLength a => Lens' a Length
len = lens getLength (flip setLength)

sumWithGetter :: HasLength a => [a] -> Length
sumWithGetter = foldl' (\x y -> x ^. len + y ^. len) 0

sumWithSetter :: HasLength a => [a] -> Length
sumWithSetter = sumWithGetter . map (\x -> x & len %~ (+ 10))

sumWithAccessorFunction :: HasLength a => [a] -> Length
sumWithAccessorFunction = foldl' (\x y -> getLength x + getLength y) 0

sumWithModifyFunction :: HasLength a => [a] -> Length
sumWithModifyFunction = sumWithAccessorFunction . map (modifyLength (+ 10))
