-- SPDX-FileCopyrightText: Copyright Preetham Gujjula
-- SPDX-License-Identifier: BSD-3-Clause
module ApplyMerge.MergeAll (applyMerge) where

import Data.List.Ordered (mergeAll)

applyMerge :: (Ord c) => (a -> b -> c) -> [a] -> [b] -> [c]
applyMerge _ [] _ = []
applyMerge f xs ys =
  mergeAll (map (\y -> map (`f` y) xs) ys)
