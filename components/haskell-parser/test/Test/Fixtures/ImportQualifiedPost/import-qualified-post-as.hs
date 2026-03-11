{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostAs where

import Data.Map qualified as M

fromPairs :: [(Int, Int)] -> M.Map Int Int
fromPairs = M.fromList
