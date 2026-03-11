{-# LANGUAGE ImportQualifiedPost #-}

module ImportQualifiedPostBasic where

import Data.List qualified

sorted :: Ord a => [a] -> [a]
sorted = Data.List.sort
