{- |
Interpolation basis functions using all given nodes.
The represented functions are equivalent to the ones from
"Numeric.Interpolation.Basis.Compact"
but less efficient for evaluation.
-}
module Numeric.Interpolation.Basis.Full (linear, hermite1) where

import qualified Numeric.Interpolation.NodeList as Nodes

import qualified Data.List.Match as Match


generic :: ny -> ny -> [x] -> [Nodes.T x ny]
generic nz ny xs =
   map (Nodes.fromList . zip xs) $
   Match.take xs $ iterate (nz:) $ ny : repeat nz

linear :: (Num b) => [a] -> [Nodes.T a b]
linear = generic 0 1

hermite1 :: (Num b) => [a] -> [Nodes.T a (b, b)]
hermite1 xs =
   generic (0,0) (1,0) xs
   ++
   generic (0,0) (0,1) xs
