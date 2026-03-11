module Language.REST.Internal.Util where

import qualified Data.List as L

-- | @removeEqBy f xs ys@ removes elements from @xs@ and @ys@ such that for each
--  element x removed from xs, an element y is removed from ys such @f x y@.
--  In other words in the result @(xs', ys')@, there does not exist any @x@ in
--  @xs'@, @y@ in @ys'@ such that @f x y@.
removeEqBy :: (Eq a) => (a -> a -> Bool) -> [a] -> [a] -> ([a], [a])
removeEqBy _ [] ys = ([], ys)
removeEqBy f (x : xs) ys
  | Just y <- L.find (f x) ys
  = removeEqBy f xs $ L.delete y ys
  | otherwise
  = let (xs', ys') = removeEqBy f xs ys in (x : xs', ys')
