module Matrix.Vector where

import DSP.Basic ((^!))
import Data.Array
         (Array, Ix, bounds, elems, range, array, assocs, listArray, (!), )


generate :: (Ix i) => (i,i) -> (i -> a) -> Array i a
generate bnds f = array bnds $ map (\i -> (i, f i)) $ range bnds

fromList :: [a] -> Array Int a
fromList xs = listArray (0, length xs - 1) xs

toList :: Array Int a -> [a]
toList = elems


norm :: (Ix i, Floating a) => Array i a -> a
norm = sqrt . sum . elems . fmap (^!2)

scale :: (Ix i, Num a) => a -> Array i a -> Array i a
scale x = fmap (x*)


lift2 :: (Ix i) => (a -> b -> c) -> Array i a -> Array i b -> Array i c
lift2 f x y =
   if bounds x == bounds y
     then array (bounds x) [ (k, f xk (y!k)) | (k, xk) <- assocs x ]
     else error "Vector.lift2: matrix dimensions mismatch"

add :: (Ix i, Num a) => Array i a -> Array i a -> Array i a
add = lift2 (+)

sub :: (Ix i, Num a) => Array i a -> Array i a -> Array i a
sub = lift2 (-)
