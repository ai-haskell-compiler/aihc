-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Matrix.Matrix
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Basic matrix routines
--
-----------------------------------------------------------------------------

module Matrix.Matrix where

import Matrix.Vector (generate)
import Data.Array
import Data.Complex

-- | Matrix-matrix multiplication: A x B = C

mm_mult :: (Ix i, Ix j, Ix k, Num a) => Array (i,j) a -- ^ A
	-> Array (j,k) a -- ^ B
	-> Array (i,k) a -- ^ C

mm_mult a b = if (ac0,ac1) /= (br0,br1)
	      then error "mm_mult: inside dimensions inconsistent"
	      else generate ((ar0,bc0),(ar1,bc1)) $ \(i,j) ->
			sum [ a!(i,k) * b!(k,j) | k <- range (ac0,ac1) ]
    where ((ar0,ac0),(ar1,ac1)) = bounds a
	  ((br0,bc0),(br1,bc1)) = bounds b

-- | Matrix-vector multiplication: A x b = c

mv_mult :: (Ix i, Ix j, Num a) => Array (i,j) a -- ^ A
	-> Array j a -- ^ b
	-> Array i a -- ^ c

mv_mult a b = if (ac0,ac1) /= bounds b
	      then error "mv_mult: dimensions inconsistent"
	      else generate (ar0,ar1) $ \i ->
			sum [ a!(i,k) * bk | (k,bk) <- assocs b ]
    where ((ar0,ac0),(ar1,ac1)) = bounds a

-- | Transpose of a matrix

m_trans :: (Ix i, Ix j, Num a) => Array (i,j) a -- ^ A
	-> Array (j,i) a -- ^ A^T

m_trans a = generate ((n0,m0),(n1,m1)) $ \(i,j) -> a!(j,i)
    where ((m0,n0),(m1,n1)) = bounds a

-- | Hermitian transpose (conjugate transpose) of a matrix

m_hermit :: (Ix i, Ix j, RealFloat a) => Array (i,j) (Complex a) -- ^ A
	 -> Array (j,i) (Complex a) -- ^ A^H

m_hermit a = generate ((n0,m0),(n1,m1)) $ \(i,j) -> conjugate (a!(j,i))
    where ((m0,n0),(m1,n1)) = bounds a


columnBounds :: (Ix i, Ix j) => Array (i,j) a -> (i,i)
columnBounds a =
   let ((m0,_n0), (m1,_n1)) = bounds a
   in  (m0,m1)

rowBounds :: (Ix i, Ix j) => Array (i,j) a -> (j,j)
rowBounds a =
   let ((_m0,n0), (_m1,n1)) = bounds a
   in  (n0,n1)

getColumn :: (Ix i, Ix j) => j -> Array (i,j) e -> Array i e
getColumn j a = ixmap (columnBounds a) (\k -> (k,j)) a

getRow :: (Ix i, Ix j) => i -> Array (i,j) e -> Array j e
getRow k a = ixmap (rowBounds a) (\j -> (k,j)) a

toColumns :: (Ix i, Ix j) => Array (i,j) a -> [Array i a]
toColumns a = map (flip getColumn a) $ range $ rowBounds a

toRows :: (Ix i, Ix j) => Array (i,j) a -> [Array j a]
toRows a = map (flip getRow a) $ range $ columnBounds a


{- |
We need the bounds of the row indices for empty input lists.
-}
fromColumns :: (Ix i) => (i,i) -> [Array i a] -> Array (i,Int) a
fromColumns bnds@(m0,m1) columns =
   if all ((bnds==) . bounds) columns
     then array ((m0,0), (m1, length columns - 1)) $ concat $
          zipWith
            (\k -> map (\(i,a) -> ((i,k),a)) . assocs)
            [0..] columns
     else error "Matrix.fromColumns: column bounds mismatch"

fromRows :: (Ix j) => (j,j) -> [Array j a] -> Array (Int,j) a
fromRows bnds@(n0,n1) rows =
   if all ((bnds==) . bounds) rows
     then array ((0,n0), (length rows - 1, n1)) $ concat $
          zipWith
            (\k -> map (\(i,a) -> ((k,i),a)) . assocs)
            [0..] rows
     else error "Matrix.fromRows: row bounds mismatch"



outer :: (Ix i, Ix j, Num a) => Array i a -> Array j a -> Array (i,j) a
outer x y =
   let (m0,m1) = bounds x
       (n0,n1) = bounds y
   in  array ((m0,n0), (m1,n1)) $ do
         (i,xi) <- assocs x
         (j,yj) <- assocs y
         return ((i,j), xi*yj)

inner :: (Ix i, Num a) => Array i a -> Array i a -> a
inner x y =
   if bounds x == bounds y
     then sum $ zipWith (*) (elems x) (elems y)
     else error "inner: dimensions mismatch"
