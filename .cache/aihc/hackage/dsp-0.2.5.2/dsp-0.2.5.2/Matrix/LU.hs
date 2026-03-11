-----------------------------------------------------------------------------
-- |
-- Module      :  DSP.Matrix.LU
-- Copyright   :  (c) Matthew Donadio 2003
-- License     :  GPL
--
-- Maintainer  :  m.p.donadio@ieee.org
-- Stability   :  experimental
-- Portability :  portable
--
-- Module implementing LU decomposition and related functions
--
-----------------------------------------------------------------------------

module Matrix.LU (lu, lu_solve, improve, inverse, lu_det, solve, det) where

import qualified Matrix.Matrix as Matrix
import qualified Matrix.Vector as Vector
import qualified Data.List as List
import Data.Array
import Data.Ord (comparing)


-- | LU decomposition via Crout's Algorithm

-- TODO: modify for partial pivoting / permutation matrix
-- TODO: add singularity check

-- I am sure these are in G&VL, but the two cases of function f below are
-- formulas (2.3.13) and (2.3.12) from NRIC with some variable renaming

lu :: Array (Int,Int) Double -- ^ A
   -> Array (Int,Int) Double -- ^ LU(A)

lu a = a'
    where a' = array bnds [ ((i,j), luij i j) | (i,j) <- range bnds ]
          luij i j =
             if i>j
               then (a!(i,j) - sum [ a'!(i,k) * a'!(k,j) | k <- [1 ..(j-1)] ]) / a'!(j,j)
               else  a!(i,j) - sum [ a'!(i,k) * a'!(k,j) | k <- [1 ..(i-1)] ]
          bnds = bounds a

-- | Solution to Ax=b via LU decomposition

-- forward is formula (2.3.6) in NRIC, but remembering that a11=1
-- backward is formula (2.3.7) in NRIC

lu_solve :: Array (Int,Int) Double -- ^ LU(A)
         -> Array Int Double -- ^ b
         -> Array Int Double -- ^ x

lu_solve a b = x
    where x = array (1,n) ([(n,xn)] ++ [ (i, backward i) | i <- (reverse [1..(n-1)]) ])
          y = array (1,n) ([(1,y1)] ++ [ (i, forward i)  | i <- [2..n] ])
          y1         = b!1
          forward  i = (b!i - sum [ a!(i,j) * y!j | j <- [1..(i-1)] ])
          xn         = y!n / a!(n,n)
          backward i = (y!i - sum [ a!(i,j) * x!j | j <- [(i+1)..n] ]) / a!(i,i)
          ((_,_),(n,_)) = bounds a

-- | Improve a solution to Ax=b via LU decomposition

-- formula (2.7.4) from NRIC

improve :: Array (Int,Int) Double -- ^ A
        -> Array (Int,Int) Double -- ^ LU(A)
        -> Array Int Double -- ^ b
        -> Array Int Double -- ^ x
        -> Array Int Double -- ^ x'

improve a a_lu b x = array (1,n) [ (i, x!i - err!i) | i <- [1..n] ]
    where err = lu_solve a_lu rhs
          rhs = array (1,n) [ (i, sum [ a!(i,j) * x!j | j <- [1..n] ] - b!i) | i <- [1..n] ]
          ((_,_),(n,_)) = bounds a

-- | Matrix inversion via LU decomposition

-- Section (2.4) from NRIC

-- TODO: build in improve

inverse :: Array (Int,Int) Double -- ^ A
        -> Array (Int,Int) Double -- ^ A^-1

inverse a0 = a'
    where a' = array (bounds a0) (arrange (makecols (lu a0)) 1)
          makecol i n' = array (1,n') [ (j, if i == j then 1.0 else 0.0) | j <- [1..n'] ]
          makecols a = [ lu_solve a (makecol i n) | i <- [1..n] ]
          ((_,_),(n,_)) = bounds a0
          arrange []     _ = []
          arrange (m:ms) j = flatten m j ++ arrange ms (j+1)
          flatten m j = map (\(i,x) -> ((i,j),x)) (assocs m)

-- | Determinant of a matrix via LU decomposition

-- Formula (2.5.1) from NRIC

lu_det :: Array (Int,Int) Double -- ^ LU(A)
       -> Double -- ^ det(A)

lu_det a = product [ a!(i,i) | i <- [ 1 .. n] ]
    where ((_,_),(n,_)) = bounds a


-- | LU solver using original matrix

solve :: Array (Int,Int) Double -- ^ A
         -> Array Int Double -- ^ b
         -> Array Int Double -- ^ x

solve a b = (lu_solve . lu) a b

-- | determinant using original matrix

{-
It is based on LU decomposition without singularity check
and thus returns NaN instead of zero if the matrix is singular.
-}
_det :: Array (Int,Int) Double -- ^ A
    -> Double -- ^ det(A)

_det a = (lu_det . lu) a

{- |
Determinant computation by implicit LU decomposition with permutations.
-}
det :: Array (Int,Int) Double -- ^ A
    -> Double -- ^ det(A)

det a =
   if rangeSize (bounds a) == 0
     then 1
     else
         let ((m0,n0), (m1,n1)) = bounds a
             v = Matrix.getColumn n0 a
             (maxi,maxv) = List.maximumBy (comparing (abs . snd)) $ assocs v
             reduced =
                ixmap ((m0,n0), (pred m1, pred n1))
                   (\(i,j) -> (if i<maxi then i else succ i, succ j)) $
                Vector.sub a $ Matrix.outer v $
                Vector.scale (recip maxv) $ Matrix.getRow maxi a
             sign = if even (rangeSize (m0,maxi)-1) then 1 else -1
             pivot = a!(maxi,n0)
         in  if pivot == 0 then 0 else sign * pivot * det reduced


-------------------------------------------------------------------------------
-- tests
-------------------------------------------------------------------------------

{-

a = array ((1,1),(3,3)) [ ((1,1), 1.0), ((1,2), 2.0), ((1,3),  3.0),
                          ((2,1), 2.0), ((2,2), 5.0), ((2,3),  3.0),
                          ((3,1), 1.0), ((3,2), 0.0), ((3,3),  8.0) ]
a' = array ((1,1),(3,3)) [ ((1,1), -40.0), ((1,2), 16.0), ((1,3),  9.0),
                           ((2,1),  13.0), ((2,2), -5.0), ((2,3), -3.0),
                           ((3,1),   5.0), ((3,2), -2.0), ((3,3), -1.0) ]

a_lu = lu a
b = array (1,3) [ (1, 1.0), (2, 2.0), (3, 5.0) ]
x   = lu_solve a_lu b
x'  = improve a a_lu b x
x'' = improve a a_lu b x'

verify = a' == inverse a && -- tests lu, lu_solve, and inverse
         det a == -1 &&     -- tests lu_det
         x == x' &&         -- tests improve
         x' == x''

-}
