module Matrix.QR.Householder (
   leastSquares,
   decompose, solve, det, detAbs,
   Reflection, reflectMatrix, reflectVector,
   Upper, matrixFromUpper, solveUpper, detUpper,
   ) where

import Matrix.Matrix (mv_mult, m_trans, getRow, getColumn, inner, outer)
import Matrix.Vector (sub, scale, norm)
import DSP.Basic (toMaybe)

import qualified Data.List as List
import Data.Array
         (Array, Ix, bounds, elems, range, rangeSize,
          accum, accumArray, assocs, ixmap, listArray, (!), (//), )


decompose :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) =>
      Array (i,j) a -- ^ A
   -> ([Reflection i a], Upper i j a) -- ^ QR(A)
decompose a =
   (\(qs,rows) -> (qs, Upper (bounds a) rows)) .
   unzip .
   List.unfoldr
      (\a0 ->
         let bnds@((m0,_), _) = bounds a0
         in  toMaybe (not $ emptyRange bnds) $
               let (q,a1) = step a0
               in  ((q, getRow m0 a1), submatrix a1))
    $ a

emptyRange :: (Ix i) => (i,i) -> Bool
emptyRange = null . range

step ::
   (Ix i, Ix j, RealFloat a) =>
   Array (i,j) a -> (Reflection i a, Array (i,j) a)
step a =
   let (m0,n0) = fst $ bounds a
       z = getColumn n0 a
       sign x = if x<0 then -1 else 1
       q = reflection $ accum (+) z [(m0, sign(z!m0) * norm z)]
   in  (q, reflectMatrix q a)

{-
Submatrices with only Ix constrained indices would not work,
because we cannot reduce a two-dimensional array by only one element.
-}
submatrix :: (Ix i, Enum i, Ix j, Enum j) => Array (i,j) e -> Array (i,j) e
submatrix a =
   let ((m0,n0), (m1,n1)) = bounds a
   in  ixmap ((succ m0, succ n0), (m1,n1)) id a


data Upper i j a = Upper ((i,j), (i,j)) [Array j a]

matrixFromUpper :: (Ix i, Ix j, Num a) => Upper i j a -> Array (i,j) a
matrixFromUpper (Upper bnds@((m0,_n0), (m1,_n1)) rows) =
   accumArray (const id) 0 bnds $ concat $
   zipWith (\k -> map (\(j,a) -> ((k,j),a)) . assocs) (range (m0,m1)) rows


newtype Reflection i a = Reflection (Array i a)

reflection :: (Ix i, Floating a) => Array i a -> Reflection i a
reflection v =
   let normv = norm v
   in  Reflection $ fmap (/ ((1-signum normv) + normv)) v

reflectMatrixFull ::
   (Ix i, Ix j, Num a) => Reflection i a -> Array (i,j) a -> Array (i,j) a
reflectMatrixFull (Reflection v) a =
   sub a $ scale 2 $ outer v $ mv_mult (m_trans a) v

reflectMatrix ::
   (Ix i, Ix j, Num a) => Reflection i a -> Array (i,j) a -> Array (i,j) a
reflectMatrix q@(Reflection v) a =
   let (k0,k1) = bounds v
       ((m0,n0), (m1,n1)) = bounds a
       bnds = ((k0,n0),(k1,n1))
   in  case (compare k0 m0, compare k1 m1) of
         (EQ,EQ) -> reflectMatrixFull q a
         (LT,_) -> error "reflectMatrix: lower reflection dimension too small"
         (_,GT) -> error "reflectMatrix: upper reflection dimension too big"
         _ -> replaceSubArray a $ reflectMatrixFull q $ subArray bnds a


reflectVectorFull :: (Ix i, Num a) => Reflection i a -> Array i a -> Array i a
reflectVectorFull (Reflection v) a = sub a $ scale (2 * inner v a) v

reflectVector :: (Ix i, Num a) => Reflection i a -> Array i a -> Array i a
reflectVector q@(Reflection v) a =
   let bnds@(k0,k1) = bounds v
       (m0,m1) = bounds a
   in  case (compare k0 m0, compare k1 m1) of
         (EQ,EQ) -> reflectVectorFull q a
         (LT,_) -> error "reflectVector: lower reflection dimension too small"
         (_,GT) -> error "reflectVector: upper reflection dimension too big"
         _ -> replaceSubArray a $ reflectVectorFull q $ subArray bnds a


subArray :: (Ix i) => (i,i) -> Array i a -> Array i a
subArray bnds = ixmap bnds id

replaceSubArray :: (Ix i) => Array i a -> Array i a -> Array i a
replaceSubArray x y = x // assocs y


{- |
Assumes that 'Upper' matrix is at least as high as wide
and that it has full rank.
-}
solveUpper ::
   (Ix i, Ix j, Fractional a) => Upper i j a -> Array i a -> Array j a
solveUpper (Upper ((m0,n0), (m1,n1)) rs0) b =
   if bounds b == (m0,m1)
     then
         listArray (n0,n1) $
         foldr
            (\(r,bi) xs ->
               let (a:as) = elems r
               in  (bi - sum (zipWith (*) as xs)) / a : xs)
            []
            (zip rs0 (elems b))
     else error "solveUpper: vertical bounds mismatch"

solve ::
   (Ix i, Ix j, Fractional a) =>
   ([Reflection i a], Upper i j a) -> Array i a -> Array j a
solve (qs, u) b = solveUpper u $ foldl (flip reflectVector) b qs

{- |
Solve an overconstrained linear problem, i.e. minimize @||Ax-b||@.
@A@ must have dimensions @m x n@ with @m>=n@ and it must have full-rank.
None of these conditions is checked.
-}
leastSquares ::
   (Ix i, Enum i, Ix j, Enum j, RealFloat a) =>
   Array (i,j) a -> Array i a -> Array j a
leastSquares = solve . decompose


detUpper :: (Ix i, Ix j, Fractional a) => Upper i j a -> a
detUpper (Upper ((_m0,n0), (_m1,n1)) rs) =
   if rangeSize (n0,n1) == length rs
     then product $ map (head . elems) rs
     else 0

{- |
Only sensible for square matrices,
but the function does not check whether the matrix is square.

For non-square matrices the sign is not of much use.
It depends on the implementation.
It is not uniquely defined by requiring
that the determinant of the orthogonal transformation has positive sign.
-}
det :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) => Array (i,j) a -> a
det a =
   let (qs,u) = decompose a
   in  (if even (length qs) then 1 else -1) * detUpper u

{- |
Absolute value of the determinant.
This is also sound for non-square matrices.
-}
detAbs :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) => Array (i,j) a -> a
detAbs = abs . detUpper . snd . decompose
