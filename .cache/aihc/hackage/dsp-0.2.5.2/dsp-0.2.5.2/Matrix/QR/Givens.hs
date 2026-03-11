module Matrix.QR.Givens  (
   leastSquares,
   decompose, solve, det, detAbs,
   Rotation, rotateVector,
   Upper, solveUpper, detUpper,
   ) where

import qualified Matrix.Sparse as Sparse
import DSP.Basic (toMaybe, (^!))

import Control.Monad (mfilter)

import qualified Data.Foldable as Fold
import qualified Data.List as List
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Array
         (Array, Ix, array, bounds, elems, rangeSize, range, (!), (//), )


data Rotation i a = Rotation (i,i) (a,a)
   deriving Show

data Upper i j a = Upper ((i,j), (i,j)) (Map i (Map j a))
   deriving Show

{- |
The decomposition routine is pretty simple.
It does not try to minimize fill-up by a clever ordering of rotations.
However, for banded matrices it will work as expected.
-}
decompose :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) =>
      Sparse.Matrix i j a -- ^ A
   -> ([Rotation i a], Upper i j a) -- ^ QR(A)
decompose a =
   (\(qs,rows) -> (concat qs, Upper (Sparse.bounds a) $ Map.fromList rows)) .
   unzip .
   List.unfoldr
      (\a0 ->
         let bnds@((m0,_), _) = Sparse.bounds a0
             (q,a1) = step a0
         in  toMaybe (not $ emptyRange bnds) $
               ((q, (m0, Sparse.getRow m0 a1)), submatrix a1))
    $ a

-- cf. QR.Householder
emptyRange :: (Ix i) => (i,i) -> Bool
emptyRange = null . range

-- | assumes that the first column is empty except the top-most element
submatrix ::
   (Ord i, Enum i, Ord j, Enum j) => Sparse.Matrix i j a -> Sparse.Matrix i j a
submatrix a =
   let ((m0,n0), mn1) = Sparse.bounds a
   in  Sparse.fromRows ((succ m0, succ n0), mn1) $
       Map.delete m0 $ Sparse.toRows a

step ::
   (Ord i, Ord j, RealFloat a) =>
   Sparse.Matrix i j a -> ([Rotation i a], Sparse.Matrix i j a)
step a =
   let bnds@((m0,n0), _) = Sparse.bounds a
       rows = Sparse.toRows a
       topRow = Map.findWithDefault Map.empty m0 rows
   in  (\((xi,xrem), finalRows) ->
         (Map.elems $ Map.mapMaybe fst finalRows,
          Sparse.fromRows bnds $ Map.insert m0 (Map.insert n0 xi xrem) $
            fmap snd finalRows)) $
       Map.mapAccumWithKey
         (\(xi,xrem) mi yrow ->
            let yrem = Map.delete n0 yrow
                jrot = Just . Rotation (m0,mi)
            in  case mfilter (0/=) $ Map.lookup n0 yrow of
                  Nothing -> ((xi,xrem),(Nothing,yrem))
                  Just yi ->
                     if xi==0
                        then ((yi,yrem), (jrot (0,-1), fmap negate xrem))
                        else
                           let rot = rotationFromXY (xi,yi)
                               (rx,ry) = rotateRows rot (xrem, yrem)
                           in  ((fst $ rotateSingle rot (xi,yi), rx),
                                (jrot rot, ry)))
         (Map.findWithDefault 0 n0 topRow, Map.delete n0 topRow)
         (Map.delete m0 rows)

-- | The argument must not be (0,0).
rotationFromXY :: (RealFloat a) => (a,a) -> (a,a)
rotationFromXY (x,y) =
   if abs x > abs y
     then let q = y/x; k = recipNorm q in (k,-q*k)
     else let q = x/y; k = recipNorm q in (q*k,-k)

recipNorm :: Floating a => a -> a
recipNorm q = recip $ sqrt (1+q^!2)

rotateSingle :: (Num a) => (a,a) -> (a,a) -> (a,a)
rotateSingle (c,s) (x,y) = (c*x-s*y, s*x+c*y)

rotateRows ::
   (Ord j, Num a) => (a,a) -> (Map j a, Map j a) -> (Map j a, Map j a)
rotateRows (c,s) (xs,ys) =
   let rs =
         Map.intersectionWith (curry $ rotateSingle (c,s)) xs ys
         `Map.union`
         fmap (\x -> ( c*x, s*x)) (Map.difference xs ys)
         `Map.union`
         fmap (\y -> (-s*y, c*y)) (Map.difference ys xs)

   in  (fmap fst rs, fmap snd rs)

rotateVector :: (Ix i, Num a) => Rotation i a -> Array i a -> Array i a
rotateVector (Rotation (i0,i1) cs) x =
   let (y0,y1) = rotateSingle cs (x!i0,x!i1)
   in  x // [(i0,y0),(i1,y1)]


{- |
Assumes that 'Upper' matrix is at least as high as wide
and that it has full rank.
-}
solveUpper ::
   (Ix i, Ix j, Fractional a) => Upper i j a -> Array i a -> Array j a
solveUpper (Upper ((m0,n0), (m1,n1)) rows0) b =
   if bounds b == (m0,m1)
     then
         array (n0,n1) $ Map.toList $
         foldr
            (\(row,bi) xs ->
               let ((j,a),as) = Map.deleteFindMin row
               in  Map.insert j
                     ((bi - Fold.sum (Map.intersectionWith (*) as xs)) / a) xs)
            Map.empty
            (zip (Map.elems rows0) (elems b))
     else error "solveUpper: vertical bounds mismatch"

solve ::
   (Ix i, Ix j, Fractional a) =>
   ([Rotation i a], Upper i j a) -> Array i a -> Array j a
solve (qs, u) b = solveUpper u $ foldl (flip rotateVector) b qs

{- |
Solve a sparse overconstrained linear problem, i.e. minimize @||Ax-b||@.
@A@ must have dimensions @m x n@ with @m>=n@ and it must have full-rank.
None of these conditions is checked.
-}
leastSquares ::
   (Ix i, Enum i, Ix j, Enum j, RealFloat a) =>
   Sparse.Matrix i j a -> Array i a -> Array j a
leastSquares = solve . decompose


detUpper ::
   (Ix i, Ix j, Fractional a) => Upper i j a -> a
detUpper (Upper ((_m0,n0), (_m1,n1)) rows) =
   if rangeSize (n0,n1) == Map.size rows
     then product $ map (snd . Map.findMin) $ Map.elems rows
     else 0

{- |
Only sensible for square matrices,
but the function does not check whether the matrix is square.
-}
det :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) => Sparse.Matrix i j a -> a
det = detUpper . snd . decompose

{- |
Absolute value of the determinant.
This is also sound for non-square matrices.
-}
detAbs :: (Ix i, Enum i, Ix j, Enum j, RealFloat a) => Sparse.Matrix i j a -> a
detAbs = abs . det
