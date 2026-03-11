module EigensystemNum where

import Orthogonals
import Data.List

mult :: Num a => [[a]] -> [[a]] -> [[a]]
mult x y = matrix_matrix x (transposed y)

matSqr :: Num a => [[a]] -> [[a]]
matSqr x = mult x x

powerIter :: (Fractional a, Ord a) => [[a]] -> [([[a]],[[a]])]
powerIter x = tail (iterate
    (\(_,z)->let s=normalize (matSqr z) in (s,(mult x s)))
    ([],x)
  )

normalize :: (Fractional a, Ord a) => [[a]] -> [[a]]
normalize x = map (map (/(matnorm1 x))) x

getGrowth :: (Fractional a, Ord a) => ([[a]],[[a]]) -> a
getGrowth (x,y) = uncurry (/) (maximumBy
    (\(_,xc) (_,xa) -> compare (abs xc) (abs xa))
    (concat (zipWith zip y x))
  )

specRadApprox :: (Fractional a, Ord a) => [[a]] -> [a]
specRadApprox = map getGrowth . powerIter

eigenValuesApprox :: (Scalar a, Fractional a) => [[a]] -> [[a]]
eigenValuesApprox = map diagonals . iterate similar_to

limit :: (Num a, Ord a) => a -> [a] -> a
limit tol (x0:x1:xs) = if abs (x1-x0) < tol * abs x0
                       then x0
		       else limit tol (x1:xs)
limit _ _ = error "Only infinite sequences are allowed"
