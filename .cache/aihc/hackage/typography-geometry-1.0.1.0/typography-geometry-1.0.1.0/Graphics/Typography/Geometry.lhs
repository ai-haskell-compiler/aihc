\begin{code}
{-# OPTIONS -XFlexibleInstances -XNamedFieldPuns #-}
-- | This module contains basic tools for geometric types and functions.
module Graphics.Typography.Geometry (Matrix2(..),
                            inverse,rotation,
                            Geometric(..),
                            leftMost,rightMost,topMost,bottomMost)
       where

import Algebra.Polynomials.Numerical

-- | The type of the transformation matrices used in all geometrical applications.
data Matrix2 a=
  -- | The application of @Matrix2 a b c d@ to vector @(x,y)@ should be
  -- @(ax+by,cx+dy)@.
  Matrix2 a a a a deriving (Show, Read, Eq)

-- | Inverses an inversible matrix. If it is not inversible,
-- The behaviour is undefined.
inverse::(Fractional a, Num a)=>Matrix2 a->Matrix2 a
inverse (Matrix2 a b c d)=
  let det=a*d-c*b in
  Matrix2 (d/det) (-b/det) (-c/det) (a/det)

     

instance Num a=>Num (Matrix2 a) where
  (+) (Matrix2 a b c d) (Matrix2 e f g h)=
    Matrix2 (a+e) (b+f) (c+g) (d+h)
  (*) (Matrix2 a b c d) (Matrix2 e f g h)=
    Matrix2 (a*e+b*g) (a*f+b*h) (c*e+d*g) (c*f+d*h)
  fromInteger a=Matrix2 (fromInteger a) 0 0 (fromInteger a)
  abs=undefined
  signum=undefined

instance Intervalize Matrix2 where
  intervalize (Matrix2 a b c d)=
    Matrix2 (interval a) (interval b) (interval c) (interval d)

  intersects (Matrix2 a b c d) (Matrix2 a' b' c' d')=
    (intersectsd a a') &&
    (intersectsd b b') &&
    (intersectsd c c') &&
    (intersectsd d d')
    
-- | A class for applying geometric applications to objects
class Geometric g where
  translate::Double->Double->g->g
  apply::Matrix2 Double->g->g

-- | The matrix of a rotation
rotation::Floating a=>a->Matrix2 a
rotation theta=
  let ct=cos theta
      st=sin theta
  in
   Matrix2 ct (-st) st ct

instance Geometric g=>Geometric [g] where
  
  translate x y cur=map (translate x y) cur
  apply m cur=map (apply m) cur
  

-- | @'leftMost' a b@ is the leftmost point between @a@ and @b@.
leftMost::(Double,Double)->(Double,Double)->(Double,Double)
leftMost u@(a,_) v@(b,_)
  | a<b = u
  | otherwise = v
-- | @'rightMost' a b@ is the rightmost point between @a@ and @b@.
rightMost::(Double,Double)->(Double,Double)->(Double,Double)
rightMost u@(a,_) v@(b,_)
  | a<b = v
  | otherwise = u
-- | @'bottomMost' a b@ is the lower point between @a@ and @b@.
bottomMost::(Double,Double)->(Double,Double)->(Double,Double)
bottomMost u@(_,a) v@(_,b)
  | a<b = u
  | otherwise = v
-- | @'topMost' a b@ is the upper point between @a@ and @b@.
topMost::(Double,Double)->(Double,Double)->(Double,Double)
topMost u@(_,a) v@(_,b)
  | a<b = v
  | otherwise = u




\end{code}
