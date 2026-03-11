module Numeric.Interpolation.Private.Piece where

sqr :: (Num a) => a -> a
sqr x = x*x


type T x y ny = (x, ny) -> (x, ny) -> x -> y

linear :: (Fractional a) => T a a a
linear (x0,y0) (x1,y1) x =
   (y0*(x1-x) + y1*(x-x0)) / (x1-x0)

hermite1 :: (Fractional a) => T a a (a, a)
hermite1 (x0,(y0,dy0)) (x1,(y1,dy1)) x =
   let d = (y1-y0)/(x1-x0)
   in  linear (x0,y0) (x1,y1) x +
       (dy0-d) * sqr ((x-x1)/(x0-x1)) * (x-x0) +
       (dy1-d) * sqr ((x-x0)/(x1-x0)) * (x-x1)

hermite1' :: (Fractional a) => T a a (a, a)
hermite1' (x0,(y0,dy0)) (x1,(y1,dy1)) x =
   let d = (y1-y0)/(x1-x0)
   in  d +
       (dy0-d) / sqr (x0-x1) * (2*(x-x1) * (x-x0) + sqr (x-x1)) +
       (dy1-d) / sqr (x1-x0) * (2*(x-x0) * (x-x1) + sqr (x-x0))

hermite1'' :: (Fractional a) => T a a (a, a)
hermite1'' (x0,(y0,dy0)) (x1,(y1,dy1)) x =
   let d = (y1-y0)/(x1-x0)
   in  2*(dy0-d) / sqr (x0-x1) * (3*x-2*x1-x0) +
       2*(dy1-d) / sqr (x1-x0) * (3*x-2*x0-x1)
