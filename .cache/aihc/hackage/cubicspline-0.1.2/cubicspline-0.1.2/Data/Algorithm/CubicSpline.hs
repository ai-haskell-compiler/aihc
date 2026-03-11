module Data.Algorithm.CubicSpline(cubicSplineCoefficients) where
import Prelude hiding (tail, init, head, last, minimum, maximum, foldr1, foldl1, (!!), read)
import Control.Arrow
import Numeric.LinearAlgebra
import Data.List(unfoldr)
import Safe

type PolyCos = [Double]

-- | Given a list of (x,y) co-ordinates, produces a list of coefficients to cubic equations, with knots at each of the initially provided x co-ordinates. Natural cubic spline interpololation is used. See: <http://en.wikipedia.org/wiki/Spline_interpolation#Interpolation_using_natural_cubic_spline>.
cubicSplineCoefficients :: [(Double, Double)] -> [PolyCos]
cubicSplineCoefficients xs = chunkBy 4 . concat . toLists $ linearSolveSVD mx solution
    where
      (mx,solution) = (fromLists *** tr' . fromLists . (:[])) .
                           unzip . map (first fillOut) . (extraConditions ++) . initNote "cubicSpline" . initNote "cubicSpline" . concat .
                           zipWith (map . first . (++)) (iterate (++[0,0,0,0]) []) $
                           map genEquations (xs `zip` drop 1 xs)

      genEquations :: ((Double, Double),(Double,Double)) -> [(PolyCos, Double)]
      genEquations ((x,y),(x',y')) = [ (zipWith (^) (repeat x)  ([0..3]::[Int]) , y)
                                      ,(zipWith (^) (repeat x') ([0..3]::[Int]) , y')
                                      ,(deriv1 ++ map negate deriv1     , 0)
                                      ,(deriv2 ++ map negate deriv2     , 0)]
              where deriv1 = [0, 1, 2*x', 3*x'^(2::Int)]
                    deriv2 = [0, 0, 2   , 6*x']
      extraConditions = [ ([0, 0, 2, 6 * fst (headNote "cubicSpline" xs)], 0)
                        , (replicate (fullLen - 4) 0 ++ [0, 0, 2, 6 * fst (lastNote "cubicSpline" xs)], 0)]
      fillOut = take fullLen . (++ repeat 0)
      fullLen = 4 * (length xs - 1)

chunkBy :: Int -> [t] -> [[t]]
chunkBy n = unfoldr go
    where go [] = Nothing
          go x  = Just $ splitAt n x
