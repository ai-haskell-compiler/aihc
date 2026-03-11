module Test.Numeric.FFTW.Common where

import qualified Numeric.Netlib.Class as Class

import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.Complex as Complex
import qualified Data.NonEmpty as NonEmpty
import Data.Array.Comfort.Storable (Array)
import Data.Complex (Complex((:+)))
import Data.NonEmpty ((!:))

import Foreign.Storable.Record.Tuple (Tuple(getTuple))
import Foreign.Storable (Storable)

import Control.DeepSeq (NFData, deepseq)
import Control.Applicative ((<*>))

import Text.Printf (printf)

import qualified Test.QuickCheck as QC


prefix :: String -> [(String, test)] -> [(String, test)]
prefix msg =
   map (\(str,test) -> (msg ++ "." ++ str, test))


arrayFloat :: array Float -> array Float
arrayFloat = id

arrayDouble :: array Double -> array Double
arrayDouble = id

arrayComplexFloat :: array (Complex Float) -> array (Complex Float)
arrayComplexFloat = id

arrayComplexDouble :: array (Complex Double) -> array (Complex Double)
arrayComplexDouble = id


floatTol :: Float
floatTol = 1e-4

doubleTol :: Double
doubleTol = 1e-10

normInf ::
   (Shape.C sh, Storable b, Class.Real a) => (b -> a) -> Array sh b -> a
normInf mag xs =
   NonEmpty.maximum $ 0 !: Array.toList (Array.map mag xs)

approx ::
   (Shape.C sh, Eq sh, Storable b, Num b, Class.Real a) =>
   (b -> a) -> a -> Array sh b -> Array sh b -> Bool
approx mag tol xs ys =
   let absTol = tol * (normInf mag xs + normInf mag ys) in
   all (<=absTol) $ Array.toList $ Array.map mag $ Array.zipWith (-) xs ys

approxReal ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   a -> Array sh a -> Array sh a -> Bool
approxReal = approx abs

approxComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   a -> Array sh (Complex a) -> Array sh (Complex a) -> Bool
approxComplex = approx Complex.magnitude


adjust :: (Shape.C sh, Class.Floating a) => Array sh a -> Array sh a
adjust xs = Array.map (fromIntegral (Shape.size (Array.shape xs)) *) xs

split ::
   (Shape.C sh, Storable a, Storable b) =>
   Array sh (Tuple (a, b)) -> (Array sh a, Array sh b)
split xys = (Array.map (fst.getTuple) xys, Array.map (snd.getTuple) xys)

scalarProduct ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Array sh (Complex a) -> Array sh (Complex a) -> Complex a
scalarProduct xs ys =
   sum $ Array.toList $
   Array.zipWith (\x y -> Complex.conjugate x * y) xs ys


factors :: (Integral n) => n -> [(n,n)]
factors n =
   concatMap (\k ->
      case divMod n k of
         (q,0) -> if q==k then [(k,q)] else [(k,q),(q,k)]
         _ -> []) $
   takeWhile (\k -> k*k<=n) $ iterate (1+) 1

genCyclicArray1 ::
   (QC.Arbitrary a, Storable a) => QC.Gen (Array (Shape.Cyclic Int) a)
genCyclicArray1 = do
   xs <- fmap (take 1000) $ QC.arbitrary
   return $ Array.fromList (Shape.Cyclic $ length xs) xs

genCyclicArray2 ::
   (QC.Arbitrary a, Storable a) =>
   QC.Gen (Array (Shape.Cyclic Int, Shape.Cyclic Int) a)
genCyclicArray2 = do
   xys <- fmap (take 1000) $ QC.arbitrary
   (n0,n1) <-
      case xys of
         _:_ -> QC.elements (factors $ length xys)
         [] -> QC.elements [(,) 0, flip (,) 0] <*> QC.choose (0,10)
   return $ Array.fromList (Shape.Cyclic n0, Shape.Cyclic n1) xys

genCyclicArray3 ::
   (QC.Arbitrary a, Storable a) =>
   QC.Gen (Array (Shape.Cyclic Int, Shape.Cyclic Int, Shape.Cyclic Int) a)
genCyclicArray3 = do
   xyzs <- fmap (take 1000) $ QC.arbitrary
   (n0,n1,n2) <-
      case xyzs of
         _:_ -> QC.elements $ do
            (n0,m0) <- factors $ length xyzs
            (n1,n2) <- factors m0
            return (n0,n1,n2)
         [] ->
            QC.elements [(,,) 0, flip (,,) 0, \n0 n1 -> (n0,n1,0)]
               <*> QC.choose (0,10)
               <*> QC.choose (0,10)
   let sh = (Shape.Cyclic n0, Shape.Cyclic n1, Shape.Cyclic n2)
   return $ Array.fromList sh xyzs


immutable ::
   (Shape.C sh, Storable a, Eq sh, Eq a, NFData b) =>
   (Array sh a -> b) -> (Array sh a -> Bool)
immutable transform arr =
   let copy = Array.map id arr
   in deepseq (transform copy) (copy==arr)



newtype FixedFloat = FixedFloat Float

instance Show FixedFloat where
   show (FixedFloat a) = printf "%.2f" a

floatList :: (Shape.C sh) => Array sh Float -> [FixedFloat]
floatList = map FixedFloat . Array.toList


newtype FixedComplexFloat = FixedComplexFloat (Complex Float)

instance Show FixedComplexFloat where
   show (FixedComplexFloat (r:+i)) =
      let str :: Float -> String
          str x =
            if abs x < 0.005
               then "0.00" --avoid "-0.00"
               else
                  if x>=0 then printf "%.2f" x else printf "(%.2f)" x
      in printf "%s:+%s" (str r) (str i)

complexFloatList ::
   (Shape.C sh) => Array sh (Complex Float) -> [FixedComplexFloat]
complexFloatList = map FixedComplexFloat . Array.toList
