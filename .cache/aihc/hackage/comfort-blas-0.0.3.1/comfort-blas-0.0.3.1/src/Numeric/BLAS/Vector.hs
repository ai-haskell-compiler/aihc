{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.BLAS.Vector (
   Vector,
   RealOf,
   ComplexOf,
   toList,
   fromList,
   autoFromList,
   CheckedArray.append, (+++),
   CheckedArray.take, CheckedArray.drop,
   CheckedArray.takeLeft, CheckedArray.takeRight,
   swap,
   reverse,
   cyclicReverse,
   CheckedArray.singleton,
   constant,
   zero,
   one,
   unit,
   dot, inner, (-*|),
   sum,
   absSum,
   norm2,
   norm2Squared,
   normInf,
   normInf1,
   argAbsMaximum,
   argAbs1Maximum,
   product,
   scale, scaleReal, (.*|),
   add, sub, (|+|), (|-|),
   negate, raise,
   mac,
   mul, mulConj,
   minimum, argMinimum,
   maximum, argMaximum,
   limits, argLimits,
   CheckedArray.foldl,
   CheckedArray.foldl1,
   CheckedArray.foldMap,

   conjugate,
   fromReal,
   toComplex,
   realPart,
   imaginaryPart,
   zipComplex,
   unzipComplex,
   ) where

import qualified Numeric.BLAS.Matrix.RowMajor as RowMajor
import qualified Numeric.BLAS.Scalar as Scalar
import qualified Numeric.BLAS.Private as Private
import Numeric.BLAS.Matrix.Modifier (Conjugation(NonConjugated, Conjugated))
import Numeric.BLAS.Scalar (ComplexOf, RealOf, minusOne)
import Numeric.BLAS.Private
         (ComplexShape, ixReal, ixImaginary, fill, copyConjugate, realPtr)

import qualified Numeric.BLAS.FFI.Generic as Blas
import qualified Numeric.BLAS.FFI.Complex as BlasComplex
import qualified Numeric.BLAS.FFI.Real as BlasReal
import qualified Numeric.Netlib.Utility as Call
import qualified Numeric.Netlib.Class as Class

import qualified Foreign.Marshal.Array.Guarded as ForeignArray
import Foreign.Marshal.Array (advancePtr)
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, peekElemOff, poke, pokeElemOff)
import Foreign.C.Types (CInt)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.ST (runST)
import Control.Monad (when, fmap, return, (=<<))
import Control.Applicative (liftA3, (<$>))

import qualified Data.Array.Comfort.Storable.Mutable.Unchecked as UMutArray
import qualified Data.Array.Comfort.Storable.Mutable as MutArray
import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Storable as CheckedArray
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array), append, (!))
import Data.Array.Comfort.Shape ((::+))

import Data.Function (id, flip, ($), (.))
import Data.Complex (Complex)
import Data.Maybe (Maybe(Nothing,Just), maybe)
import Data.Tuple.HT (mapFst, uncurry3)
import Data.Tuple (fst, snd)
import Data.Ord ((>=), (>))
import Data.Eq (Eq, (==))

import Prelude (Int, Integral, fromIntegral, (-), Char, error, IO)


{- $setup
>>> import Test.NumberModule.Type (Number_)
>>> import Test.Generator (genNumber)
>>> import Test.Slice (shapeInt)
>>> import Test.Utility (approx)
>>> import qualified Numeric.BLAS.Matrix.RowMajor as Matrix
>>> import qualified Numeric.BLAS.Vector as Vector
>>> import qualified Numeric.Netlib.Class as Class
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.List as List
>>> import Numeric.BLAS.Vector ((+++), (|+|), (|-|))
>>> import Numeric.BLAS.Scalar (RealOf, absolute, minusOne)
>>> import Data.Array.Comfort.Storable (Array, (!))
>>> import Data.Complex (Complex((:+)))
>>> import Data.Monoid ((<>))
>>> import Data.Tuple.HT (mapPair)
>>> import Control.Applicative (liftA2)
>>> import Control.Monad (replicateM)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>>
>>> type Vector = Vector.Vector (Shape.ZeroBased Int)
>>> type Real_ = RealOf Number_
>>> type Complex_ = Complex Real_
>>>
>>> maxElem :: Integer
>>> maxElem = 10
>>>
>>> maxDim :: Int
>>> maxDim = 100
>>>
>>> genVector ::
>>>    (Shape.C sh, Class.Floating a) =>
>>>    sh -> QC.Gen a -> QC.Gen (Vector.Vector sh a)
>>> genVector shape genElem =
>>>    fmap (Vector.fromList shape) $
>>>    replicateM (Shape.size shape) genElem
>>>
>>> real_ :: QC.Gen Real_
>>> real_ = genNumber maxElem
>>>
>>> complex_ :: QC.Gen Complex_
>>> complex_ = genNumber maxElem
>>>
>>> number_ :: QC.Gen Number_
>>> number_ = genNumber maxElem
>>>
>>> isNonEmpty :: Shape.C sh => Array sh a -> Bool
>>> isNonEmpty xs = Shape.size (Array.shape xs) > 0
>>>
>>> forVector ::
>>>    (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (Vector a -> prop) -> QC.Property
>>> forVector genElem =
>>>    QC.forAllShrink
>>>       (flip genVector genElem . shapeInt =<< QC.choose (0,maxDim))
>>>       (map Vector.autoFromList . QC.shrink . Vector.toList)
>>>
>>> forVector2 ::
>>>    (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (Vector a -> Vector a -> prop) -> QC.Property
>>> forVector2 genElem prop =
>>>    QC.forAllShrink
>>>       (do shape <- fmap shapeInt $ QC.choose (0,maxDim)
>>>           liftA2 (,) (genVector shape genElem) (genVector shape genElem))
>>>       (map (mapPair (Vector.autoFromList, Vector.autoFromList) . unzip) .
>>>        QC.shrink .
>>>        uncurry zip . mapPair (Vector.toList, Vector.toList))
>>>       (uncurry prop)
>>>
>>> type CyclicVector = Vector.Vector (Shape.Cyclic Int)
>>>
>>> genCyclicVector ::
>>>    (Class.Floating a) =>
>>>    Integer -> Int -> QC.Gen (CyclicVector a)
>>> genCyclicVector maxE dim =
>>>    fmap (Vector.fromList (Shape.Cyclic dim)) $
>>>    replicateM dim $ genNumber maxE
>>>
>>> cyclicVectorFromListGen :: (Class.Floating a) => [a] -> CyclicVector a
>>> cyclicVectorFromListGen xs = Vector.fromList (Shape.Cyclic $ length xs) xs
>>>
>>> cyclicVectorFromList :: [Number_] -> CyclicVector Number_
>>> cyclicVectorFromList = cyclicVectorFromListGen
>>>
>>> forCyclicVector ::
>>>    (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (CyclicVector a -> prop) -> QC.Property
>>> forCyclicVector genElem =
>>>    QC.forAllShrink
>>>       (flip genVector genElem . Shape.Cyclic =<< QC.choose (0,maxDim))
>>>       (map cyclicVectorFromListGen . QC.shrink . Vector.toList)
-}


type Vector = Array


toList :: (Shape.C sh, Storable a) => Vector sh a -> [a]
toList = Array.toList

fromList :: (Shape.C sh, Storable a) => sh -> [a] -> Vector sh a
fromList = CheckedArray.fromList

autoFromList :: (Storable a) => [a] -> Vector (Shape.ZeroBased Int) a
autoFromList = Array.vectorFromList


{- |
prop> QC.forAll number_ $ \x -> Vector.constant () x == Vector.singleton x

prop> :{
   QC.forAll (QC.choose (0,1000)) $ \n ->
   QC.forAll number_ $ \x ->
      Vector.sum (Vector.constant (shapeInt n) x) == fromIntegral n * x
:}

However, singleton does not need 'Class.Floating' constraint.
-}
constant :: (Shape.C sh, Class.Floating a) => sh -> a -> Vector sh a
constant sh a = Array.unsafeCreateWithSize sh $ fill a

{- |
prop> :{
   QC.forAll (QC.choose (0,1000)) $ \n ->
   Vector.sum (Vector.zero (shapeInt n)) == (0 :: Number_)
:}
-}
zero :: (Shape.C sh, Class.Floating a) => sh -> Vector sh a
zero = flip constant Scalar.zero

{- |
prop> :{
   QC.forAll (QC.choose (0,1000)) $ \n ->
   Vector.sum (Vector.one (shapeInt n)) == (fromIntegral n :: Number_)
:}
-}
one :: (Shape.C sh, Class.Floating a) => sh -> Vector sh a
one = flip constant Scalar.one

{- |
prop> :{
   QC.forAll (fmap shapeInt $ QC.choose (1,1000)) $ \sh ->
   QC.forAll (QC.elements $ Shape.indices sh) $ \k ->
   Vector.sum (Vector.unit sh k) == (1 :: Number_)
:}
-}
unit ::
   (Shape.Indexed sh, Class.Floating a) =>
   sh -> Shape.Index sh -> Vector sh a
unit sh ix = Array.unsafeCreateWithSize sh $ \n xPtr -> do
   fill Scalar.zero n xPtr
   pokeElemOff xPtr (Shape.offset sh ix) Scalar.one


{- |
Precedence and associativity (right) of (List.++).
This also matches '(::+)'.
-}
infixr 5 +++


{- |
prop> forVector number_ $ \xs -> forVector number_ $ \ys -> forVector number_ $ \zs -> Vector.toList ((xs +++ ys) +++ zs) == Vector.toList (xs +++ (ys +++ zs))
-}
(+++) ::
   (Shape.C shx, Shape.C shy, Storable a) =>
   Vector shx a -> Vector shy a -> Vector (shx::+shy) a
(+++) = append


{- |
prop> Vector.autoFromList [] == (Vector.reverse $ Vector.autoFromList [] :: Vector Number_)
prop> Vector.autoFromList [1] == (Vector.reverse $ Vector.autoFromList [1] :: Vector Number_)
prop> Vector.autoFromList [3,2,1] == (Vector.reverse $ Vector.autoFromList [1,2,3] :: Vector Number_)

prop> forVector number_ $ \xs -> reverse (Vector.toList xs) == Vector.toList (Vector.reverse xs)
prop> forVector number_ $ \xs -> xs == Vector.reverse (Vector.reverse xs)
prop> forVector number_ $ \xs -> forVector number_ $ \ys -> Vector.reverse (xs <> ys) == Vector.reverse ys <> Vector.reverse xs
-}
reverse ::
   (Integral n, Class.Floating a) =>
   Vector (Shape.ZeroBased n) a -> Vector (Shape.ZeroBased n) a
reverse (Array sh x) =
   Array.unsafeCreateWithSize sh $ \n yPtr -> evalContT $ do
      nPtr <- Call.cint n
      xPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      incyPtr <- Call.cint (-1)
      liftIO $ Blas.copy nPtr xPtr incxPtr yPtr incyPtr


{- |
prop> cyclicVectorFromList [] == Vector.cyclicReverse (cyclicVectorFromList [])
prop> cyclicVectorFromList [1] == Vector.cyclicReverse (cyclicVectorFromList [1])
prop> cyclicVectorFromList [1,3,2] == Vector.cyclicReverse (cyclicVectorFromList [1,2,3])
prop> cyclicVectorFromList [1,6,5,4,3,2] == Vector.cyclicReverse (cyclicVectorFromList [1,2,3,4,5,6])

prop> forCyclicVector number_ $ \xs -> xs == Vector.cyclicReverse (Vector.cyclicReverse xs)
-}
cyclicReverse ::
   (Integral n, Class.Floating a) =>
   Vector (Shape.Cyclic n) a -> Vector (Shape.Cyclic n) a
cyclicReverse (Array sh x) =
   Array.unsafeCreateWithSize sh $ \n yPtr -> evalContT $ when (n>0) $ do
      nPtr <- Call.cint (n-1)
      xPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      incyPtr <- Call.cint (-1)
      liftIO $ do
         poke yPtr =<< peek xPtr
         Blas.copy nPtr (advancePtr xPtr 1) incxPtr (advancePtr yPtr 1) incyPtr


{- |
prop> :{
   QC.forAll (QC.choose (1,100)) $ \dim ->
   QC.forAll (QC.choose (0, dim-1)) $ \i ->
   QC.forAll (QC.choose (0, dim-1)) $ \j ->
      Vector.unit (Shape.ZeroBased dim) i
      ==
      (Vector.swap i j (Vector.unit (Shape.ZeroBased dim) j) :: Vector Number_)
:}
-}
swap ::
   (Shape.Indexed sh, Storable a) =>
   Shape.Index sh -> Shape.Index sh -> Vector sh a -> Vector sh a
swap i j x =
   runST (do
      y <- MutArray.thaw x
      xi <- MutArray.read y i
      xj <- MutArray.read y j
      MutArray.write y i xj
      MutArray.write y j xi
      UMutArray.unsafeFreeze y)


infixl 7 -*|, .*|

newtype Dot f a = Dot {runDot :: f a -> f a -> a}

{- |
prop> :{
   forVector2 number_ $ \xs ys ->
      Vector.dot xs ys
      ==
      Matrix.multiply (Matrix.singleRow xs) (Matrix.singleColumn ys) ! ((),())
:}

prop> :{
   QC.forAll (QC.choose (1,100)) $ \dim ->
   QC.forAll (QC.choose (0, dim-1)) $ \i ->
   QC.forAll (QC.choose (0, dim-1)) $ \j ->
      Vector.dot
         (Vector.unit (shapeInt dim) i)
         (Vector.unit (shapeInt dim) j)
      ==
      (fromIntegral (fromEnum (i==j)) :: Number_)
:}
-}
dot, (-*|) ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Vector sh a -> Vector sh a -> a
(-*|) = dot
dot =
   runDot $
   Class.switchFloating
      (Dot dotReal)
      (Dot dotReal)
      (Dot $ dotComplex 'T')
      (Dot $ dotComplex 'T')

{- |
Scalar product

prop> forVector2 number_ $ \xs ys -> Vector.inner xs ys == Vector.dot (Vector.conjugate xs) ys
-}
inner ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Vector sh a -> Vector sh a -> a
inner =
   runDot $
   Class.switchFloating
      (Dot dotReal)
      (Dot dotReal)
      (Dot $ dotComplex 'C')
      (Dot $ dotComplex 'C')

dotReal ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Vector sh a -> Vector sh a -> a
dotReal arrX@(Array shX _x) (Array shY y) = unsafePerformIO $ do
   Call.assert "dot: shapes mismatch" (shX == shY)
   evalContT $ do
      (nPtr, sxPtr, incxPtr) <- vectorArgs arrX
      syPtr <- ContT $ withForeignPtr y
      incyPtr <- Call.cint 1
      liftIO $ BlasReal.dot nPtr sxPtr incxPtr syPtr incyPtr

{-
We cannot use 'cdot' because Haskell's FFI
does not support Complex numbers as return values.
-}
dotComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Char -> Vector sh (Complex a) -> Vector sh (Complex a) -> Complex a
dotComplex trans (Array shX x) (Array shY y) = unsafePerformIO $ do
   Call.assert "dot: shapes mismatch" (shX == shY)
   evalContT $ do
      let m = Shape.size shX
      transPtr <- Call.char trans
      mPtr <- Call.cint m
      nPtr <- Call.cint 1
      alphaPtr <- Call.number Scalar.one
      xPtr <- ContT $ withForeignPtr x
      ldxPtr <- Call.leadingDim m
      yPtr <- ContT $ withForeignPtr y
      incyPtr <- Call.cint 1
      betaPtr <- Call.number Scalar.zero
      zPtr <- Call.alloca
      inczPtr <- Call.cint 1
      liftIO $
         Private.gemv
            transPtr mPtr nPtr alphaPtr xPtr ldxPtr
            yPtr incyPtr betaPtr zPtr inczPtr
      liftIO $ peek zPtr

{- |
prop> forVector number_ $ \xs -> Vector.sum xs == List.sum (Vector.toList xs)
-}
sum :: (Shape.C sh, Class.Floating a) => Vector sh a -> a
sum (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr -> Private.sum (Shape.size sh) xPtr 1


{- |
Sum of the absolute values of real numbers or components of complex numbers.
For real numbers it is equivalent to 'Numeric.LAPACK.Vector.norm1'.
-}
absSum :: (Shape.C sh, Class.Floating a) => Vector sh a -> RealOf a
absSum arr = unsafePerformIO $
   evalContT $ liftIO . uncurry3 asum =<< vectorArgs arr

asum :: Class.Floating a => Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)
asum =
   getNrm $
   Class.switchFloating
      (Nrm BlasReal.asum) (Nrm BlasReal.asum)
      (Nrm BlasComplex.casum) (Nrm BlasComplex.casum)


{- |
Euclidean norm of a vector or Frobenius norm of a matrix.
-}
norm2 :: (Shape.C sh, Class.Floating a) => Vector sh a -> RealOf a
norm2 arr = unsafePerformIO $
   evalContT $ liftIO . uncurry3 nrm2 =<< vectorArgs arr

nrm2 :: Class.Floating a => Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)
nrm2 =
   getNrm $
   Class.switchFloating
      (Nrm BlasReal.nrm2) (Nrm BlasReal.nrm2)
      (Nrm BlasComplex.cnrm2) (Nrm BlasComplex.cnrm2)

newtype Nrm a =
   Nrm {getNrm :: Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)}


newtype Norm f a = Norm {getNorm :: f a -> RealOf a}

norm2Squared :: (Shape.C sh, Class.Floating a) => Vector sh a -> RealOf a
norm2Squared =
   getNorm $
   Class.switchFloating
      (Norm norm2SquaredReal)
      (Norm norm2SquaredReal)
      (Norm $ norm2SquaredReal . decomplex)
      (Norm $ norm2SquaredReal . decomplex)

norm2SquaredReal :: (Shape.C sh, Class.Real a) => Vector sh a -> a
norm2SquaredReal arr =
   unsafePerformIO $ evalContT $ do
      (nPtr, sxPtr, incxPtr) <- vectorArgs arr
      liftIO $ BlasReal.dot nPtr sxPtr incxPtr sxPtr incxPtr


{- |
prop> forVector number_ $ \xs -> Vector.normInf xs == List.maximum (0 : List.map absolute (Vector.toList xs))
-}
normInf :: (Shape.C sh, Class.Floating a) => Vector sh a -> RealOf a
normInf arr = unsafePerformIO $
   fmap (Scalar.absolute . maybe Scalar.zero snd) $ absMax arr

{- |
Computes (almost) the infinity norm of the vector.
For complex numbers every element is replaced
by the sum of the absolute component values first.
-}
normInf1 :: (Shape.C sh, Class.Floating a) => Vector sh a -> RealOf a
normInf1 arr = unsafePerformIO $
   evalContT $ do
      (nPtr, sxPtr, incxPtr) <- vectorArgs arr
      liftIO $
         fmap (Scalar.norm1 . maybe Scalar.zero snd) $
         peekElemOff1 sxPtr =<< Blas.iamax nPtr sxPtr incxPtr


{- |
Returns the index and value of the element with the maximal absolute value.
Caution: It actually returns the value of the element, not its absolute value!

>>> Vector.argAbsMaximum $ Vector.autoFromList [1:+2, 3:+4, 5, 6 :: Complex_]
(3,6.0 :+ 0.0)

prop> forVector number_ $ \xs -> isNonEmpty xs ==> let (xi,xm) = Vector.argAbsMaximum xs in xs!xi == xm
prop> forVector number_ $ \xs -> isNonEmpty xs ==> let (_xi,xm) = Vector.argAbsMaximum xs in List.all (\x -> absolute x <= absolute xm) $ Vector.toList xs
prop> forVector number_ $ \xs -> forVector number_ $ \ys -> isNonEmpty xs && isNonEmpty ys ==> let (_xi,xm) = Vector.argAbsMaximum xs; (_yi,ym) = Vector.argAbsMaximum ys; (zi,zm) = Vector.argAbsMaximum (xs+++ys) in case zi of Left _ -> xm==zm && absolute xm >= absolute ym; Right _ -> ym==zm && absolute xm < absolute ym
-}
argAbsMaximum ::
   (Shape.InvIndexed sh, Class.Floating a) =>
   Vector sh a -> (Shape.Index sh, a)
argAbsMaximum arr = unsafePerformIO $
   fmap
      (maybe
         (error "Vector.argAbsMaximum: empty vector")
         (mapFst (Shape.uncheckedIndexFromOffset $ Array.shape arr))) $
   absMax arr

absMax ::
   (Shape.C sh, Class.Floating a) =>
   Vector sh a -> IO (Maybe (Int, a))
absMax arr@(Array sh x) =
   case Scalar.complexSingletonOfFunctor arr of
      Scalar.Real -> evalContT $ do
         (nPtr, sxPtr, incxPtr) <- vectorArgs arr
         liftIO $ peekElemOff1 sxPtr =<< Blas.iamax nPtr sxPtr incxPtr
      Scalar.Complex -> evalContT $ do
         let n = Shape.size sh
         sxPtr <- ContT $ withForeignPtr x
         liftIO $ peekElemOff1 sxPtr =<< absMaxComplex n sxPtr

absMaxComplex :: (Class.Real a) => Int -> Ptr (Complex a) -> IO CInt
absMaxComplex n sxPtr =
   ForeignArray.alloca n $ \syPtr -> do
      let xrPtr = realPtr sxPtr
      Private.mul    NonConjugated n xrPtr 2 xrPtr 2 syPtr 1
      let xiPtr = advancePtr xrPtr 1
      Private.mulAdd NonConjugated n xiPtr 2 xiPtr 2 Scalar.one syPtr 1
      evalContT $ do
         nPtr <- Call.cint n
         incyPtr <- Call.cint 1
         liftIO $ Blas.iamax nPtr syPtr incyPtr


{- |
Returns the index and value of the element with the maximal absolute value.
The function does not strictly compare the absolute value of a complex number
but the sum of the absolute complex components.
Caution: It actually returns the value of the element, not its absolute value!

>>> Vector.argAbs1Maximum $ Vector.autoFromList [1:+2, 3:+4, 5, 6 :: Complex_]
(1,3.0 :+ 4.0)

prop> forVector real_ $ \xs -> isNonEmpty xs ==> Vector.argAbsMaximum xs == Vector.argAbs1Maximum xs
-}
argAbs1Maximum ::
   (Shape.InvIndexed sh, Class.Floating a) =>
   Vector sh a -> (Shape.Index sh, a)
argAbs1Maximum arr = unsafePerformIO $
   evalContT $ do
      (nPtr, sxPtr, incxPtr) <- vectorArgs arr
      liftIO $
         fmap
            (maybe
               (error "Vector.argAbs1Maximum: empty vector")
               (mapFst (Shape.uncheckedIndexFromOffset $ Array.shape arr))) $
         peekElemOff1 sxPtr =<< Blas.iamax nPtr sxPtr incxPtr

vectorArgs ::
   (Shape.C sh) => Array sh a -> ContT r IO (Ptr CInt, Ptr a, Ptr CInt)
vectorArgs (Array sh x) =
   liftA3 (,,)
      (Call.cint $ Shape.size sh)
      (ContT $ withForeignPtr x)
      (Call.cint 1)

peekElemOff1 :: (Storable a) => Ptr a -> CInt -> IO (Maybe (Int, a))
peekElemOff1 ptr k1 =
   let k1i = fromIntegral k1
       ki = k1i-1
   in if k1i == 0
         then return Nothing
         else Just . (,) ki <$> peekElemOff ptr ki


{- |
prop> QC.forAll (QC.choose (0,10)) $ \dim -> QC.forAll (genVector (shapeInt dim) (genNumber 3)) $ \xs -> approx 1e-2 (Vector.product xs) (List.product (Vector.toList (xs :: Vector Number_)))
-}
product :: (Shape.C sh, Class.Floating a) => Vector sh a -> a
product (Array sh x) = unsafePerformIO $
   withForeignPtr x $ \xPtr -> Private.product (Shape.size sh) xPtr 1


{- |
For restrictions see 'limits'.

prop> forVector real_ $ \xs -> isNonEmpty xs ==> Vector.minimum xs == List.minimum (Vector.toList xs)
prop> forVector real_ $ \xs -> isNonEmpty xs ==> Vector.maximum xs == List.maximum (Vector.toList xs)
prop> forVector real_ $ \xs -> isNonEmpty xs ==> - Vector.maximum xs == Vector.minimum (Vector.negate xs)
-}
minimum, maximum :: (Shape.C sh, Class.Real a) => Vector sh a -> a
minimum = fst . limits
maximum = snd . limits

{- |
For restrictions see 'limits'.
-}
argMinimum, argMaximum ::
   (Shape.InvIndexed sh, Shape.Index sh ~ ix, Class.Real a) =>
   Vector sh a -> (ix,a)
argMinimum = fst . argLimits
argMaximum = snd . argLimits

{- |
prop> forVector real_ $ \xs -> isNonEmpty xs ==> Vector.limits xs == Array.limits xs

In contrast to 'CheckedArray.limits'
this implementation is based on fast BLAS functions.
It should be faster than @Array.minimum@ and @Array.maximum@
although it is certainly not as fast as possible.
Boths limits share the precision of the limit with the larger absolute value.
This implies for example that you cannot rely on the property
that @raise (- minimum x) x@ has only non-negative elements.
-}
limits :: (Shape.C sh, Class.Real a) => Vector sh a -> (a,a)
limits xs0 =
   let xs = Array.mapShape Shape.Deferred xs0
       x0 = snd $ argAbs1Maximum xs
       x1 = xs ! fst (argAbs1Maximum (raise (-x0) xs))
   in if x0>=0 then (x1,x0) else (x0,x1)

{- |
For restrictions see 'limits'.
-}
argLimits ::
   (Shape.InvIndexed sh, Shape.Index sh ~ ix, Class.Real a) =>
   Vector sh a -> ((ix,a),(ix,a))
argLimits xs =
   let p0@(_i0,x0) = argAbs1Maximum xs
       p1 = (i1,xs!i1); i1 = fst $ argAbs1Maximum $ raise (-x0) xs
   in if x0>=0 then (p1,p0) else (p0,p1)


{- |
prop> forVector number_ $ \xs -> Vector.negate xs == Vector.scale minusOne xs
prop> forVector number_ $ \xs -> Vector.scale 2 xs == xs |+| xs
-}
scale, _scale, (.*|) ::
   (Shape.C sh, Class.Floating a) =>
   a -> Vector sh a -> Vector sh a
(.*|) = scale

scale alpha (Array sh x) = Array.unsafeCreateWithSize sh $ \n syPtr -> do
   evalContT $ do
      alphaPtr <- Call.number alpha
      nPtr <- Call.cint n
      sxPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      incyPtr <- Call.cint 1
      liftIO $ Blas.copy nPtr sxPtr incxPtr syPtr incyPtr
      liftIO $ Blas.scal nPtr alphaPtr syPtr incyPtr

_scale a (Array sh b) = Array.unsafeCreateWithSize sh $ \n cPtr -> do
   let m = 1
   let k = 1
   evalContT $ do
      transaPtr <- Call.char 'N'
      transbPtr <- Call.char 'N'
      mPtr <- Call.cint m
      kPtr <- Call.cint k
      nPtr <- Call.cint n
      alphaPtr <- Call.number Scalar.one
      aPtr <- Call.number a
      ldaPtr <- Call.leadingDim m
      bPtr <- ContT $ withForeignPtr b
      ldbPtr <- Call.leadingDim k
      betaPtr <- Call.number Scalar.zero
      ldcPtr <- Call.leadingDim m
      liftIO $
         Blas.gemm
            transaPtr transbPtr mPtr nPtr kPtr alphaPtr
            aPtr ldaPtr bPtr ldbPtr betaPtr cPtr ldcPtr


scaleReal ::
   (Shape.C sh, Class.Floating a) =>
   RealOf a -> Vector sh a -> Vector sh a
scaleReal =
   getScaleReal $
   Class.switchFloating
      (ScaleReal scale)
      (ScaleReal scale)
      (ScaleReal $ \x -> recomplex . scale x . decomplex)
      (ScaleReal $ \x -> recomplex . scale x . decomplex)

newtype ScaleReal f a = ScaleReal {getScaleReal :: RealOf a -> f a -> f a}


decomplex ::
   (Class.Real a) =>
   Vector sh (Complex a) -> Vector (sh, ComplexShape) a
decomplex (Array sh a) = Array (sh, Shape.static) (castForeignPtr a)

recomplex ::
   (Class.Real a) =>
   Vector (sh, ComplexShape) a -> Vector sh (Complex a)
recomplex (Array (sh, Shape.NestedTuple _) a) = Array sh (castForeignPtr a)



infixl 6 |+|, |-|, `add`, `sub`


{- |
prop> forVector2 number_ $ \xs ys -> xs |+| ys == ys |+| xs
prop> forVector2 number_ $ \xs ys -> xs == xs |-| ys |+| ys
-}
add, sub, (|+|), (|-|) ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Vector sh a -> Vector sh a -> Vector sh a
add = mac Scalar.one
sub x y = mac minusOne y x

(|+|) = add
(|-|) = sub

mac ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   a -> Vector sh a -> Vector sh a -> Vector sh a
mac alpha (Array shX x) (Array shY y) =
   Array.unsafeCreateWithSize shX $ \n szPtr -> do
   Call.assert "mac: shapes mismatch" (shX == shY)
   evalContT $ do
      nPtr <- Call.cint n
      saPtr <- Call.number alpha
      sxPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      syPtr <- ContT $ withForeignPtr y
      incyPtr <- Call.cint 1
      inczPtr <- Call.cint 1
      liftIO $ Blas.copy nPtr syPtr incyPtr szPtr inczPtr
      liftIO $ Blas.axpy nPtr saPtr sxPtr incxPtr szPtr inczPtr


{- |
prop> forVector number_ $ \xs -> xs == Vector.negate (Vector.negate xs)
-}
negate :: (Shape.C sh, Class.Floating a) => Vector sh a -> Vector sh a
negate =
   getConjugate $
   Class.switchFloating
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)


{- |
prop> QC.forAll (genNumber maxElem) $ \d -> forVector number_ $ \xs -> xs == Vector.raise (-d) (Vector.raise d xs)
-}
raise :: (Shape.C sh, Class.Floating a) => a -> Vector sh a -> Vector sh a
raise c (Array shX x) =
   Array.unsafeCreateWithSize shX $ \n yPtr -> evalContT $ do
      nPtr <- Call.cint n
      cPtr <- Call.number c
      onePtr <- Call.number Scalar.one
      inccPtr <- Call.cint 0
      xPtr <- ContT $ withForeignPtr x
      inc1Ptr <- Call.cint 1
      liftIO $ do
         Blas.copy nPtr xPtr inc1Ptr yPtr inc1Ptr
         Blas.axpy nPtr onePtr cPtr inccPtr yPtr inc1Ptr


{- |
prop> forVector2 number_ $ \xs ys -> Vector.mul xs ys == Vector.mul ys xs
-}
mul ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Vector sh a -> Vector sh a -> Vector sh a
mul = mulConjugation NonConjugated

{- |
prop> forVector2 number_ $ \xs ys -> Vector.mulConj xs ys == Vector.mul (Vector.conjugate xs) ys
-}
mulConj ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Vector sh a -> Vector sh a -> Vector sh a
mulConj = mulConjugation Conjugated

mulConjugation ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Conjugation -> Vector sh a -> Vector sh a -> Vector sh a
mulConjugation conj (Array shA a) (Array shX x) =
      Array.unsafeCreateWithSize shX $ \n yPtr -> do
   Call.assert "mul: shapes mismatch" (shA == shX)
   evalContT $ do
      aPtr <- ContT $ withForeignPtr a
      xPtr <- ContT $ withForeignPtr x
      liftIO $ Private.mul conj n aPtr 1 xPtr 1 yPtr 1


newtype Conjugate f a = Conjugate {getConjugate :: f a -> f a}

conjugate ::
   (Shape.C sh, Class.Floating a) =>
   Vector sh a -> Vector sh a
conjugate =
   getConjugate $
   Class.switchFloating
      (Conjugate id)
      (Conjugate id)
      (Conjugate complexConjugate)
      (Conjugate complexConjugate)

complexConjugate ::
   (Shape.C sh, Class.Real a) =>
   Vector sh (Complex a) -> Vector sh (Complex a)
complexConjugate (Array sh x) = Array.unsafeCreateWithSize sh $ \n syPtr ->
   evalContT $ do
      nPtr <- Call.cint n
      sxPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      incyPtr <- Call.cint 1
      liftIO $ copyConjugate nPtr sxPtr incxPtr syPtr incyPtr


fromReal ::
   (Shape.C sh, Class.Floating a) => Vector sh (RealOf a) -> Vector sh a
fromReal =
   getFromReal $
   Class.switchFloating
      (FromReal id)
      (FromReal id)
      (FromReal complexFromReal)
      (FromReal complexFromReal)

newtype FromReal f a = FromReal {getFromReal :: f (RealOf a) -> f a}

toComplex ::
   (Shape.C sh, Class.Floating a) => Vector sh a -> Vector sh (ComplexOf a)
toComplex =
   getToComplex $
   Class.switchFloating
      (ToComplex complexFromReal)
      (ToComplex complexFromReal)
      (ToComplex id)
      (ToComplex id)

newtype ToComplex f a = ToComplex {getToComplex :: f a -> f (ComplexOf a)}

complexFromReal ::
   (Shape.C sh, Class.Real a) => Vector sh a -> Vector sh (Complex a)
complexFromReal (Array sh x) =
   Array.unsafeCreateWithSize sh $ \n yPtr ->
   case realPtr yPtr of
      yrPtr -> evalContT $ do
         nPtr <- Call.cint n
         xPtr <- ContT $ withForeignPtr x
         incxPtr <- Call.cint 1
         incyPtr <- Call.cint 2
         inczPtr <- Call.cint 0
         zPtr <- Call.number Scalar.zero
         liftIO $ do
            Blas.copy nPtr xPtr incxPtr yrPtr incyPtr
            Blas.copy nPtr zPtr inczPtr (advancePtr yrPtr 1) incyPtr


realPart ::
   (Shape.C sh, Class.Floating a) => Vector sh a -> Vector sh (RealOf a)
realPart =
   getToReal $
   Class.switchFloating
      (ToReal id)
      (ToReal id)
      (ToReal $ RowMajor.takeColumn ixReal . decomplex)
      (ToReal $ RowMajor.takeColumn ixReal . decomplex)
{-
      (ToReal $ RowMajor.takeColumn Complex.realPart . decomplex)
      (ToReal $ RowMajor.takeColumn Complex.realPart . decomplex)
-}

newtype ToReal f a = ToReal {getToReal :: f a -> f (RealOf a)}

imaginaryPart ::
   (Shape.C sh, Class.Real a) => Vector sh (Complex a) -> Vector sh a
imaginaryPart = RowMajor.takeColumn ixImaginary . decomplex
-- imaginaryPart = RowMajor.takeColumn Complex.imagPart . decomplex


zipComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Vector sh a -> Vector sh a -> Vector sh (Complex a)
zipComplex (Array shr xr) (Array shi xi) =
   Array.unsafeCreateWithSize shr $ \n yPtr -> evalContT $ do
      liftIO $ Call.assert "zipComplex: shapes mismatch" (shr==shi)
      nPtr <- Call.cint n
      xrPtr <- ContT $ withForeignPtr xr
      xiPtr <- ContT $ withForeignPtr xi
      let yrPtr = realPtr yPtr
      incxPtr <- Call.cint 1
      incyPtr <- Call.cint 2
      liftIO $ do
         Blas.copy nPtr xrPtr incxPtr yrPtr incyPtr
         Blas.copy nPtr xiPtr incxPtr (advancePtr yrPtr 1) incyPtr

unzipComplex ::
   (Shape.C sh, Class.Real a) =>
   Vector sh (Complex a) -> (Vector sh a, Vector sh a)
unzipComplex x = (realPart x, imaginaryPart x)
