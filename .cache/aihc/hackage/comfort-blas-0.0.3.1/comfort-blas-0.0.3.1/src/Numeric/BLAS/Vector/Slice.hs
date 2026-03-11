{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.BLAS.Vector.Slice (
   T,
   shape,
   Vector,
   RealOf,
   ComplexOf,
   slice,
   fromStorableVector,
   fromVector,
   toVector,
   extract,
   access,
   replicate,
   append,
   Chunk, chunk,
   concat,
   dot, inner,
   sum,
   absSum,
   norm2,
   norm2Squared,
   normInf,
   normInf1,
   argAbsMaximum,
   argAbs1Maximum,
   product,
   scale, scaleReal,
   add, sub,
   negate, raise,
   mac,
   mul, mulConj,
   minimum, argMinimum,
   maximum, argMaximum,
   limits, argLimits,

   conjugate,
   fromReal,
   toComplex,
   realFromComplexVector,
   realPart,
   imaginaryPart,
   zipComplex,
   unzipComplex,
   ) where

import qualified Numeric.BLAS.Slice as Slice

import qualified Numeric.BLAS.Scalar as Scalar
import qualified Numeric.BLAS.Private as Private
import Numeric.BLAS.Matrix.Modifier (Conjugation(NonConjugated, Conjugated))
import Numeric.BLAS.Scalar (ComplexOf, RealOf)
import Numeric.BLAS.Private (ComplexShape, copyConjugate, realPtr)

import qualified Numeric.BLAS.FFI.Generic as Blas
import qualified Numeric.BLAS.FFI.Complex as BlasComplex
import qualified Numeric.BLAS.FFI.Real as BlasReal
import qualified Numeric.Netlib.Utility as Call
import qualified Numeric.Netlib.Class as Class

import qualified Foreign.Marshal.Array.Guarded as ForeignArray
import Foreign.Marshal.Array (advancePtr)
import Foreign.ForeignPtr (withForeignPtr, castForeignPtr)
import Foreign.Ptr (Ptr)
import Foreign.Storable (Storable, peek, peekElemOff)
import Foreign.C.Types (CInt)

import System.IO.Unsafe (unsafePerformIO)

import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (fmap, return, (=<<))
import Control.Applicative (liftA2, pure, (<$>), (<*>))

import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Shape as Shape
import qualified Data.StorableVector.Base as SVB
import qualified Data.Foldable as Fold
import qualified Data.NonEmpty as NonEmpty
import qualified Data.List as List
import Data.Array.Comfort.Storable.Unchecked (Array(Array))
import Data.Array.Comfort.Shape ((::+)((::+)))

import Text.Show (Show)
import Data.Function (($), (.))
import Data.Complex (Complex)
import Data.Maybe (Maybe(Nothing,Just), maybe)
import Data.Tuple.HT (mapFst, uncurry3)
import Data.Tuple (fst, snd, uncurry)
import Data.Ord ((>=))
import Data.Eq (Eq, (==))

import Prelude (Int, fromIntegral, (-), (+), (*), error, IO)


{- $setup
>>> :set -XTypeOperators
>>> :set -XGADTs
>>> import qualified Test.Slice as TestSlice
>>> import Test.NumberModule.Numeric.BLAS.Vector
>>>    (maxElem, maxDim, genVector, number_, real_, complex_)
>>> import Test.NumberModule.Type (Number_)
>>> import Test.Generator (genNumber)
>>> import Test.Utility (approx, approxReal)
>>>
>>> import qualified Numeric.BLAS.Matrix.RowMajor as Matrix
>>> import qualified Numeric.BLAS.Vector.Slice as VectorSlice
>>> import qualified Numeric.BLAS.Vector as Vector
>>> import qualified Numeric.BLAS.Slice as Slice
>>> import qualified Numeric.BLAS.Scalar as Scalar
>>> import qualified Numeric.Netlib.Class as Class
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Data.List as List
>>> import Numeric.BLAS.Vector ((+++), (|+|))
>>> import Numeric.BLAS.Scalar (RealOf, absolute, realPart, minusOne)
>>> import Data.Array.Comfort.Shape ((::+)((::+)))
>>> import Data.Tuple.HT (mapPair)
>>> import Data.Complex (Complex)
>>> import Control.Applicative (liftA2)
>>>
>>> import qualified Test.QuickCheck as QC
>>> import Test.QuickCheck ((==>))
>>>
>>> type Real_ = RealOf Number_
>>> type Complex_ = Complex Real_
>>>
>>> maxDim1 :: Int
>>> maxDim1 = 10
>>>
>>> type ShapeInt = Shape.ZeroBased Int
>>> type Shape = ShapeInt ::+ (ShapeInt, ShapeInt) ::+ ShapeInt
>>> type Vector = Vector.Vector Shape
>>> type Sliced = VectorSlice.T Shape ShapeInt
>>>
>>> genDim :: QC.Gen Int
>>> genDim = QC.choose (0,maxDim)
>>>
>>> genShapeDim :: Int -> QC.Gen Shape
>>> genShapeDim numRows = do
>>>    left <- fmap Shape.ZeroBased $ QC.choose (0,maxDim)
>>>    right <- fmap Shape.ZeroBased $ QC.choose (0,maxDim)
>>>    columns <- fmap Shape.ZeroBased $ QC.choose (1,maxDim1)
>>>    return (left ::+ (Shape.ZeroBased numRows, columns) ::+ right)
>>>
>>> genShape :: QC.Gen Shape
>>> genShape = genShapeDim =<< QC.choose (0,maxDim1)
>>>
>>> forAll_ :: (Show a) => QC.Gen a -> (a -> QC.Property) -> QC.Property
>>> forAll_ = QC.forAll
>>>
>>> isNonEmpty :: Shape.C sh => VectorSlice.T shA sh a -> Bool
>>> isNonEmpty xs = Shape.size (VectorSlice.shape xs) > 0
>>>
>>> takeColumn ::
>>>    (Shape.Indexed sh1, Shape.C sh, Shape.C sh0, Shape.C sh2) =>
>>>    Shape.Index sh1 ->
>>>    Vector.Vector (sh0 ::+ (sh, sh1) ::+ sh2) a ->
>>>    VectorSlice.T (sh0 ::+ (sh, sh1) ::+ sh2) sh a
>>> takeColumn c =
>>>    VectorSlice.slice (Slice.column c . Slice.left . Slice.right) .
>>>    VectorSlice.fromVector
>>>
>>> listFromSlice ::
>>>    (Shape.C sh, Class.Floating a) => VectorSlice.T shA sh a -> [a]
>>> listFromSlice = Vector.toList . VectorSlice.toVector
>>>
>>> genSlicedDim ::
>>>    (Class.Floating a) =>
>>>    Int -> QC.Gen a -> QC.Gen (Int, Vector a)
>>> genSlicedDim numRows genElem = do
>>>    shape@(_::+(_rows,columns)::+_) <- genShapeDim numRows
>>>    c <- QC.elements (Shape.indices columns)
>>>    fmap ((,) c) $ genVector shape genElem
>>>
>>> genSliced ::
>>>    (Class.Floating a) =>
>>>    QC.Gen a -> QC.Gen (Int, Vector a)
>>> genSliced genElem = flip genSlicedDim genElem =<< genDim
>>>
>>> shrinkSliced ::
>>>    (Shape.C sh0, Shape.Indexed sh1, QC.Arbitrary a, Class.Floating a) =>
>>>    (Shape.Index sh1,
>>>     Vector.Vector (ShapeInt ::+ (sh0, sh1) ::+ ShapeInt) a) ->
>>>    [(Shape.Index sh1,
>>>      Vector.Vector (ShapeInt ::+ (sh0, sh1) ::+ ShapeInt) a)]
>>> shrinkSliced (c,xs) =
>>>    let xs0 = Vector.takeLeft xs in
>>>    let xs1 = Vector.takeRight xs in
>>>    let xs10 = Vector.takeLeft xs1 in
>>>    let xs11 = Vector.takeRight xs1 in
>>>    map (\(ysl,ysr) ->
>>>          (c,
>>>           Vector.autoFromList ysl +++ xs10 +++ Vector.autoFromList ysr)) $
>>>    QC.shrink (Vector.toList xs0, Vector.toList xs11)
>>>
>>> forSliced ::
>>>    (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (Sliced a -> prop) -> QC.Property
>>> forSliced genElem prop =
>>>    QC.forAllShrink (genSliced genElem) shrinkSliced
>>>       (prop . uncurry takeColumn)
>>>
>>> genSliced2 ::
>>>    (Class.Floating a) =>
>>>    QC.Gen a -> QC.Gen ((Int, Vector a), (Int, Vector a))
>>> genSliced2 genElem = do
>>>    dim <- genDim
>>>    liftA2 (,) (genSlicedDim dim genElem) (genSlicedDim dim genElem)
>>>
>>> forSliced2 ::
>>>    (QC.Testable prop, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (Sliced a -> Sliced a -> prop) -> QC.Property
>>> forSliced2 genElem prop =
>>>    QC.forAll (genSliced2 genElem)
>>>       (uncurry prop . mapPair (uncurry takeColumn, uncurry takeColumn))
-}


type Vector = Array


shape :: T sh slice a -> slice
shape (Cons (Slice.Cons _s _k slc) _arr) = slc

mapShape :: (slice0 -> slice1) -> T sh slice0 a -> T sh slice1 a
mapShape f (Cons (Slice.Cons s k slc) arr) =
   Cons (Slice.Cons s k (f slc)) arr


increment :: T sh slice a -> Int
increment (Cons (Slice.Cons _s k _slc) _arr) = k


startArg ::
   (Storable a) =>
   T sh slice a -> Call.FortranIO r (Ptr a)
startArg (Cons (Slice.Cons s _k _slice) (Array _sh x)) = do
   sxPtr <- ContT $ withForeignPtr x
   return (advancePtr sxPtr s)

sliceArg ::
   (Storable a) =>
   T sh slice a -> Call.FortranIO r (Ptr a, Ptr CInt)
sliceArg x =
   liftA2 (,) (startArg x) (Call.cint $ increment x)

sizeSliceArg ::
   (Shape.C sh, Storable a) =>
   T shA sh a -> ContT r IO (Ptr CInt, Ptr a, Ptr CInt)
sizeSliceArg x =
   liftA2
      (\nPtr (xPtr,incxPtr) -> (nPtr, xPtr,incxPtr))
      (Call.cint $ Shape.size $ shape x)
      (sliceArg x)

infixl 4 <*|>

(<*|>) ::
   (Storable a) =>
   Call.FortranIO r (Ptr a -> Ptr CInt -> b) ->
   T sh slice a ->
   Call.FortranIO r b
f <*|> x  =  fmap uncurry f <*> sliceArg x


data T sh slice a = Cons (Slice.T slice) (Vector sh a)
   deriving (Show)

toVector ::
   (Shape.C slice, Class.Floating a) =>
   T sh slice a -> Vector slice a
toVector x =
   Array.unsafeCreateWithSize (shape x) $ \n syPtr ->
   evalContT $ Call.run $
      pure Blas.copy
         <*> Call.cint n
         <*|> x
         <*> pure syPtr
         <*> Call.cint 1

{- |
Non-copying conversion from @StorableVector@.
-}
fromStorableVector :: SVB.Vector a -> T ShapeInt ShapeInt a
fromStorableVector xs =
   case SVB.toForeignPtr xs of
      (buffer, s, l) ->
         Cons
            (Slice.Cons s 1 (Shape.ZeroBased l))
            (Array (Shape.ZeroBased (s+l)) buffer)

fromVector :: (Shape.C sh) => Vector sh a -> T sh sh a
fromVector xs = Cons (Slice.fromShape $ Array.shape xs) xs

slice :: (Slice.T shA -> Slice.T shB) -> T sh shA a -> T sh shB a
slice f (Cons slc xs) = Cons (f slc) xs

{- |
prop> QC.forAll genShape $ \shape@(_::+(_rows,columns)::+_) -> QC.forAll (QC.elements (Shape.indices columns)) $ \c -> QC.forAll (genVector shape number_) $ \xs -> VectorSlice.extract (Slice.column c . Slice.left . Slice.right) xs == Matrix.takeColumn c (Vector.takeLeft (Vector.takeRight (xs :: Vector Number_)))

prop> forAll_ (TestSlice.genShapeSelect 4 100) $ \(TestSlice.ShapeSelect sh select) -> QC.forAll (genVector sh number_) $ \xs -> case TestSlice.instantiate sh select of TestSlice.Extraction slice cut -> VectorSlice.extract slice xs == cut xs
-}
extract ::
   (Shape.C slice, Shape.C sh, Class.Floating a) =>
   (Slice.T sh -> Slice.T slice) -> Vector sh a -> Vector slice a
extract slc xs = toVector $ slice slc $ fromVector xs


access, (!) ::
   (Shape.C shA, Shape.Indexed sh, Storable a) =>
   T shA sh a -> Shape.Index sh -> a
access (Cons (Slice.Cons s k ssh) (Array sh x)) ix =
   Array (Shape.Deferred sh) x
   Array.!
   Shape.DeferredIndex (s + k * Shape.offset ssh ix)
(!) = access



{- |
prop> QC.forAll genShape $ \shape a -> VectorSlice.toVector (VectorSlice.replicate shape a) == Vector.constant shape (a :: Number_)
-}
replicate :: (Shape.C sh, Storable a) => sh -> a -> T () sh a
replicate sh a =
   Cons
      (Slice.Cons 0 0 sh)
      (Array.fromAssociations a () [])


type ShapeInt = Shape.ZeroBased Int

{- |
>>> List.map realPart $ Vector.toList $ VectorSlice.append (VectorSlice.fromVector $ Vector.autoFromList [3,1,4,1]) (VectorSlice.replicate (Shape.ZeroBased (3::Int)) (0::Number_))
[3.0,1.0,4.0,1.0,0.0,0.0,0.0]
-}
append ::
   (Shape.C sliceA, Shape.C sliceB, Class.Floating a) =>
   T shA sliceA a -> T shB sliceB a -> Vector (sliceA::+sliceB) a
append xs ys =
   Array.unsafeCreate (shape xs ::+ shape ys) $ \zPtr ->
   evalContT $ do
      let chunkX = chunk $ mapShape (Shape.ZeroBased . Shape.size) xs
      let chunkY = chunk $ mapShape (Shape.ZeroBased . Shape.size) ys
      nxPtr <- Call.cint $ chunkSize chunkX
      nyPtr <- Call.cint $ chunkSize chunkY
      (xPtr, incxPtr) <- chunkData chunkX
      (yPtr, incyPtr) <- chunkData chunkY
      inczPtr <- Call.cint 1
      liftIO $ Blas.copy nxPtr xPtr incxPtr zPtr inczPtr
      liftIO $ Blas.copy nyPtr yPtr incyPtr
                     (advancePtr zPtr $ chunkSize chunkX) inczPtr

data Chunk a =
   Chunk {
      chunkSize :: Int,
      chunkData :: ContT () IO (Ptr a, Ptr CInt)
   }

chunk :: (Storable a) => T sh ShapeInt a -> Chunk a
chunk x@(Cons slc _dat) = Chunk (Shape.size $ Slice.shape slc) (sliceArg x)

{- |
>>> List.map realPart $ Vector.toList $ let matrix = Vector.fromList (Shape.ZeroBased (4::Int), Shape.ZeroBased (3::Int)) $ map (\x -> fromInteger x :: Number_) [0 ..] in VectorSlice.concat $ map (\k -> VectorSlice.chunk (VectorSlice.slice (Slice.column k) $ VectorSlice.fromVector matrix)) [0,1,2]
[0.0,3.0,6.0,9.0,1.0,4.0,7.0,10.0,2.0,5.0,8.0,11.0]
-}
concat :: (Class.Floating a) => [Chunk a] -> Vector ShapeInt a
concat xs =
   let offsets = NonEmpty.scanl (+) 0 $ List.map chunkSize xs
   in Array.unsafeCreate (Shape.ZeroBased $ NonEmpty.last offsets) $ \yPtr ->
      Fold.for_ (List.zip (NonEmpty.flatten offsets) xs) $ \(offset, x) ->
      evalContT $ do
         nPtr <- Call.cint $ chunkSize x
         (xPtr, incxPtr) <- chunkData x
         incyPtr <- Call.cint 1
         liftIO $ Blas.copy nPtr xPtr incxPtr (advancePtr yPtr offset) incyPtr


dot ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   T shA sh a -> T shB sh a -> a
dot =
   runDot $
   Class.switchFloating
      (Dot dotReal)
      (Dot dotReal)
      (Dot dotComplex)
      (Dot dotComplex)


{- |
prop> forSliced2 number_ $ \xs ys -> VectorSlice.inner xs ys == Vector.dot (VectorSlice.conjugate xs) (VectorSlice.toVector ys)
prop> forSliced number_ $ \xs -> VectorSlice.inner xs xs == Scalar.fromReal (VectorSlice.norm2Squared xs)
-}
inner ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   T shA sh a -> T shB sh a -> a
inner =
   runDot $
   Class.switchFloating
      (Dot dotReal)
      (Dot dotReal)
      (Dot $ innerComplex . toVector)
      (Dot $ innerComplex . toVector)


newtype Dot f g a = Dot {runDot :: f a -> g a -> a}

dotReal ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   T shA sh a -> T shB sh a -> a
dotReal x y = unsafePerformIO $ do
   let shX = shape x
   let shY = shape y
   Call.assert "dot: shapes mismatch" (shX == shY)
   evalContT $ do
      nPtr <- Call.cint $ Shape.size shX
      (sxPtr, incxPtr) <- sliceArg x
      (syPtr, incyPtr) <- sliceArg y
      liftIO $ BlasReal.dot nPtr sxPtr incxPtr syPtr incyPtr

{-
We cannot use 'cdot' because Haskell's FFI
does not support Complex numbers as return values.
-}
dotComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   T shA sh (Complex a) -> T shB sh (Complex a) -> Complex a
dotComplex x y = unsafePerformIO $ do
   Call.assert "dot: shapes mismatch" (shape x == shape y)
   evalContT $ do
      transPtr <- Call.char 'N'
      mPtr <- Call.cint 1
      nPtr <- Call.cint $ Shape.size $ shape x
      alphaPtr <- Call.number Scalar.one
      (xPtr, ldxPtr) <- sliceArg x
      (yPtr, incyPtr) <- sliceArg y
      betaPtr <- Call.number Scalar.zero
      zPtr <- Call.alloca
      inczPtr <- Call.cint 1
      liftIO $
         Private.gemv
            transPtr mPtr nPtr alphaPtr xPtr ldxPtr
            yPtr incyPtr betaPtr zPtr inczPtr
      liftIO $ peek zPtr

{-
In contrast to Vector.inner
we cannot use gemv with 'C' because we need leading dimension > 1.
-}
innerComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   Vector sh (Complex a) -> T shB sh (Complex a) -> Complex a
innerComplex (Array shX x) y = unsafePerformIO $ do
   Call.assert "dot: shapes mismatch" (shX == shape y)
   evalContT $ do
      let m = Shape.size shX
      transPtr <- Call.char 'C'
      mPtr <- Call.cint m
      nPtr <- Call.cint 1
      alphaPtr <- Call.number Scalar.one
      xPtr <- ContT $ withForeignPtr x
      ldxPtr <- Call.leadingDim m
      (yPtr, incyPtr) <- sliceArg y
      betaPtr <- Call.number Scalar.zero
      zPtr <- Call.alloca
      inczPtr <- Call.cint 1
      liftIO $
         Private.gemv
            transPtr mPtr nPtr alphaPtr xPtr ldxPtr
            yPtr incyPtr betaPtr zPtr inczPtr
      liftIO $ peek zPtr


{- |
prop> forSliced number_ $ \xs -> VectorSlice.sum xs == List.sum (listFromSlice xs)
-}
sum :: (Shape.C sh, Class.Floating a) => T shA sh a -> a
sum x = unsafePerformIO $ evalContT $ do
   xPtr <- startArg x
   liftIO $ Private.sum (Shape.size $ shape x) xPtr (increment x)


{- |
Sum of the absolute values of real numbers or components of complex numbers.
For real numbers it is equivalent to 'Numeric.LAPACK.Vector.norm1'.
-}
absSum :: (Shape.C sh, Class.Floating a) => T shA sh a -> RealOf a
absSum arr = unsafePerformIO $
   evalContT $ liftIO . uncurry3 asum =<< sizeSliceArg arr

asum :: Class.Floating a => Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)
asum =
   getNrm $
   Class.switchFloating
      (Nrm BlasReal.asum) (Nrm BlasReal.asum)
      (Nrm BlasComplex.casum) (Nrm BlasComplex.casum)


{- |
Euclidean norm of a vector or Frobenius norm of a matrix.
-}
norm2 :: (Shape.C sh, Class.Floating a) => T shA sh a -> RealOf a
norm2 arr = unsafePerformIO $
   evalContT $ liftIO . uncurry3 nrm2 =<< sizeSliceArg arr

nrm2 :: Class.Floating a => Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)
nrm2 =
   getNrm $
   Class.switchFloating
      (Nrm BlasReal.nrm2) (Nrm BlasReal.nrm2)
      (Nrm BlasComplex.cnrm2) (Nrm BlasComplex.cnrm2)

newtype Nrm a = Nrm {getNrm :: Ptr CInt -> Ptr a -> Ptr CInt -> IO (RealOf a)}


newtype Norm f a = Norm {getNorm :: f a -> RealOf a}

norm2Squared :: (Shape.C sh, Class.Floating a) => T shA sh a -> RealOf a
norm2Squared =
   getNorm $
   Class.switchFloating
      (Norm norm2SquaredReal)
      (Norm norm2SquaredReal)
      (Norm norm2SquaredComplex)
      (Norm norm2SquaredComplex)

norm2SquaredReal :: (Shape.C sh, Class.Real a) => T shA sh a -> a
norm2SquaredReal x =
   unsafePerformIO $ evalContT $ do
      (nPtr, sxPtr, incxPtr) <- sizeSliceArg x
      liftIO $ BlasReal.dot nPtr sxPtr incxPtr sxPtr incxPtr

norm2SquaredComplex :: (Shape.C sh, Class.Real a) => T shA sh (Complex a) -> a
norm2SquaredComplex x =
   unsafePerformIO $ evalContT $ do
      nPtr <- Call.cint $ Shape.size $ shape x
      xPtr <- startArg x
      let xrPtr = realPtr xPtr
      let xiPtr = advancePtr xrPtr 1
      incxPtr <- Call.cint (increment x * 2)
      liftIO $
         liftA2 (+)
            (BlasReal.dot nPtr xrPtr incxPtr xrPtr incxPtr)
            (BlasReal.dot nPtr xiPtr incxPtr xiPtr incxPtr)


{- |
prop> forSliced number_ $ \xs -> VectorSlice.normInf xs == List.maximum (0 : List.map absolute (listFromSlice xs))
-}
normInf :: (Shape.C sh, Class.Floating a) => T shA sh a -> RealOf a
normInf arr = unsafePerformIO $
   fmap (Scalar.absolute . maybe Scalar.zero snd) $ absMax arr

{- |
Computes (almost) the infinity norm of the vector.
For complex numbers every element is replaced
by the sum of the absolute component values first.

prop> forSliced number_ $ \xs -> VectorSlice.normInf1 xs == List.maximum (0 : List.map Scalar.norm1 (listFromSlice xs))
-}
normInf1 :: (Shape.C sh, Class.Floating a) => T shA sh a -> RealOf a
normInf1 x = unsafePerformIO $
   evalContT $ do
      (nPtr, sxPtr, incxPtr) <- sizeSliceArg x
      liftIO $
         fmap (Scalar.norm1 . maybe Scalar.zero snd) $
         peekElemOff1 sxPtr (increment x) =<< Blas.iamax nPtr sxPtr incxPtr


{- |
Returns the index and value of the element with the maximal absolute value.
Caution: It actually returns the value of the element, not its absolute value!

prop> forSliced number_ $ \xs -> isNonEmpty xs ==> let (xi,xm) = VectorSlice.argAbsMaximum xs in VectorSlice.access xs xi == xm
prop> forSliced number_ $ \xs -> isNonEmpty xs ==> let (_xi,xm) = VectorSlice.argAbsMaximum xs in List.all (\x -> absolute x <= absolute xm) $ listFromSlice xs
prop> forSliced number_ $ \xs -> forSliced number_ $ \ys -> isNonEmpty xs && isNonEmpty ys ==> let (_xi,xm) = VectorSlice.argAbsMaximum xs; (_yi,ym) = VectorSlice.argAbsMaximum ys; (zi,zm) = Vector.argAbsMaximum (VectorSlice.toVector xs +++ VectorSlice.toVector ys) in case zi of Left _ -> xm==zm && absolute xm >= absolute ym; Right _ -> ym==zm && absolute xm < absolute ym
-}
argAbsMaximum ::
   (Shape.InvIndexed sh, Class.Floating a) =>
   T shA sh a -> (Shape.Index sh, a)
argAbsMaximum arr = unsafePerformIO $
   fmap
      (maybe
         (error "Vector.argAbsMaximum: empty vector")
         (mapFst (Shape.uncheckedIndexFromOffset $ shape arr))) $
   absMax arr

absMax ::
   (Shape.C sh, Class.Floating a) =>
   T shA sh a -> IO (Maybe (Int, a))
absMax x =
   case Scalar.complexSingletonOfFunctor x of
      Scalar.Real -> evalContT $ do
         (nPtr, sxPtr, incxPtr) <- sizeSliceArg x
         liftIO $
            peekElemOff1 sxPtr (increment x) =<< Blas.iamax nPtr sxPtr incxPtr
      Scalar.Complex -> evalContT $ do
         let n = Shape.size $ shape x
         sxPtr <- startArg x
         let incx = increment x
         liftIO $ peekElemOff1 sxPtr incx =<< absMaxComplex n sxPtr incx

absMaxComplex :: (Class.Real a) => Int -> Ptr (Complex a) -> Int -> IO CInt
absMaxComplex n sxPtr incx =
   ForeignArray.alloca n $ \syPtr -> do
      let xrPtr = realPtr sxPtr
      let incx2 = 2*incx
      Private.mul    NonConjugated n xrPtr incx2 xrPtr incx2 syPtr 1
      let xiPtr = advancePtr xrPtr 1
      Private.mulAdd NonConjugated n xiPtr incx2 xiPtr incx2 Scalar.one syPtr 1
      evalContT $ do
         nPtr <- Call.cint n
         incyPtr <- Call.cint 1
         liftIO $ Blas.iamax nPtr syPtr incyPtr


{- |
Returns the index and value of the element with the maximal absolute value.
The function does not strictly compare the absolute value of a complex number
but the sum of the absolute complex components.
Caution: It actually returns the value of the element, not its absolute value!

prop> forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.argAbsMaximum xs == VectorSlice.argAbs1Maximum xs
-}
argAbs1Maximum ::
   (Shape.InvIndexed sh, Class.Floating a) =>
   T shA sh a -> (Shape.Index sh, a)
argAbs1Maximum x = unsafePerformIO $
   evalContT $ do
      (nPtr, sxPtr, incxPtr) <- sizeSliceArg x
      liftIO $
         fmap
            (maybe
               (error "Vector.argAbs1Maximum: empty vector")
               (mapFst (Shape.uncheckedIndexFromOffset $ shape x))) $
         peekElemOff1 sxPtr (increment x) =<< Blas.iamax nPtr sxPtr incxPtr

peekElemOff1 :: (Storable a) => Ptr a -> Int -> CInt -> IO (Maybe (Int, a))
peekElemOff1 ptr inc k1 =
   let k1i = fromIntegral k1
       ki = k1i-1
   in if k1i == 0
         then return Nothing
         else Just . (,) ki <$> peekElemOff ptr (ki*inc)


{- |
prop> QC.forAll genShape $ \sh@(_::+(_rows,columns)::+_) -> QC.forAll (QC.elements (Shape.indices columns)) $ \c -> QC.forAll (genVector sh $ genNumber 3) $ \xt -> let xs = takeColumn c xt in approx 1e-2 (VectorSlice.product xs) (List.product (listFromSlice (xs :: Sliced Number_)))
-}
product :: (Shape.C sh, Class.Floating a) => T shA sh a -> a
product x = unsafePerformIO $ evalContT $ do
   xPtr <- startArg x
   liftIO $ Private.product (Shape.size $ shape x) xPtr (increment x)


{- |
For restrictions see 'limits'.

prop> forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.minimum xs == List.minimum (listFromSlice xs)
prop> forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.maximum xs == List.maximum (listFromSlice xs)
-}
minimum, maximum :: (Shape.C shA, Shape.C sh, Class.Real a) => T shA sh a -> a
minimum = fst . limits
maximum = snd . limits

{- |
For restrictions see 'limits'.
-}
argMinimum, argMaximum ::
   (Shape.C shA, Shape.InvIndexed sh, Shape.Index sh ~ ix, Class.Real a) =>
   T shA sh a -> (ix,a)
argMinimum = fst . argLimits
argMaximum = snd . argLimits

{- |
prop> forSliced real_ $ \xs -> isNonEmpty xs ==> VectorSlice.limits xs == Array.limits (VectorSlice.toVector xs)

In contrast to 'Array.limits'
this implementation is based on fast BLAS functions.
It should be faster than @Array.minimum@ and @Array.maximum@
although it is certainly not as fast as possible.
Boths limits share the precision of the limit with the larger absolute value.
This implies for example that you cannot rely on the property
that @raise (- minimum x) x@ has only non-negative elements.
-}
limits :: (Shape.C shA, Shape.C sh, Class.Real a) => T shA sh a -> (a,a)
limits xs0 =
   let xs = mapShape Shape.Deferred xs0
       x0 = snd $ argAbs1Maximum xs
       x1 = xs ! fst (argAbs1Maximum (fromVector (raise (-x0) xs)))
   in if x0>=0 then (x1,x0) else (x0,x1)

{- |
For restrictions see 'limits'.
-}
argLimits ::
   (Shape.C shA, Shape.InvIndexed sh, Shape.Index sh ~ ix, Class.Real a) =>
   T shA sh a -> ((ix,a),(ix,a))
argLimits xs =
   let p0@(_i0,x0) = argAbs1Maximum xs
       p1 = (i1,xs!i1); i1 = fst $ argAbs1Maximum $ fromVector $ raise (-x0) xs
   in if x0>=0 then (p1,p0) else (p0,p1)


{- |
prop> forSliced number_ $ \xs -> VectorSlice.negate xs == VectorSlice.scale minusOne xs
prop> forSliced number_ $ \xs -> VectorSlice.scale 2 xs == VectorSlice.add xs xs
-}
scale, _scale ::
   (Shape.C sh, Class.Floating a) =>
   a -> T shA sh a -> Vector sh a

scale alpha x = Array.unsafeCreateWithSize (shape x) $ \n syPtr -> do
   evalContT $ do
      alphaPtr <- Call.number alpha
      nPtr <- Call.cint n
      (sxPtr, incxPtr) <- sliceArg x
      incyPtr <- Call.cint 1
      liftIO $ Blas.copy nPtr sxPtr incxPtr syPtr incyPtr
      liftIO $ Blas.scal nPtr alphaPtr syPtr incyPtr

_scale a b = Array.unsafeCreateWithSize (shape b) $ \n cPtr -> do
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
      (bPtr, ldbPtr) <- sliceArg b
      betaPtr <- Call.number Scalar.zero
      ldcPtr <- Call.leadingDim m
      liftIO $
         Blas.gemm
            transaPtr transbPtr mPtr nPtr kPtr alphaPtr
            aPtr ldaPtr bPtr ldbPtr betaPtr cPtr ldcPtr


{- |
Complex implementation requires double number of multiplications
compared to 'Numeric.BLAS.Vector.scaleReal'.
-}
scaleReal ::
   (Shape.C sh, Class.Floating a) =>
   RealOf a -> T shA sh a -> Vector sh a
scaleReal =
   getScaleReal $
   Class.switchFloating
      (ScaleReal scale)
      (ScaleReal scale)
      (ScaleReal $ scale . Scalar.fromReal)
      (ScaleReal $ scale . Scalar.fromReal)

newtype ScaleReal f g a = ScaleReal {getScaleReal :: RealOf a -> f a -> g a}



infixl 6 `add`, `sub`


{- |
prop> forSliced2 number_ $ \xs ys -> VectorSlice.add xs ys == VectorSlice.add ys xs
prop> forSliced2 number_ $ \xs ys -> VectorSlice.toVector xs == VectorSlice.sub xs ys |+| VectorSlice.toVector ys
-}
add, sub ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   T shA sh a -> T shB sh a -> Vector sh a
add = mac Scalar.one
sub x y = mac Scalar.minusOne y x

mac ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   a -> T shA sh a -> T shB sh a -> Vector sh a
mac alpha x y =
   Array.unsafeCreateWithSize (shape x) $ \n szPtr -> do
   Call.assert "mac: shapes mismatch" (shape x == shape y)
   evalContT $ do
      nPtr <- Call.cint n
      saPtr <- Call.number alpha
      (sxPtr, incxPtr) <- sliceArg x
      (syPtr, incyPtr) <- sliceArg y
      inczPtr <- Call.cint 1
      liftIO $ Blas.copy nPtr syPtr incyPtr szPtr inczPtr
      liftIO $ Blas.axpy nPtr saPtr sxPtr incxPtr szPtr inczPtr


{- |
prop> forSliced number_ $ \xs -> VectorSlice.toVector xs == Vector.negate (VectorSlice.negate xs)
-}
negate :: (Shape.C sh, Class.Floating a) => T shA sh a -> Vector sh a
negate =
   getConjugate $
   Class.switchFloating
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)
      (Conjugate $ scaleReal Scalar.minusOne)


{- |
prop> QC.forAll (genNumber maxElem) $ \d -> forSliced number_ $ \xs -> VectorSlice.toVector xs == Vector.raise (-d) (VectorSlice.raise d xs)
-}
raise :: (Shape.C sh, Class.Floating a) => a -> T shA sh a -> Vector sh a
raise c x =
   Array.unsafeCreateWithSize (shape x) $ \n yPtr -> evalContT $ do
      nPtr <- Call.cint n
      cPtr <- Call.number c
      onePtr <- Call.number Scalar.one
      inccPtr <- Call.cint 0
      (xPtr, incxPtr) <- sliceArg x
      inc1Ptr <- Call.cint 1
      liftIO $ do
         Blas.copy nPtr xPtr incxPtr yPtr inc1Ptr
         Blas.axpy nPtr onePtr cPtr inccPtr yPtr inc1Ptr


{- |
prop> forSliced2 number_ $ \xs ys -> VectorSlice.mul xs ys == VectorSlice.mul ys xs
-}
mul ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   T shA sh a -> T shB sh a -> Vector sh a
mul = mulConjugation NonConjugated

{- |
prop> forSliced2 number_ $ \xs ys -> VectorSlice.mulConj xs ys == Vector.mul (VectorSlice.conjugate xs) (VectorSlice.toVector ys)
-}
mulConj ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   T shA sh a -> T shB sh a -> Vector sh a
mulConj = mulConjugation Conjugated

mulConjugation ::
   (Shape.C sh, Eq sh, Class.Floating a) =>
   Conjugation -> T shA sh a -> T shB sh a -> Vector sh a
mulConjugation conj a x =
      Array.unsafeCreateWithSize (shape x) $ \n yPtr -> do
   Call.assert "mul: shapes mismatch" (shape a == shape x)
   evalContT $ do
      aPtr <- startArg a
      xPtr <- startArg x
      liftIO $ Private.mul conj n aPtr (increment a) xPtr (increment x) yPtr 1


newtype Conjugate f g a = Conjugate {getConjugate :: f a -> g a}

conjugate ::
   (Shape.C sh, Class.Floating a) =>
   T shA sh a -> Vector sh a
conjugate =
   getConjugate $
   Class.switchFloating
      (Conjugate toVector)
      (Conjugate toVector)
      (Conjugate complexConjugate)
      (Conjugate complexConjugate)

complexConjugate ::
   (Shape.C sh, Class.Real a) =>
   T shA sh (Complex a) -> Vector sh (Complex a)
complexConjugate x = Array.unsafeCreateWithSize (shape x) $ \n syPtr ->
   evalContT $ do
      nPtr <- Call.cint n
      (sxPtr, incxPtr) <- sliceArg x
      incyPtr <- Call.cint 1
      liftIO $ copyConjugate nPtr sxPtr incxPtr syPtr incyPtr


fromReal ::
   (Shape.C sh, Class.Floating a) => T shA sh (RealOf a) -> Vector sh a
fromReal =
   getFromReal $
   Class.switchFloating
      (FromReal toVector)
      (FromReal toVector)
      (FromReal complexFromReal)
      (FromReal complexFromReal)

newtype FromReal f g a = FromReal {getFromReal :: f (RealOf a) -> g a}

toComplex ::
   (Shape.C sh, Class.Floating a) => T shA sh a -> Vector sh (ComplexOf a)
toComplex =
   getToComplex $
   Class.switchFloating
      (ToComplex complexFromReal)
      (ToComplex complexFromReal)
      (ToComplex toVector)
      (ToComplex toVector)

newtype ToComplex f g a = ToComplex {getToComplex :: f a -> g (ComplexOf a)}

complexFromReal ::
   (Shape.C sh, Class.Real a) => T shA sh a -> Vector sh (Complex a)
complexFromReal x =
   Array.unsafeCreateWithSize (shape x) $ \n yPtr ->
   case realPtr yPtr of
      yrPtr -> evalContT $ do
         nPtr <- Call.cint n
         (xPtr, incxPtr) <- sliceArg x
         incyPtr <- Call.cint 2
         inczPtr <- Call.cint 0
         zPtr <- Call.number Scalar.zero
         liftIO $ do
            Blas.copy nPtr xPtr incxPtr yrPtr incyPtr
            Blas.copy nPtr zPtr inczPtr (advancePtr yrPtr 1) incyPtr


realFromComplexVector ::
   (Shape.C sh) =>
   Vector sh (Complex a) -> T (sh, ComplexShape) (sh, ComplexShape) a
realFromComplexVector (Array sh a) =
   let csh = (sh, Shape.static) in
   Cons (Slice.fromShape csh) (Array csh (castForeignPtr a))


realPart ::
   (Shape.C sh, Class.Real a) =>
   T shA sh (Complex a) -> T (shA, ComplexShape) sh a
realPart (Cons (Slice.Cons s k slc) (Array sh a)) =
   Cons
      (Slice.Cons (2*s) (2*k) slc)
      (Array (sh, Shape.static) (castForeignPtr a))

imaginaryPart ::
   (Shape.C sh, Class.Real a) =>
   T shA sh (Complex a) -> T (shA, ComplexShape) sh a
imaginaryPart (Cons (Slice.Cons s k slc) (Array sh a)) =
   Cons
      (Slice.Cons (2*s+1) (2*k) slc)
      (Array (sh, Shape.static) (castForeignPtr a))


zipComplex ::
   (Shape.C sh, Eq sh, Class.Real a) =>
   T shA sh a -> T shB sh a -> Vector sh (Complex a)
zipComplex xr xi =
   Array.unsafeCreateWithSize (shape xr) $ \n yPtr -> evalContT $ do
      liftIO $ Call.assert "zipComplex: shapes mismatch" (shape xr == shape xi)
      nPtr <- Call.cint n
      (xrPtr, incxrPtr) <- sliceArg xr
      (xiPtr, incxiPtr) <- sliceArg xi
      let yrPtr = realPtr yPtr
      incyPtr <- Call.cint 2
      liftIO $ do
         Blas.copy nPtr xrPtr incxrPtr yrPtr incyPtr
         Blas.copy nPtr xiPtr incxiPtr (advancePtr yrPtr 1) incyPtr

{- |
prop> forSliced complex_ $ \xs -> approxReal 1e-2 (VectorSlice.norm2 xs) $ let (xrs,xis) = VectorSlice.unzipComplex xs in sqrt $ VectorSlice.norm2Squared xrs + VectorSlice.norm2Squared xis
-}
unzipComplex ::
   (Shape.C sh, Class.Real a) =>
   T shA sh (Complex a) ->
   (T (shA,ComplexShape) sh a, T (shA,ComplexShape) sh a)
unzipComplex x = (realPart x, imaginaryPart x)
