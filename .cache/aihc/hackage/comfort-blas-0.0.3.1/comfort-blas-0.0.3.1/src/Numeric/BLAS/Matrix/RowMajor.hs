{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
module Numeric.BLAS.Matrix.RowMajor (
   Matrix,
   Square,
   Vector,
   height, width,
   Array2.singleRow, Array2.flattenRow,
   Array2.singleColumn, Array2.flattenColumn,
   identity,
   takeRow,
   takeColumn,
   fromRows,
   above,
   beside,
   takeTop, takeBottom,
   takeLeft, takeRight,
   tensorProduct,
   decomplex,
   recomplex,
   scaleRows,
   scaleColumns,
   multiplyVectorLeft,
   multiplyVectorRight,
   Transposable(..), nonTransposed, transposed,
   transposeTransposable,
   multiply,
   multiplyTransposable,
   kronecker,
   kroneckerTransposable,
   kroneckerLeftTransposable,
   ) where

import qualified Numeric.BLAS.Private as Private
import Numeric.BLAS.Matrix.Modifier (Conjugation(NonConjugated,Conjugated))
import Numeric.BLAS.Scalar (zero, one)
import Numeric.BLAS.Private (ShapeInt, shapeInt, ComplexShape, pointerSeq, fill)

import qualified Numeric.BLAS.FFI.Generic as Blas
import qualified Numeric.Netlib.Utility as Call
import qualified Numeric.Netlib.Class as Class

import Foreign.Marshal.Array (copyArray, advancePtr)
import Foreign.ForeignPtr (ForeignPtr, withForeignPtr, castForeignPtr)
import Foreign.Storable (Storable, poke)

import Control.Monad.Trans.Cont (ContT(ContT), evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (liftA2)

import qualified Data.Array.Comfort.Storable.Unchecked as Array
import qualified Data.Array.Comfort.Storable.Dim2 as Array2
import qualified Data.Array.Comfort.Shape.SubSize as SubSize
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Unchecked (Array(Array))
import Data.Array.Comfort.Shape ((::+))

import Data.Foldable (forM_)
import Data.Complex (Complex)
import Data.Tuple.HT (swap)


{- $setup
>>> import Test.NumberModule.Type (Number_)
>>> import Test.NumberModule.Numeric.BLAS.Vector (forVector, genVector, number_)
>>> import Test.Slice (ShapeInt, shapeInt)
>>> import qualified Numeric.BLAS.Matrix.RowMajor as Matrix
>>> import qualified Numeric.BLAS.Vector as Vector
>>> import qualified Numeric.Netlib.Class as Class
>>> import Numeric.BLAS.Scalar (RealOf)
>>> import qualified Data.Array.Comfort.Storable as Array
>>> import qualified Data.Array.Comfort.Shape as Shape
>>> import qualified Test.QuickCheck as QC
>>>
>>> type Matrix = Matrix.Matrix (Shape.ZeroBased Int) (Shape.ZeroBased Int)
>>> type Real_ = RealOf Number_
>>>
>>> maxDim :: Int
>>> maxDim = 10
>>>
>>> forMatrix ::
>>>    (QC.Testable prop, QC.Arbitrary a, Class.Floating a, Show a) =>
>>>    QC.Gen a -> (Matrix a -> prop) -> QC.Property
>>> forMatrix genElem =
>>>    QC.forAll
>>>       (do height <- fmap shapeInt $ QC.choose (0,maxDim)
>>>           width <- fmap shapeInt $ QC.choose (0,maxDim)
>>>           genVector (height, width) genElem)
>>>
>>> genIdentityTrans ::
>>>    (Shape.C sh, Class.Floating a) =>
>>>    sh -> QC.Gen (Matrix.Transposable sh sh a)
>>> genIdentityTrans sh = do
>>>    trans <- QC.arbitrary
>>>    return $
>>>       if trans
>>>          then Matrix.transposed (Matrix.identity sh)
>>>          else Matrix.nonTransposed (Matrix.identity sh)
>>>
>>> transpose ::
>>>    (Shape.C height, Eq height, Shape.C width, Class.Floating a) =>
>>>    Matrix.Matrix height width a -> Matrix.Matrix width height a
>>> transpose a =
>>>    Matrix.multiplyTransposable
>>>       (Matrix.transposed a)
>>>       (Matrix.nonTransposed (Matrix.identity (Matrix.height a)))
-}


type Matrix height width = Array (height,width)
{- |
There is also 'Shape.Square'
but this would be incompatible with other matrix operations.
This might be addressed in a new Matrix.Square module.
But for advanced type hacks you can already use the @lapack@ package.
-}
type Square sh = Matrix sh sh
type Vector = Array


height :: Matrix height width a -> height
height = fst . Array.shape

width :: Matrix height width a -> width
width = snd . Array.shape


{- |
>>> Matrix.identity (Shape.ZeroBased 0) :: Matrix.Square (Shape.ZeroBased Int) Real_
StorableArray.fromList (ZeroBased {... 0},ZeroBased {... 0}) []
>>> Matrix.identity (Shape.ZeroBased 3) :: Matrix.Square (Shape.ZeroBased Int) Real_
StorableArray.fromList (ZeroBased {... 3},ZeroBased {... 3}) [1.0,0.0,0.0,0.0,1.0,0.0,0.0,0.0,1.0]
-}
identity :: (Shape.C sh, Class.Floating a) => sh -> Square sh a
identity sh =
   Array.unsafeCreateWithAutoSizes (sh,sh) $
      \(SubSize.Sub blockSize (SubSize.Atom nint, SubSize.Atom _)) yPtr ->
   evalContT $ do
      nPtr <- Call.alloca
      xPtr <- Call.number zero
      incxPtr <- Call.cint 0
      incyPtr <- Call.cint 1
      liftIO $ do
         poke nPtr $ fromIntegral blockSize
         Blas.copy nPtr xPtr incxPtr yPtr incyPtr
         let n = fromIntegral nint
         poke nPtr n
         poke xPtr one
         poke incyPtr (n+1)
         Blas.copy nPtr xPtr incxPtr yPtr incyPtr


takeRow ::
   (Shape.Indexed height, Shape.C width, Shape.Index height ~ ix,
    Storable a) =>
   ix -> Matrix height width a -> Vector width a
takeRow ix (Array (height_,width_) x) =
   Array.unsafeCreateWithSize width_ $ \n yPtr ->
   withForeignPtr x $ \xPtr ->
      copyArray yPtr (advancePtr xPtr (n * Shape.offset height_ ix)) n

takeColumn ::
   (Shape.C height, Shape.Indexed width, Shape.Index width ~ ix,
    Class.Floating a) =>
   ix -> Matrix height width a -> Vector height a
takeColumn ix (Array (height_,width_) x) =
   Array.unsafeCreateWithSize height_ $ \n yPtr -> evalContT $ do
      let offset = Shape.offset width_ ix
      nPtr <- Call.cint n
      xPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint $ Shape.size width_
      incyPtr <- Call.cint 1
      liftIO $ Blas.copy nPtr (advancePtr xPtr offset) incxPtr yPtr incyPtr


fromRows ::
   (Shape.C width, Eq width, Storable a) =>
   width -> [Vector width a] -> Matrix ShapeInt width a
fromRows width_ rows =
   Array.unsafeCreateWithAutoSizes (shapeInt $ length rows, width_) $
         \(SubSize.Atom _, SubSize.Atom widthSize) dstPtr ->

      forM_ (zip (pointerSeq widthSize dstPtr) rows) $
         \(dstRowPtr, Array.Array rowWidth srcFPtr) ->
         withForeignPtr srcFPtr $ \srcPtr -> do
            Call.assert
               "Matrix.fromRows: non-matching vector size"
               (width_ == rowWidth)
            copyArray dstRowPtr srcPtr widthSize

infixr 2 `above`
infixr 3 `beside`

above ::
   (Shape.C heightA, Shape.C heightB) =>
   (Shape.C width, Eq width) =>
   (Storable a) =>
   Matrix heightA width a ->
   Matrix heightB width a ->
   Matrix (heightA::+heightB) width a
above = Array2.above

beside ::
   (Shape.C widthA, Shape.C widthB) =>
   (Shape.C height, Eq height) =>
   (Storable a) =>
   Matrix height widthA a ->
   Matrix height widthB a ->
   Matrix height (widthA::+widthB) a
beside = Array2.beside

takeTop ::
   (Shape.C heightA, Shape.C heightB, Shape.C width, Storable a) =>
   Matrix (heightA::+heightB) width a ->
   Matrix heightA width a
takeTop = Array2.takeTop

takeBottom ::
   (Shape.C heightA, Shape.C heightB, Shape.C width, Storable a) =>
   Matrix (heightA::+heightB) width a ->
   Matrix heightB width a
takeBottom = Array2.takeBottom

takeLeft ::
   (Shape.C height, Shape.C widthA, Shape.C widthB, Storable a) =>
   Matrix height (widthA::+widthB) a ->
   Matrix height widthA a
takeLeft = Array2.takeLeft

takeRight ::
   (Shape.C height, Shape.C widthA, Shape.C widthB, Storable a) =>
   Matrix height (widthA::+widthB) a ->
   Matrix height widthB a
takeRight = Array2.takeRight


{-# WARNING tensorProduct "Don't use conjugation. Left and Right are swapped." #-}
tensorProduct ::
   (Shape.C height, Shape.C width, Class.Floating a) =>
   Either Conjugation Conjugation ->
   Vector height a -> Vector width a -> Matrix height width a
tensorProduct side (Array height_ x) (Array width_ y) =
   Array.unsafeCreateWithAutoSizes (height_,width_) $
      \(SubSize.Atom n, SubSize.Atom m) cPtr -> do
   let trans conjugated =
         case conjugated of NonConjugated -> 'T'; Conjugated -> 'C'
   let ((transa,transb),(lda,ldb)) =
         case side of
            Left c -> ((trans c, 'N'),(1,1))
            Right c -> (('N', trans c),(m,n))
   evalContT $ do
      transaPtr <- Call.char transa
      transbPtr <- Call.char transb
      mPtr <- Call.cint m
      nPtr <- Call.cint n
      kPtr <- Call.cint 1
      alphaPtr <- Call.number one
      aPtr <- ContT $ withForeignPtr y
      ldaPtr <- Call.leadingDim lda
      bPtr <- ContT $ withForeignPtr x
      ldbPtr <- Call.leadingDim ldb
      betaPtr <- Call.number zero
      ldcPtr <- Call.leadingDim m
      liftIO $
         Blas.gemm
            transaPtr transbPtr mPtr nPtr kPtr alphaPtr
            aPtr ldaPtr bPtr ldbPtr betaPtr cPtr ldcPtr


decomplex ::
   (Class.Real a) =>
   Matrix height width (Complex a) ->
   Matrix height (width, ComplexShape) a
decomplex (Array (height_,width_) a) =
   Array (height_, (width_, Shape.static)) (castForeignPtr a)

recomplex ::
   (Class.Real a) =>
   Matrix height (width, ComplexShape) a ->
   Matrix height width (Complex a)
recomplex (Array (height_, (width_, Shape.NestedTuple _)) a) =
   Array (height_,width_) (castForeignPtr a)


scaleRows ::
   (Shape.C height, Eq height, Shape.C width, Class.Floating a) =>
   Vector height a -> Matrix height width a -> Matrix height width a
scaleRows (Array heightX x) (Array shape a) =
   Array.unsafeCreateWithAutoSizes shape $
      \(SubSize.Atom m, SubSize.Atom n) bPtr -> do

   Call.assert "scaleRows: sizes mismatch" (heightX == fst shape)
   evalContT $ do
      nPtr <- Call.cint n
      xPtr <- ContT $ withForeignPtr x
      aPtr <- ContT $ withForeignPtr a
      incaPtr <- Call.cint 1
      incbPtr <- Call.cint 1
      liftIO $ sequence_ $ take m $
         zipWith3
            (\xkPtr akPtr bkPtr -> do
               Blas.copy nPtr akPtr incaPtr bkPtr incbPtr
               Blas.scal nPtr xkPtr bkPtr incbPtr)
            (pointerSeq 1 xPtr)
            (pointerSeq n aPtr)
            (pointerSeq n bPtr)

scaleColumns ::
   (Shape.C height, Shape.C width, Eq width, Class.Floating a) =>
   Vector width a -> Matrix height width a -> Matrix height width a
scaleColumns (Array widthX x) (Array shape a) =
   Array.unsafeCreateWithAutoSizes shape $
      \(SubSize.Atom m, SubSize.Atom n) bPtr -> do

   Call.assert "scaleColumns: sizes mismatch" (widthX == snd shape)
   evalContT $ do
      transPtr <- Call.char 'N'
      nPtr <- Call.cint n
      klPtr <- Call.cint 0
      kuPtr <- Call.cint 0
      alphaPtr <- Call.number one
      xPtr <- ContT $ withForeignPtr x
      ldxPtr <- Call.leadingDim 1
      aPtr <- ContT $ withForeignPtr a
      incaPtr <- Call.cint 1
      betaPtr <- Call.number zero
      incbPtr <- Call.cint 1
      liftIO $ sequence_ $ take m $
         zipWith
            (\akPtr bkPtr ->
               Private.gbmv transPtr
                  nPtr nPtr klPtr kuPtr alphaPtr xPtr ldxPtr
                  akPtr incaPtr betaPtr bkPtr incbPtr)
            (pointerSeq n aPtr)
            (pointerSeq n bPtr)


{- |
>>> Matrix.multiplyVectorLeft (Array.vectorFromList [3,1,4]) (Array.fromList (Shape.ZeroBased (3::Int), Shape.Range 'a' 'b') [0,1,0,0,1,0::Real_])
StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [4.0,3.0]

prop> :{
   forVector number_ $ \xs ->
   Matrix.multiplyVectorLeft xs (Matrix.identity (Array.shape xs)) == xs
:}
-}
multiplyVectorLeft ::
   (Eq height, Shape.C height, Shape.C width, Class.Floating a) =>
   Vector height a -> Matrix height width a -> Vector width a
multiplyVectorLeft x a = multiplyVector x (NonTransposed a)

{- |
>>> Matrix.multiplyVectorRight (Array.fromList (Shape.Range 'a' 'b', Shape.ZeroBased (3::Int)) [0,0,1,1,0,0]) (Array.vectorFromList [3,1,4::Real_])
StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [4.0,3.0]
>>> Matrix.multiplyVectorRight (Array.fromList (Shape.Range 'a' 'b', Shape.ZeroBased (3::Int)) [2,7,1,8,2,8]) (Array.vectorFromList [3,1,4::Real_])
StorableArray.fromList (Range {rangeFrom = 'a', rangeTo = 'b'}) [17.0,58.0]

prop> :{
   forVector number_ $ \xs ->
   Matrix.multiplyVectorRight (Matrix.identity (Array.shape xs)) xs == xs
:}

prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \x ->
   Matrix.singleColumn (Matrix.multiplyVectorRight a x)
   ==
   Matrix.multiply a (Matrix.singleColumn x)
:}

prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genVector (fst $ Array.shape a) number_) $ \x ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \y ->
   Vector.dot x (Matrix.multiplyVectorRight a y)
   ==
   Vector.dot (Matrix.multiplyVectorLeft x a) y
:}

prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genVector (snd $ Array.shape a) number_) $ \x ->
   Matrix.multiplyVectorRight a x
   ==
   Matrix.multiplyVectorLeft x (transpose a)
:}
-}
multiplyVectorRight ::
   (Shape.C height, Shape.C width, Eq width, Class.Floating a) =>
   Matrix height width a -> Vector width a -> Vector height a
multiplyVectorRight a x = multiplyVector x (Transposed a)


data Transposable height width a =
     NonTransposed (Matrix height width a)
   | Transposed (Matrix width height a)
   deriving (Show)


nonTransposed :: Matrix height width a -> Transposable height width a
nonTransposed = NonTransposed

transposed :: Matrix height width a -> Transposable width height a
transposed = Transposed


transposeTransposable ::
   Transposable height width a -> Transposable width height a
transposeTransposable at =
   case at of
      NonTransposed a -> Transposed a
      Transposed a -> NonTransposed a


inspectTransposable ::
   Transposable height width a -> (Char, (height, width), ForeignPtr a)
inspectTransposable at =
   case at of
      NonTransposed (Array shA fptr) -> ('N', shA, fptr)
      Transposed (Array shA fptr) -> ('T', swap shA, fptr)

multiplyVector ::
   (Shape.C height, Shape.C width, Eq height, Class.Floating a) =>
   Vector height a -> Transposable height width a -> Vector width a
multiplyVector (Array sh x) at =
   let (transChar, (height_,width_), a) = inspectTransposable at in
   Array.unsafeCreateWithSize width_ $ \m0 yPtr -> do
   Call.assert
      "Matrix.RowMajor.multiplyVector: shapes mismatch"
      (height_ == sh)
   let n0 = Shape.size height_
   let (m,n) =
         case at of
            NonTransposed _ -> (m0,n0)
            Transposed _ -> (n0,m0)
   if n0==0
      then fill zero m0 yPtr
      else evalContT $ do

      let lda = m
      transPtr <- Call.char transChar
      mPtr <- Call.cint m
      nPtr <- Call.cint n
      alphaPtr <- Call.number one
      aPtr <- ContT $ withForeignPtr a
      ldaPtr <- Call.leadingDim lda
      xPtr <- ContT $ withForeignPtr x
      incxPtr <- Call.cint 1
      betaPtr <- Call.number zero
      incyPtr <- Call.cint 1
      liftIO $
         Blas.gemv
            transPtr mPtr nPtr alphaPtr aPtr ldaPtr
            xPtr incxPtr betaPtr yPtr incyPtr


{- |
>>> :{
   Matrix.multiply
      (Array.fromList (shapeInt 2, shapeInt 2) [1000,100,10,1])
      (Array.fromList (shapeInt 2, shapeInt 3) [0..5::Real_])
:}
... [300.0,1400.0,2500.0,3.0,14.0,25.0]

prop> :{
   forMatrix number_ $ \a ->
      Matrix.multiply (Matrix.identity (Matrix.height a)) a == a
:}
prop> :{
   forMatrix number_ $ \a ->
      Matrix.multiply a (Matrix.identity (Matrix.width a)) == a
:}
prop> :{
   forMatrix number_ $ \a ->
   forMatrix number_ $ \c ->
   QC.forAll (genVector (Matrix.width a, Matrix.height c) number_) $ \b ->
      Matrix.multiply a (Matrix.multiply b c)
      ==
      Matrix.multiply (Matrix.multiply a b) c
:}
-}
multiply ::
   (Shape.C height, Shape.C width, Shape.C fuse, Eq fuse, Class.Floating a) =>
   Matrix height fuse a -> Matrix fuse width a -> Matrix height width a
multiply a b = multiplyTransposable (NonTransposed a) (NonTransposed b)

{- |
prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.height a)) $ \eye ->
      a == Matrix.multiplyTransposable eye (Matrix.nonTransposed a)
:}
prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.width a)) $ \eye ->
      a == Matrix.multiplyTransposable (Matrix.nonTransposed a) eye
:}
prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (genIdentityTrans (Matrix.width a)) $ \leftEye ->
   QC.forAll (genIdentityTrans (Matrix.height a)) $ \rightEye ->
      Matrix.multiplyTransposable leftEye (Matrix.transposed a)
      ==
      Matrix.multiplyTransposable (Matrix.transposed a) rightEye
:}
prop> :{
   forMatrix number_ $ \a ->
   QC.forAll (QC.choose (0,maxDim)) $ \n ->
   QC.forAll (genVector (Matrix.width a, shapeInt n) number_) $ \b ->
      transpose (Matrix.multiply a b)
      ==
      Matrix.multiplyTransposable (Matrix.transposed b) (Matrix.transposed a)
:}
-}
multiplyTransposable ::
   (Shape.C height, Shape.C width, Shape.C fuse, Eq fuse, Class.Floating a) =>
   Transposable height fuse a ->
   Transposable fuse width a ->
   Matrix height width a
multiplyTransposable a b = multiplyColumnMajor b a

multiplyColumnMajor ::
   (Shape.C height, Shape.C width, Shape.C fuse, Eq fuse, Class.Floating a) =>
   Transposable fuse height a ->
   Transposable width fuse a ->
   Matrix width height a
multiplyColumnMajor at bt =
   let (transa, (widthA,heightA), a) = inspectTransposable at in
   let (transb, (widthB,heightB), b) = inspectTransposable bt in
   Array.unsafeCreate (widthB,heightA) $ \cPtr -> do
   Call.assert
      "Matrix.RowMajor.multiply: shapes mismatch"
      (widthA == heightB)

   evalContT $ do
   let m = Shape.size heightA
   let k = Shape.size widthA
   let n = Shape.size widthB
   let lda = case at of NonTransposed _ -> m; Transposed _ -> k
   let ldb = case bt of NonTransposed _ -> k; Transposed _ -> n
   let ldc = m
   if k==0
      then liftIO $ fill zero (m*n) cPtr
      else do
      transaPtr <- Call.char transa
      transbPtr <- Call.char transb
      mPtr <- Call.cint m
      nPtr <- Call.cint n
      kPtr <- Call.cint k
      alphaPtr <- Call.number one
      aPtr <- ContT $ withForeignPtr a
      ldaPtr <- Call.leadingDim lda
      bPtr <- ContT $ withForeignPtr b
      ldbPtr <- Call.leadingDim ldb
      betaPtr <- Call.number zero
      ldcPtr <- Call.leadingDim ldc
      liftIO $
         Blas.gemm
            transaPtr transbPtr mPtr nPtr kPtr alphaPtr aPtr ldaPtr
            bPtr ldbPtr betaPtr cPtr ldcPtr


{- |
>>> :{
   Matrix.kronecker
      (Array.fromList (shapeInt 2, shapeInt 2) [0,1,-1,0::Real_])
      (Array.fromList (shapeInt 2, shapeInt 3) [1..6])
:}
... [0.0,0.0,0.0,1.0,2.0,3.0,0.0,0.0,0.0,4.0,5.0,6.0,-1.0,-2.0,-3.0,0.0,0.0,0.0,-4.0,-5.0,-6.0,0.0,0.0,0.0]

>>> :{
   Matrix.kronecker
      (Array.fromList (shapeInt 2, shapeInt 2) [1,2,3,4::Real_])
      (Array.fromList (shapeInt 2, shapeInt 3) [1,2,4,8,16,32])
:}
... [1.0,2.0,4.0,2.0,4.0,8.0,8.0,16.0,32.0,16.0,32.0,64.0,3.0,6.0,12.0,4.0,8.0,16.0,24.0,48.0,96.0,32.0,64.0,128.0]

prop> :{
   QC.forAll (QC.choose (0,5)) $ \m ->
   QC.forAll (QC.choose (0,5)) $ \n ->
      Matrix.kronecker
         (Matrix.identity (shapeInt m))
         (Matrix.identity (shapeInt n))
      ==
      (Matrix.identity (shapeInt m, shapeInt n)
         :: Matrix.Square (ShapeInt, ShapeInt) Number_)
:}
-}
kronecker ::
   (Shape.C heightA, Shape.C widthA, Shape.C heightB, Shape.C widthB,
    Class.Floating a) =>
   Matrix heightA widthA a ->
   Matrix heightB widthB a ->
   Matrix (heightA,heightB) (widthA,widthB) a
kronecker a b = kroneckerLeftTransposable (NonTransposed a) b

kroneckerTransposable ::
   (Shape.C heightA, Shape.C widthA, Shape.C heightB, Shape.C widthB,
    Class.Floating a) =>
   Transposable heightA widthA a ->
   Transposable heightB widthB a ->
   Transposable (heightA,heightB) (widthA,widthB) a
kroneckerTransposable at bt =
   case bt of
      NonTransposed b -> NonTransposed $ kroneckerLeftTransposable at b
      Transposed b ->
         Transposed $ kroneckerLeftTransposable (transposeTransposable at) b

kroneckerLeftTransposable ::
   (Shape.C heightA, Shape.C widthA, Shape.C heightB, Shape.C widthB,
    Class.Floating a) =>
   Transposable heightA widthA a ->
   Matrix heightB widthB a ->
   Matrix (heightA,heightB) (widthA,widthB) a
kroneckerLeftTransposable at (Array (heightB,widthB) b) =
   let (_trans, (heightA,widthA), a) = inspectTransposable at
   in Array.unsafeCreate ((heightA,heightB), (widthA,widthB)) $ \cPtr ->
      evalContT $ do
   let (ma,na) = (Shape.size heightA, Shape.size widthA)
   let (mb,nb) = (Shape.size heightB, Shape.size widthB)
   let (lda,istep) =
         case at of
            NonTransposed _ -> (1,na)
            Transposed _ -> (ma,1)
   transaPtr <- Call.char 'N'
   transbPtr <- Call.char 'T'
   mPtr <- Call.cint na
   nPtr <- Call.cint nb
   kPtr <- Call.cint 1
   alphaPtr <- Call.number one
   aPtr <- ContT $ withForeignPtr a
   ldaPtr <- Call.leadingDim lda
   bPtr <- ContT $ withForeignPtr b
   ldbPtr <- Call.leadingDim 1
   betaPtr <- Call.number zero
   ldcPtr <- Call.leadingDim nb
   liftIO $
      forM_ (liftA2 (,) (take ma [0..]) (take mb [0..])) $ \(i,j) -> do
         let aiPtr = advancePtr aPtr (istep*i)
         let bjPtr = advancePtr bPtr (nb*j)
         let cijPtr = advancePtr cPtr (na*nb*(j+mb*i))
         Blas.gemm
            transbPtr transaPtr nPtr mPtr kPtr alphaPtr
            bjPtr ldbPtr aiPtr ldaPtr betaPtr cijPtr ldcPtr
