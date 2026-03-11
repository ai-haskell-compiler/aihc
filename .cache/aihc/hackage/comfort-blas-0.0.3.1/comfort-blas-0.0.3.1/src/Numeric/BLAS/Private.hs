{-# LANGUAGE TypeFamilies #-}
module Numeric.BLAS.Private where

import qualified Numeric.BLAS.FFI.Real as BlasReal
import qualified Numeric.BLAS.FFI.Generic as Blas
import qualified Numeric.Netlib.Utility as Call
import qualified Numeric.Netlib.Class as Class
import Numeric.BLAS.Matrix.Modifier (Conjugation(NonConjugated, Conjugated))
import Numeric.BLAS.Scalar (RealOf, zero, one, minusOne, isZero)

import qualified Foreign.Marshal.Array.Guarded as ForeignArray
import qualified Foreign.Marshal.Utils as Marshal
import qualified Foreign.C.String as CStr
import Foreign.Marshal.Array (advancePtr)
import Foreign.C.Types (CChar, CInt)
import Foreign.Ptr (Ptr, castPtr)
import Foreign.Storable (Storable, peek, pokeElemOff, peekElemOff)

import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad (when)
import Control.Applicative (liftA2)

import qualified Data.Array.Comfort.Shape as Shape

import qualified Data.Complex as Complex
import Data.Complex (Complex((:+)))

import Prelude hiding (sum)


type ShapeInt = Shape.ZeroBased Int

shapeInt :: Int -> ShapeInt
shapeInt = Shape.ZeroBased


realPtr :: Ptr a -> Ptr (RealOf a)
realPtr = castPtr


pointerSeq :: (Storable a) => Int -> Ptr a -> [Ptr a]
pointerSeq k ptr = iterate (flip advancePtr k) ptr


fill :: (Class.Floating a) => a -> Int -> Ptr a -> IO ()
fill a n dstPtr = evalContT $ do
   nPtr <- Call.cint n
   srcPtr <- Call.number a
   incxPtr <- Call.cint 0
   incyPtr <- Call.cint 1
   liftIO $ Blas.copy nPtr srcPtr incxPtr dstPtr incyPtr


copyConjugate ::
   (Class.Floating a) =>
   Ptr CInt -> Ptr a -> Ptr CInt -> Ptr a -> Ptr CInt -> IO ()
copyConjugate nPtr xPtr incxPtr yPtr incyPtr = do
   Blas.copy nPtr xPtr incxPtr yPtr incyPtr
   lacgv nPtr yPtr incyPtr



newtype Sum a = Sum {runSum :: Int -> Ptr a -> Int -> IO a}

sum :: Class.Floating a => Int -> Ptr a -> Int -> IO a
sum =
   runSum $
   Class.switchFloating
      (Sum sumReal)
      (Sum sumReal)
      (Sum sumComplex)
      (Sum sumComplex)

sumReal :: Class.Real a => Int -> Ptr a -> Int -> IO a
sumReal n xPtr incx =
   evalContT $ do
      nPtr <- Call.cint n
      incxPtr <- Call.cint incx
      yPtr <- Call.real one
      incyPtr <- Call.cint 0
      liftIO $ BlasReal.dot nPtr xPtr incxPtr yPtr incyPtr

sumComplex, sumComplexAlt ::
   Class.Real a => Int -> Ptr (Complex a) -> Int -> IO (Complex a)
sumComplex n xPtr incx =
   evalContT $ do
      nPtr <- Call.cint n
      let sxPtr = realPtr xPtr
      incxPtr <- Call.cint (2*incx)
      yPtr <- Call.real one
      incyPtr <- Call.cint 0
      liftIO $
         liftA2 (Complex.:+)
            (BlasReal.dot nPtr sxPtr incxPtr yPtr incyPtr)
            (BlasReal.dot nPtr (advancePtr sxPtr 1) incxPtr yPtr incyPtr)

sumComplexAlt n aPtr inca =
   evalContT $ do
      transPtr <- Call.char 'N'
      mPtr <- Call.cint 2
      nPtr <- Call.cint n
      onePtr <- Call.number one
      inc0Ptr <- Call.cint 0
      let saPtr = realPtr aPtr
      ldaPtr <- Call.leadingDim (2*inca)
      sxPtr <- Call.allocaArray n
      incxPtr <- Call.cint 1
      betaPtr <- Call.number zero
      yPtr <- Call.alloca
      let syPtr = realPtr yPtr
      incyPtr <- Call.cint 1
      liftIO $ do
         Blas.copy nPtr onePtr inc0Ptr sxPtr incxPtr
         gemv
            transPtr mPtr nPtr onePtr saPtr ldaPtr
            sxPtr incxPtr betaPtr syPtr incyPtr
         peek yPtr


mul ::
   (Class.Floating a) =>
   Conjugation -> Int -> Ptr a -> Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
mul conj n aPtr inca xPtr incx yPtr incy =
   mulAdd conj n aPtr inca xPtr incx zero yPtr incy

mulAdd ::
   (Class.Floating a) =>
   Conjugation ->
   Int -> Ptr a -> Int -> Ptr a -> Int -> a -> Ptr a -> Int -> IO ()
mulAdd conj n aPtr inca xPtr incx beta yPtr incy = evalContT $ do
   transPtr <- Call.char $ case conj of NonConjugated -> 'N'; Conjugated -> 'C'
   nPtr <- Call.cint n
   klPtr <- Call.cint 0
   kuPtr <- Call.cint 0
   alphaPtr <- Call.number one
   ldaPtr <- Call.leadingDim inca
   incxPtr <- Call.cint incx
   betaPtr <- Call.number beta
   incyPtr <- Call.cint incy
   liftIO $
      Blas.gbmv transPtr
         nPtr nPtr klPtr kuPtr alphaPtr aPtr ldaPtr
         xPtr incxPtr betaPtr yPtr incyPtr

{- |
Use the foldBalanced trick.
-}
product :: (Class.Floating a) => Int -> Ptr a -> Int -> IO a
product n aPtr inca =
   case compare n 1 of
      LT -> return one
      EQ -> peek aPtr
      GT -> let n2 = div n 2; new = n-n2
            in ForeignArray.alloca (2*new-1) $ \xPtr -> do
         mulPairs n2 aPtr inca xPtr 1
         when (odd n) $ pokeElemOff xPtr n2 =<< peekElemOff aPtr ((n-1)*inca)
         productLoop new xPtr

{- |
If 'mul' would be based on a scalar loop
we would not need to cut the vector into chunks.

The invariance is:
When calling @productLoop n xPtr@,
starting from xPtr there is storage allocated for 2*n-1 elements.
-}
productLoop :: (Class.Floating a) => Int -> Ptr a -> IO a
productLoop n xPtr =
   if n==1
      then peek xPtr
      else do
         let n2 = div n 2
         mulPairs n2 xPtr 1 (advancePtr xPtr n) 1
         productLoop (n-n2) (advancePtr xPtr (2*n2))

mulPairs ::
   (Class.Floating a) =>
   Int -> Ptr a -> Int -> Ptr a -> Int -> IO ()
mulPairs n aPtr inca xPtr incx =
   let inca2 = 2*inca
   in mul NonConjugated n aPtr inca2 (advancePtr aPtr inca) inca2 xPtr incx


newtype LACGV a = LACGV {getLACGV :: Ptr CInt -> Ptr a -> Ptr CInt -> IO ()}

lacgv :: Class.Floating a => Ptr CInt -> Ptr a -> Ptr CInt -> IO ()
lacgv =
   getLACGV $
   Class.switchFloating
      (LACGV $ const $ const $ const $ return ())
      (LACGV $ const $ const $ const $ return ())
      (LACGV clacgv)
      (LACGV clacgv)

clacgv :: Class.Real a => Ptr CInt -> Ptr (Complex a) -> Ptr CInt -> IO ()
clacgv nPtr xPtr incxPtr =
   Marshal.with minusOne $ \saPtr -> do
      incx <- peek incxPtr
      Marshal.with (2*incx) $ \incyPtr ->
         BlasReal.scal nPtr saPtr (advancePtr (realPtr xPtr) 1) incyPtr


{-
Work around an inconsistency of BLAS.
In case of a zero-column matrix
BLAS's gemv and gbmv do not initialize the target vector.
In contrast, these work-arounds do.
-}
{-# INLINE gemv #-}
gemv ::
   (Class.Floating a) =>
   Ptr CChar -> Ptr CInt -> Ptr CInt ->
   Ptr a -> Ptr a -> Ptr CInt ->
   Ptr a -> Ptr CInt -> Ptr a -> Ptr a -> Ptr CInt -> IO ()
gemv transPtr mPtr nPtr
      alphaPtr aPtr ldaPtr xPtr incxPtr betaPtr yPtr incyPtr = do
   initializeMV transPtr mPtr nPtr betaPtr yPtr incyPtr
   Blas.gemv transPtr mPtr nPtr
      alphaPtr aPtr ldaPtr xPtr incxPtr betaPtr yPtr incyPtr

{-# INLINE gbmv #-}
gbmv ::
   (Class.Floating a) =>
   Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr CInt -> Ptr CInt ->
   Ptr a -> Ptr a -> Ptr CInt -> Ptr a -> Ptr CInt ->
   Ptr a -> Ptr a -> Ptr CInt -> IO ()
gbmv transPtr mPtr nPtr klPtr kuPtr
      alphaPtr aPtr ldaPtr xPtr incxPtr betaPtr yPtr incyPtr = do
   initializeMV transPtr mPtr nPtr betaPtr yPtr incyPtr
   Blas.gbmv transPtr mPtr nPtr klPtr kuPtr
      alphaPtr aPtr ldaPtr xPtr incxPtr betaPtr yPtr incyPtr

initializeMV ::
   Class.Floating a =>
   Ptr CChar -> Ptr CInt -> Ptr CInt -> Ptr a -> Ptr a -> Ptr CInt -> IO ()
initializeMV transPtr mPtr nPtr betaPtr yPtr incyPtr = do
   trans <- peek transPtr
   let (mtPtr,ntPtr) =
         if trans == CStr.castCharToCChar 'N'
            then (mPtr,nPtr) else (nPtr,mPtr)
   n <- peek ntPtr
   beta <- peek betaPtr
   when (n == 0 && isZero beta) $
      Marshal.with 0 $ \incbPtr ->
      Blas.copy mtPtr betaPtr incbPtr yPtr incyPtr


{-
ToDo:

type ComplexShape =
         Shape.NestedTuple Shape.TupleAccessor (Complex Shape.Element)

This would allow the use of Complex.realPart as accessor,
but it requires GHC>7.6.3 or so, where realPart has no RealFloat constraint.
-}
type ComplexShape = Shape.NestedTuple Shape.TupleIndex (Complex Shape.Element)

ixReal, ixImaginary :: Shape.ElementIndex (Complex Shape.Element)
ixReal :+ ixImaginary =
   Shape.indexTupleFromShape (Shape.static :: ComplexShape)
