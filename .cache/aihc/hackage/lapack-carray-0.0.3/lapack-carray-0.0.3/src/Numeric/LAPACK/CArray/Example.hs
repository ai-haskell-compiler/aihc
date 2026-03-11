module Numeric.LAPACK.CArray.Example where

import qualified Numeric.LAPACK.FFI.Real as Lapack
import qualified Numeric.Netlib.Class as Class
import qualified Numeric.Netlib.CArray.Utility as Call

import qualified Data.Array.IOCArray as IOCArray
import qualified Data.Array.CArray as CArray
import Data.Array.IOCArray (IOCArray)
import Data.Array.CArray (CArray)

import qualified Foreign.C.Types as C
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, FunPtr)

import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<*>), (<$>))



choleskyDecompose ::
   (Class.Real a) => Char -> Int -> IOCArray (Int,Int) a -> IO ()
choleskyDecompose uplo kd ab = do
   (n,ldab) <- Call.unzipBounds <$> IOCArray.getBounds ab
   evalContT $
      Call.runChecked "Banded.choleskyDecompose" $
         pure Lapack.pbtrf
          <*> Call.char uplo
          <*> Call.range n
          <*> Call.cint kd
          <*> Call.ioarray ab
          <*> Call.range ldab

choleskySolve ::
   (Class.Real a) =>
   Char -> Int -> CArray (Int,Int) a -> IOCArray (Int,Int) a -> IO ()
choleskySolve uplo kd ab b = do
   let (n,ldab) = Call.unzipBounds $ CArray.bounds ab
   (nrhs,ldb) <- Call.unzipBounds <$> IOCArray.getBounds b
   evalContT $
      Call.runChecked "Banded.choleskySolve" $
         pure Lapack.pbtrs
          <*> Call.char uplo
          <*> Call.range n
          <*> Call.cint kd
          <*> Call.range nrhs
          <*> Call.array ab
          <*> Call.range ldab
          <*> Call.ioarray b
          <*> Call.range ldb


leastSquares ::
   (Class.Real a) =>
   Char -> Int -> IOCArray (Int,Int) a -> IOCArray (Int,Int) a -> Int -> IO ()
leastSquares trans m a b lwork = do
   (n,lda) <- Call.unzipBounds <$> IOCArray.getBounds a
   (nrhs,ldb) <- Call.unzipBounds <$> IOCArray.getBounds b
   evalContT $
      Call.runChecked "Dense.leastSquares" $
         pure Lapack.gels
          <*> Call.char trans
          <*> Call.cint m
          <*> Call.range n
          <*> Call.range nrhs
          <*> Call.ioarray a
          <*> Call.range lda
          <*> Call.ioarray b
          <*> Call.range ldb
          <*> Call.allocaArray lwork
          <*> Call.cint lwork


eigenvalues ::
   Class.Real a =>
   Char -> Char -> FunPtr (Ptr a -> Ptr a -> IO Bool) ->
   IOCArray (Int,Int) a -> (Int,Int) -> Int ->
   IO (C.CInt, CArray Int a, CArray Int a, CArray (Int,Int) a)
eigenvalues jobvs sort select a ldvs lwork = do
   (n,lda) <- Call.unzipBounds <$> IOCArray.getBounds a
   wr <- Call.newArray n
   wi <- Call.newArray n
   vs <- Call.newArray (n,ldvs)
   sdim <- evalContT $ do
      sdimPtr <- Call.alloca
      Call.runChecked "Dense.eigenvalues" $
         pure Lapack.gees
          <*> Call.char jobvs
          <*> Call.char sort
          <*> pure select
          <*> Call.range n
          <*> Call.ioarray a
          <*> Call.range lda
          <*> pure sdimPtr
          <*> Call.ioarray wr
          <*> Call.ioarray wi
          <*> Call.ioarray vs
          <*> Call.range ldvs
          <*> Call.allocaArray lwork
          <*> Call.cint lwork
          <*> Call.allocaArray (CArray.rangeSize n)
      liftIO $ peek sdimPtr
   pure (,,,)
      <*> pure sdim
      <*> Call.freezeArray wr
      <*> Call.freezeArray wi
      <*> Call.freezeArray vs
