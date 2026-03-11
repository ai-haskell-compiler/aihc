module Numeric.LAPACK.ComfortArray.Example where

import qualified Numeric.LAPACK.FFI.Real as Lapack
import qualified Numeric.Netlib.Class as Class
import qualified Numeric.Netlib.ComfortArray.Utility as Call
import Numeric.Netlib.ComfortArray.Utility (ZeroInt)

import qualified Data.Array.Comfort.Storable.Mutable as MutArray
import qualified Data.Array.Comfort.Storable as Array
import qualified Data.Array.Comfort.Shape as Shape
import Data.Array.Comfort.Storable.Mutable (IOArray)
import Data.Array.Comfort.Storable (Array)

import qualified Foreign.C.Types as C
import Foreign.Storable (peek)
import Foreign.Ptr (Ptr, FunPtr)

import Control.Monad.Trans.Cont (evalContT)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative (pure, (<*>))



choleskyDecompose ::
   (Class.Real a) => Char -> Int -> IOArray (ZeroInt,ZeroInt) a -> IO ()
choleskyDecompose uplo kd ab = do
   let (n,ldab) = MutArray.shape ab
   evalContT $
      Call.runChecked "Banded.choleskyDecompose" $
         pure Lapack.pbtrf
          <*> Call.char uplo
          <*> Call.shapeSize n
          <*> Call.cint kd
          <*> Call.ioarray ab
          <*> Call.shapeSize ldab

choleskySolve ::
   (Class.Real a) =>
   Char -> Int ->
   Array (ZeroInt,ZeroInt) a -> IOArray (ZeroInt,ZeroInt) a -> IO ()
choleskySolve uplo kd ab b = do
   let (n,ldab) = Array.shape ab
   let (nrhs,ldb) = MutArray.shape b
   evalContT $
      Call.runChecked "Banded.choleskySolve" $
         pure Lapack.pbtrs
          <*> Call.char uplo
          <*> Call.shapeSize n
          <*> Call.cint kd
          <*> Call.shapeSize nrhs
          <*> Call.array ab
          <*> Call.shapeSize ldab
          <*> Call.ioarray b
          <*> Call.shapeSize ldb


leastSquares ::
   (Class.Real a) =>
   Char -> Int ->
   IOArray (ZeroInt,ZeroInt) a -> IOArray (ZeroInt,ZeroInt) a -> Int -> IO ()
leastSquares trans m a b lwork = do
   let (n,lda) = MutArray.shape a
   let (nrhs,ldb) = MutArray.shape b
   evalContT $
      Call.runChecked "Dense.leastSquares" $
         pure Lapack.gels
          <*> Call.char trans
          <*> Call.cint m
          <*> Call.shapeSize n
          <*> Call.shapeSize nrhs
          <*> Call.ioarray a
          <*> Call.shapeSize lda
          <*> Call.ioarray b
          <*> Call.shapeSize ldb
          <*> Call.allocaArray lwork
          <*> Call.cint lwork


eigenvalues ::
   Class.Real a =>
   Char -> Char -> FunPtr (Ptr a -> Ptr a -> IO Bool) ->
   IOArray (ZeroInt,ZeroInt) a -> ZeroInt -> Int ->
   IO (C.CInt, Array ZeroInt a, Array ZeroInt a, Array (ZeroInt,ZeroInt) a)
eigenvalues jobvs sort select a ldvs lwork = do
   let (n,lda) = MutArray.shape a
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
          <*> Call.shapeSize n
          <*> Call.ioarray a
          <*> Call.shapeSize lda
          <*> pure sdimPtr
          <*> Call.ioarray wr
          <*> Call.ioarray wi
          <*> Call.ioarray vs
          <*> Call.shapeSize ldvs
          <*> Call.allocaArray lwork
          <*> Call.cint lwork
          <*> Call.allocaArray (Shape.size n)
      liftIO $ peek sdimPtr
   pure (,,,)
      <*> pure sdim
      <*> Call.freezeArray wr
      <*> Call.freezeArray wi
      <*> Call.freezeArray vs
