{-# LANGUAGE NoImplicitPrelude #-}
module Test.Sound.Synthesizer.Generic.Fourier (tests) where

import qualified Synthesizer.Generic.Fourier as Fourier
import qualified Synthesizer.Generic.Cyclic as Cyclic
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Analysis as AnaG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Storable.Signal as SigSt
import qualified Synthesizer.State.Signal as SigS

import Test.QuickCheck (Testable, Arbitrary, arbitrary, quickCheck, )
import Test.Utility (approxEqualAbs, approxEqualComplexAbs, )

import qualified Number.Complex as Complex

import qualified Algebra.Ring                  as Ring
import qualified Algebra.Additive              as Additive

import Control.Monad (liftM2, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


tolerance :: Double
tolerance = 1e-10

normalize ::
   SigSt.T (Complex.T Double) -> SigSt.T (Complex.T Double)
normalize xs =
   FiltNRG.amplifyVector
      (recip $ max (0.1::Double) $ AnaG.volumeVectorMaximum xs) xs

newtype Normed = Normed (SigSt.T (Complex.T Double))
   deriving (Show)

instance Arbitrary Normed where
   arbitrary = fmap (Normed . normalize) arbitrary


data Normed2 =
      Normed2
         (SigSt.T (Complex.T Double))
         (SigSt.T (Complex.T Double))
   deriving (Show)

instance Arbitrary Normed2 where
   arbitrary =
      liftM2
         (\x y ->
            let len = min (CutG.length x) (CutG.length y)
            in  Normed2
                   (normalize $ CutG.take len x)
                   (normalize $ CutG.take len y))
         arbitrary
         arbitrary


-- could be moved to NumericPrelude
class Complex a where
   conjugate :: a -> a

instance (Additive.C a) => Complex (Complex.T a) where
   conjugate = Complex.conjugate

scalarProduct ::
   (SigG.Read sig y, Ring.C y, Complex y) =>
   sig y -> sig y -> y
scalarProduct xs ys =
   SigS.sum $
   SigS.zipWith (*)
      (SigG.toState xs)
      (SigS.map conjugate $ SigG.toState ys)

(=~=) ::
   SigSt.T (Complex.T Double) ->
   SigSt.T (Complex.T Double) ->
   Bool
(=~=) xs ys =
   SigG.length xs == SigG.length ys &&
   (SigG.foldR (&&) True $
    SigG.zipWith (approxEqualComplexAbs tolerance) xs ys)

simple ::
   (Testable t) =>
   (SigSt.T (Complex.T Double) -> t) -> IO ()
simple = quickCheck

tests :: [(String, IO ())]
tests =
   ("fourier inverse",
      quickCheck $ \(Normed x) ->
         x =~=
         (FiltNRG.amplify (recip $ fromIntegral $ SigG.length x) $
          Fourier.transformBackward $ Fourier.transformForward x)) :
   ("double fourier = reverse",
      quickCheck $ \(Normed x) ->
         x =~=
         (Cyclic.reverse $
          FiltNRG.amplify (recip $ fromIntegral $ SigG.length x) $
          Fourier.transformForward $
          Fourier.transformForward x)) :
   ("fourier of reverse",
      quickCheck $ \(Normed x) ->
         Cyclic.reverse (Fourier.transformForward x) =~=
         Fourier.transformForward (Cyclic.reverse x)) :
   ("fourier of conjugate",
      quickCheck $ \(Normed x) ->
         (SigG.map Complex.conjugate $ Fourier.transformForward x)
         =~=
         (Fourier.transformForward $
          SigG.map Complex.conjugate $ Cyclic.reverse x)) :
   ("additivity",
      quickCheck $ \(Normed2 x y) ->
         SigG.mix (Fourier.transformForward x) (Fourier.transformForward y)
         =~=
         Fourier.transformForward (SigG.mix x y)) :
   ("isometry",
      simple $ \xs x0 ->
         let x = normalize (SigG.cons x0 xs)
         in  approxEqualAbs tolerance
                (AnaG.volumeVectorEuclideanSqr $ Fourier.transformForward x)
                (fromIntegral (SigG.length x) *
                 AnaG.volumeVectorEuclideanSqr x)) :
   ("unitarity",
      quickCheck $ \(Normed2 x y) ->
         approxEqualComplexAbs tolerance
            (scalarProduct
               (Fourier.transformForward x) (Fourier.transformForward y))
            (fromIntegral (SigG.length x) * scalarProduct x y)) :
   ("convolution",
      quickCheck $ \(Normed2 x y) ->
         SigG.zipWith (*)
            (Fourier.transformForward x)
            (Fourier.transformForward y)
         =~=
         Fourier.transformForward (Cyclic.convolve x y)) :
   ("convolution cyclic",
      quickCheck $ \(Normed2 x y) ->
         Fourier.convolveCyclic x y
         =~=
         Cyclic.convolve x y) :
   ("convolution long",
      quickCheck $ \(Normed x) (Normed y) ->
         FiltNRG.karatsubaFinite (*) x y
         =~=
         Fourier.convolveWithWindow (Fourier.window x) y) :
   []
