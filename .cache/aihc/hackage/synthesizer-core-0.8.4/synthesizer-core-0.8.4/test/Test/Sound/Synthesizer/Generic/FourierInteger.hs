{-# LANGUAGE NoImplicitPrelude #-}
module Test.Sound.Synthesizer.Generic.FourierInteger (tests) where

import qualified Synthesizer.Generic.Fourier as Fourier
import qualified Synthesizer.Generic.Cyclic as Cyclic
import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.State.Signal as SigS
import qualified Synthesizer.Plain.Signal as Sig

import Test.QuickCheck (Testable, Arbitrary, arbitrary, quickCheck, )

import qualified Synthesizer.Basic.NumberTheory as NT

import qualified Number.ResidueClass.Check as RC
import Number.ResidueClass.Check ((/:), )

import qualified Algebra.ToInteger             as ToInteger
import qualified Algebra.IntegralDomain        as Integral
import qualified Algebra.Ring                  as Ring

import Control.Monad (liftM2, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


newtype Modulus a = Modulus a
   deriving (Show)

instance Ring.C a => Arbitrary (Modulus a) where
   arbitrary = fmap (Modulus . (2+) . fromInteger) arbitrary


data ModularSignal =
      ModularSignal (Modulus Integer) (Sig.T (RC.T Integer))
   deriving (Show)

instance Arbitrary ModularSignal where
   arbitrary =
      fmap (uncurry ModularSignal . signal) arbitrary


data ModularSignal2 =
      ModularSignal2
         (Modulus Integer) (Sig.T (RC.T Integer)) (Sig.T (RC.T Integer))
   deriving (Show)

instance Arbitrary ModularSignal2 where
   arbitrary =
      liftM2
         (\x y ->
            let len = min (CutG.length x) (CutG.length y)
                m = NT.fastFourierRing len
            in  ModularSignal2
                   (Modulus m)
                   (fmap (/: m) $ CutG.take len x)
                   (fmap (/: m) $ CutG.take len y))
         arbitrary
         arbitrary

scalarProduct ::
   Modulus Integer ->
   Sig.T (RC.T Integer) -> Sig.T (RC.T Integer) ->
   RC.T Integer
scalarProduct (Modulus m) xs ys =
   SigS.foldL (+) (RC.zero m) $
   SigS.zipWith (*)
      (SigG.toState xs)
      (SigG.toState ys)

signal ::
   Sig.T Integer -> (Modulus Integer, Sig.T (RC.T Integer))
signal xs =
   let m = NT.fastFourierRing $ length xs
   in  (Modulus m, fmap (/: m) xs)

modular ::
   (Integral.C a, ToInteger.C b) =>
   Modulus a -> b -> RC.T a
modular (Modulus m) =
   RC.fromRepresentative m . fromIntegral


simple ::
   (Testable t) =>
   (Sig.T Integer -> t) -> IO ()
simple = quickCheck

tests :: [(String, IO ())]
tests =
   ("fourier inverse",
      quickCheck $ \(ModularSignal m x) ->
         (Fourier.transformBackward $ Fourier.transformForward x)
         ==
         FiltNRG.amplify (modular m $ length x) x) :
   ("double fourier = reverse",
      quickCheck $ \(ModularSignal m x) ->
         (Cyclic.reverse $
          Fourier.transformForward $
          Fourier.transformForward x)
         ==
         FiltNRG.amplify (modular m $ length x) x) :
   ("fourier of reverse",
      quickCheck $ \(ModularSignal _m x) ->
         Cyclic.reverse (Fourier.transformForward x) ==
         Fourier.transformForward (Cyclic.reverse x)) :
   ("homogenity",
      quickCheck $ \(ModularSignal m x) y ->
         (FiltNRG.amplify (modular m (y::Integer)) $
          Fourier.transformForward x)
         ==
         (Fourier.transformForward $
          FiltNRG.amplify (modular m y) x)) :
   ("additivity",
      quickCheck $ \(ModularSignal2 _m x y) ->
         SigG.mix (Fourier.transformForward x) (Fourier.transformForward y)
         ==
         Fourier.transformForward (SigG.mix x y)) :
{-
   ("isometry",
      simple $ \xs x0 ->
         let (m,x) = signal (SigG.cons x0 xs)
         in  (AnaG.volumeVectorEuclideanSqr $ Fourier.transformForward x)
             ==
             (modular m (SigG.length x) *
              AnaG.volumeVectorEuclideanSqr x)) :
-}
   ("unitarity",
      quickCheck $ \(ModularSignal2 m x y) ->
         {-
         since there is no equivalent of a complex conjugate
         we have to take the scalar product with the backwards transform.
         -}
         scalarProduct m
            (Fourier.transformForward x) (Fourier.transformBackward y)
         ==
         modular m (length x) * scalarProduct m x y) :
   ("convolution",
      quickCheck $ \(ModularSignal2 _m x y) ->
         SigG.zipWith (*)
            (Fourier.transformForward x)
            (Fourier.transformForward y)
         ==
         Fourier.transformForward (Cyclic.convolve x y)) :
   ("convolution cyclic",
      quickCheck $ \(ModularSignal2 _m x y) ->
         Fourier.convolveCyclic x y
         ==
         Cyclic.convolve x y) :
   ("convolution long",
      simple $ \x0 y0 ->
         let m = Modulus $ NT.fastFourierRing $
                 2 * (NT.ceilingPowerOfTwo $ length x0)
             x = fmap (modular m) x0
             y = fmap (modular m) y0
         in  fmap (modular m) (FiltNRG.karatsubaFinite (*) x0 y0)
             ==
             Fourier.convolveWithWindow (Fourier.window x) y) :
   ("convolution long modular",
      simple $ \x0 y0 ->
         let m = Modulus $ NT.fastFourierRing $
                 2 * (NT.ceilingPowerOfTwo $ length x0)
             x = fmap (modular m) x0
             y = fmap (modular m) (y0 :: Sig.T Integer)
         in  FiltNRG.karatsubaFinite (*) x y
             ==
             Fourier.convolveWithWindow (Fourier.window x) y) :
   []
