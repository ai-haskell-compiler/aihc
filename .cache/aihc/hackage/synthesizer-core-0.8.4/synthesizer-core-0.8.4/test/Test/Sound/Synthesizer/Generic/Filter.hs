{-# LANGUAGE NoImplicitPrelude #-}
module Test.Sound.Synthesizer.Generic.Filter (tests) where

import qualified Synthesizer.Generic.Filter.NonRecursive as FiltNRG
import qualified Synthesizer.Generic.Cyclic as Cyclic
import qualified Synthesizer.Generic.Signal as SigG
import qualified Synthesizer.Generic.Cut as CutG
import qualified Synthesizer.Plain.Signal as Sig

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty

import Test.QuickCheck (Testable, quickCheck, )

import qualified Algebra.Laws                  as Law

import NumericPrelude.Numeric
import NumericPrelude.Base


simple ::
   (Testable t) =>
   (Sig.T Int -> t) -> IO ()
simple = quickCheck

(=|=) ::
   (Eq sig, CutG.Transform sig) =>
   sig -> sig -> Bool
x =|= y =
   CutG.take 100 x == CutG.take 100 y

tests :: [(String, IO ())]
tests =
   ("identity",
      simple $ Law.identity FiltNRG.generic $ SigG.singleton one) :
   ("commutativity",
      simple $ Law.commutative FiltNRG.generic) :
   ("distributivity",
      simple $ Law.leftDistributive FiltNRG.generic SigG.mix) :
   ("karatsuba finite",
      simple $ \x y -> FiltNRG.generic x y == FiltNRG.karatsubaFinite (*) x y) :
   ("karatsuba finite-infinite",
      simple $ \x y -> FiltNRG.generic x y == FiltNRG.karatsubaFiniteInfinite (*) x y) :
   ("karatsuba infinite",
      simple $ \x y -> FiltNRG.generic x y == FiltNRG.karatsubaInfinite (*) x y) :
   ("karatsuba finite-infinite cycle",
      simple $ \x yn ->
         case NonEmpty.toInfiniteList yn of
            y -> FiltNRG.generic x y =|= FiltNRG.karatsubaFiniteInfinite (*) x y) :
   ("karatsuba infinite cycle",
      simple $ \x yn ->
         case NonEmpty.toInfiniteList yn of
            y -> FiltNRG.generic x y =|= FiltNRG.karatsubaInfinite (*) x y) :
   ("convolve triple",
      quickCheck $ \x y ->
         Cyclic.sumAndConvolveTriple x y ==
         Cyclic.sumAndConvolveTripleAlt x (y :: Cyclic.Triple Integer)) :
   ("periodic summation",
      simple $ \x y n ->
         let periodic = Cyclic.fromSignal SigG.defaultLazySize (1 + abs n)
         in  Cyclic.convolve (periodic x) (periodic y) ==
             periodic (FiltNRG.generic x y)) :
   []
