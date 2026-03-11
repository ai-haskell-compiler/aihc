module Test.Sound.Synthesizer.Plain.Filter.Hilbert (tests) where

import qualified Synthesizer.Plain.Filter.Recursive.Hilbert as Hilbert
import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal as Sig

import qualified Synthesizer.Causal.Process as Causal

import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty

import Test.QuickCheck (quickCheck, {- Property, (==>) -})

-- import qualified Algebra.Module                as Module
-- import qualified Algebra.RealField             as RealField
-- import qualified Algebra.Ring                  as Ring
-- import qualified Algebra.Additive              as Additive
-- import qualified Number.Complex as Complex

import Data.Tuple.HT (mapPair, )

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


cascade :: NonEmpty.T (Rational, Rational) -> Sig.T Rational -> Bool
cascade ks xs =
   let p = uncurry Hilbert.Parameter $ unzip $
           map (mapPair (Allpass.Parameter, Allpass.Parameter)) $
           NonEmpty.toList ks
   in  Hilbert.run2 p xs ==
       Causal.apply (Hilbert.causal2 p) xs
{-
   in  map Complex.real (Hilbert.run2 p xs) == xs
-}


tests :: [(String, IO ())]
tests =
   ("hilbert", quickCheck cascade) :
   []
