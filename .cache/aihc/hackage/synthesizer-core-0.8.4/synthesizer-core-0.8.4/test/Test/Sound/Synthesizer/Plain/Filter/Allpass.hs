module Test.Sound.Synthesizer.Plain.Filter.Allpass (tests) where

import qualified Synthesizer.Plain.Filter.Recursive.Allpass as Allpass
import qualified Synthesizer.Plain.Signal as Sig

import qualified Number.NonNegative as NonNeg

-- import qualified Test.Sound.Synthesizer.Plain.NonEmpty as NonEmpty

import Test.QuickCheck (quickCheck, {- Property, (==>) -})

import qualified Data.List.HT as ListHT

import Control.Monad.Trans.State (runState)

-- import qualified Algebra.Module                as Module
-- import qualified Algebra.RealField             as RealField
-- import qualified Algebra.Ring                  as Ring
-- import qualified Algebra.Additive              as Additive

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()


{- this will not work due to the poles
parameter :: Double -> Double -> Bool
parameter phase freq =
   approxEqual eps phase
      (Allpass.makePhase (Allpass.parameter phase freq) freq)
-}


cascadeStep :: Rational -> Rational -> (Rational, Rational, [Rational]) -> Bool
cascadeStep k u (s0,s1,ns) =
   let p = Allpass.Parameter k
       s = s0:s1:ns
   in  ListHT.allEqual $
          runState (Allpass.cascadeStepStack p u) s :
          runState (Allpass.cascadeStepRec p u) s :
          runState (Allpass.cascadeStepScanl p u) s :
          []


cascade :: NonNeg.Int -> Sig.T Rational -> Sig.T Rational -> Bool
cascade order ks xs =
   let ps = map Allpass.Parameter ks
       n = NonNeg.toNumber order
   in  Allpass.cascadeState n ps xs ==
       Allpass.cascadeIterative n ps xs


tests :: [(String, IO ())]
tests =
   ("cascadeStep", quickCheck cascadeStep) :
   ("cascade", quickCheck cascade) :
   []
