module Test.Sound.Synthesizer.Plain.Wave (Ring, ringWave, tests) where

import qualified Synthesizer.Basic.Wave       as Wave
import qualified Synthesizer.Basic.Phase      as Phase

import qualified Test.QuickCheck as QC
import Test.QuickCheck
         (quickCheck, Arbitrary(arbitrary), elements, oneof, choose, )

import qualified Algebra.RealTranscendental    as RealTrans
import qualified Algebra.Ring                  as Ring

import Control.Monad (liftM, liftM2, )
import System.Random (Random)


import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()




data Ring a = Ring {ringName :: String, ringWave :: Wave.T a a}

instance Show (Ring a) where
   show = ringName

instance (Ord a, Ring.C a) => Arbitrary (Ring a) where
   arbitrary = elements $
      Ring "saw"      Wave.saw :
      Ring "square"   Wave.square :
      Ring "triangle" Wave.triangle :
      []




data ZeroDCOffset a = ZeroDCOffset {zdcName :: String, zdcWave :: Wave.T a a}

instance Show (ZeroDCOffset a) where
   show = zdcName

instance (RealTrans.C a, Random a) => Arbitrary (ZeroDCOffset a) where
   arbitrary =
      let cons n w = return (ZeroDCOffset n w)
      in  oneof $
            cons "sine"     Wave.sine :
            cons "saw"      Wave.saw :
            cons "square"   Wave.square :
            cons "triangle" Wave.triangle :
            liftM
               (ZeroDCOffset "squareBalanced" . Wave.squareBalanced)
               (choose (negate one, one)) :
            liftM2
               (\w r -> ZeroDCOffset "trapezoidBalanced" (Wave.trapezoidBalanced w r))
               (choose (zero, one))
               (choose (negate one, one)) :
            []


zeroDCOffset :: ZeroDCOffset Double -> QC.Property
zeroDCOffset w =
   QC.forAll (QC.choose (100,600)) $ \periodInt ->
   let period    = fromIntegral periodInt
       xs = take periodInt $ map Phase.fromRepresentative $
            map (/period) $ iterate (1+) 0.5
   in  abs (sum (map (Wave.apply (zdcWave w)) xs))  <  period / fromInteger 100


tests :: [(String, IO ())]
tests =
   ("zeroDCOffset",  quickCheck zeroDCOffset) :
   []
