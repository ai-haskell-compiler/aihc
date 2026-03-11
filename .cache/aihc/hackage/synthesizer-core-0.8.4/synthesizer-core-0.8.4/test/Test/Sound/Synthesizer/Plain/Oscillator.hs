module Test.Sound.Synthesizer.Plain.Oscillator (tests) where

import qualified Synthesizer.Plain.Oscillator as Osci
import qualified Synthesizer.Basic.Wave       as Wave

import qualified Test.Sound.Synthesizer.Plain.Wave as WaveTest

import Test.QuickCheck (quickCheck, )

import qualified Algebra.RealField             as RealField

import NumericPrelude.Numeric
import NumericPrelude.Base
import Prelude ()



phaseShapeMod :: (RealField.C a, Eq b) => (Wave.T a b) -> a -> [a] -> Bool
phaseShapeMod wave freq phases =
   Osci.phaseMod wave freq phases ==
   Osci.shapeMod (Wave.phaseOffset wave) zero freq phases

phaseShapeModRational ::
   WaveTest.Ring Rational -> Integer -> Integer -> [Integer] -> Bool
phaseShapeModRational w denom0 freq0 phases0 =
   let denom  = 1 + abs denom0
       freq   = freq0 % denom
       phases = map (% denom) phases0
   in  phaseShapeMod (WaveTest.ringWave w) freq phases



tests :: [(String, IO ())]
tests =
   ("phaseShapeModRational",  quickCheck phaseShapeModRational) :
   []
