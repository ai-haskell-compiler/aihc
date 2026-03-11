module Main where

import qualified Test.Sound.Synthesizer.Plain.Analysis       as Analysis
import qualified Test.Sound.Synthesizer.Plain.Control        as Control
import qualified Test.Sound.Synthesizer.Plain.Filter         as Filter
import qualified Test.Sound.Synthesizer.Plain.Filter.FirstOrder as Filt1
import qualified Test.Sound.Synthesizer.Plain.Interpolation  as Interpolation
import qualified Test.Sound.Synthesizer.Plain.Oscillator     as Oscillator
import qualified Test.Sound.Synthesizer.Plain.Wave           as Wave
import qualified Test.Sound.Synthesizer.Basic.NumberTheory   as NumberTheory
import qualified Test.Sound.Synthesizer.Basic.ToneModulation as ToneModulation
import qualified Test.Sound.Synthesizer.Plain.ToneModulation as ToneModulationL
import qualified Test.Sound.Synthesizer.Generic.ToneModulation as ToneModulationG
import qualified Test.Sound.Synthesizer.Generic.Permutation as Permutation
import qualified Test.Sound.Synthesizer.Generic.Fourier as Fourier
import qualified Test.Sound.Synthesizer.Generic.FourierInteger as FourierInteger
import qualified Test.Sound.Synthesizer.Generic.Filter  as FilterG
import qualified Test.Sound.Synthesizer.Generic.Cut  as CutG
import qualified Test.Sound.Synthesizer.Causal.Analysis as AnalysisC
import qualified Test.Sound.Synthesizer.Storable.Cut as Cut

import Data.Tuple.HT (mapFst, )


prefix :: String -> [(String, IO ())] -> [(String, IO ())]
prefix msg =
   map (mapFst (\str -> msg ++ "." ++ str))

main :: IO ()
main =
   mapM_ (\(msg,io) -> putStr (msg++": ") >> io) $
   concat $
      prefix "Plain.Analysis"       Analysis.tests :
      prefix "Plain.Control"        Control.tests :
      prefix "Plain.Filter.FirstOrder" Filt1.tests :
      prefix "Plain.Filter"         Filter.tests :
      prefix "Plain.Interpolation"  Interpolation.tests :
      prefix "Plain.Oscillator"     Oscillator.tests :
      prefix "Plain.Wave"           Wave.tests :
      prefix "Storable.Cut"         Cut.tests :
      prefix "Generic.Cut"          CutG.tests :
      prefix "Basic.ToneModulation" ToneModulation.tests :
      prefix "Plain.ToneModulation" ToneModulationL.tests :
      prefix "Generic.ToneModulation" ToneModulationG.tests :
      prefix "Generic.Permutation"    Permutation.tests :
      prefix "Generic.Fourier"        Fourier.tests :
      prefix "Basic.NumberTheory"     NumberTheory.tests :
      prefix "Generic.FourierInteger" FourierInteger.tests :
      prefix "Generic.Filter"         FilterG.tests :
      prefix "Causal.Analysis"        AnalysisC.tests :
      []
