module Main where

import qualified Synthesizer.ALSA.Dimensional.Server.Run  as Run
import qualified Synthesizer.ALSA.Dimensional.Server.Test as Test

main :: IO ()
main =
   case fromInteger 106 :: Int of
      001 -> Test.sequence1
      100 -> Run.volume
      101 -> Run.pitchBend
      102 -> Run.volumePitchBend1
      103 -> Run.keyboard
      104 -> Run.keyboardMulti
      105 -> Run.keyboardFM
      106 -> Run.keyboardDetuneFM
      107 -> Run.keyboardFilter
      _ -> error "not implemented"
