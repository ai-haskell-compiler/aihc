{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.NominalDiffTimeAndPico where

import Data.Time
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo NominalDiffTime Pico where
  to = secondsToNominalDiffTime

instance IsomorphicTo Pico NominalDiffTime where
  to = nominalDiffTimeToSeconds
