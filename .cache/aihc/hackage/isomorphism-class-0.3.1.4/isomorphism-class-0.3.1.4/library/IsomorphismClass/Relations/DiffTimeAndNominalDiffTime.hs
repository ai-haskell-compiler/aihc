{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.DiffTimeAndNominalDiffTime where

import Data.Time
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo DiffTime NominalDiffTime where
  to = picosecondsToDiffTime . picoToInteger . nominalDiffTimeToSeconds
    where
      picoToInteger :: Pico -> Integer
      picoToInteger (MkFixed p) = p

instance IsomorphicTo NominalDiffTime DiffTime where
  to = secondsToNominalDiffTime . MkFixed . diffTimeToPicoseconds
