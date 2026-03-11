{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.DiffTimeAndPico where

import Data.Time
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo DiffTime Pico where
  to = picosecondsToDiffTime . picoToInteger
    where
      picoToInteger :: Pico -> Integer
      picoToInteger (MkFixed p) = p

instance IsomorphicTo Pico DiffTime where
  to = MkFixed . diffTimeToPicoseconds
