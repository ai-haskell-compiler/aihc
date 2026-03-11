module Synthesizer.Format where

class C sig where
   format :: Show x => Int -> sig x -> ShowS
