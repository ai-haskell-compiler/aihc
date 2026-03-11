-- |
-- Module      :  DobutokO.Sound.Decibel
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Decibel (
  -- * Type synonym with different semantics
  StrengthsDb
  -- * Working with StrengthsDb and Strengths and others
  , dBOmegaRatio
  , strength2dB_Abs
  , strengthdB2ampl
  , strengths2dB
  , strengthsDb2ampl
) where

import qualified Data.Vector as V
import DobutokO.Sound.Functional.Params (Strengths)

-- | Is used to represent a set of volumes in the dB scale for SoX \"vol\" effect. Usually, the zero value corresponds to the sound with volume 
-- level equal by absolute value to 1.0 (the extremum one). So for most cases, its elements are negative numbers not less than (-120).
type StrengthsDb = V.Vector Float

-- | Returns the frequency for which its ratio with the second 'Float' argument being under lg and being multiplied with 20 returns 
-- the first 'Float'  argument. For example, @dBOmegaRatio 6 440 ~ 880@ (actually, 877.9154185863069).
dBOmegaRatio :: Float -> Float -> Float
dBOmegaRatio dB omega0 = omega0 * 10 ** (dB / fromIntegral 20) 

-- | Converts the absolute value of the argument to dB value compared to 1.0. Usually, is less than 0. The argument should not be equal to zero (0), 
-- otherwise, it is equal to -'Infinity'.
strength2dB_Abs :: Float -> Float
strength2dB_Abs vol = 20 * logBase 10 (abs vol)

-- | Converts the absolute value of the argument from dB value to amplitude with 0 equivalent to amplitude of 1.0 (or -1.0 for inverted sound). 
-- Usually, is used for negative arguments (or at least not positive ones). 
strengthdB2ampl :: Float -> Float
strengthdB2ampl dB = 10 ** (dB / fromIntegral 20)

-- | Converts the 'V.Vector' of the absolute values of the argument to the 'V.Vector' of dB values compared to 1.0. 
-- Usually, its elements are less than 0. If some element in the argument is equal to zero (0), the corresponding resulting element is equal to -'Infinity'.
strengths2dB :: Strengths -> StrengthsDb
strengths2dB v = V.map strength2dB_Abs v

-- | Converts the 'V.Vector' of dB values to the 'V.Vector' of the amplitudes with 0 being equivalent to amplitude of 1.0 (or -1.0 for inverted sound). 
-- Usually, is used for negative elements of the first argument (or at least not positive ones). 
strengthsDb2ampl :: StrengthsDb -> Strengths
strengthsDb2ampl v = V.map strengthdB2ampl v

