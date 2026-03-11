-- |
-- Module      :  DobutokO.Sound.Octaves
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Octaves (
  -- * Work with octaves
  octaveUp
  , octaveDown
  , liftInOctave
  , liftInOctaveV
) where

import Data.Maybe (fromJust)
import qualified Data.Vector as V
import DobutokO.Sound.Functional.Basics

-- | Returns an analogous note in the higher octave (its frequency in Hz).
octaveUp :: Float -> Float
octaveUp x = 2 * x
{-# INLINE octaveUp #-}

-- | Returns an analogous note in the lower octave (its frequency in Hz).
octaveDown :: Float -> Float
octaveDown x = x / 2
{-# INLINE octaveDown #-}

-- | Function lifts the given frequency to the given number of the octave (in American notation, from 0 to 8). This number is an 'Int' parameter.
-- The function also takes into account the lower pure quint for the closest note.
-- If it is not practical to determine the number, then the function returns 'Nothing'.
liftInOctave :: Int -> Float -> Maybe Float
liftInOctave n x
  | compare n 0 == LT || compare n 8 == GT = Nothing
  | compare (closestNote x) 24.4996 == GT =
      case compare (fromJust . whichOctave $ x) n of
        EQ -> Just (closestNote x)
        LT -> let z  = logBase 2.0 (V.unsafeIndex notes (n * 12) / closestNote x)
                  z1 = truncate z in
                   if abs (z - fromIntegral z1) > 0.999 || abs (z - fromIntegral z1) < 0.001
                     then Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 1) octaveUp $ closestNote x)
                     else Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 2) octaveUp $ closestNote x)
        _  -> let z  = logBase 2.0 (closestNote x / V.unsafeIndex notes (n * 12))
                  z1 = truncate z in
                   if abs (z - fromIntegral z1) > 0.999 || abs (z - fromIntegral z1) < 0.001
                     then Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 2) octaveDown $ closestNote x)
                     else Just (V.unsafeLast . V.iterateN (fromIntegral z1 + 1) octaveDown $ closestNote x)
  | otherwise = Nothing

-- | Function lifts the 'V.Vector' of 'Float' representing frequencies to the given octave with the 'Int' number. Better to use numbers in the range [1..8].
-- The function also takes into account the lower pure quint for the obtained note behaviour. If it is not practical to determine the octave, the resulting
-- frequency is omitted from the resulting 'V.Vector'.
liftInOctaveV :: Int -> V.Vector Float -> V.Vector Float
liftInOctaveV n = V.mapMaybe (liftInOctave n)

--------------------------------------------------------------------------------------------------------------------------------

