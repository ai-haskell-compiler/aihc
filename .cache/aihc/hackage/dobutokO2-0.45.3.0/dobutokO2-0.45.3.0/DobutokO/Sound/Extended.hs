-- |
-- Module      :  DobutokO.Sound.Extended
-- Copyright   :  (c) OleksandrZhabenko 2020, 2024
-- License     :  MIT
-- Stability   :  Experimental
-- Maintainer  :  oleksandr.zhabenko@yahoo.com
--
-- Helps to create experimental music from a file (or its part) and a Ukrainian text. 
-- It can also generate a timbre for the notes. Uses SoX inside.

{-# OPTIONS_GHC -threaded #-}

module DobutokO.Sound.Extended (
  -- * Even more extended
  dviykyTA
  , triykyTA
  , chetvirkyTA
  , p'yatirkyTA
  , shistkyTA
  , simkyTA
  , visimkyTA
  , dev'yatkyTA
  , desyatkyTA
  , odynadtsyatkyTA
  , octavesTA
) where

import qualified Data.Vector as V
import DobutokO.Sound.Functional.Basics (NotePairs,notes)

dviykyTA :: NotePairs
dviykyTA = V.generate 107 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 1)))

triykyTA :: NotePairs
triykyTA = V.generate 106 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 2)))

chetvirkyTA :: NotePairs
chetvirkyTA = V.generate 105 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 3)))

p'yatirkyTA :: NotePairs
p'yatirkyTA = V.generate 104 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 4)))

shistkyTA :: NotePairs
shistkyTA = V.generate 103 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 5)))

simkyTA :: NotePairs
simkyTA = V.generate 102 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 6)))

visimkyTA :: NotePairs
visimkyTA = V.generate 101 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 7)))

dev'yatkyTA :: NotePairs
dev'yatkyTA = V.generate 100 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 8)))

desyatkyTA :: NotePairs
desyatkyTA = V.generate 99 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 9)))

odynadtsyatkyTA  :: NotePairs
odynadtsyatkyTA = V.generate 98 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 10)))

octavesTA :: NotePairs
octavesTA = V.generate 97 (\i -> (V.unsafeIndex notes i, V.unsafeIndex notes (i + 11)))

--------------------------------------------------------------------------------------------------------------------------
