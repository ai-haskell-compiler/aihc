module UnpackChecks (unpackChecks) where

import HiddenProduct (Span, makeSpan, spanWidth)

-- | The field unpacks the two words of a product whose constructor
-- 'HiddenProduct' does not export. This module builds and matches that
-- constructor, so its object refers to the info table of the constructor.
data Labelled = Labelled Char {-# UNPACK #-} !Span

label :: Int -> Labelled
label n = Labelled 'w' (makeSpan n (n * 3))

width :: Labelled -> Int
width (Labelled _ span') = spanWidth span'

unpackChecks :: Bool
unpackChecks = width (label 5) == 10
