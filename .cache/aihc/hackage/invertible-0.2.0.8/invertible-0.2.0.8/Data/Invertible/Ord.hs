-- |
-- Bidirectional operations over 'Ordering'.
{-# LANGUAGE Safe, TypeOperators, QuasiQuotes #-}
module Data.Invertible.Ord
  ( down
  ) where

import Data.Invertible.Bijection
import Data.Invertible.TH

-- |Invert an 'Ordering' (see 'Data.Ord.Down').
down :: Ordering <-> Ordering
down =
  [biCase|
    LT <-> GT
    EQ <-> EQ
    GT <-> LT
  |]
