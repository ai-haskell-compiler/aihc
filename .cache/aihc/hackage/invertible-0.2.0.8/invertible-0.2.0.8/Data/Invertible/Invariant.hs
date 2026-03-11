-- |
-- Use bijections on 'Invariant' functors from "Data.Functor.Invariant".
{-# LANGUAGE TypeOperators #-}
module Data.Invertible.Invariant
  ( invmap
  ) where

import Data.Invertible.Bijection
import qualified Data.Functor.Invariant as I

-- |Apply a bijection over an 'I.Invariant' using 'I.invmap'.
invmap :: I.Invariant f => a <-> b -> f a -> f b
invmap (f :<->: g) = I.invmap f g
