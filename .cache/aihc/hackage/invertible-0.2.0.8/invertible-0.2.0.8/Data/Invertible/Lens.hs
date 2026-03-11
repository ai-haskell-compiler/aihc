-- |
-- Convert bijections to and from lens isomorphisms in "Control.Lens.Iso".
{-# LANGUAGE RankNTypes, TypeOperators #-}
module Data.Invertible.Lens
  ( toIso
  , unIso
  ) where

import Data.Invertible.Bijection
import qualified Control.Lens.Iso as L

-- |Convert an isomorphism to a lens.
toIso :: a <-> b -> L.Iso' a b
toIso (f :<->: g) = L.iso f g

-- |Convert a lens to an isomorphism.
unIso :: L.AnIso' a b -> a <-> b
unIso l = L.withIso l (:<->:)
