-- |
-- Convert bijections to and from (total) 'P.Iso'.
{-# LANGUAGE TypeOperators #-}
module Data.Invertible.PartialIsomorphism
  ( toIso
  , fromIso
  , (<$>)
  ) where

import Prelude hiding ((<$>))

import Data.Invertible.Bijection
import qualified Control.Isomorphism.Partial as P
import qualified Control.Isomorphism.Partial.Unsafe as P

-- |Convert a bijection to a (total) partial isomorphism.
toIso :: a <-> b -> P.Iso a b
toIso (f :<->: g) = P.Iso (Just . f) (Just . g)

-- |Convert a partial isomorphism to a bijection by propagating 'Nothing's.
fromIso :: P.Iso a b -> Maybe a <-> Maybe b
fromIso (P.Iso f g) = (f =<<) :<->: (g =<<)

-- |Apply a bijection over a 'P.IsoFunctor' using 'P.<$>'.
(<$>) :: P.IsoFunctor f => a <-> b -> f a -> f b
(<$>) = (P.<$>) . toIso
