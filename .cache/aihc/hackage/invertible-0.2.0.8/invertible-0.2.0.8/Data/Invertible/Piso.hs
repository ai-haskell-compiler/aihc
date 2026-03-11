-- |
-- Convert bijections to and from (total) 'P.Piso'.
{-# LANGUAGE TypeOperators #-}
module Data.Invertible.Piso
  ( toPiso
  , fromPiso
  ) where

import Data.Invertible.Bijection
import qualified Data.Piso as P

-- |Convert a bijection to a (total) partial isomorphism.
toPiso :: a <-> b -> P.Piso a b
toPiso (f :<->: g) = P.Piso f (Just . g)

-- |Convert a partial isomorphism to a total bijection using a default value for the failure case.
fromPiso :: b -> P.Piso a b -> Maybe a <-> b
fromPiso b (P.Piso f g) = maybe b f :<->: g
