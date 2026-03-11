-- |
-- Convert bijections to and from semigroupoids 'S.Iso'.
module Data.Invertible.Semigroupoid
  ( toIso
  , fromIso
  ) where

import Data.Invertible.Bijection
import qualified Data.Isomorphism as S

-- |Convert a bijection to a semigroupoid isomorphism.
toIso :: Bijection a b c -> S.Iso a b c
toIso (f :<->: g) = S.Iso f g

-- |Convert a semigroupoid isomorphism to a bijection.
fromIso :: S.Iso a b c -> Bijection a b c
fromIso (S.Iso f g) = f :<->: g
