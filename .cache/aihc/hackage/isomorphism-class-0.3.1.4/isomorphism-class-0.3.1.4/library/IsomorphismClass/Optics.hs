module IsomorphismClass.Optics where

import Data.Profunctor
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

-- | Van-Laarhoven-style Isomorphism, compatible with the \"lens\" library.
isomorphicToIso :: (IsomorphicTo a b, Profunctor p, Functor f) => p b (f b) -> p a (f a)
isomorphicToIso = dimap from (fmap to)
