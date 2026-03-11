{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndList where

import qualified Data.Vector
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome (Vector a) [a] where
  to = Data.Vector.fromList

instance IsSome [a] (Vector a) where
  to = Data.Vector.toList

instance IsMany (Vector a) [a]

instance IsMany [a] (Vector a)

instance Is (Vector a) [a]

instance Is [a] (Vector a)
