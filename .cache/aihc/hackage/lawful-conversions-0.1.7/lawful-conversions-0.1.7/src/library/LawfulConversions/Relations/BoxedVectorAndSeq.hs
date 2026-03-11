{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.BoxedVectorAndSeq where

import qualified Data.Sequence
import qualified Data.Vector
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome (Vector a) (Seq a) where
  to = Data.Vector.fromList . toList

instance IsSome (Seq a) (Vector a) where
  to = Data.Sequence.fromList . Data.Vector.toList

instance IsMany (Vector a) (Seq a)

instance IsMany (Seq a) (Vector a)

instance Is (Vector a) (Seq a)

instance Is (Seq a) (Vector a)
