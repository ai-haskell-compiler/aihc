{-# OPTIONS_GHC -Wno-orphans #-}

module LawfulConversions.Relations.ListAndSeq where

import qualified Data.Sequence
import LawfulConversions.Classes
import LawfulConversions.Prelude

instance IsSome [a] (Seq a) where
  to = toList

instance IsSome (Seq a) [a] where
  to = Data.Sequence.fromList

instance IsMany [a] (Seq a)

instance IsMany (Seq a) [a]

instance Is [a] (Seq a)

instance Is (Seq a) [a]
