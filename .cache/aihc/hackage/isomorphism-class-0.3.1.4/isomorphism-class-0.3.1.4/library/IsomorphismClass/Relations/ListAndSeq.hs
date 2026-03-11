{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.ListAndSeq where

import qualified Data.Sequence
import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo [a] (Seq a) where
  to = toList

instance IsomorphicTo (Seq a) [a] where
  to = Data.Sequence.fromList
