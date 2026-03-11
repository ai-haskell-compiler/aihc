{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntSetAndSetOfInt where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo (Set Int) IntSet where
  to = fromList . toList

instance IsomorphicTo IntSet (Set Int) where
  to = fromList . toList
