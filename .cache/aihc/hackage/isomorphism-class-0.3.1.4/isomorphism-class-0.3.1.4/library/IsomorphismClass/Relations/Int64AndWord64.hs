{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int64AndWord64 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo Int64 Word64 where
  to = fromIntegral

instance IsomorphicTo Word64 Int64 where
  to = fromIntegral
