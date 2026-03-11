{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int16AndWord16 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo Int16 Word16 where
  to = fromIntegral

instance IsomorphicTo Word16 Int16 where
  to = fromIntegral
