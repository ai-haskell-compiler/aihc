{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.Int8AndWord8 where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo Int8 Word8 where
  to = fromIntegral

instance IsomorphicTo Word8 Int8 where
  to = fromIntegral
