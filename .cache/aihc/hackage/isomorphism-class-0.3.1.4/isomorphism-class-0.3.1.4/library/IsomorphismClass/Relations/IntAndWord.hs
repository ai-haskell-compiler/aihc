{-# OPTIONS_GHC -Wno-orphans #-}

module IsomorphismClass.Relations.IntAndWord where

import IsomorphismClass.Classes
import IsomorphismClass.Prelude

instance IsomorphicTo Int Word where
  to = fromIntegral

instance IsomorphicTo Word Int where
  to = fromIntegral
