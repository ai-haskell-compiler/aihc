{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.BCP47.Hashable () where

import Data.BCP47 (BCP47, toText)
import Data.Hashable (Hashable (..))

instance Hashable BCP47 where
  hashWithSalt i = hashWithSalt i . toText
