{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.GenValidity.UUID.Typed where

import Data.GenValidity
import Data.GenValidity.UUID ()
import Data.UUID.Typed

instance GenValid (UUID a) where
  genValid = UUID <$> genValid
  shrinkValid (UUID u) = UUID <$> shrinkValid u
