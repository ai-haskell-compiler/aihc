-- |
-- Bidirectional version of "Data.Bool".
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Bool
  ( not
  ) where

import Prelude hiding (not)
import qualified Data.Bool as B

import Data.Invertible.Bijection
import Data.Invertible.Internal

-- |Boolean 'B.not'.
not :: Bool <-> Bool
not = involution B.not
