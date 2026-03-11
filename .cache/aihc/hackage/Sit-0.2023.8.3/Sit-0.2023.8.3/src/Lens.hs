{-# LANGUAGE NoMonomorphismRestriction #-}

-- | Wrapper module for lens functionality

module Lens
  ( module Data.Lens.Light
  , module Lens
  ) where

import Data.Lens.Light

over = modL
set  = setL
use  = access
_2   = lens snd $ \ y (x, _) -> (x, y)
