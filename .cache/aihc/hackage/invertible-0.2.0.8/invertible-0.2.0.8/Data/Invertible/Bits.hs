-- |
-- Bidirectional version of "Data.Bits".
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Bits
  ( complement
  ) where

import qualified Data.Bits as B

import Data.Invertible.Bijection
import Data.Invertible.Internal

-- |'B.complement' all the bits in the argument.
complement :: B.Bits a => a <-> a
complement = involution B.complement
