-- |
-- Bidirectional versions of 'Enum' functions.
{-# LANGUAGE Safe, TypeOperators #-}
module Data.Invertible.Enum
  ( enum
  , succ
  ) where

import qualified Prelude
import Prelude hiding (succ)

import Data.Invertible.Bijection

-- |Convert between an 'Int' and an 'Enum' with 'P.toEnum' and 'P.fromEnum'.
enum :: Enum a => Int <-> a
enum = toEnum :<->: fromEnum

-- |Combine 'Prelude.succ' and 'pred'
succ :: Enum a => a <-> a
succ = Prelude.succ :<->: pred
