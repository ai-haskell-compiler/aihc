-- |
-- Bidirectional version of "Data.Coerce".
{-# LANGUAGE TypeOperators #-}
module Data.Invertible.Coerce
  ( coerce
  ) where

import qualified Data.Coerce as C

import Data.Invertible.Bijection

-- |Safely 'C.coerce' between values of types that have the same representation.
coerce :: (C.Coercible a b, C.Coercible b a) => a <-> b
coerce = C.coerce :<->: C.coerce
