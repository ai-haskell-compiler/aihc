
module Streaming.Primitive where

import Control.Monad.Primitive
import Streaming



-- | Orphan instance providing a primitive monad instance for streams. Allows
-- impurely folds into mutable vectors from streams.

instance (Monad m, PrimMonad m, Functor f) ⇒ PrimMonad (Stream f m) where
  type PrimState (Stream f m) = PrimState m
  {-# Inline primitive #-}
  primitive = lift . primitive

