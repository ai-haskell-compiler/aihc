-----------------------------------------------------------------------------
-- |
-- Module     : Control.Monad.Interleave
-- Copyright  : Copyright (c) Patrick Perry <patperry@stanford.edu>, Sergey Vinokurov <serg.foo@gmail.com>
-- License    : BSD3
-- Maintainer : Sergey Vinokurov <serg.foo@gmail.com>
--
-- Monads with an unsaveInterleaveIO-like operation.

{-# LANGUAGE CPP #-}

module Control.Monad.Interleave
  ( MonadInterleave(..)
  ) where

import qualified Control.Monad.ST as Strict (ST)
import qualified Control.Monad.ST.Lazy as Lazy (ST)
import System.IO.Unsafe

#if MIN_VERSION_base(4, 4, 0)
import qualified Control.Monad.ST.Lazy.Unsafe as Lazy (unsafeInterleaveST)
import qualified Control.Monad.ST.Unsafe as Strict (unsafeInterleaveST)
#else
import qualified Control.Monad.ST as Strict (unsafeInterleaveST)
import qualified Control.Monad.ST.Lazy as Lazy (unsafeInterleaveST)
#endif

-- | Monads that have an operation like 'unsafeInterleaveIO'.
class Monad m => MonadInterleave m where
  -- | Get the baton from the monad without doing any computation.
  unsafeInterleave :: m a -> m a

instance MonadInterleave IO where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = unsafeInterleaveIO

instance MonadInterleave (Strict.ST s) where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = Strict.unsafeInterleaveST

instance MonadInterleave (Lazy.ST s) where
  {-# INLINE unsafeInterleave #-}
  unsafeInterleave = Lazy.unsafeInterleaveST
