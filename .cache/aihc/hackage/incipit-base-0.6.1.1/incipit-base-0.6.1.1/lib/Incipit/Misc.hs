-- |Misc combinators.
module Incipit.Misc where

import Incipit.Base

-- |Convenience alias for @pure ()@.
unit ::
  Applicative f =>
  f ()
unit =
  pure ()
{-# inline unit #-}

-- |Variant of 'when' that takes a monadic action for the condition.
whenM ::
  Monad m =>
  m Bool ->
  m () ->
  m ()
whenM condM action =
  condM >>= \ cond -> when cond action
{-# inline whenM #-}

-- |Variant of 'unless' that takes a monadic action for the condition.
unlessM ::
  Monad m =>
  m Bool ->
  m () ->
  m ()
unlessM condM action =
  condM >>= \ cond -> unless cond action
{-# inline unlessM #-}

-- |Variant of @if@/@then@/@else@ that takes a monadic action for the condition and branches.
ifM ::
  Monad m =>
  m Bool ->
  m a ->
  m a ->
  m a
ifM condM onTrue onFalse =
  condM >>= \ cond -> if cond then onTrue else onFalse
{-# inline ifM #-}
