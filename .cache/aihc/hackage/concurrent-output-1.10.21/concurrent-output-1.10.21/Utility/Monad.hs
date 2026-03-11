{- monadic stuff
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.Monad where

{- if with a monadic conditional. -}
ifM :: Monad m => m Bool -> (m a, m a) -> m a
ifM cond (thenclause, elseclause) = do
	c <- cond
	if c then thenclause else elseclause

{- do nothing -}
noop :: Monad m => m ()
noop = return ()
