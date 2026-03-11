{-# LANGUAGE BlockArguments, TupleSections #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Monad.StateT (StateT(..), lift) where

import Control.Applicative (Alternative(..))
import Control.Arrow (first)
import Control.Monad (MonadPlus, (>=>))

---------------------------------------------------------------------------

-- * NEWTYPE STATE T
-- * INSTANCE
--	+ FUNCTOR
--	+ APPLICATIVE AND ALTERNATIVE
--	+ MONAD AND MONAD PLUS

---------------------------------------------------------------------------
-- NEWTYPE STATE T
---------------------------------------------------------------------------

newtype StateT s m a = StateT { runStateT :: s -> m (a, s) }

lift :: Functor m => m a -> StateT s m a
lift m = StateT \s -> (, s) <$> m

---------------------------------------------------------------------------
-- INSTANCE
---------------------------------------------------------------------------

-- FUNCTOR

instance Functor m => Functor (StateT s m) where
	f `fmap` StateT k = StateT $ ((f `first`) <$>) . k

-- APPLICATIVE AND ALTERNATIVE

instance Monad m => Applicative (StateT s m) where
	pure = StateT . (pure .) . (,)
	StateT kf <*> mx = StateT $ kf >=> \(f, s') -> (f <$> mx) `runStateT` s'

instance MonadPlus m => Alternative (StateT s m) where
	empty = StateT $ const empty
	StateT k <|> StateT l = StateT $ (<|>) <$> k <*> l

-- MONAD AND MONAD PLUS

instance Monad m => Monad (StateT s m) where
	StateT k >>= f = StateT $ k >=> \(x, s') -> f x `runStateT` s'

instance MonadPlus m => MonadPlus (StateT s m)
