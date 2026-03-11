
{-# LANGUAGE CPP #-}

-- | Phantom State Transformer type and functions.
module Control.Applicative.PhantomState (
    PhantomStateT
  , PhantomState
  , useState
  , changeState
  , useAndChangeState
  , runPhantomStateT
  , runPhantomState
  ) where

import Control.Applicative
import Data.Functor.Identity
-- Conditional imports
#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (Monoid (..))
#endif
#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup (Semigroup (..))
#endif

-- | The Phantom State Transformer is like the
--   State Monad Transformer, but it does not hold
--   any value. Therefore, it automatically discards
--   the result of any computation. Only changes in
--   the state and effects will remain. This transformer
--   produces a new 'Applicative' functor from any 'Monad'.
--   The primitive operations in this functor are:
--
-- * 'useState': Performs effects. State is unchanged.
-- * 'changeState': Changes state. No effect is performed.
-- * 'useAndChangeState': Changes state and performs effects.
--
--   Although 'useState' and 'changeState' are defined in
--   terms of 'useAndChangeState':
--
-- >    useState f = useAndChangeState (\s -> f s *> pure s)
-- > changeState f = useAndChangeState (pure . f)
--
--   So 'useAndChangeState' is the only actual primitive.
--
--   Use 'runPhantomStateT' (or 'runPhantomState') to get
--   the result of a phantom state computation.
--
newtype PhantomStateT s m a = PhantomStateT (s -> m s)

-- | Type synonym of 'PhantomStateT' where the underlying 'Monad' is the 'Identity' monad.
type PhantomState s = PhantomStateT s Identity

-- | Perform an applicative action using the current state, leaving
--   the state unchanged. The result will be discarded, so only the
--   effect will remain.
useState :: Applicative m => (s -> m a) -> PhantomStateT s m ()
{-# INLINE useState #-}
useState f = useAndChangeState $ \s -> f s *> pure s

-- | Modify the state using a pure function. No effect will be produced,
--   only the state will be modified.
changeState :: Applicative m => (s -> s) -> PhantomStateT s m ()
{-# INLINE changeState #-}
changeState f = useAndChangeState $ \s -> pure (f s)

-- | Combination of 'useState' and 'changeState'. It allows you to change the state while
--   performing any effects. The new state will be the result of applying the argument
--   function to the old state. The following equations hold:
--
-- >    useState f *> changeState g }
-- >                                } = useAndChangeState (\s -> f s *> g s)
-- > changeState g *>    useState f }
--
useAndChangeState :: (s -> m s) -> PhantomStateT s m ()
{-# INLINE useAndChangeState #-}
useAndChangeState = PhantomStateT

-- | Perform a phantom state computation by setting an initial state
--   and running all the actions from there.
runPhantomStateT :: PhantomStateT s m a -- ^ Phantom state computation
                 -> s -- ^ Initial state
                 -> m s -- ^ Final result
{-# INLINE runPhantomStateT #-}
runPhantomStateT (PhantomStateT f) x = f x

-- | Specialized version of 'runPhantomStateT' where the underlying
--   'Monad' is the 'Identity' monad.
runPhantomState :: PhantomState s a -- ^ Phantom state computation
                -> s -- ^ Initial state
                -> s -- ^ Final result
{-# INLINE runPhantomState #-}
runPhantomState f = runIdentity . runPhantomStateT f

-- Instances

instance Functor (PhantomStateT s m) where
  {-# INLINE fmap #-}
  fmap _ (PhantomStateT f) = PhantomStateT f

instance Monad m => Applicative (PhantomStateT s m) where
  {-# INLINE pure #-}
  pure _ = PhantomStateT return
  {-# INLINE (<*>) #-}
  PhantomStateT f <*> PhantomStateT g = PhantomStateT (\x -> f x >>= g)
  {-# INLINE  (*>) #-}
  PhantomStateT f  *> PhantomStateT g = PhantomStateT (\x -> f x >>= g)
  {-# INLINE (<*) #-}
  PhantomStateT f <*  PhantomStateT g = PhantomStateT (\x -> f x >>= g)

instance (Monad m, Alternative m) => Alternative (PhantomStateT s m) where
  {-# INLINE empty #-}
  empty = PhantomStateT (const empty)
  {-# INLINE (<|>) #-}
  PhantomStateT f <|> PhantomStateT g = PhantomStateT (\x -> f x <|> g x)

instance Monad m => Monoid (PhantomStateT s m a) where
  {-# INLINE mempty #-}
  mempty = pure undefined

#if MIN_VERSION_base(4,9,0)
instance Monad m => Semigroup (PhantomStateT s m a) where
  {-# INLINE (<>) #-}
  (<>) = (*>)
#endif
