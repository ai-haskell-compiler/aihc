{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.StateStack
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A state monad which allows the state to be saved and restored on a
-- stack.
--
-- [Computation type:] Computations with implicit access to a
-- read/write state, with additional operations for pushing the
-- current state on a stack and later restoring the state from the top
-- of the stack.
--
-- [Binding strategy:] Same as for the usual state monad; the state
-- and accompanying stack of saved states are threaded through
-- computations.
--
-- [Useful for:] Remembering state while emitting commands for some
-- system which itself has saveable/restorable state, such as OpenGL
-- or Cairo.
--
-- Simple example:
--
-- > ghci> let p = get >>= liftIO . print
-- > ghci> evalStateStackT (put 2 >> p >> save >> put 3 >> p >> restore >> p) 0
-- > 2
-- > 3
-- > 2
--
-----------------------------------------------------------------------------

module Control.Monad.StateStack
       (
         -- * The @MonadStateStack@ class

         MonadStateStack(..)

         -- * The @StateStackT@ transformer

       , StateStackT(..), StateStack

         -- * Running @StateStackT@ and @StateStack@ computations

       , runStateStackT, evalStateStackT, execStateStackT
       , runStateStack,  evalStateStack,  execStateStack

       , liftState

       ) where

import           Control.Arrow                     (second)
import           Control.Arrow                     (first, (&&&))
import qualified Control.Monad.State               as St

import           Control.Monad
import           Control.Monad.Identity
import           Control.Monad.Trans
import           Control.Monad.Trans.Cont
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Maybe
import           Control.Monad.Trans.Reader        (ReaderT)
import           Control.Monad.Trans.State.Lazy    as Lazy
import           Control.Monad.Trans.State.Strict  as Strict
import           Control.Monad.Trans.Writer.Lazy   as Lazy
import           Control.Monad.Trans.Writer.Strict as Strict

import qualified Control.Monad.Cont.Class          as CC
import qualified Control.Monad.IO.Class            as IC


------------------------------------------------------------
--  Implementation
------------------------------------------------------------

-- | A monad transformer which adds a save/restorable state to an
--   existing monad.
newtype StateStackT s m a = StateStackT { unStateStackT :: St.StateT (s,[s]) m a }
  deriving (Functor, Applicative, Monad, MonadTrans, IC.MonadIO)

-- | Class of monads which support a state along with a stack for
--   saving and restoring states.
class St.MonadState s m => MonadStateStack s m where
  save    :: m ()   -- ^ Save the current state on the stack
  restore :: m ()   -- ^ Restore the top state from the stack

instance Monad m => St.MonadState s (StateStackT s m) where
  get   = StateStackT $ St.gets fst
  put s = StateStackT $ (St.modify . first) (const s)

instance Monad m => MonadStateStack s (StateStackT s m) where
  save    = StateStackT $ St.modify (fst &&& uncurry (:))
  restore = StateStackT . St.modify $ \(cur,hist) ->
              case hist of
                []        -> (cur,hist)
                (r:hist') -> (r,hist')

-- | Run a @StateStackT@ computation from an initial state, resulting
--   in a computation of the underlying monad which yields the return
--   value and final state.
runStateStackT :: Monad m => StateStackT s m a -> s -> m (a, s)
runStateStackT m s = (liftM . second) fst . flip St.runStateT (s,[]) . unStateStackT $ m

-- | Like 'runStateStackT', but discard the final state.
evalStateStackT :: Monad m => StateStackT s m a -> s -> m a
evalStateStackT m s = liftM fst $ runStateStackT m s

-- | Like 'runStateStackT', but discard the return value and yield
--   only the final state.
execStateStackT :: Monad m => StateStackT s m a -> s -> m s
execStateStackT m s = liftM snd $ runStateStackT m s

type StateStack s a = StateStackT s Identity a

-- | Run a @StateStack@ computation from an initial state, resulting
--   in a pair of the final return value and final state.
runStateStack :: StateStack s a -> s -> (a,s)
runStateStack m s = runIdentity $ runStateStackT m s

-- | Like 'runStateStack', but discard the final state.
evalStateStack :: StateStack s a -> s -> a
evalStateStack m s = runIdentity $ evalStateStackT m s

-- | Like 'runStateStack', but discard the return value and yield
--   only the final state.
execStateStack :: StateStack s a -> s -> s
execStateStack m s = runIdentity $ execStateStackT m s

-- | @StateT@ computations can always be lifted to @StateStackT@
--   computations which do not manipulate the state stack.
liftState :: Monad m => St.StateT s m a -> StateStackT s m a
liftState st = StateStackT . St.StateT $ \(s,ss) -> (liftM . second) (flip (,) ss) (St.runStateT st s)

------------------------------------------------------------
--  Applying monad transformers to MonadStateStack monads
------------------------------------------------------------

instance MonadStateStack s m => MonadStateStack s (ContT r m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (ExceptT e m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (IdentityT m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (MaybeT m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (ReaderT r m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (Lazy.StateT s m) where
  save    = lift save
  restore = lift restore

instance MonadStateStack s m => MonadStateStack s (Strict.StateT s m) where
  save    = lift save
  restore = lift restore

instance (Monoid w, MonadStateStack s m) => MonadStateStack s (Lazy.WriterT w m) where
  save    = lift save
  restore = lift restore

instance (Monoid w, MonadStateStack s m) => MonadStateStack s (Strict.WriterT w m) where
  save    = lift save
  restore = lift restore

------------------------------------------------------------
--  Applying StateStackT to other monads
------------------------------------------------------------

instance CC.MonadCont m => CC.MonadCont (StateStackT s m) where
  callCC c = StateStackT $ CC.callCC (unStateStackT . (\k -> c (StateStackT . k)))

{-  -- These require UndecidableInstances =(
instance EC.MonadError e m => EC.MonadError e (StateStackT s m) where
  throwError     = lift . EC.throwError
  catchError m h = StateStackT $ EC.catchError (unStateStackT m) (unStateStackT . h)

instance RC.MonadReader r m => RC.MonadReader r (StateStackT s m) where
  ask     = lift RC.ask
  local f = StateStackT . RC.local f . unStateStackT
-}
