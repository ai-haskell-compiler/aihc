-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.State.Class
-- Copyright   :  (c) Andy Gill 2001,
--                (c) Oregon Graduate Institute of Science and Technology, 2001
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  ross@soi.city.ac.uk
-- Stability   :  experimental
-- Portability :  non-portable (type families)
--
-- MonadState class.
--
--      This module is inspired by the paper
--      /Functional Programming with Overloading and
--          Higher-Order Polymorphism/,
--        Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
--          Advanced School of Functional Programming, 1995.

-----------------------------------------------------------------------------

module Control.Monad.State.Class (
    MonadState(..),
    modify,
    gets,
  ) where

import Control.Monad.Trans (lift)
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, get, put)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, get, put)
import qualified Control.Monad.Trans.State.Lazy as Lazy (StateT, get, put)
import qualified Control.Monad.Trans.State.Strict as Strict (StateT, get, put)
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict

-- ---------------------------------------------------------------------------
-- | /get/ returns the state from the internals of the monad.
--
-- /put/ replaces the state inside the monad.

class (Monad m) => MonadState m where
    type StateType m
    get :: m (StateType m)
    put :: StateType m -> m ()

-- | Monadic state transformer.
--
--      Maps an old state to a new state inside a state monad.
--      The old state is thrown away.
--
-- >      Main> :t modify ((+1) :: Int -> Int)
-- >      modify (...) :: (MonadState Int a) => a ()
--
--    This says that @modify (+1)@ acts over any
--    Monad that is a member of the @MonadState@ class,
--    with an @Int@ state.

modify :: (MonadState m) => (StateType m -> StateType m) -> m ()
modify f = do
    s <- get
    put (f s)

-- | Gets specific component of the state, using a projection function
-- supplied.

gets :: (MonadState m) => (StateType m -> a) -> m a
gets f = do
    s <- get
    return (f s)

instance (Monad m) => MonadState (Lazy.StateT s m) where
    type StateType (Lazy.StateT s m) = s
    get = Lazy.get
    put = Lazy.put

instance (Monad m) => MonadState (Strict.StateT s m) where
    type StateType (Strict.StateT s m) = s
    get = Strict.get
    put = Strict.put

instance (Monad m, Monoid w) => MonadState (LazyRWS.RWST r w s m) where
    type StateType (LazyRWS.RWST r w s m) = s
    get = LazyRWS.get
    put = LazyRWS.put

instance (Monad m, Monoid w) => MonadState (StrictRWS.RWST r w s m) where
    type StateType (StrictRWS.RWST r w s m) = s
    get = StrictRWS.get
    put = StrictRWS.put

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadState m) => MonadState (ContT r m) where
    type StateType (ContT r m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (ExceptT e m) where
    type StateType (ExceptT e m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (IdentityT m) where
    type StateType (IdentityT m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (MaybeT m) where
    type StateType (MaybeT m) = StateType m
    get = lift get
    put = lift . put

instance (MonadState m) => MonadState (ReaderT r m) where
    type StateType (ReaderT r m) = StateType m
    get = lift get
    put = lift . put

instance (Monoid w, MonadState m) => MonadState (Lazy.WriterT w m) where
    type StateType (Lazy.WriterT w m) = StateType m
    get = lift get
    put = lift . put

instance (Monoid w, MonadState m) => MonadState (Strict.WriterT w m) where
    type StateType (Strict.WriterT w m) = StateType m
    get = lift get
    put = lift . put
