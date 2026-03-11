{- |
Module      :  Control.Monad.Reader.Class
Copyright   :  (c) Andy Gill 2001,
               (c) Oregon Graduate Institute of Science and Technology 2001,
               (c) Jeff Newbern 2003-2007,
               (c) Andriy Palamarchuk 2007
License     :  BSD-style (see the file LICENSE)

Maintainer  :  ross@soi.city.ac.uk
Stability   :  experimental
Portability :  non-portable (type families)

[Computation type:] Computations which read values from a shared environment.

[Binding strategy:] Monad values are functions from the environment to a value.
The bound function is applied to the bound value, and both have access
to the shared environment.

[Useful for:] Maintaining variable bindings, or other shared environment.

[Zero and plus:] None.

[Example type:] @'Reader' [(String,Value)] a@

The 'Reader' monad (also called the Environment monad).
Represents a computation, which can read values from
a shared environment, pass values from function to function,
and execute sub-computations in a modified environment.
Using 'Reader' monad for such computations is often clearer and easier
than using the 'Control.Monad.State.State' monad.

  Inspired by the paper
  /Functional Programming with Overloading and Higher-Order Polymorphism/,
    Mark P Jones (<http://web.cecs.pdx.edu/~mpj/>)
    Advanced School of Functional Programming, 1995.
-}

module Control.Monad.Reader.Class (
    MonadReader(..),
    asks,
    ) where

import Control.Monad.Trans.Cont as Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader (ReaderT)
import qualified Control.Monad.Trans.Reader as ReaderT (ask, local)
import qualified Control.Monad.Trans.RWS.Lazy as LazyRWS (RWST, ask, local)
import qualified Control.Monad.Trans.RWS.Strict as StrictRWS (RWST, ask, local)
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Monad.Trans

-- ----------------------------------------------------------------------------
-- class MonadReader
--  asks for the internal (non-mutable) state.

-- | See examples in "Control.Monad.Reader".
-- Note, the partially applied function type @(->) r@ is a simple reader monad.
-- See the @instance@ declaration below.
class (Monad m) => MonadReader m where
    type EnvType m

    -- | Retrieves the monad environment.
    ask   :: m (EnvType m)

    -- | Executes a computation in a modified environment.
    local :: (EnvType m -> EnvType m)
                        -- ^ The function to modify the environment.
          -> m a        -- ^ @Reader@ to run in the modified environment.
          -> m a

-- | Retrieves a function of the current environment.
asks :: (MonadReader m)
    => (EnvType m -> a) -- ^ The selector function to apply to the environment.
    -> m a
asks f = do
    r <- ask
    return (f r)

-- ----------------------------------------------------------------------------
-- The partially applied function type is a simple reader monad

instance MonadReader ((->) r) where
    type EnvType ((->) r) = r
    ask       = id
    local f m = m . f

instance (Monad m) => MonadReader (ReaderT r m) where
    type EnvType (ReaderT r m) = r
    ask = ReaderT.ask
    local = ReaderT.local

instance (Monoid w, Monad m) => MonadReader (LazyRWS.RWST r w s m) where
    type EnvType (LazyRWS.RWST r w s m) = r
    ask = LazyRWS.ask
    local = LazyRWS.local

instance (Monoid w, Monad m) => MonadReader (StrictRWS.RWST r w s m) where
    type EnvType (StrictRWS.RWST r w s m) = r
    ask = StrictRWS.ask
    local = StrictRWS.local

-- ---------------------------------------------------------------------------
-- Instances for other mtl transformers

instance (MonadReader m) => MonadReader (ContT r m) where
    type EnvType (ContT r m) = EnvType m
    ask   = lift ask
    local = Cont.liftLocal ask local

instance (MonadReader m) => MonadReader (ExceptT e m) where
    type EnvType (ExceptT e m) = EnvType m
    ask   = lift ask
    local = mapExceptT . local

instance (MonadReader m) => MonadReader (IdentityT m) where
    type EnvType (IdentityT m) = EnvType m
    ask   = lift ask
    local = mapIdentityT . local

instance (MonadReader m) => MonadReader (MaybeT m) where
    type EnvType (MaybeT m) = EnvType m
    ask   = lift ask
    local = mapMaybeT . local

instance (MonadReader m) => MonadReader (Lazy.StateT s m) where
    type EnvType (Lazy.StateT s m) = EnvType m
    ask   = lift ask
    local = Lazy.mapStateT . local

instance (MonadReader m) => MonadReader (Strict.StateT s m) where
    type EnvType (Strict.StateT s m) = EnvType m
    ask   = lift ask
    local = Strict.mapStateT . local

instance (Monoid w, MonadReader m) => MonadReader (Lazy.WriterT w m) where
    type EnvType (Lazy.WriterT w m) = EnvType m
    ask   = lift ask
    local = Lazy.mapWriterT . local

instance (Monoid w, MonadReader m) => MonadReader (Strict.WriterT w m) where
    type EnvType (Strict.WriterT w m) = EnvType m
    ask   = lift ask
    local = Strict.mapWriterT . local
