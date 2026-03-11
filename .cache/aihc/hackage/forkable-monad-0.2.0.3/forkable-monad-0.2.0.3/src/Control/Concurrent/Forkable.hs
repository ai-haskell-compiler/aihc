-- Copyright 2010 Google Inc.
--
-- Author: David Anderson <dave@natulte.net>
--
-- See LICENSE and forkable-monad.cabal for authorship and licensing
-- information.

-- | This module defines a generic version of @Control.Concurrent@'s
-- 'C.forkIO', which can directly run some complex monadic actions as
-- well as plain 'IO' actions.
--
-- @Control.Concurrent@'s 'C.forkIO' accepts an 'IO' computation only,
-- and requires the caller to reconstruct the full monadic stack by
-- hand in the new thread. In contrast, this module's 'forkIO' runs a
-- computation in the same monad as the parent thread, transparently
-- transplanting the monad stack to the new thread.
--
-- For example, the following code which uses @Control.Concurrent@'s
-- 'C.forkIO':
--
-- > type MyMonad = ReaderT Int (StateT String IO)
-- >
-- > forkAndDo :: MyMonad ThreadId
-- > forkAndDo = do
-- >     r <- ask
-- >     s <- lift get
-- >     liftIO $ forkIO $ (runStateT (runReaderT forkedDo r) s >> return ())
-- >
-- > forkedDo :: MyMonad ()
-- > forkedDo = liftIO $ putStrLn "forkedDo running"
--
-- can be reexpressed with this module's 'forkIO' as:
--
-- > type MyMonad = ReaderT Int (StateT String IO)
-- >
-- > forkAndDo :: MyMonad ThreadId
-- > forkAndDo = forkIO forkedDo
-- >
-- > forkedDo :: MyMonad ()
-- > forkedDo = liftIO $ putStrLn "forkedDo running"
--
-- 'forkIO' can operate on any monad that is an instance of
-- 'ForkableMonad'. 'ForkableMonad' instances are defined for
-- 'ReaderT' and 'StateT', as well as 'IO'. Here is the precise
-- meaning of \"transplant\" for each of these:
--
--   * 'IO' requires no special work, since @Control.Concurrent@'s
--     'C.forkIO' already provides the \"transplanting\" of an 'IO'
--     action to a new thread.
--
--   * 'ReaderT' makes the parent thread's environment available for
--     consultation in the new thread.
--
--   * 'StateT' makes a /copy/ of the parent thread's state available
--     in the new thread. The states in the two threads are not
--     linked, so it is expected that they will diverge as each thread
--     updates its own copy of the state.
--
-- Other standard transformers (notably @WriterT@, @ErrorT@ and
-- @RWST@) do not have an instance defined, because those instances
-- can only be defined through data loss.
--
-- For example, the current output of a @Writer@ cannot be accessed
-- from within the monad, so the best that can be done is to create a
-- new pristine @Writer@ state for the new thread, and to discard all
-- data written in that thread when the thread terminates.
--
-- If you want to use 'forkIO' on a monad stack that includes one of
-- these lossy monads, you will need to define the 'ForkableMonad'
-- instances yourself. The same goes for any custom monads you may
-- have in the stack.
--
-- This module reexports @Control.Concurrent@ overlayed with the
-- generic 'forkIO', so you can simply change your import from
-- @Control.Concurrent@ to @Control.Concurrent.Forkable@ to use this
-- module's 'forkIO' in your existing concurrent code.
module Control.Concurrent.Forkable
       ( ForkableMonad (forkIO)
       , module Control.Concurrent
       ) where

import qualified Control.Concurrent as C
import Control.Concurrent hiding (forkIO)
import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Control.Monad.Trans.State

class (Monad m) => ForkableMonad m where
    -- | Spark off a new thread to run the monadic computation passed
    -- as the first argument, and return the 'ThreadId' of the newly
    -- created thread.
    --
    -- The new thread will run the computation using the same monadic
    -- context as the parent thread.
    --
    -- As a convenience, this forkIO accepts a computation returning
    -- any value, not just unit. This value is discarded when the
    -- computation terminates.
    forkIO :: m a -> m C.ThreadId

instance ForkableMonad IO where
    forkIO act = C.forkIO (act >> return ())

instance (ForkableMonad m) => ForkableMonad (ReaderT r m) where
    forkIO act = do
        env <- ask
        lift $ forkIO (runReaderT act env)

instance (ForkableMonad m) => ForkableMonad (StateT s m) where
    forkIO act = do
        st <- get
        lift $ forkIO (runStateT act st)
