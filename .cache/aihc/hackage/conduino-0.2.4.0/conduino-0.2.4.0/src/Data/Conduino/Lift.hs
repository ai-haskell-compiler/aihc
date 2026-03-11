{-# LANGUAGE LambdaCase    #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE ViewPatterns  #-}

-- |
-- Module      : Data.Conduino.Lift
-- Copyright   : (c) Justin Le 2019
-- License     : BSD3
--
-- Maintainer  : justin@jle.im
-- Stability   : experimental
-- Portability : non-portable
--
-- Working with underlying monad transformers and 'Pipe'.
--
-- There is no "general abstraction" for dealing with each monad
-- transformer, but we can translate the semantics that each monad
-- transformer provides into meaningful 'Pipe' operations.
--
-- For example, a @'Pipe' i o u ('State' s) a@ is a pipe working over
-- stateful effects --- it can pull information and modify an underlying
-- state to do its job.  It takes in @i@ and outputs @o@, using an
-- underlying state @s@.
--
-- However, such a pipe is similar to @s -> 'Pipe'
-- i o u 'Data.Functor.Identity.Identity' (a, s)@.  Giving some starting
-- state, it takes in @i@ and outputs @o@, and when it completes, it
-- returns an @a@ and an @s@, the final state after all its processing is
-- done.
--
-- The /general/ idea is that:
--
-- *  A pipe over a monad transformer /shares that monadic context/ over
--    /every pipe/ in a composition.
--
--    For example, if @p@, @q@, and @r@ are all pipes over 'StateT', the @p
--    .| q .| r@ will all share a common global state.
--
--    If @p@, @q@, and @r@ are all pipes over 'ExceptT', then @p .| q .| r@
--    will all short-circult fail each other: if @q@ fails, then they all
--    fail, etc.
--
--    If @p@, @q@, and @r@ are all pipes over 'WriterT' then @p .| q .| r@
--    will all accumulate to a shared global log.
--
--    If @p@, @q@, and @r@ are all pipes over 'ReaderT' then @p .| q .| r@
--    will use the same identical environment.
--
-- *  Using the @runX@ family of functions ('runStateP', 'runExceptP',
--    etc.) lets you /isolate/ out the common context within a composition
--    of pipes.
--
--    For example, if @p@ is a pipe over 'StateT', then @a .| 'void' ('runStateP'
--    s0 p) .| b@, @a@ and @b@ will not be able to use the state of @p@.
--
--    If @p@ is a pipe over 'ExceptT', then in @a .| void ('runExceptP' p) .|
--    b@, a failure in @p@ will not cause all the others to fail.
--
-- Both of these representations have different advantages and
-- disadvantages, that are separate and unique for each individual monad
-- transformer on a case-by-case basis.  This module provides functions on
-- such a case-by-case basis as you need them.
--
-- @since 0.2.1.0
module Data.Conduino.Lift (
  -- * State
  -- ** Lazy
    stateP, runStateP, evalStateP, execStateP
  -- ** Strict
  , statePS, runStatePS, evalStatePS, execStatePS
  -- * Except
  , exceptP, runExceptP, runExceptP_
  -- * Reader
  , readerP, runReaderP
  -- * Writer
  -- ** Lazy
  , writerP, runWriterP, execWriterP
  -- ** Strict
  , writerPS, runWriterPS, execWriterPS
  -- * RWS
  -- ** Lazy
  , rwsP, runRWSP, evalRWSP, execRWSP
  -- ** Strict
  , rwsPS, runRWSPS, evalRWSPS, execRWSPS
  -- * Catch
  , catchP, runCatchP
  ) where

import           Control.Monad
import           Control.Monad.Catch.Pure
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Free
import           Control.Monad.Trans.Free.Church
import           Control.Monad.Trans.RWS           (RWST(..))
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Control.Monad.Trans.Writer
import           Data.Conduino
import           Data.Conduino.Internal
import           Data.Functor
import qualified Control.Monad.Trans.RWS           as RWS
import qualified Control.Monad.Trans.RWS.Strict    as RWSS
import qualified Control.Monad.Trans.State.Strict  as SS
import qualified Control.Monad.Trans.Writer.Strict as WS

-- | Turn a "state-modifying 'Pipe'" into a 'Pipe' that runs over 'StateT',
-- so you can chain it with other 'StateT' pipes.
--
-- Note that this will /overwrite/ whatever state exists with
-- the @s@ that it gets when it terminates.  If any other pipe in this
-- chain modifies or uses state, all modifications will be overwritten when
-- the @(a, s)@-producing pipe terminates.
--
-- @since 0.2.1.0
stateP
    :: Monad m
    => (s -> Pipe i o u m (a, s))
    -> Pipe i o u (StateT s m) a
stateP f = do
    s       <- lift get
    (x, s') <- hoistPipe lift (f s)
    x <$ lift (put s')

-- | Like 'runStateP', but ignoring the final result.  It returns the final
-- state after the pipe succesfuly terminates.
--
-- @since 0.2.1.0
execStateP
    :: Monad m
    => s
    -> Pipe i o u (StateT s m) a
    -> Pipe i o u m s
execStateP s = fmap snd . runStateP s
{-# INLINE execStateP #-}

-- | Takes a 'Pipe' over 'StateT' and "hides" the state from the outside
-- world.  Give an initial state --- the pipe behaves the same way, but to
-- the external user it is abstracted away.  See 'runStateP' for more
-- information.
--
-- This can be cleaner than 'runStateP' because if @a@ is @()@, you
-- don't have to sprinkle in 'void' everywhere.  However, it's only really
-- useful if you don't need to get the final state upon termination.
--
-- @since 0.2.1.0
evalStateP
    :: Monad m
    => s
    -> Pipe i o u (StateT s m) a
    -> Pipe i o u m a
evalStateP s = fmap fst . runStateP s
{-# INLINE evalStateP #-}

-- | 'stateP', but for "Control.Monad.Trans.State.Strict".
--
-- @since 0.2.1.0
statePS
    :: Monad m
    => (s -> Pipe i o u m (a, s))
    -> Pipe i o u (SS.StateT s m) a
statePS f = do
    s       <- lift SS.get
    (x, s') <- hoistPipe lift (f s)
    x <$ lift (SS.put s')
{-# INLINE statePS #-}

-- | 'execStateP', but for "Control.Monad.Trans.State.Strict".
--
-- @since 0.2.1.0
execStatePS
    :: Monad m
    => s
    -> Pipe i o u (SS.StateT s m) a
    -> Pipe i o u m s
execStatePS s = fmap snd . runStatePS s
{-# INLINE execStatePS #-}

-- | 'evalStateP', but for "Control.Monad.Trans.State.Strict".
--
-- @since 0.2.1.0
evalStatePS
    :: Monad m
    => s
    -> Pipe i o u (SS.StateT s m) a
    -> Pipe i o u m a
evalStatePS s = fmap fst . runStatePS s
{-# INLINE evalStatePS #-}

-- | Turn a "failable-result" 'Pipe' into a pipe over 'ExceptT'.
--
-- Note that a 'throwE' failure will only ever happen when the input pipe
-- "succesfully" terminates with 'Left'.  It would never happen before the
-- pipe terminates, since you don't get the @'Either' e a@ until the pipe
-- succesfully terminates.
--
-- @since 0.2.1.0
exceptP
    :: Monad m
    => Pipe i o u m (Either e a)
    -> Pipe i o u (ExceptT e m) a
exceptP p = hoistPipe lift p >>= \case
    Left  e -> lift $ throwE e
    Right x -> pure x
{-# INLINE exceptP #-}

-- | Turn a 'Pipe' that runs over 'ExceptT' into an "early-terminating
-- 'Pipe'" that "succesfully" returns 'Left' or 'Right'.
--
-- The main usage of this is to "isolate" the short-circuiting failure of
-- 'ExceptT' to only happen within one component of a chain.  For example,
-- of @p@, @q@, and @r@ are all pipes under 'ExceptT', then:
--
-- @
--     p
--  .| q
--  .| r
-- @
--
-- will short-circuit fail if /any/ of @p@, @q@, or @r@ fail.  We have
-- global failure only.
--
-- However, if you use 'runExceptP', we isolate the short-circuiting
-- failure to only a single type.
--
-- @
--     void (runExceptP p)
--  .| void (runExceptP q)
--  .| runExceptP r
-- @
--
-- In this case, if (for example) @q@ fails, it won't cause the whole thing
-- to fail: it will just be the same as if @q@ succesfully terminates
-- normally.
--
-- This is also useful if you want to chain a pipe over 'ExceptT' with
-- pipes that don't have 'ExceptT' at all: for example if @a@ and @b@ are
-- "non-erroring" pipes (/not/ over 'ExceptT'), you can do:
--
-- @
--     a
--  .| void (runExceptP q)
--  .| b
-- @
--
-- And @a@ and @b@ will be none the wiser to the fact that @q@ uses
-- 'ExceptT' internally.
--
-- Note to avoid the usage of 'void', 'runExceptP_' might be more useful.
--
-- @since 0.2.1.0
runExceptP
    :: Monad m
    => Pipe i o u (ExceptT e m) a
    -> Pipe i o u m (Either e a)
runExceptP (Pipe (FT f)) = Pipe $ FT $ \pr bd -> either (pr . Left) pure =<< runExceptT (
      f (lift . pr . Right)
        (\c -> lift . bd (either (pr . Left) pure <=< runExceptT . c))
    )
{-# INLINE runExceptP #-}

-- | A handy version of 'runExceptP' that discards its output, so it can be
-- easier to chain using '.|'.  It's useful if you are using 'runExceptP'
-- to "isolate" failures from the rest of a chain.
--
-- @since 0.2.1.0
runExceptP_
    :: Monad m
    => Pipe i o u (ExceptT e m) a
    -> Pipe i o u m ()
runExceptP_ = void . runExceptP
{-# INLINE runExceptP_ #-}

-- | Like 'exceptP', but for 'CatchT'.  See 'exceptP' for usage details and
-- caveats.  In general, can be useful for chaining with other 'CatchT'
-- pipes.
--
-- Note that a 'throwM' failure will only ever happen when the input pipe
-- "succesfully" terminates with 'Left'.  It would never happen before the
-- pipe terminates, since you don't get the @'Either' 'SomeException' a@
-- until the pipe succesfully terminates.
--
-- @since 0.2.1.0
catchP
    :: Monad m
    => Pipe i o u m (Either SomeException a)
    -> Pipe i o u (CatchT m) a
catchP p = hoistPipe lift p >>= \case
    Left  e -> lift $ throwM e
    Right x -> pure x
{-# INLINE catchP #-}

-- | Like 'runExceptP', but for 'CatchT'.  See 'runExceptP' for usage
-- details.  In general, can be useful for "isolating" a 'CatchT' pipe from
-- the rest of its chain.
--
-- @since 0.2.1.0
runCatchP
    :: Monad m
    => Pipe i o u (CatchT m) a
    -> Pipe i o u m (Either SomeException a)
-- runCatchP (Pipe (FT f)) = Pipe $ FT $ \pr bd -> either (pr . Left) pure =<< runCatchT (
--       f (lift . pr . Right)
--         (\c -> lift . bd (either (pr . Left) pure <=< runCatchT . c))
--     )
runCatchP = withRecPipe go
  where
    go (FreeT p) = FreeT $ runCatchT p <&>
      either (Pure . Left)
             (\case Pure x -> Pure (Right x); Free l -> Free (go <$> l))
      -- \case
      -- Left  e        -> Pure $ Left e
      -- Right (Pure x) -> Pure $ Right x
      -- Right (Free l) -> Free $ go <$> l
{-# INLINE runCatchP #-}

-- | Turn a "parameterized 'Pipe'" into a 'Pipe' that runs over 'ReaderT',
-- so you can chain it with other 'ReaderT' pipes.
--
-- Essentially, instead of directly providing the @r@ in an @r -> 'Pipe'
-- i o u m a@, the @r@ instead comes from the globally shared environment.
--
-- @since 0.2.1.0
readerP
    :: Monad m
    => (r -> Pipe i o u m a)
    -> Pipe i o u (ReaderT r m) a
readerP f = hoistPipe lift . f =<< lift ask

-- | Turn a pipe over 'ReaderT' into a directly parameterized pipe.
-- Instead of getting the parameter from the globally shared 'ReaderT'
-- environment, give it directly instead.
--
-- It can be useful to "ignore" a globally shared environment and just give
-- the @r@ directly and immediately.
--
-- @since 0.2.1.0
runReaderP
    :: Monad m
    => r
    -> Pipe i o u (ReaderT r m) a
    -> Pipe i o u m a
runReaderP r = hoistPipe (`runReaderT` r)

-- | Turn a pipe returning an @(a, w)@ tuple upon termination into a pipe
-- returning @a@, logging the @w@ in an underlying 'WriterT' context.
--
-- This can be useful for composing your pipe with other 'WriterT' pipes,
-- aggregating all to a common global log.
--
-- However, be aware that this only ever 'tell's when the pipe succesfuly
-- terminates.  It doesn't do "streaming logging" -- it only makes one
-- log payload at the point of succesful termination.  To do streaming
-- logging (logging things as you get them), you should probably just
-- directly use 'WriterT' instead, with 'Data.Conduino.Combinators.repeatM'
-- or 'Data.Conduino.Combinators.iterM' or something similar.
--
-- @since 0.2.1.0
writerP
    :: (Monad m, Monoid w)
    => Pipe i o u m (a, w)
    -> Pipe i o u (WriterT w m) a
writerP p = do
    (x, w) <- hoistPipe lift p
    x <$ lift (tell w)

-- | Turn a 'Pipe' that runs over 'WriterT' into a 'Pipe' that returns the
-- final log when it terminates.
--
-- The main usage of this is to "isolate" the log from other pipes in the
-- same chain.  For example, of @p@, @q@, and @r@ are all pipes under
-- 'WriterT', then:
--
-- @
--     p
--  .| q
--  .| r
-- @
--
-- will all share underlying log, and all logging from any of them will
-- accumulate together in an interleaved way.  It is essentially a global
-- log.
--
-- However, if you use 'runWriterP', you can all have them use different
-- encapsulated logs.
--
-- @
--     void (runWriterP p)
--  .| void (runWriterP q)
--  .| runWriterP r
-- @
--
-- In this case, each of those three chained pipes will use their own
-- internal logs, without sharing.
--
-- This is also useful if you want to chain a pipe over 'WriterT' with
-- pipes that don't use state at all: for example if @a@ and @b@ are
-- "non-logging" pipes (/not/ over 'WriterT'), you can do:
--
-- @
--     a
--  .| void (runWriterP q)
--  .| b
-- @
--
-- And @a@ and @b@ will be none the wiser to the fact that @q@ uses
-- 'WriterT' internally.
--
-- @since 0.2.1.0
runWriterP
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m (a, w)
runWriterP = withRecPipe (go mempty)
  where
    go w (FreeT p) = FreeT $ runWriterT p <&> \(r, (w <>)->w') ->
      case r of
        Pure x -> Pure (x, w')
        Free l -> Free $ go w' <$> l
{-# INLINE runWriterP #-}

-- | 'runWriterP', but only returning the final log after succesful
-- termination.
--
-- @since 0.2.1.0
execWriterP
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m w
execWriterP = fmap snd . runWriterP
{-# INLINE execWriterP #-}

-- | 'writerP', but for "Control.Monad.Trans.Writer.Strict".
--
-- @since 0.2.1.0
writerPS
    :: (Monad m, Monoid w)
    => Pipe i o u m (a, w)
    -> Pipe i o u (WS.WriterT w m) a
writerPS p = do
    (x, w) <- hoistPipe lift p
    x <$ lift (WS.tell w)

-- | 'runWriterP', but for "Control.Monad.Trans.Writer.Strict".
--
-- @since 0.2.1.0
runWriterPS
    :: (Monad m, Monoid w)
    => Pipe i o u (WS.WriterT w m) a
    -> Pipe i o u m (a, w)
runWriterPS = withRecPipe (go mempty)
  where
    go w (FreeT p) = FreeT $ WS.runWriterT p <&> \(r, (w <>)->w') ->
      case r of
        Pure x -> Pure (x, w')
        Free l -> Free $ go w' <$> l
{-# INLINE runWriterPS #-}

-- | 'execWriterP', but for "Control.Monad.Trans.Writer.Strict".
--
-- @since 0.2.1.0
execWriterPS
    :: (Monad m, Monoid w)
    => Pipe i o u (WriterT w m) a
    -> Pipe i o u m w
execWriterPS = fmap snd . runWriterP
{-# INLINE execWriterPS #-}

-- | Turn a parameterized, state-transforming, log-producing 'Pipe' into
-- a 'Pipe' over 'RWST', which can be useful for chaining it with other
-- 'RWST' pipes.
--
-- See 'stateP' and 'writerP' for more details on caveats, including:
--
-- *  Logging only happens when the @(a,s,w)@-returning pipe terminates.
--    There is no "streaming logging" --- the resulting @w@ is logged all
--    at once.
-- *  When the @(a,s,w)@-returning pipe terminates, whatever state in the
--    'RWST' is overwritten with the @s@ returned.  If other pipes in the
--    chain modify the @s@, their modifications will be overwritten.
--
-- @since 0.2.1.0
rwsP
    :: (Monad m, Monoid w)
    => (r -> s -> Pipe i o u m (a, s, w))
    -> Pipe i o u (RWST r w s m) a
rwsP f = do
    r <- lift RWS.ask
    s <- lift RWS.get
    (x, s', w) <- hoistPipe lift (f r s)
    lift (RWS.tell w)
    x <$ lift (RWS.put s')

-- | Turn a 'Pipe' that runs over 'RWST' into a state-modifying,
-- environment-using, log-accumulating 'Pipe'.  See 'runStateP',
-- 'runWriterP', and 'runReaderP' for the uses and semantics.
--
-- @since 0.2.1.0
runRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (a, s, w)
runRWSP r = withRecPipe . go mempty
  where
    go w s (FreeT p) = FreeT $ runRWST p r s <&> \(q, s', (w <>)->w') ->
      case q of
        Pure x -> Pure (x, s', w')
        Free l -> Free $ go w' s' <$> l
{-# INLINE runRWSP #-}

-- | 'runRWSP', but ignoring the final state.
--
-- @since 0.2.1.0
evalRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (a, w)
evalRWSP r s = fmap (\(x,_,w) -> (x,w)) . runRWSP r s
{-# INLINE evalRWSP #-}

-- | 'runRWSP', but ignoring the result value.
--
-- @since 0.2.1.0
execRWSP
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWST r w s m) a
    -> Pipe i o u m (s, w)
execRWSP r s = fmap (\(_,s',w) -> (s',w)) . runRWSP r s
{-# INLINE execRWSP #-}

-- | 'rwsP', but for "Control.Monad.Trans.RWS.Strict".
--
-- @since 0.2.1.0
rwsPS
    :: (Monad m, Monoid w)
    => (r -> s -> Pipe i o u m (a, s, w))
    -> Pipe i o u (RWSS.RWST r w s m) a
rwsPS f = do
    r <- lift RWSS.ask
    s <- lift RWSS.get
    (x, s', w) <- hoistPipe lift (f r s)
    lift (RWSS.tell w)
    x <$ lift (RWSS.put s')

-- | 'runRWSPS', but for "Control.Monad.Trans.RWS.Strict".
--
-- @since 0.2.1.0
runRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (a, s, w)
runRWSPS r = withRecPipe . go mempty
  where
    go w s (FreeT p) = FreeT $ RWSS.runRWST p r s <&> \(q, s', (w <>)->w') ->
      case q of
        Pure x -> Pure (x, s', w')
        Free l -> Free $ go w' s' <$> l
{-# INLINE runRWSPS #-}

-- | 'evalRWSPS', but for "Control.Monad.Trans.RWS.Strict".
--
-- @since 0.2.1.0
evalRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (a, w)
evalRWSPS r s = fmap (\(x,_,w) -> (x,w)) . runRWSPS r s
{-# INLINE evalRWSPS #-}

-- | 'execRWSPS', but for "Control.Monad.Trans.RWS.Strict".
--
-- @since 0.2.1.0
execRWSPS
    :: (Monad m, Monoid w)
    => r
    -> s
    -> Pipe i o u (RWSS.RWST r w s m) a
    -> Pipe i o u m (s, w)
execRWSPS r s = fmap (\(_,s',w) -> (s',w)) . runRWSPS r s
{-# INLINE execRWSPS #-}
