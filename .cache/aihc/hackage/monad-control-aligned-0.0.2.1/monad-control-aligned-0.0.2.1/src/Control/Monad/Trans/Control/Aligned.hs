{-# LANGUAGE CPP
           , NoImplicitPrelude
           , RankNTypes
           , TypeFamilies
           , TupleSections
           , FunctionalDependencies
           , FlexibleInstances
           , UndecidableInstances
           , MultiParamTypeClasses #-}

{-# LANGUAGE Safe #-}

#if MIN_VERSION_transformers(0,4,0)
-- Hide warnings for the deprecated ErrorT transformer:
{-# OPTIONS_GHC -fno-warn-warnings-deprecations #-}
#endif

{- |
Copyright   :  Bas van Dijk, Anders Kaseorg
License     :  BSD3
Maintainer  :  Bas van Dijk <v.dijk.bas@gmail.com>

This module defines the type class 'MonadBaseControl', a subset of
'MonadBase' into which generic control operations such as @catch@ can be
lifted from @IO@ or any other base monad. Instances are based on monad
transformers in 'MonadTransControl', which includes all standard monad
transformers in the @transformers@ library except @ContT@ and @SelectT@.

See the <http://hackage.haskell.org/package/lifted-base lifted-base>
package which uses @monad-control@ to lift @IO@
operations from the @base@ library (like @catch@ or @bracket@) into any monad
that is an instance of @MonadBase@ or @MonadBaseControl@.

See the following tutorial by Michael Snoyman on how to use this package:

<https://www.yesodweb.com/book/monad-control>

=== Quick implementation guide

Given a base monad @B@ and a stack of transformers @T@:

* Define instances @'MonadTransControl' T@ for all transformers @T@, using the
  @'defaultLiftWith'@ and @'defaultRestoreT'@ functions on the constructor and
  deconstructor of @T@.

* Define an instance @'MonadBaseControl' B B@ for the base monad:

    @
    instance MonadBaseControl B B where
        type StM B a   = a
        liftBaseWith f = f 'id'
        restoreM       = 'return'
    @

* Define instances @'MonadBaseControl' B m => 'MonadBaseControl' B (T m)@ for
  all transformers:

    @
    instance MonadBaseControl b m => MonadBaseControl b (T m) where
        type StM (T m) a = 'ComposeSt' T m a
        liftBaseWith f   = 'defaultLiftBaseWith'
        restoreM         = 'defaultRestoreM'
    @
-}

module Control.Monad.Trans.Control.Aligned
    ( -- * MonadTransControl
      MonadTransControl(..), Run

      -- ** Defaults
      -- $MonadTransControlDefaults
    , RunDefault, defaultLiftWith, defaultRestoreT

      -- * MonadBaseControl
    , MonadBaseControl (..), RunInBase

      -- ** Defaults
      -- $MonadBaseControlDefaults
    , RunInBaseDefault, defaultLiftBaseWith, defaultRestoreM

      -- * Utility functions
    , control, controlT, embed, embed_, captureT, captureM

    , liftBaseOp, liftBaseOp_

    , liftBaseDiscard, liftBaseOpDiscard

    , liftThrough
    ) where


--------------------------------------------------------------------------------
-- Imports
--------------------------------------------------------------------------------

-- from base:
import Data.Function ( (.), ($), const )
import Data.Monoid   ( Monoid, mempty )
import Control.Monad ( Monad, (>>=), return, liftM )
import System.IO     ( IO )
import Data.Maybe    ( Maybe )
import Data.Either   ( Either )
-- <<<<<<< HEAD:src/Control/Monad/Trans/Control.hs
-- import Control.Monad ( void )
-- import Prelude       ( id )
-- =======
import Data.Functor.Identity ( Identity (..) )
import Data.Functor.Compose  ( Compose (..) )
import Data.Tuple    ( swap )
-- >>>>>>> 696e8dd (exposing monadic state for augmentation):Control/Monad/Trans/Control.hs

import           Control.Monad.ST.Lazy.Safe           ( ST )
import qualified Control.Monad.ST.Safe      as Strict ( ST )

-- from stm:
import Control.Monad.STM ( STM )

-- from transformers:
import Control.Monad.Trans.Class    ( MonadTrans )

import Control.Monad.Trans.Identity ( IdentityT(IdentityT), runIdentityT )
import Control.Monad.Trans.Maybe    ( MaybeT   (MaybeT),    runMaybeT )
import Control.Monad.Trans.Reader   ( ReaderT  (ReaderT),   runReaderT )
import Control.Monad.Trans.State    ( StateT   (StateT),    runStateT )
import Control.Monad.Trans.Writer   ( WriterT  (WriterT),   runWriterT )
import Control.Monad.Trans.RWS      ( RWST     (RWST),      runRWST )
import Control.Monad.Trans.Except   ( ExceptT  (ExceptT),   runExceptT )

#if !(MIN_VERSION_transformers(0,6,0))
import Control.Monad.Trans.List     ( ListT    (ListT),     runListT )
import Control.Monad.Trans.Error    ( ErrorT   (ErrorT),    runErrorT, Error )
#endif

import qualified Control.Monad.Trans.RWS.Strict    as Strict ( RWST   (RWST),    runRWST )
import qualified Control.Monad.Trans.State.Strict  as Strict ( StateT (StateT),  runStateT )
import qualified Control.Monad.Trans.Writer.Strict as Strict ( WriterT(WriterT), runWriterT )

import Data.Functor.Identity ( Identity )

-- from transformers-base:
import Control.Monad.Base ( MonadBase )

-- <<<<<<< HEAD:src/Control/Monad/Trans/Control.hs
-- =======
#if MIN_VERSION_base(4,3,0)
import Control.Monad ( void )
#else
import Data.Functor (Functor, fmap)
void :: Functor f => f a -> f ()
void = fmap (const ())
#endif

import Prelude (id, (<$>), pure)

--------------------------------------------------------------------------------
-- MonadTransControl type class
--------------------------------------------------------------------------------

class MonadTrans t => MonadTransControl t stT | t -> stT where
  -- | @liftWith@ is similar to 'lift' in that it lifts a computation from
  -- the argument monad to the constructed monad.
  --
  -- Instances should satisfy similar laws as the 'MonadTrans' laws:
  --
  -- @liftWith (\\_ -> return a) = return a@
  --
  -- @liftWith (\\_ -> m >>= f)  =  liftWith (\\_ -> m) >>= (\\a -> liftWith (\\_ -> f a))@
  --
  -- The difference with 'lift' is that before lifting the @m@ computation
  -- @liftWith@ captures the state of @t@. It then provides the @m@
  -- computation with a 'Run' function that allows running @t n@ computations in
  -- @n@ (for all @n@) on the captured state.
  liftWith :: Monad m => (Run t stT -> m a) -> t m a

  -- | Construct a @t@ computation from the monadic state of @t@ that is
  -- returned from a 'Run' function.
  --
  -- Instances should satisfy:
  --
  -- @liftWith (\\run -> run t) >>= restoreT . return = t@
  restoreT :: Monad m => m (stT a) -> t m a

-- | A function that runs a transformed monad @t n@ on the monadic state that
-- was captured by 'liftWith'
--
-- A @Run t@ function yields a computation in @n@ that returns the monadic state
-- of @t@. This state can later be used to restore a @t@ computation using
-- 'restoreT'.
type Run t stT = forall n b. Monad n => t n b -> n (stT b)


--------------------------------------------------------------------------------
-- Defaults for MonadTransControl
--------------------------------------------------------------------------------

-- $MonadTransControlDefaults
--
-- The following functions can be used to define a 'MonadTransControl' instance
-- for a monad transformer which simply is a newtype around another monad
-- transformer which already has a @MonadTransControl@ instance. For example:
--
-- @
-- {-\# LANGUAGE GeneralizedNewtypeDeriving \#-}
-- {-\# LANGUAGE UndecidableInstances \#-}
-- {-\# LANGUAGE TypeFamilies \#-}
--
-- newtype CounterT m a = CounterT {unCounterT :: StateT Int m a}
--   deriving (Monad, MonadTrans)
--
-- instance MonadTransControl CounterT where
--     type StT CounterT a = StT (StateT Int) a
--     liftWith = 'defaultLiftWith' CounterT unCounterT
--     restoreT = 'defaultRestoreT' CounterT
-- @

-- | A function like 'Run' that runs a monad transformer @t@ which wraps the
-- monad transformer @t'@. This is used in 'defaultLiftWith'.
type RunDefault t stT = forall n b. Monad n => t n b -> n (stT b)

-- | Default definition for the 'liftWith' method.
defaultLiftWith :: (Monad m, MonadTransControl n stT)
                => (forall b.   n m b -> t m b)     -- ^ Monad constructor
                -> (forall o b. t o b -> n o b)     -- ^ Monad deconstructor
                -> (RunDefault t stT -> m a)
                -> t m a
defaultLiftWith t unT = \f -> t $ liftWith $ \run -> f $ run . unT
{-# INLINABLE defaultLiftWith #-}

-- | Default definition for the 'restoreT' method.
defaultRestoreT :: (Monad m, MonadTransControl n stT)
                => (n m a -> t m a)     -- ^ Monad constructor
                -> m (stT a)
                -> t m a
defaultRestoreT t = t . restoreT
{-# INLINABLE defaultRestoreT #-}


--------------------------------------------------------------------------------
-- MonadTransControl instances
--------------------------------------------------------------------------------

instance MonadTransControl IdentityT Identity where
    liftWith f = IdentityT $ f $ \mx -> Identity <$> runIdentityT mx
    restoreT mx = IdentityT $ runIdentity <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl MaybeT Maybe where
    liftWith f = MaybeT $ liftM return $ f $ runMaybeT
    restoreT = MaybeT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

#if !(MIN_VERSION_transformers(0,6,0))
instance Error e => MonadTransControl (ErrorT e) (Either e) where
    liftWith f = ErrorT $ liftM return $ f $ runErrorT
    restoreT = ErrorT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
#endif

instance MonadTransControl (ExceptT e) (Either e) where
    liftWith f = ExceptT $ liftM return $ f $ runExceptT
    restoreT = ExceptT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

#if !(MIN_VERSION_transformers(0,6,0))
instance MonadTransControl ListT [] where
    liftWith f = ListT $ liftM return $ f $ runListT
    restoreT = ListT
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}
#endif

instance MonadTransControl (ReaderT r) Identity where
    liftWith f = ReaderT $ \r -> f $ \t -> Identity <$> runReaderT t r
    restoreT mx = ReaderT $ \_ -> runIdentity <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (StateT s) ((,) s) where
    liftWith f = StateT $ \s -> (,s) <$> f (\t -> swap <$> runStateT t s)
    restoreT mx = StateT $ \_ -> swap <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance MonadTransControl (Strict.StateT s) ((,) s) where
    liftWith f = Strict.StateT $ \s -> (,s) <$> f (\t -> swap <$> Strict.runStateT t s)
    restoreT mx = Strict.StateT $ \_ -> swap <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (WriterT w) ((,) w) where
    liftWith f = WriterT $ (,mempty) <$> (f $ \t -> swap <$> runWriterT t)
    restoreT mx = WriterT $ swap <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (Strict.WriterT w) ((,) w) where
    liftWith f = Strict.WriterT $ (,mempty) <$> (f $ \t -> swap <$> Strict.runWriterT t)
    restoreT mx = Strict.WriterT $ swap <$> mx
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (RWST r w s) ((,,) w s) where
    liftWith f = RWST $ \r s -> (,s,mempty) <$> (f $ \t -> (\(a,s,w) -> (w,s,a)) <$> runRWST t r s)
    restoreT mSt = RWST $ \_ _ -> (\(w,s,a) -> (a,s,w)) <$> mSt
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}

instance Monoid w => MonadTransControl (Strict.RWST r w s) ((,,) w s) where
    liftWith f = Strict.RWST $ \r s -> (,s,mempty) <$> (f $ \t -> (\(a,s,w) -> (w,s,a)) <$> Strict.runRWST t r s)
    restoreT mSt = Strict.RWST $ \_ _ -> (\(w,s,a) -> (a,s,w)) <$> mSt
    {-# INLINABLE liftWith #-}
    {-# INLINABLE restoreT #-}


--------------------------------------------------------------------------------
-- MonadBaseControl type class
--------------------------------------------------------------------------------

class MonadBase b m => MonadBaseControl b m stM | m -> b stM where

    -- | @liftBaseWith@ is similar to 'liftIO' and 'liftBase' in that it
    -- lifts a base computation to the constructed monad.
    --
    -- Instances should satisfy similar laws as the 'MonadIO' and 'MonadBase' laws:
    --
    -- @liftBaseWith (\\_ -> return a) = return a@
    --
    -- @liftBaseWith (\\_ -> m >>= f)  =  liftBaseWith (\\_ -> m) >>= (\\a -> liftBaseWith (\\_ -> f a))@
    --
    -- As <https://stackoverflow.com/a/58106822/1477667 Li-yao Xia explains>, parametricity
    -- guarantees that
    --
    -- @f <$> liftBaseWith q = liftBaseWith $ \runInBase -> f <$> q runInBase@
    --
    -- The difference with 'liftBase' is that before lifting the base computation
    -- @liftBaseWith@ captures the state of @m@. It then provides the base
    -- computation with a 'RunInBase' function that allows running @m@
    -- computations in the base monad on the captured state.
    liftBaseWith :: (RunInBase m b stM -> b a) -> m a

    -- | Construct a @m@ computation from the monadic state of @m@ that is
    -- returned from a 'RunInBase' function.
    --
    -- Instances should satisfy:
    --
    -- @liftBaseWith (\\runInBase -> runInBase m) >>= restoreM = m@
    restoreM :: stM a -> m a

-- | A function that runs a @m@ computation on the monadic state that was
-- captured by 'liftBaseWith'
--
-- A @RunInBase m@ function yields a computation in the base monad of @m@ that
-- returns the monadic state of @m@. This state can later be used to restore the
-- @m@ computation using 'restoreM'.
type RunInBase m b stM = forall a. m a -> b (stM a)


--------------------------------------------------------------------------------
-- MonadBaseControl instances for all monads in the base library
--------------------------------------------------------------------------------

instance MonadBaseControl IO IO Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl Maybe Maybe Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl (Either e) (Either e) Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl [] [] Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl ((->) r) ((->) r) Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl Identity Identity Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl STM STM Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

#if MIN_VERSION_base(4,4,0)
instance MonadBaseControl (Strict.ST s) (Strict.ST s) Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance MonadBaseControl (ST s) (ST s) Identity where
  liftBaseWith f = f (\x -> Identity <$> x)
  restoreM x = pure (runIdentity x)
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}
#endif

#undef BASE


--------------------------------------------------------------------------------
-- Defaults for MonadBaseControl
--------------------------------------------------------------------------------

-- $MonadBaseControlDefaults
--
-- Note that by using the following default definitions it's easy to make a
-- monad transformer @T@ an instance of 'MonadBaseControl':
--
-- @
-- instance MonadBaseControl b m => MonadBaseControl b (T m) where
--     type StM (T m) a = 'ComposeSt' T m a
--     liftBaseWith     = 'defaultLiftBaseWith'
--     restoreM         = 'defaultRestoreM'
-- @
--
-- Defining an instance for a base monad @B@ is equally straightforward:
--
-- @
-- instance MonadBaseControl B B where
--     type StM B a   = a
--     liftBaseWith f = f 'id'
--     restoreM       = 'return'
-- @

-- | A function like 'RunInBase' that runs a monad transformer @t@ in its base
-- monad @b@. It is used in 'defaultLiftBaseWith'.
type RunInBaseDefault (t :: (* -> *) -> * -> *) (m :: * -> *) (b :: * -> *) (stM :: * -> *) (stT :: * -> *) = forall a. t m a -> b (Compose stM stT a)

-- | Default definition for the 'liftBaseWith' method.
--
-- Note that it composes a 'liftWith' of @t@ with a 'liftBaseWith' of @m@ to
-- give a 'liftBaseWith' of @t m@:
--
-- @
-- defaultLiftBaseWith = \\f -> 'liftWith' $ \\run ->
--                               'liftBaseWith' $ \\runInBase ->
--                                 f $ runInBase . run
-- @
defaultLiftBaseWith :: (MonadTransControl t stT, MonadBaseControl b m stM)
                    => (RunInBaseDefault t m b stM stT -> b a) -> t m a
defaultLiftBaseWith f = liftWith $ \run ->
                          liftBaseWith $ \runInBase ->
                            f (\t -> Compose <$> runInBase (run t))
{-# INLINABLE defaultLiftBaseWith #-}

-- | Default definition for the 'restoreM' method.
--
-- Note that: @defaultRestoreM = 'restoreT' . 'restoreM'@
defaultRestoreM :: (MonadTransControl t stT, MonadBaseControl b m stM)
                => Compose stM stT a -> t m a
defaultRestoreM (Compose x) = restoreT (restoreM x)
{-# INLINABLE defaultRestoreM #-}


--------------------------------------------------------------------------------
-- MonadBaseControl transformer instances
--------------------------------------------------------------------------------

instance (MonadBaseControl b m stM) => MonadBaseControl b (IdentityT m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM) => MonadBaseControl b (MaybeT m) (Compose stM Maybe) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

#if !(MIN_VERSION_transformers(0,6,0))
instance (MonadBaseControl b m stM) => MonadBaseControl b (ListT m) (Compose stM []) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}
#endif

instance (MonadBaseControl b m stM) => MonadBaseControl b (ReaderT r m) (Compose stM Identity) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM) => MonadBaseControl b (Strict.StateT s m) (Compose stM ((,) s)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM) => MonadBaseControl b (StateT s m) (Compose stM ((,) s)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM) => MonadBaseControl b (ExceptT e m) (Compose stM (Either e)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}



#if !(MIN_VERSION_transformers(0,6,0))
instance (MonadBaseControl b m stM, Error e) => MonadBaseControl b (ErrorT e m) (Compose stM (Either e)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}
#endif

instance (MonadBaseControl b m stM, Monoid w) => MonadBaseControl b (Strict.WriterT w m) (Compose stM ((,) w)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM, Monoid w) => MonadBaseControl b (WriterT w m) (Compose stM ((,) w)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM, Monoid w) => MonadBaseControl b (Strict.RWST r w s m) (Compose stM ((,,) w s)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

instance (MonadBaseControl b m stM, Monoid w) => MonadBaseControl b (RWST r w s m) (Compose stM ((,,) w s)) where
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM
  {-# INLINABLE liftBaseWith #-}
  {-# INLINABLE restoreM #-}

#undef BODY
#undef TRANS
#undef TRANS_CTX

--------------------------------------------------------------------------------
-- * Utility functions
--------------------------------------------------------------------------------

-- | An often used composition: @control f = 'liftBaseWith' f >>= 'restoreM'@
control :: MonadBaseControl b m stM => (RunInBase m b stM -> b (stM a)) -> m a
control f = liftBaseWith f >>= restoreM
{-# INLINABLE control #-}

-- | Lift a computation and restore the monadic state immediately:
-- @controlT f = 'liftWith' f >>= 'restoreT' . return@.
controlT :: (MonadTransControl t stT, Monad (t m), Monad m)
         => (Run t stT -> m (stT a)) -> t m a
controlT f = liftWith f >>= restoreT . return
{-# INLINABLE controlT #-}

-- | Embed a transformer function as an function in the base monad returning a
-- mutated transformer state.
embed :: MonadBaseControl b m stM => (a -> m c) -> m (a -> b (stM c))
embed f = liftBaseWith $ \runInBase -> return (runInBase . f)
{-# INLINABLE embed #-}

-- | Performs the same function as 'embed', but discards transformer state
-- from the embedded function.
embed_ :: MonadBaseControl b m stM => (a -> m ()) -> m (a -> b ())
embed_ f = liftBaseWith $ \runInBase -> return (void . runInBase . f)
{-# INLINABLE embed_ #-}

-- | Capture the current state of a transformer
captureT :: (MonadTransControl t stT, Monad (t m), Monad m) => t m (stT ())
captureT = liftWith $ \runInM -> runInM (return ())
{-# INLINABLE captureT #-}

-- | Capture the current state above the base monad
captureM :: MonadBaseControl b m stM => m (stM ())
captureM = liftBaseWith $ \runInBase -> runInBase (return ())
{-# INLINABLE captureM #-}

-- | @liftBaseOp@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b c) -> b c)@
--
-- to:
--
-- @('MonadBaseControl' b m => (a -> m c) -> m c)@
--
-- For example:
--
-- @liftBaseOp alloca :: 'MonadBaseControl' 'IO' m => (Ptr a -> m c) -> m c@
liftBaseOp :: MonadBaseControl b m stM
           => ((a -> b (stM c)) -> b (stM d))
           -> ((a ->      m c)  ->      m d)
liftBaseOp f = \g -> control $ \runInBase -> f $ runInBase . g
{-# INLINABLE liftBaseOp #-}

-- | @liftBaseOp_@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b a -> b a)@
--
-- to:
--
-- @('MonadBaseControl' b m => m a -> m a)@
--
-- For example:
--
-- @liftBaseOp_ mask_ :: 'MonadBaseControl' 'IO' m => m a -> m a@
liftBaseOp_ :: MonadBaseControl b m stM
            => (b (stM a) -> b (stM c))
            -> (     m a  ->      m c)
liftBaseOp_ f = \m -> control $ \runInBase -> f $ runInBase m
{-# INLINABLE liftBaseOp_ #-}

-- | @liftBaseDiscard@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @(b () -> b a)@
--
-- to:
--
-- @('MonadBaseControl' b m => m () -> m a)@
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard forkIO :: 'MonadBaseControl' 'IO' m => m () -> m ThreadId@
liftBaseDiscard :: MonadBaseControl b m stM => (b () -> b a) -> (m () -> m a)
liftBaseDiscard f = \m -> liftBaseWith $ \runInBase -> f $ void $ runInBase m
{-# INLINABLE liftBaseDiscard #-}

-- | @liftBaseOpDiscard@ is a particular application of 'liftBaseWith' that allows
-- lifting control operations of type:
--
-- @((a -> b ()) -> b c)@
--
-- to:
--
-- @('MonadBaseControl' b m => (a -> m ()) -> m c)@
--
-- Note that, while the argument computation @m ()@ has access to the captured
-- state, all its side-effects in @m@ are discarded. It is run only for its
-- side-effects in the base monad @b@.
--
-- For example:
--
-- @liftBaseDiscard (runServer addr port) :: 'MonadBaseControl' 'IO' m => m () -> m ()@
liftBaseOpDiscard :: MonadBaseControl b m stM
                  => ((a -> b ()) -> b c)
                  ->  (a -> m ()) -> m c
liftBaseOpDiscard f g = liftBaseWith $ \runInBase -> f $ void . runInBase . g
{-# INLINABLE liftBaseOpDiscard #-}

-- | Transform an action in @t m@ using a transformer that operates on the underlying monad @m@
liftThrough
    :: (MonadTransControl t stT, Monad (t m), Monad m)
    => (m (stT a) -> m (stT b)) -- ^
    -> t m a -> t m b
liftThrough f t = do
  st <- liftWith $ \run -> do
    f $ run t
  restoreT $ return st
