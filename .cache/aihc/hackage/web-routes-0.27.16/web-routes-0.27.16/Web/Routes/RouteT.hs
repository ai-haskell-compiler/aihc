{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Web.Route.RouteT
-- Copyright   :  (c) 2010 Jeremy Shaw
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  partners@seereason.com
-- Stability   :  experimental
-- Portability :  portable
--
-- Declaration of the 'RouteT' monad transformer
-----------------------------------------------------------------------------
module Web.Routes.RouteT where

import Control.Applicative (Applicative((<*>), pure), Alternative((<|>), empty))
import Control.Monad (MonadPlus(mzero, mplus))
import Control.Monad.Catch (MonadCatch(catch), MonadThrow(throwM))
import Control.Monad.Cont(MonadCont(callCC))
#if MIN_VERSION_mtl(2,3,0)
import Control.Monad.Except (MonadError(throwError, catchError))
#else
import Control.Monad.Error (MonadError(throwError, catchError))
#endif
#if !MIN_VERSION_base(4,13,0)
-- Control.Monad.Fail import is redundant since GHC 8.8.1
import Prelude hiding (fail)
import Control.Monad.Fail (MonadFail(..))
#endif
import Control.Monad.Fix (MonadFix(mfix))
import Control.Monad.Reader(MonadReader(ask,local))
import Control.Monad.RWS (MonadRWS)
import Control.Monad.State(MonadState(get,put))
import Control.Monad.Trans (MonadTrans(lift), MonadIO(liftIO))
import Control.Monad.Writer(MonadWriter(listen, tell, pass))
import Data.Text (Text)


-- * RouteT Monad Transformer

-- |monad transformer for generating URLs
newtype RouteT url m a = RouteT { unRouteT :: (url -> [(Text, Maybe Text)] -> Text) -> m a }

class (Monad m) => MonadRoute m where
    type URL m
    askRouteFn :: m (URL m -> [(Text, Maybe Text)] -> Text)

instance MonadCatch m => MonadCatch (RouteT url m) where
    catch action handler =
        RouteT $ \ fn -> catch (action' fn) (\ e -> handler' e fn)
        where
          action' = unRouteT action
          handler' e = unRouteT (handler e)

instance MonadThrow m => MonadThrow (RouteT url m) where
    throwM = throwM'
        where
          throwM' e = RouteT $ \ _fn -> throwM e

-- | convert a 'RouteT' based route handler to a handler that can be used with the 'Site' type
--
-- NOTE: this function used to be the same as 'unRouteT'. If you want the old behavior, just call 'unRouteT'.
runRouteT :: (url -> RouteT url m a)
          -> ((url -> [(Text, Maybe Text)] -> Text) -> url -> m a)
runRouteT r = \f u -> (unRouteT (r u)) f

-- | Transform the computation inside a @RouteT@.
mapRouteT :: (m a -> n b) -> RouteT url m a -> RouteT url n b
mapRouteT f (RouteT m) = RouteT $ f . m

-- | Execute a computation in a modified environment
withRouteT :: ((url' -> [(Text, Maybe Text)] -> Text) -> (url -> [(Text, Maybe Text)] -> Text)) -> RouteT url m a -> RouteT url' m a
withRouteT f (RouteT m) = RouteT $ m . f

liftRouteT :: m a -> RouteT url m a
liftRouteT m = RouteT (const m)

askRouteT :: (Monad m) => RouteT url m (url -> [(Text, Maybe Text)] -> Text)
askRouteT = RouteT return

instance (Functor m) => Functor (RouteT url m) where
  fmap f = mapRouteT (fmap f)

instance (Applicative m) => Applicative (RouteT url m) where
  pure = liftRouteT . pure
  f <*> v = RouteT $ \ url -> unRouteT f url <*> unRouteT v url

instance (Alternative m) => Alternative (RouteT url m) where
    empty   = liftRouteT empty
    m <|> n = RouteT $ \ url -> unRouteT m url <|> unRouteT n url

instance (Monad m) => Monad (RouteT url m) where
    return   = liftRouteT . return
    m >>= k  = RouteT $ \ url -> do
        a <- unRouteT m url
        unRouteT (k a) url

instance (MonadFail m) => MonadFail (RouteT url m) where
    fail msg = liftRouteT (fail msg)

instance (MonadPlus m, Monad (RouteT url m)) => MonadPlus (RouteT url m) where
    mzero       = liftRouteT mzero
    m `mplus` n = RouteT $ \ url -> unRouteT m url `mplus` unRouteT n url

instance (MonadCont m) => MonadCont (RouteT url m) where
    callCC f = RouteT $ \url ->
        callCC $ \c ->
        unRouteT (f (\a -> RouteT $ \_ -> c a)) url

instance (MonadError e m) => MonadError e (RouteT url m) where
  throwError = liftRouteT . throwError
  catchError action handler = RouteT $ \f -> catchError (unRouteT action f) (\e -> unRouteT (handler e) f)

instance (MonadFix m) => MonadFix (RouteT url m) where
    mfix f = RouteT $ \ url -> mfix $ \ a -> unRouteT (f a) url

instance (MonadIO m) => MonadIO (RouteT url m) where
  liftIO = lift . liftIO

instance (MonadReader r m) => MonadReader r (RouteT url m) where
  ask   = liftRouteT ask
  local f = mapRouteT (local f)

instance (MonadRWS r w s m) => MonadRWS r w s (RouteT url m)

instance (MonadState s m) => MonadState s (RouteT url m) where
  get = liftRouteT get
  put s = liftRouteT $ put s

instance MonadTrans (RouteT url) where
  lift = liftRouteT

instance (MonadWriter w m) => MonadWriter w (RouteT url m) where
  tell   w = liftRouteT $ tell w
  listen m = mapRouteT listen m
  pass   m = mapRouteT pass   m

instance (Monad m) => MonadRoute (RouteT url m) where
    type URL (RouteT url m) = url
    askRouteFn = askRouteT

showURL :: (MonadRoute m) => URL m -> m Text
showURL url =
    do showFn <- askRouteFn
       return (showFn url [])

showURLParams  :: (MonadRoute m) => URL m -> [(Text, Maybe Text)] -> m Text
showURLParams url params =
    do showFn <- askRouteFn
       return (showFn url params)

nestURL :: (url1 -> url2) -> RouteT url1 m a -> RouteT url2 m a
nestURL transform (RouteT r) =
    do RouteT $ \showFn ->
           r (\url params -> showFn (transform url) params)
