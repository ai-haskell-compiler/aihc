{-# LANGUAGE
    FlexibleContexts
  , Rank2Types
  #-}

-- |
-- Module      : Network.Wai.Trans
-- Copyright   : (c) 2015, 2016, 2017, 2018 Athan Clark
-- License     : BSD-style
-- Maintainer  : athan.clark@gmail.com
-- Stability   : experimental
-- Portability : GHC
--
-- Simple utilities for embedding a monad transformer stack in an 'Network.Wai.Application'
-- or 'Network.Wai.Middleware' - with 'MiddlewareT', your transformer stack is shared
-- across all attached middlewares until run. You can also lift existing 'Network.Wai.Middleware'
-- to 'MiddlewareT', given some extraction function.


module Network.Wai.Trans where

import Network.Wai (Application, Middleware, Request, Response, ResponseReceived)

import Data.Singleton.Class (Extractable (runSingleton))
import Control.Monad.Catch (Exception, MonadCatch (catch))
import Control.Monad.Trans.Control.Aligned (MonadBaseControl (liftBaseWith))


-- | Isomorphic to @Kleisli (ContT ResponseReceived m) Request Response@
type ApplicationT m = Request -> (Response -> m ResponseReceived) -> m ResponseReceived
type MiddlewareT m = ApplicationT m -> ApplicationT m

-- * Lift and Run

liftApplication :: MonadBaseControl IO m stM
                => Extractable stM
                => Application -- ^ To lift
                -> ApplicationT m
liftApplication app req resp = liftBaseWith (\runInBase -> app req (\r -> runSingleton <$> runInBase (resp r)))

liftMiddleware :: MonadBaseControl IO m stM
               => Extractable stM
               => Middleware -- ^ To lift
               -> MiddlewareT m
liftMiddleware mid app req respond = do
  app' <- runApplicationT app
  liftBaseWith (\runInBase -> mid app' req (fmap runSingleton . runInBase . respond))

runApplicationT :: MonadBaseControl IO m stM
                => Extractable stM
                => ApplicationT m -- ^ To run
                -> m Application
runApplicationT app = liftBaseWith $ \runInBase ->
  pure $ \req respond -> fmap runSingleton $ runInBase $ app req (\x -> liftBaseWith (\_ -> respond x))

runMiddlewareT :: MonadBaseControl IO m stM
               => Extractable stM
               => MiddlewareT m -- ^ To run
               -> m Middleware
runMiddlewareT mid = liftBaseWith $ \runInBase ->
  pure $ \app req respond -> do
    app' <- fmap runSingleton $ runInBase $ runApplicationT (mid (liftApplication app))
    app' req respond

-- * Monad Morphisms

hoistApplicationT :: Monad m
                  => Monad n
                  => (forall a. m a -> n a) -- ^ To
                  -> (forall a. n a -> m a) -- ^ From
                  -> ApplicationT m
                  -> ApplicationT n
hoistApplicationT to from app req resp =
  to $ app req (from . resp)

hoistMiddlewareT :: Monad m
                 => Monad n
                 => (forall a. m a -> n a) -- ^ To
                 -> (forall a. n a -> m a) -- ^ From
                 -> MiddlewareT m
                 -> MiddlewareT n
hoistMiddlewareT to from mid =
  hoistApplicationT to from . mid . hoistApplicationT from to

-- * Exception Catching

catchApplicationT :: MonadCatch m
                  => Exception e
                  => ApplicationT m
                  -> (e -> ApplicationT m) -- ^ Handler
                  -> ApplicationT m
catchApplicationT x f req respond =
  x req respond `catch` (\e -> f e req respond)

catchMiddlewareT :: MonadCatch m
                 => Exception e
                 => MiddlewareT m
                 -> (e -> MiddlewareT m) -- ^ Handler
                 -> MiddlewareT m
catchMiddlewareT x f app =
  x app `catchApplicationT` (`f` app)
