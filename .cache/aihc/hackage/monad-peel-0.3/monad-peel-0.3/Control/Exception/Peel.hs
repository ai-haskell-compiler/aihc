{-# LANGUAGE ExistentialQuantification #-}

{- |
Module      :  Control.Exception.Peel
Copyright   :  © Anders Kaseorg, 2010
License     :  BSD-style

Maintainer  :  Anders Kaseorg <andersk@mit.edu>
Stability   :  experimental
Portability :  non-portable (extended exceptions)

This is a wrapped version of Control.Exception with types generalized
from 'IO' to all monads in 'MonadPeelIO'.
-}

module Control.Exception.Peel (
  module Control.Exception.Extensible,
  throwIO, ioError,
  catch, catches, Handler(..), catchJust,
  handle, handleJust,
  try, tryJust,
  evaluate,
  bracket, bracket_, bracketOnError,
  finally, onException,
  ) where

import Prelude hiding (catch, ioError)
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.IO.Peel
import Control.Exception.Extensible hiding (
  throwIO, ioError,
  catch, catches, Handler(..), catchJust,
  handle, handleJust,
  try, tryJust,
  evaluate,
  bracket, bracket_, bracketOnError,
  finally, onException,
  )
import qualified Control.Exception.Extensible as E

-- |Generalized version of 'E.throwIO'.
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . E.throwIO

-- |Generalized version of 'E.ioError'.
ioError :: MonadIO m => IOError -> m a
ioError = liftIO . E.ioError

-- |Generalized version of 'E.catch'.
catch :: (MonadPeelIO m, Exception e) =>
         m a -- ^ The computation to run
         -> (e -> m a) -- ^ Handler to invoke if an exception is raised
         -> m a
catch a handler = do
  k <- peelIO
  join $ liftIO $ E.catch (k a) (\e -> k $ handler e)

-- |Generalized version of 'E.catchJust'.
catchJust :: (MonadPeelIO m, Exception e) =>
             (e -> Maybe b) -- ^ Predicate to select exceptions
             -> m a -- ^ Computation to run
             -> (b -> m a) -- ^ Handler
             -> m a
catchJust p a handler = do
  k <- peelIO
  join $ liftIO $ E.catchJust p (k a) (\e -> k (handler e))

-- |Generalized version of 'E.catches'.
catches :: MonadPeelIO m => m a -> [Handler m a] -> m a
catches a handlers = do
  k <- peelIO
  join $ liftIO $ E.catches (k a) [E.Handler $ \e -> k $ handler e |
                                   Handler handler <- handlers]

-- |Generalized version of 'E.Handler'.
data Handler m a = forall e. Exception e => Handler (e -> m a)

-- |Generalized version of 'E.handle'.
handle :: (MonadPeelIO m, Exception e) => (e -> m a) -> m a -> m a
handle handler a = do
  k <- peelIO
  join $ liftIO $ E.handle (\e -> k (handler e)) (k a)

-- |Generalized version of 'E.handleJust'.
handleJust :: (MonadPeelIO m, Exception e) =>
             (e -> Maybe b) -> (b -> m a) -> m a -> m a
handleJust p handler a = do
  k <- peelIO
  join $ liftIO $ E.handleJust p (\e -> k (handler e)) (k a)

sequenceEither :: Monad m => Either e (m a) -> m (Either e a)
sequenceEither (Left e) = return $ Left e
sequenceEither (Right m) = liftM Right m

-- |Generalized version of 'E.try'.
try :: (MonadPeelIO m, Exception e) => m a -> m (Either e a)
try = liftIOOp_ (liftM sequenceEither . E.try)

-- |Generalized version of 'E.tryJust'.
tryJust :: (MonadPeelIO m, Exception e) =>
           (e -> Maybe b) -> m a -> m (Either b a)
tryJust p = liftIOOp_ (liftM sequenceEither . E.tryJust p)

-- |Generalized version of 'E.evaluate'.
evaluate :: MonadIO m => a -> m a
evaluate = liftIO . E.evaluate

-- |Generalized version of 'E.bracket'.  Note, any monadic side
-- effects in @m@ of the \"release\" computation will be discarded; it
-- is run only for its side effects in @IO@.
bracket :: MonadPeelIO m =>
           m a -- ^ computation to run first (\"acquire resource\")
           -> (a -> m b) -- ^ computation to run last (\"release resource\")
           -> (a -> m c) -- ^ computation to run in-between
           -> m c
bracket before after thing = do
  k <- peelIO
  k' <- peelIO
  k'' <- peelIO
  join $ liftIO $
    E.bracket (k before) (\x -> k' $ x >>= after) (\x -> k'' $ x >>= thing)

-- |Generalized version of 'E.bracket_'.  Note, any monadic side
-- effects in @m@ of /both/ the \"acquire\" and \"release\"
-- computations will be discarded.  To keep the monadic side effects
-- of the \"acquire\" computation, use 'bracket' with constant
-- functions instead.
bracket_ :: MonadPeelIO m => m a -> m b -> m c -> m c
bracket_ before after thing = do
  k <- peelIO
  k' <- peelIO
  k'' <- peelIO
  join $ liftIO $ E.bracket_ (k before) (k' after) (k'' thing)

-- |Generalized version of 'E.bracketOnError'.
bracketOnError :: MonadPeelIO m =>
                  m a -- ^ computation to run first (\"acquire resource\")
                  -> (a -> m b) -- ^ computation to run last (\"release resource\")
                  -> (a -> m c) -- ^ computation to run in-between
                  -> m c
bracketOnError before after thing = do
  k <- peelIO
  k' <- peelIO
  k'' <- peelIO
  join $ liftIO $
    E.bracket (k before) (\x -> k' $ x >>= after) (\x -> k'' $ x >>= thing)

-- |Generalized version of 'E.finally'.  Note, any monadic side
-- effects in @m@ of the \"afterward\" computation will be discarded.
finally :: MonadPeelIO m =>
           m a -- ^ computation to run first
           -> m b -- ^ computation to run afterward (even if an exception was raised)
           -> m a
finally a sequel = do
  k <- peelIO
  k' <- peelIO
  join $ liftIO $ E.finally (k a) (k' sequel)

-- |Generalized version of 'E.onException'.
onException :: MonadPeelIO m => m a -> m b -> m a
onException m what = do
  k <- peelIO
  k' <- peelIO
  join $ liftIO $ E.onException (k m) (k' what)
