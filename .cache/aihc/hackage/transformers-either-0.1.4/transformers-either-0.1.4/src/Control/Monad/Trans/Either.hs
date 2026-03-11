{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Control.Monad.Trans.Either
-- Copyright   :  (C) 2017 Tim McGilchrist
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  timmcgil@gmail.com
-- Stability   :  experimental
-- Portability :  portable
--
-- This monad transformer extends "Control.Monad.Trans.Except" with a more
-- familar "Either" naming.
-----------------------------------------------------------------------------
module Control.Monad.Trans.Either (
  -- * Control.Monad.Trans.Either
    EitherT
  , pattern EitherT
  , newEitherT
  , runEitherT
  , eitherT
  , left
  , right
  , mapEitherT
  , hoistEither
  , bimapEitherT

  -- * Extensions
  , firstEitherT
  , secondEitherT
  , hoistMaybe
  , hoistEitherT
  , handleIOEitherT
  , handleEitherT
  , handlesEitherT
  , handleLeftT

  , catchIOEitherT
  , catchEitherT
  , catchesEitherT
  , catchLeftT

  , bracketEitherT
  , bracketExceptionT

  , hushM
  , onLeft
  , onNothing
  ) where

import           Control.Exception (Exception, IOException, SomeException)
import qualified Control.Exception as Exception
import           Control.Monad (Monad (..), (=<<))
import           Control.Monad.Catch (Handler (..), MonadCatch, MonadMask, catchAll, mask, throwM)
import qualified Control.Monad.Catch as Catch
import           Control.Monad.IO.Class (MonadIO, liftIO)
import           Control.Monad.Trans.Class (lift)
import           Control.Monad.Trans.Except (ExceptT (..))

import           Data.Either (Either (..), either)
import           Data.Foldable (Foldable, foldr)
import           Data.Function (const, flip, id, ($), (.))
import           Data.Functor (Functor (..))
import           Data.Maybe (Maybe (..), maybe)

import           System.IO (IO)

------------------------------------------------------------------------
-- Control.Monad.Trans.Either

-- | Type alias for "ExceptT"
--
type EitherT = ExceptT

pattern EitherT :: m (Either x a) -> ExceptT x m a
pattern EitherT m = ExceptT m

-- | Extractor for computations in the either monad.
-- (The inverse of 'newEitherT').
runEitherT :: EitherT x m a -> m (Either x a)
runEitherT (ExceptT m) = m
{-# INLINE runEitherT #-}

-- | Constructor for computations in the either monad.
-- (The inverse of 'runEitherT').
newEitherT :: m (Either x a) -> EitherT x m a
newEitherT =
  ExceptT
{-# INLINE newEitherT #-}

-- | Map over both arguments at the same time.
--
-- Specialised version of 'bimap' for 'EitherT'.
eitherT :: Monad m => (x -> m b) -> (a -> m b) -> EitherT x m a -> m b
eitherT f g m =
  either f g =<< runEitherT m
{-# INLINE eitherT #-}

-- | Constructor for left computations.
left :: Monad m => x -> EitherT x m a
left =
  EitherT . return . Left
{-# INLINE left #-}

-- | Constructor for right computations.
right :: Monad m => a -> EitherT x m a
right =
  return
{-# INLINE right #-}

-- |
mapEitherT :: (m (Either x a) -> n (Either y b)) -> EitherT x m a -> EitherT y n b
mapEitherT f =
  EitherT . f . runEitherT
{-# INLINE mapEitherT #-}

-- | Hoist an 'Either' into an 'EitherT' m.
hoistEither :: Monad m => Either x a -> EitherT x m a
hoistEither =
  EitherT . return
{-# INLINE hoistEither #-}

-- | Map the unwrapped computation using the given function.
bimapEitherT :: Functor m => (x -> y) -> (a -> b) -> EitherT x m a -> EitherT y m b
bimapEitherT f g =
  let
    h (Left  e) = Left  (f e)
    h (Right a) = Right (g a)
  in
    mapEitherT (fmap h)
{-# INLINE bimapEitherT #-}

-- | Map the 'Left' unwrapped computation using the given function.
firstEitherT :: Functor m => (x -> y) -> EitherT x m a -> EitherT y m a
firstEitherT f =
  bimapEitherT f id
{-# INLINE firstEitherT #-}

-- | Map the 'Right' unwrapped computation using the given function.
secondEitherT :: Functor m => (a -> b) -> EitherT x m a -> EitherT x m b
secondEitherT =
  bimapEitherT id
{-# INLINE secondEitherT #-}

-- | Hoist 'Maybe' a into 'Right' a.
hoistMaybe :: Monad m => x -> Maybe a -> EitherT x m a
hoistMaybe x =
  maybe (left x) return
{-# INLINE hoistMaybe #-}

-- | Hoist 'Either' m into an 'Either' n.
hoistEitherT :: (forall b. m b -> n b) -> EitherT x m a -> EitherT x n a
hoistEitherT f =
  EitherT . f . runEitherT
{-# INLINE hoistEitherT #-}

------------------------------------------------------------------------
-- Error handling

-- | Try an 'IO' action inside an 'EitherT'. If the 'IO' action throws an
-- 'IOException', catch it and wrap it with the provided handler to convert it
-- to the error type of the 'EitherT' transformer. Exceptions other than
-- 'IOException' will escape the EitherT transformer.
--
-- Note: 'IOError' is a type synonym for 'IOException'.
handleIOEitherT :: MonadIO m => (IOException -> x) -> IO a -> EitherT x m a
handleIOEitherT wrap =
  firstEitherT wrap . newEitherT . liftIO . Exception.try
{-# INLINE handleIOEitherT #-}

-- | Flipped 'handleIOEitherT'.
catchIOEitherT :: MonadIO m => IO a -> (IOException -> x) -> EitherT x m a
catchIOEitherT = flip handleIOEitherT
{-# INLINE catchIOEitherT #-}

-- | Try any monad action and catch the specified exception, wrapping it to
-- convert it to the error type of the 'EitherT' transformer. Exceptions other
-- that the specified exception type will escape the 'EitherT' transformer.
--
-- *Warning*: This function should be used with caution!
-- In particular, it is bad practice to catch 'SomeException' because that
-- includes asynchronous exceptions like stack/heap overflow, thread killed and
-- user interrupt. Trying to handle 'StackOverflow', 'HeapOverflow' and
-- 'ThreadKilled' exceptions could cause your program to crash or behave in
-- unexpected ways.
handleEitherT :: (MonadCatch m, Exception e) => (e -> x) -> m a -> EitherT x m a
handleEitherT wrap =
  firstEitherT wrap . newEitherT . Catch.try
{-# INLINE handleEitherT #-}

-- | Flipped 'handleEitherT'.
catchEitherT :: (MonadCatch m, Exception e) => m a -> (e -> x) -> EitherT x m a
catchEitherT = flip handleEitherT
{-# INLINE catchEitherT #-}

-- | Try a monad action and catch any of the exceptions caught by the provided
-- handlers. The handler for each exception type needs to wrap it to convert it
-- to the error type of the 'EitherT' transformer. Exceptions not explicitly
-- handled by the provided handlers will escape the 'EitherT' transformer.
handlesEitherT :: (Foldable f, MonadCatch m) => f (Handler m x) -> m a -> EitherT x m a
handlesEitherT wrappers action =
  newEitherT (fmap Right action `Catch.catch` fmap (fmap Left) handler)
  where
    handler e =
      let probe (Handler h) xs =
            maybe xs h (Exception.fromException e)
      in
        foldr probe (Catch.throwM e) wrappers

-- | Flipped 'handlesEitherT'.
catchesEitherT  :: (Foldable f, MonadCatch m) => m a -> f (Handler m x) -> EitherT x m a
catchesEitherT = flip handlesEitherT
{-# INLINE catchesEitherT #-}

-- | Handle an error. Equivalent to 'handleError' in mtl package.
handleLeftT :: Monad m => (e -> EitherT e m a) -> EitherT e m a -> EitherT e m a
handleLeftT handler thing = do
  r <- lift $ runEitherT thing
  case r of
    Left e ->
      handler e
    Right a ->
      return a
{-# INLINE handleLeftT #-}

-- | Flipped 'handleLeftT'.
catchLeftT  :: Monad m => EitherT e m a -> (e -> EitherT e m a) -> EitherT e m a
catchLeftT = flip handleLeftT
{-# INLINE catchLeftT #-}

-- | Acquire a resource in 'EitherT' and then perform an action with
-- it, cleaning up afterwards regardless of 'left'.
--
-- This function does not clean up in the event of an exception.
-- Prefer 'bracketExceptionT' in any impure setting.
bracketEitherT :: Monad m => EitherT e m a -> (a -> EitherT e m b) -> (a -> EitherT e m c) -> EitherT e m c
bracketEitherT before after thing = do
    a <- before
    r <- (\err -> after a >> left err) `handleLeftT` thing a
    -- If handleLeftT already triggered, then `after` already ran *and* we are
    -- in a Left state, so `after` will not run again here.
    _ <- after a
    return r
{-# INLINE bracketEitherT #-}

-- | Acquire a resource in EitherT and then perform an action with it,
-- cleaning up afterwards regardless of 'left' or exception.
--
-- Like 'bracketEitherT', but the cleanup is called even when the bracketed
-- function throws an exception. Exceptions in the bracketed function are caught
-- to allow the cleanup to run and then rethrown.
bracketExceptionT ::
     MonadMask m
  => EitherT e m a
  -> (a -> EitherT e m c)
  -> (a -> EitherT e m b)
  -> EitherT e m b
bracketExceptionT acquire release run =
  EitherT $ bracketF
    (runEitherT acquire)
    (\r -> case r of
      Left _ ->
        -- Acquire failed, we have nothing to release
        return . Right $ ()
      Right r' ->
        -- Acquire succeeded, we need to try and release
        runEitherT (release r') >>= \x -> return $ case x of
          Left err -> Left (Left err)
          Right _ -> Right ())
    (\r -> case r of
      Left err ->
        -- Acquire failed, we have nothing to run
        return . Left $ err
      Right r' ->
        -- Acquire succeeded, we can do some work
        runEitherT (run r'))
{-# INLINE bracketExceptionT #-}

-- This is for internal use only. The 'bracketF' function catches all exceptions
-- so the cleanup function can be called and then rethrow the exception.
data BracketResult a =
    BracketOk a
  | BracketFailedFinalizerOk SomeException
  | BracketFailedFinalizerError a

-- Bracket where you care about the output of the finalizer. If the finalizer fails
-- with a value level fail, it will return the result of the finalizer.
-- Finalizer:
--  - Left indicates a value level fail.
--  - Right indicates that the finalizer has a value level success, and its results can be ignored.
--
bracketF :: MonadMask m => m a -> (a -> m (Either b c)) -> (a -> m b) -> m b
bracketF a f g =
  mask $ \restore -> do
    a' <- a
    x <- restore (BracketOk `fmap` g a') `catchAll`
           (\ex -> either BracketFailedFinalizerError (const $ BracketFailedFinalizerOk ex) `fmap` f a')
    case x of
      BracketFailedFinalizerOk ex ->
        throwM ex
      BracketFailedFinalizerError b ->
        return b
      BracketOk b -> do
        z <- f a'
        return $ either id (const b) z
{-# INLINE bracketF #-}

-- | Convert an Either to a Maybe and execute the supplied handler
-- in the Left case.
hushM :: Monad m => Either e a -> (e -> m ()) -> m (Maybe a)
hushM r f = case r of
  Right a -> return (Just a)
  Left e -> f e >> return Nothing
{-# INLINE hushM #-}

-- | Handle the Left constructor in returned Either.
onLeft :: forall e x m a. Monad m => (e -> ExceptT x m a) -> EitherT x m (Either e a) -> EitherT x m a
onLeft h f = f >>= either h return
{-# INLINE onLeft #-}

-- | Handle the Nothing constructor in returned Maybe.
onNothing :: forall x m a. Monad m => EitherT x m a -> EitherT x m (Maybe a) -> EitherT x m a
onNothing h f = f >>= maybe h return
{-# INLINE onNothing #-}
