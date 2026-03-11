{- 
    Copyright 2010 Mario Blazevic

    This file is part of the Streaming Component Combinators (SCC) project.

    The SCC project is free software: you can redistribute it and/or modify it under the terms of the GNU General Public
    License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later
    version.

    SCC is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty
    of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

    You should have received a copy of the GNU General Public License along with SCC.  If not, see
    <http://www.gnu.org/licenses/>.
-}

-- | This module defines classes of monads that can perform multiple computations in parallel and, more importantly,
-- combine the results of those parallel computations.
-- 
-- There are two classes exported by this module, 'MonadParallel' and 'MonadFork'. The former is more generic, but the
-- latter is easier to use: when invoking any expensive computation that could be performed in parallel, simply wrap the
-- call in 'forkExec'. The function immediately returns a handle to the running computation. The handle can be used to
-- obtain the result of the computation when needed:
--
-- @
--   do child <- forkExec expensive
--      otherStuff
--      result <- child
-- @
--
-- In this example, the computations /expensive/ and /otherStuff/ would be performed in parallel. When using the
-- 'MonadParallel' class, both parallel computations must be specified at once:
--
-- @
--   bindM2 (\\ childResult otherResult -> ...) expensive otherStuff
-- @
--
-- In either case, for best results the costs of the two computations should be roughly equal.
--
-- Any monad that is an instance of the 'MonadFork' class is also an instance of the 'MonadParallel' class, and the
-- following law should hold:
-- 
-- @ bindM2 f ma mb = do {a' <- forkExec ma; b <- mb; a <- a'; f a b} @ 
--
-- When operating with monads free of side-effects, such as 'Identity' or 'Maybe', 'forkExec' is equivalent to 'return'
-- and 'bindM2' is equivalent to @ \\ f ma mb -> do {a <- ma; b <- mb; f a b} @ &#x2014; the only difference is in the
-- resource utilisation. With the 'IO' monad, on the other hand, there may be visible difference in the results because
-- the side effects of /ma/ and /mb/ may be arbitrarily reordered.

{-# LANGUAGE ScopedTypeVariables #-}

module Control.Monad.Parallel
   (
    -- * Classes
    MonadParallel(..), MonadFork(..),
    bindM3,
    -- * Control.Monad equivalents
    ap, forM, forM_, liftM2, liftM3, mapM, mapM_, replicateM, replicateM_, sequence, sequence_
   )
where

import Prelude ()
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar, readMVar)
import Control.Exception (SomeException, throwIO, mask, try)
import Control.Monad (Monad, (>>=), return, liftM)
import Control.Monad.Trans.Identity (IdentityT(IdentityT, runIdentityT))
import Control.Monad.Trans.Maybe (MaybeT(MaybeT, runMaybeT))
import Control.Monad.Trans.Except (ExceptT(ExceptT), runExceptT)
import Control.Monad.Trans.Reader (ReaderT(ReaderT, runReaderT))
import Control.Parallel (par, pseq)
import Data.Either (Either(..), either)
import Data.Function (($), (.), const)
import Data.Functor.Identity (Identity)
import Data.Int (Int)
import Data.List ((++), foldr, map, replicate)
import Data.Maybe (Maybe(Just, Nothing))
import System.IO (IO)

-- | Class of monads that can perform two computations in parallel and bind their results together.
class Monad m => MonadParallel m where
   -- | Perform two monadic computations in parallel; when they are both finished, pass the results to the function.
   -- Apart from the possible ordering of side effects, this function is equivalent to
   -- @\\f ma mb-> do {a <- ma; b <- mb; f a b}@
   bindM2 :: (a -> b -> m c) -> m a -> m b -> m c
   bindM2 f ma mb = let ma' = ma >>= return
                        mb' = mb >>= return
                    in ma' `par` (mb' `pseq` do {a <- ma'; b <- mb'; f a b})

-- | Class of monads that can fork a parallel computation.
class MonadParallel m => MonadFork m where
   -- | Fork a child monadic computation to be performed in parallel with the current one.
   forkExec :: m a -> m (m a)
   forkExec e = let result = e >>= return
                in result `par` (return result)

-- | Perform three monadic computations in parallel; when they are all finished, pass their results to the function.
bindM3 :: MonadParallel m => (a -> b -> c -> m d) -> m a -> m b -> m c -> m d
bindM3 f ma mb mc = bindM2 (\f' c-> f' c) (liftM2 f ma mb) mc

-- | Like 'Control.Monad.liftM2', but evaluating its two monadic arguments in parallel.
liftM2 :: MonadParallel m => (a -> b -> c) -> m a -> m b -> m c
liftM2 f m1 m2 = bindM2 (\a b-> return (f a b)) m1 m2

-- | Like 'Control.Monad.liftM3', but evaluating its three monadic arguments in parallel.
liftM3  :: (MonadParallel m) => (a1 -> a2 -> a3 -> r) -> m a1 -> m a2 -> m a3 -> m r
liftM3 f m1 m2 m3 = bindM3 (\a b c-> return (f a b c)) m1 m2 m3

-- | Like 'Control.Monad.ap', but evaluating the function and its argument in parallel.
ap :: MonadParallel m => m (a -> b) -> m a -> m b
ap mf ma = bindM2 (\f a-> return (f a)) mf ma

-- | Like 'Control.Monad.sequence', but executing the actions in parallel.
sequence :: MonadParallel m => [m a] -> m [a]
sequence ms = foldr k (return []) ms where
   k m m' = liftM2 (:) m m'

-- | Like 'Control.Monad.sequence_', but executing the actions in parallel.
sequence_ :: MonadParallel m => [m a] -> m () 
sequence_ ms = foldr (liftM2 (\ _ _ -> ())) (return ()) ms

-- | Like 'Control.Monad.mapM', but applying the function to the individual list items in parallel.
mapM :: MonadParallel m => (a -> m b) -> [a] -> m [b]
mapM f list = sequence (map f list)

-- | Like 'Control.Monad.mapM_', but applying the function to the individual list items in parallel.
mapM_ :: MonadParallel m => (a -> m b) -> [a] -> m ()
mapM_ f list = sequence_ (map f list)

-- | Like 'Control.Monad.forM', but applying the function to the individual list items in parallel.
forM :: MonadParallel m => [a] -> (a -> m b) -> m [b]
forM list f = sequence (map f list)

-- | Like 'Control.Monad.forM_', but applying the function to the individual list items in parallel.
forM_ :: MonadParallel m => [a] -> (a -> m b) -> m ()
forM_ list f = sequence_ (map f list)

-- | Like 'Control.Monad.replicateM', but executing the action multiple times in parallel.
replicateM :: MonadParallel m => Int -> m a -> m [a]
replicateM n action = sequence (replicate n action)

-- | Like 'Control.Monad.replicateM_', but executing the action multiple times in parallel.
replicateM_ :: MonadParallel m => Int -> m a -> m ()
replicateM_ n action = sequence_ (replicate n action)

-- | Any monad that allows the result value to be extracted, such as `Identity` or `Maybe` monad, can implement
-- `bindM2` by using `par`.
instance MonadParallel Identity
instance MonadParallel Maybe
instance MonadParallel []

instance MonadParallel ((->) r) where
   bindM2 f ma mb r = let a = ma r
                          b = mb r
                      in a `par` (b `pseq` f a b r)

-- | IO is parallelizable by `forkIO`.
instance MonadParallel IO where
   bindM2 f ma mb = do waitForB <- forkExec mb
                       a <- ma
                       b <- waitForB
                       f a b

instance MonadParallel m => MonadParallel (IdentityT m) where
   bindM2 f ma mb = IdentityT (bindM2 f' (runIdentityT ma) (runIdentityT mb))
     where f' a b = runIdentityT (f a b)

instance MonadParallel m => MonadParallel (MaybeT m) where
   bindM2 f ma mb = MaybeT (bindM2 f' (runMaybeT ma) (runMaybeT mb))
     where f' (Just a) (Just b) = runMaybeT (f a b)
           f' _ _ = return Nothing

instance MonadParallel m => MonadParallel (ExceptT e m) where
   bindM2 f ma mb = ExceptT (bindM2 f' (runExceptT ma) (runExceptT mb))
     where f' (Right a) (Right b) = runExceptT (f a b)
           f' (Left e) _ = return (Left e)
           f' _ (Left e) = return (Left e)

instance MonadParallel m => MonadParallel (ReaderT r m) where
   bindM2 f ma mb = ReaderT (\r-> bindM2 (f' r) (runReaderT ma r) (runReaderT mb r))
     where f' r a b = runReaderT (f a b) r

instance MonadFork Maybe
instance MonadFork []

instance MonadFork ((->) r) where
   forkExec e = \r-> let result = e r
                     in result `par` (return result)

-- | IO is forkable by `forkIO`.
instance MonadFork IO where
   forkExec ma = do
      v <- newEmptyMVar
      _ <- mask $ \restore -> forkIO $ try (restore ma) >>= putMVar v
      return $ readMVar v >>= either (\e -> throwIO (e :: SomeException)) return

instance MonadFork m => MonadFork (IdentityT m) where
   forkExec ma = IdentityT (liftM IdentityT $ forkExec (runIdentityT ma))

instance MonadFork m => MonadFork (MaybeT m) where
   forkExec ma = MaybeT (liftM (Just . MaybeT) $ forkExec (runMaybeT ma))

instance MonadFork m => MonadFork (ExceptT e m) where
   forkExec ma = ExceptT (liftM (Right . ExceptT) $ forkExec (runExceptT ma))

instance MonadFork m => MonadFork (ReaderT r m) where
   forkExec ma = ReaderT (\r-> liftM (ReaderT . const) $ forkExec (runReaderT ma r))
