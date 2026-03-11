{-# LANGUAGE BlockArguments #-}

-- | A datatype that encapsulates @with...@-style callback-taking functions that
-- acquire and release resources.
--
-- Here's [a video about how it works](https://youtu.be/2v7BAQh_dRs).
module Cauldron.Managed
  ( -- * The Managed monad for handling resources
    Managed,
    managed,
    with,
    runManaged,
  )
where

import Control.Concurrent.MVar
import Control.Exception.Base
import Control.Monad.Fix
import Control.Monad.IO.Class
import GHC.IO.Unsafe

-- | This is a copy of the @Managed@ type from the
-- [managed](https://hackage.haskell.org/package/managed) package, with a dodgy
-- 'Control.Monad.Fix.MonadFix' instance tacked on.
newtype Managed a = Managed (forall b. (a -> IO b) -> IO b)

-- | Build a 'Managed' value from a @withFoo@-style resource-handling function
-- that accepts a continuation, like 'System.IO.withFile'.
--
-- Passing functions that do weird things like running their continuation
-- /twice/ will tear apart the fabric of reality. Why would you want to do that?
-- Pass only @withFoo@-style functions.
managed :: (forall r. (a -> IO r) -> IO r) -> Managed a
managed = Managed

-- | This instance is a little dodgy (continuation-like monads don't have proper
-- 'MonadFix' instances) but it is nevertheless useful because it lets us use
-- 'Managed' with 'allowSelfDeps'. Follow the recommendations for the 'managed'
-- function.
--
-- [\"if you embrace the unsafety, it could be a fun way to tie knots.\"](https://stackoverflow.com/questions/25827227/why-cant-there-be-an-instance-of-monadfix-for-the-continuation-monad#comment113010373_63906214)
instance MonadFix Managed where
  -- https://stackoverflow.com/a/63906214
  -- See also the implementation for fixIO https://hackage.haskell.org/package/base-4.19.0.0/docs/src/System.IO.html#fixIO
  mfix f = Managed \k -> do
    m <- newEmptyMVar
    x <-
      unsafeDupableInterleaveIO
        ( readMVar m `catch` \BlockedIndefinitelyOnMVar ->
            throwIO FixIOException
        )
    unManage (f x) \x' -> do
      putMVar m x'
      k x'
    where
      unManage (Managed a) = a

instance Functor Managed where
  fmap f (Managed m) = Managed (\k -> m (\x -> k (f x)))
  {-# INLINE fmap #-}

instance Applicative Managed where
  pure x = Managed (\k -> k x)
  {-# INLINE pure #-}
  Managed f <*> Managed g = Managed (\bfr -> f (\ab -> g (\x -> bfr (ab x))))
  {-# INLINE (<*>) #-}

instance Monad Managed where
  return = pure
  {-# INLINE return #-}
  m >>= k = Managed (\c -> with m (\a -> with (k a) c))
  {-# INLINE (>>=) #-}

instance MonadIO Managed where
  liftIO m = Managed \return_ -> do
    a <- m
    return_ a
  {-# INLINE liftIO #-}

instance MonadFail Managed where
  fail s =
    Managed
      ( \return_ -> do
          a <- fail @IO s
          return_ a
      )

instance (Semigroup a) => Semigroup (Managed a) where
  (<>) = liftA2 (<>)

instance (Monoid a) => Monoid (Managed a) where
  mempty = pure mempty

-- | Make use of the managed resource by supplying a callback.
with :: Managed a -> (a -> IO b) -> IO b
with (Managed r) = r

-- | Run a `Managed` computation, enforcing that no acquired resources leak
runManaged :: Managed () -> IO ()
runManaged (Managed r) = r pure
