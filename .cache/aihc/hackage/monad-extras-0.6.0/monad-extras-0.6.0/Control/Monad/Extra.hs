{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Monad.Extra where

import Control.Applicative
import Control.Monad hiding (mapM_)
import Control.Monad.IO.Class
import Control.Monad.STM
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Control
import Data.Bits
import Data.Foldable
import Data.IORef
import Data.Maybe (catMaybes)
import Data.Monoid
import Prelude hiding (mapM_)
import System.IO.Unsafe

-- | Synonym for @return ()@.
skip :: Monad m => m ()
skip = return ()

-- | Discards a value
discard :: Monad m => a -> m ()
discard _ = return ()

-- | Synonym for @pure ()@.
obvious :: Applicative f => f ()
obvious = pure ()

-- | Function name for '>>=', as 'fmap' is to '<$>'.
bind :: Monad m => m a -> (a -> m b) -> m b
bind = (>>=)

-- | Combinator for working with monadic values:
--
-- >>> om when (return True) $ print "Hello"
-- "Hello"
-- >>> return True >>= flip when (print "Hello")
-- "Hello"
-- >>> om forM_ (return [True]) print
-- True
-- >>> flip forM_ print =<< return [True]
-- True
-- >>> mapM_ print =<< return [True]
-- True
--
-- Subsumes the need for individual functions for 'whenM', 'unlessM', etc.
om :: Monad m => (a -> b -> m c) -> m a -> b -> m c
om f m = (m >>=) . flip f

-- | Variant of 'om' which changes the roles of the 2nd and 3rd arguments.
--
-- >>> nom mapM_ print $ return [True]
-- True
-- >>> mapM_ print =<< return [True]
-- True
nom :: Monad m => (a -> b -> m c) -> a -> m b -> m c
nom f x m = m >>= f x

-- | Convenience function if all you want to use is
--   'Control.Monad.Trans.Cont.callCC'.
doCallCC :: Monad m => ((r -> ContT r m b) -> ContT r m r) -> m r
doCallCC = flip runContT return . callCC

-- | Return a continuation that one can jump back to within 'ContT'.
--
-- >>> flip runContT return $ do { k <- label; ...; k }
label :: ContT r m (ContT r m a)
label = callCC $ \k -> let m = k m in return m

-- | Short-hand for @liftIO@.
io :: MonadIO m => IO a -> m a
io = liftIO

-- | Lift a 'Maybe' value into the 'MaybeT' monad transformer.
liftMaybe :: MonadPlus m => Maybe a -> m a
liftMaybe = maybe mzero return

-- | A monadic version of @mapMaybe :: (a -> Maybe b) -> [a] -> [b]@.
mapMaybeM :: (Monad m, Functor m) => (a -> m (Maybe b)) -> [a] -> m [b]
mapMaybeM f xs = catMaybes <$> mapM f xs

-- | A transformer-friendly version of 'atomically'.
atomicallyM :: MonadIO m => STM a -> m a
atomicallyM = liftIO . atomically

-- | Embed a transformer (Kleisli) arrow as an arrow in the base monad
--   returning a mutated transformer state.  If you do not want the
--   transformation and your base monad is IO, use 'embedIO'.
embed :: (MonadBaseControl base m) => (a -> m b) -> m (a -> base (StM m b))
embed f = control $ \run -> run $ return (run . f)

-- | Return an IO action that closes over the current monad transformer, but
--   throws away any residual effects within that transformer.
embedIO :: (MonadBaseControl IO m, MonadIO m) => (a -> m b) -> m (a -> IO b)
embedIO f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a -> do
        _ <- run $ do
             res <- f a
             liftIO $ writeIORef result res
        readIORef result

embedIO2 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> m r) -> m (a -> b -> IO r)
embedIO2 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b -> do
        _ <- run $ do
             res <- f a b
             liftIO $ writeIORef result res
        readIORef result

embedIO3 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> m r) -> m (a -> b -> c -> IO r)
embedIO3 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c -> do
        _ <- run $ do
             res <- f a b c
             liftIO $ writeIORef result res
        readIORef result

embedIO4 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> m r) -> m (a -> b -> c -> d -> IO r)
embedIO4 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d -> do
        _ <- run $ do
             res <- f a b c d
             liftIO $ writeIORef result res
        readIORef result

embedIO5 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> m r) -> m (a -> b -> c -> d -> e -> IO r)
embedIO5 f = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e -> do
        _ <- run $ do
             res <- f a b c d e
             liftIO $ writeIORef result res
        readIORef result

embedIO6 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> m r)
          -> m (a -> b -> c -> d -> e -> f -> IO r)
embedIO6 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f -> do
        _ <- run $ do
             res <- x a b c d e f
             liftIO $ writeIORef result res
        readIORef result

embedIO7 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> IO r)
embedIO7 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g -> do
        _ <- run $ do
             res <- x a b c d e f g
             liftIO $ writeIORef result res
        readIORef result

embedIO8 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> h -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> h -> IO r)
embedIO8 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g h -> do
        _ <- run $ do
             res <- x a b c d e f g h
             liftIO $ writeIORef result res
        readIORef result

embedIO9 :: (MonadBaseControl IO m, MonadIO m)
          => (a -> b -> c -> d -> e -> f -> g -> h -> i -> m r)
          -> m (a -> b -> c -> d -> e -> f -> g -> h -> i -> IO r)
embedIO9 x = liftBaseWith $ \run -> do
    result <- newIORef undefined
    return $ \a b c d e f g h i -> do
        _ <- run $ do
             res <- x a b c d e f g h i
             liftIO $ writeIORef result res
        readIORef result

-- | Draw monadic actions from a list until one of them yields a value
--   satisfying the predicate, and then return all the values up to and
--   including the first that succeeds in a list within that monad.
sequenceUntil :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceUntil _ [] = return []
sequenceUntil p (m:ms) = do
    a <- m
    if p a
        then return [a]
        else do
            as <- sequenceUntil p ms
            return (a:as)

-- | Draw monadic actions from a list until one of them yields a value
--   failing the predicate, and then return all the passing values
--   (discarding the final, failing value) in a list within that
--   monad.
sequenceWhile :: Monad m => (a -> Bool) -> [m a] -> m [a]
sequenceWhile _ [] = return []
sequenceWhile p (m:ms) = do
    a <- m
    if p a
        then do
            as <- sequenceWhile p ms
            return (a:as)
        else return []

-- | Monadic equivalent to 'iterate'.  Note that it will not terminate, but may
--   still be useful in the main event loop of a program, for example.
iterateM :: Monad m => (a -> m a) -> a -> m [a]
iterateM f x = do
    x' <- f x
    (x':) `liftM` iterateM f x'

-- | Monadic equivalent to 'iterate', which uses Maybe to know when to
--   terminate.
iterateMaybeM :: Monad m => (a -> m (Maybe a)) -> a -> m [a]
iterateMaybeM f x = do
    mx' <- f x
    case mx' of
        Nothing -> return []
        Just x' -> (x':) `liftM` iterateMaybeM f x'

-- | A monadic unfold.
unfoldM :: Monad m => (s -> m (Maybe (a, s))) -> s -> m [a]
unfoldM f s = do
    mres <- f s
    case mres of
        Nothing      -> return []
        Just (a, s') -> liftM2 (:) (return a) (unfoldM f s')

-- | A monadic unfold which does not interact with the result.  The only action
--   this function provides therefore is to iterate through the values in 's'
--   and produce side-effects in IO.
unfoldM_ :: Monad m => (s -> m (Maybe s)) -> s -> m ()
unfoldM_ f s = f s >>= mapM_ (unfoldM_ f)

-- | A monadic unfold.
unfoldMapM :: (Monad m, Monoid a) => (s -> m (Maybe (a, s))) -> s -> m a
unfoldMapM f s = do
    mres <- f s
    case mres of
        Nothing      -> return mempty
        Just (a, s') -> liftM2 mappend (return a) (unfoldMapM f s')

fold1M :: Monad m => (a -> a -> m a) -> [a] -> m a
fold1M _ []     = error "foldl1M: empty list"
fold1M f (x:xs) = foldM f x xs

-- | Assuming the function passed in is associative, divide up the work binary
-- tree-wise.
assocFoldl1 :: (a -> a -> a) -> [a] -> a
assocFoldl1 _ [] = error "assocFold1: empty list"
assocFoldl1 _ [x] = x
assocFoldl1 f [x, y] = f x y
assocFoldl1 f xs = case splitAt (shiftR (length xs) 1) xs of
    ([y], zs) -> f y (assocFoldl1 f zs)
    (ys,  zs) -> f (assocFoldl1 f ys) (assocFoldl1 f zs)

-- | Assuming the function passed in is associative, divide up the work binary
-- tree-wise.
assocFoldl1M :: Monad m => (a -> a -> m a) -> [a] -> m a
assocFoldl1M _ [] = error "assocFold1M: empty list"
assocFoldl1M _ [x] = return x
assocFoldl1M f [x, y] = f x y
assocFoldl1M f xs = case splitAt (shiftR (length xs) 1) xs of
    ([y], zs) -> f y =<< assocFoldl1M f zs
    (ys,  zs) -> do
        y' <- assocFoldl1M f ys
        z' <- assocFoldl1M f zs
        f y' z'
