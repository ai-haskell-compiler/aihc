-- | Defines a version of the ListT monad transformer, used in the REST search

module Language.REST.Internal.ListT where

import           Control.Applicative
import           Control.Monad.Trans

newtype ListT m a = ListT {
  runListT :: m [a]
}

instance (Monad m) => Functor (ListT m) where
  fmap f (ListT mxs) = ListT $ do
    map f <$> mxs

instance (Monad m) => Applicative (ListT m) where
  pure x                    = ListT (return [x])
  (ListT mf) <*> (ListT mx) = ListT $ do
    fs <- mf
    xs <- mx
    return $ do
      f <- fs
      map f xs

instance (Monad m) => Monad (ListT m) where
  return x         = ListT (return [x])
  (ListT mxs) >>= f = ListT $ do
    xs <- mxs
    res <- mapM (runListT . f) xs
    return $ concat res

instance (Monad m) => Alternative (ListT m) where
  empty                       = ListT (return [])
  (ListT mxs) <|> (ListT mys) = ListT $ do
    xs <- mxs
    if not $ null xs
      then mxs
      else mys

instance MonadTrans ListT where
  lift mx = ListT $ do
    x <- mx
    return [x]
