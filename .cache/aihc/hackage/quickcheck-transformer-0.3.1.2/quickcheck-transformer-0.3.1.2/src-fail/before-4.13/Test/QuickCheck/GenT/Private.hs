module Test.QuickCheck.GenT.Private where

import qualified Test.QuickCheck.Random as QC
import qualified System.Random as Random

import Control.Monad.Trans.Class (MonadTrans, lift)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad (ap)
import Control.Applicative (Applicative, pure, (<*>))


newtype GenT m a = GenT { unGenT :: QC.QCGen -> Int -> m a }

instance (Functor m) => Functor (GenT m) where
  fmap f m = GenT $ \r n -> fmap f $ unGenT m r n

instance (Monad m) => Monad (GenT m) where
  return a = GenT (\_ _ -> return a)
  m >>= k = GenT $ \r n -> do
    let (r1, r2) = Random.split r
    a <- unGenT m r1 n
    unGenT (k a) r2 n
  fail msg = GenT (\_ _ -> fail msg)

instance (Functor m, Monad m) => Applicative (GenT m) where
  pure = return
  (<*>) = ap

instance MonadTrans GenT where
  lift m = GenT (\_ _ -> m)

instance (MonadIO m) => MonadIO (GenT m) where
  liftIO = lift . liftIO
