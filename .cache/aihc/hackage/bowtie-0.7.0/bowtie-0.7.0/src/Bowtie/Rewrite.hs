{-# LANGUAGE UndecidableInstances #-}

module Bowtie.Rewrite where

import Bowtie.Jot (Jot, pattern JotP)
import Control.Exception (Exception)
import Control.Monad ((>=>))
import Control.Monad.Except (ExceptT (..), MonadError (..), runExceptT)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Identity (Identity (..))
import Control.Monad.Reader (MonadReader (..), ReaderT (..), asks)
import Control.Monad.State (MonadState (..))
import Control.Monad.Trans (MonadTrans (..))
import Data.Bitraversable (Bitraversable (..))
import Data.Sequence.NonEmpty (NESeq)
import Data.Sequence.NonEmpty qualified as NESeq
import Data.Typeable (Typeable)
import Data.Void (Void, absurd)

data AnnoErr k e = AnnoErr
  { annoErrKey :: !k
  , annoErrVal :: !e
  }
  deriving stock (Eq, Ord, Show)

instance
  (Show k, Typeable k, Show e, Typeable e)
  => Exception (AnnoErr k e)

unwrapAnnoErr :: Either (AnnoErr k Void) a -> a
unwrapAnnoErr = either (absurd . annoErrVal) id

newtype RwT k e m a = RwT {unRwT :: ReaderT (NESeq k) (ExceptT (AnnoErr k e) m) a}
  deriving newtype (Functor, Applicative, Monad)

type Rw k e = RwT k e Identity

instance MonadTrans (RwT k e) where
  lift = RwT . lift . lift

runRwT :: RwT k e m a -> k -> m (Either (AnnoErr k e) a)
runRwT m = runExceptT . runReaderT (unRwT m) . NESeq.singleton

runRw :: Rw k e a -> k -> Either (AnnoErr k e) a
runRw m = runIdentity . runRwT m

embedRwT :: m (Either (AnnoErr k e) a) -> RwT k e m a
embedRwT n = RwT (ReaderT (const (ExceptT n)))

embedRw :: Either (AnnoErr k e) a -> Rw k e a
embedRw = embedRwT . Identity

pushRw :: (Monad m) => k -> RwT k e m a -> RwT k e m a
pushRw b m = RwT (local (NESeq.|> b) (unRwT m))

peekRw :: (Monad m) => RwT k e m k
peekRw = RwT (asks NESeq.last)

peeksRw :: (Monad m) => (k -> a) -> RwT k e m a
peeksRw f = RwT (asks (f . NESeq.last))

askRw :: (Monad m) => RwT k e m (NESeq k)
askRw = RwT ask

asksRw :: (Monad m) => (NESeq k -> a) -> RwT k e m a
asksRw f = RwT (asks f)

throwRw :: (Monad m) => e -> RwT k e m a
throwRw e = RwT (asks NESeq.last >>= \b -> throwError (AnnoErr b e))

instance (MonadReader r m) => MonadReader r (RwT k e m) where
  ask = lift ask
  reader f = lift (reader f)
  local f m = RwT $ do
    bs <- ask
    ea <- lift (lift (local f (runExceptT (runReaderT (unRwT m) bs))))
    either throwError pure ea

instance (MonadState s m) => MonadState s (RwT k e m) where
  get = lift get
  put = lift . put
  state f = lift (state f)

instance (MonadIO m) => MonadIO (RwT k e m) where
  liftIO = lift . liftIO

wrapRw :: g a (Jot g k a) -> Rw k e (Jot g k a)
wrapRw = peeksRw . flip JotP

jotRw :: (Bitraversable g) => (g a z -> Rw k e z) -> Jot g k a -> Either (AnnoErr k e) z
jotRw f = runIdentity . jotRwT (bitraverse pure id >=> f)

jotRwT :: (Monad m, Bitraversable g) => (g a (RwT k e m z) -> RwT k e m z) -> Jot g k a -> m (Either (AnnoErr k e) z)
jotRwT f j0@(JotP b0 _) = runRwT (go j0) b0 where go (JotP b g) = pushRw b (f (fmap go g))
