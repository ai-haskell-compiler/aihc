module Control.Monad.HReader
       ( HReaderT(..)
       , runHReaderT
       , subHSetHReaderT
       , narrowHReaderT
       , module Control.Monad.HReader.Class
       ) where

import Control.Monad.Base
import Control.Monad.Catch
import Control.Monad.Cont
import Control.Monad.HReader.Class
import Control.Monad.Morph
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans.Control
import Control.Monad.Writer
import Data.HSet
import Data.Typeable
import GHC.Generics

import qualified Control.Monad.Trans.Reader as Reader

#if MIN_VERSION_mtl(2, 2, 1)
import Control.Monad.Except
#else
import Control.Monad.Error
#endif

#if !(MIN_VERSION_base(4,8,0))
import Control.Applicative
#endif

-- | Monad transformer which is like 'ReaderT' but for `HSet` only
newtype HReaderT els m a = HReaderT
    { unHReaderT :: ReaderT (HSet els) m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadError e, MonadCont, MonadWriter w
               , MonadState s, MonadBase b
               , MonadThrow, MonadCatch, MonadMask
               , Typeable, Generic  )

runHReaderT :: HSet els -> HReaderT els m a -> m a
runHReaderT h (HReaderT r) = runReaderT r h

-- | Run a local reader with a subset of HSet elements.
subHSetHReaderT :: (Monad m, Applicative m, SubHSettable els subels)
                => HReaderT subels m a -> HReaderT els m a
subHSetHReaderT hr = do
  hset <- askHSet
  lift $ runHReaderT (subHSet hset) hr

{- | Convenient variant of 'subHSetHReaderT' with proxy type to make it
posible to run nested HReaderT in place without complex type
declarations, e.g.

@
narrowHReaderT (Proxy :: Proxy '[String, Int]) $ do
  doThingsWithString
  doThingsWithInt
  doThingsWithOtherStuff -- < this will not compile
@

-}

narrowHReaderT :: (Monad m, Applicative m, SubHSettable els subels)
               => proxy subels -> HReaderT subels m a -> HReaderT els m a
narrowHReaderT _ = subHSetHReaderT

instance MonadTrans (HReaderT els) where
  lift = HReaderT . lift

instance (MonadReader r m) => MonadReader r (HReaderT els m) where
  ask = lift ask
  local f ma = HReaderT $ do
    h <- ask
    lift $ do
      local f $ runHReaderT h ma

instance (Monad m, Applicative m) => MonadHReader (HReaderT els m) where
  type MHRElements (HReaderT els m) = els
  askHSet = HReaderT ask
  hlocal f (HReaderT r) = HReaderT $ Reader.local f r

deriving instance MFunctor (HReaderT els)
deriving instance MMonad (HReaderT els)

#if MIN_VERSION_monad_control(1, 0, 0)
instance MonadTransControl (HReaderT els) where
  type StT (HReaderT els) a = StT (ReaderT (HSet els)) a
  liftWith action = HReaderT $ do
    liftWith $ \runTrans -> action (runTrans . unHReaderT)
  restoreT = HReaderT . restoreT

instance (MonadBaseControl b m) => MonadBaseControl b (HReaderT els m) where
  type StM (HReaderT els m) a = StM (ReaderT (HSet els) m) a
  liftBaseWith action = HReaderT $ do
    liftBaseWith $ \runInBase -> action (runInBase . unHReaderT)
  restoreM = HReaderT . restoreM
#else
instance MonadTransControl (HReaderT els) where
  newtype StT (HReaderT els) a
    = HRtTT
      { unHRtTT :: StT (ReaderT (HSet els)) a
      }
  liftWith action = HReaderT $ do
    liftWith $ \runTrans -> do
      action ((HRtTT `liftM`) . runTrans . unHReaderT)
  restoreT st = HReaderT $ restoreT $ unHRtTT `liftM` st

instance (MonadBaseControl b m) => MonadBaseControl b (HReaderT els m) where
  newtype StM (HReaderT els m) a
    = HRtMT (StM (ReaderT (HSet els) m) a)
  liftBaseWith action = HReaderT $ do
    liftBaseWith $ \runInBase -> do
      action ((HRtMT `liftM`) . runInBase . unHReaderT)
  restoreM (HRtMT st) = HReaderT $ restoreM st
#endif
