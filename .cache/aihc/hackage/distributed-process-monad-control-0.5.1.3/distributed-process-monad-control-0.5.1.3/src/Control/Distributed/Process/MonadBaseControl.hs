{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

#if MIN_VERSION_monad_control(1,0,0)
{-# LANGUAGE UndecidableInstances #-}
#endif

{-# OPTIONS_GHC -fno-warn-orphans #-}
-- | This module only exports instances for 'MonadBase' 'IO' and
-- 'MonadBaseControl' 'IO' for the 'Process' monad. This is for use
-- in conjunction with a library requiring these instances, such as the
-- <http://hackage.haskell.org/package/lifted-base lifted-base> package.
--
--
-- Please excercise caution in usage of this instance, as it can
-- enable use of functions such as 'forkIO' (or, 'fork' from lifted-base)
-- which compromise invariants in the Process monad and can lead to confusing
-- and subtle issues. Always use the Cloud Haskell functions such as `spawnLocal` instead.
--
-- example usage:
--
-- >import Control.Distributed.Process.MonadBaseControl()
-- >import Control.Concurrent.MVar.Lifted (withMVar)
--
-- >processWithMVar :: MVar a -> (a -> Process b) -> Process b
-- >processWithMvar = withMVar
module Control.Distributed.Process.MonadBaseControl
  (
  ) where

import Control.Distributed.Process.Internal.Types
  ( Process(..)
  , LocalProcess
  )


import Control.Monad.Base (MonadBase(..))
import Control.Monad.Trans.Control (MonadBaseControl(..))
import Control.Monad.Trans.Reader (ReaderT)

deriving instance MonadBase IO Process


#if MIN_VERSION_monad_control(1,0,0)
instance MonadBaseControl IO Process where
  type StM Process a = StM (ReaderT LocalProcess IO) a
  liftBaseWith f = Process $ liftBaseWith $ \ rib -> f (rib . unProcess)
  restoreM = Process . restoreM
#else
instance MonadBaseControl IO Process where
  newtype StM Process a = StProcess {_unSTProcess :: StM (ReaderT LocalProcess IO) a}
  restoreM (StProcess m) = Process $ restoreM m
  liftBaseWith f = Process $ liftBaseWith $ \ rib -> f (fmap StProcess . rib . unProcess)
#endif
