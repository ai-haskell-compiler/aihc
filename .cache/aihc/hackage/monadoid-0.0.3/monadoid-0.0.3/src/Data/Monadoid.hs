{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveFunctor
  , DeriveTraversable
  , DeriveGeneric
  , DeriveDataTypeable
  , TypeFamilies
  , FlexibleInstances
  , UndecidableInstances
  , MultiParamTypeClasses
  #-}

{-|

Module      : Data.Monadoid
Copyright   : (c) 2017 Athan Clark
License     : BSD-3
Maintainer  : athan.clark@gmail.com
Stability   : experimental
Portability : GHC

 -}

module Data.Monadoid where

import GHC.Generics (Generic)
import Data.Data (Data, Typeable)

import Control.Monad.Trans (MonadTrans (lift))
import Control.Monad.Reader.Class (MonadReader)
import Control.Monad.State.Class (MonadState)
import Control.Monad.Writer.Class (MonadWriter)
import Control.Monad.Error.Class (MonadError)
import Control.Monad.Cont.Class (MonadCont)
import Control.Monad.RWS.Class (MonadRWS)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Base (MonadBase)

import Control.Monad.Trans.Control ( MonadTransControl (liftWith, restoreT), StT
                                   , MonadBaseControl (liftBaseWith, restoreM), StM
                                   )



newtype Monadoid m a = Monadoid {runMonadoid :: m a}
  deriving ( Show, Eq, Ord, Functor, Applicative, Monad, Foldable, Traversable, Generic, Data, Typeable
           , MonadReader r, MonadWriter w, MonadState s, MonadRWS r w s, MonadError e, MonadIO, MonadBase b
           , MonadCont
           )


instance MonadTrans Monadoid where
  lift = Monadoid

instance MonadTransControl Monadoid where
  type StT Monadoid a = a
  liftWith withRun = lift (withRun runMonadoid)
  restoreT = lift

instance (MonadBase b m, MonadBaseControl b m) => MonadBaseControl b (Monadoid m) where
  type StM (Monadoid m) a = StM m a
  liftBaseWith withRunBase = lift $ liftBaseWith $ \runLower -> withRunBase $ runLower . runMonadoid
  restoreM = lift . restoreM

-- TODO: MonadResource? Other popular ones


instance (Applicative m, Semigroup a) => Semigroup (Monadoid m a) where
  x <> y = (<>) <$> x <*> y

-- | The only important instance
instance (Applicative m, Monoid a) => Monoid (Monadoid m a) where
  mempty = pure mempty
