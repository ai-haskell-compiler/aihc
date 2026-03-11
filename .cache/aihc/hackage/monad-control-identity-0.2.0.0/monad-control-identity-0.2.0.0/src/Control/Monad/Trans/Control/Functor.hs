module Control.Monad.Trans.Control.Functor (
  MonadTransFunctor (..)
, hoistTrans
) where

import Control.Monad.Base
import Control.Monad.Trans.Control
import Control.Monad.Trans.Control.Identity
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader

{- | This type class is generalization of functions like 'mapReaderT'
  and 'mapIdentityT'.
-}
class MonadTransControlIdentity t => MonadTransFunctor t where
  liftMap :: (m a -> n b) -> t m a -> t n b

instance MonadTransFunctor IdentityT where
  liftMap f = IdentityT . f . runIdentityT

instance MonadTransFunctor (ReaderT r) where
  liftMap f m = ReaderT $ f . runReaderT m

-- | Lift the inner monad of a monad transformer from the base monad.
hoistTrans :: (MonadBaseControl b m, MonadBaseControl b (t m), MonadTransFunctor t)
           => t b a
           -> t m a
hoistTrans a = (=<<) restoreM $ liftBaseWith $ \ runInBase ->
                 runInBase $ liftMap liftBase $ a
