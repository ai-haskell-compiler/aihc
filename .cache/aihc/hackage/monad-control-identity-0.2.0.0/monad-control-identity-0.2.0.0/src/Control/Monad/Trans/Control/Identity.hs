{-# LANGUAGE DerivingVia, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving, StandaloneDeriving, TypeFamilies, Rank2Types, UndecidableInstances #-}

module Control.Monad.Trans.Control.Identity (

-- * MonadTransControlIdentity
  MonadTransControlIdentity (..)
{- | 'MonadTransControlIdentity' instances can easily be created for
  monad transformers, because of the superclass 'MonadTransControl':

@
newtype ExampleT = ...
  deriving ('Monad', 'Control.Monad.Trans.Class.MonadTrans')

instance 'MonadTransControl' ExampleT where
  ...

instance 'MonadTransControlIdentity' ExampleT where
  'liftWithIdentity' f = 'liftWith' $ \ runT -> f runT
@
-}

-- * MonadBaseControlIdentity
-- | Regarding the 'IO' base monad this can be seen as an alternative,
-- but equivalent, way to implement 'MonadUnliftIO'.
, MonadBaseControlIdentity (..)
{- | 'MonadBaseControlIdentity' instances can be created just as
  easily for monad transformers:

@
instance 'MonadBaseControlIdentity' b m => 'MonadBaseControlIdentity' b (ExampleT m) where
  'liftBaseWithIdentity' = 'defaultLiftBaseWithIdentity'
@
  -}
, defaultLiftBaseWithIdentity

) where

import Control.Monad.Base
import Control.Monad.ST.Lazy as L
import Control.Monad.ST.Strict as S
import Control.Monad.STM
import Control.Monad.Trans.Control
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Reader
import Data.Functor.Identity

{- | The 'MonadTransControlIdentity' type class is a stronger version of
  'MonadTransControl':

  'MonadTransControl' instances are aware of the monadic state of the
  transformer and allow to save and restore this state.
  'MonadTransControlIdentity' instances on the other hand exist only for
  exactly those transformers, that don't have any monadic state.

  So for any instance of this class this should hold:

  @forall a. 'StT' t a ~ a@

  This can't be given as a constraint to the class due to limitations
  regarding the @TypeFamilies@ extension.
-}
class MonadTransControl t => MonadTransControlIdentity t where
  liftWithIdentity :: Monad m => ((forall x. t m x -> m x) -> m a) -> t m a

instance MonadTransControlIdentity IdentityT where
  liftWithIdentity f = liftWith $ \ runT -> f runT

instance MonadTransControlIdentity (ReaderT r) where
  liftWithIdentity f = liftWith $ \ runT -> f runT

{- | The 'MonadBaseControlIdentity' type class is a stronger version of
  'MonadBaseControl'.

  Just like 'MonadTransControlIdentity' instances of
  'MonadBaseControlIdentity' hold no monadic state:

  @forall a. 'StM' m a ~ a@
-}
class MonadBaseControl b m => MonadBaseControlIdentity b m | m -> b where
  liftBaseWithIdentity :: ((forall x. m x -> b x) -> b a) -> m a

defaultLiftBaseWithIdentity :: (MonadBaseControlIdentity b m, MonadTransControlIdentity t)
                            => ((forall x. t m x -> b x) -> b a)
                            -> t m a
defaultLiftBaseWithIdentity inner = liftWithIdentity $ \ runId ->
  liftBaseWithIdentity $ \ runIdInBase ->
    inner $ runIdInBase . runId

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (IdentityT m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

instance MonadBaseControlIdentity b m => MonadBaseControlIdentity b (ReaderT r m) where
  liftBaseWithIdentity = defaultLiftBaseWithIdentity

deriving via Base IO instance MonadBaseControlIdentity IO IO
deriving via Base Maybe instance MonadBaseControlIdentity Maybe Maybe
deriving via Base (Either e) instance MonadBaseControlIdentity (Either e) (Either e)
deriving via Base [] instance MonadBaseControlIdentity [] []
deriving via Base ((->) r) instance MonadBaseControlIdentity ((->) r) ((->) r)
deriving via Base Identity instance MonadBaseControlIdentity Identity Identity
deriving via Base STM instance MonadBaseControlIdentity STM STM
deriving via Base (S.ST s) instance MonadBaseControlIdentity (S.ST s) (S.ST s)
deriving via Base (L.ST s) instance MonadBaseControlIdentity (L.ST s) (L.ST s)

newtype Base m a = MkBase { getBase :: m a }
  deriving newtype (Functor, Applicative, Monad)

instance Monad m => MonadBase m (Base m) where
  liftBase = MkBase

instance Monad m => MonadBaseControl m (Base m) where
  type StM (Base m) a = a
  liftBaseWith inner = MkBase $ inner getBase
  restoreM = return

instance Monad m => MonadBaseControlIdentity m (Base m) where
  liftBaseWithIdentity inner = MkBase $ inner getBase
