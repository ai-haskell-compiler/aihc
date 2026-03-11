{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Control.Also where

import Control.Applicative
import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.Cont
import Control.Monad.Trans.Except
import Control.Monad.Trans.Identity
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.RWS.Lazy as Lazy
import Control.Monad.Trans.RWS.Strict as Strict
import Control.Monad.Trans.State.Lazy as Lazy
import Control.Monad.Trans.State.Strict as Strict
import Control.Monad.Trans.Writer.Lazy as Lazy
import Control.Monad.Trans.Writer.Strict as Strict
import Control.Newtype.Generics
import Data.Functor.Identity
import GHC.Generics

#if MIN_VERSION_base(4,9,0) && !MIN_VERSION_base(4,10,0)
import Data.Semigroup
#endif

-- | Combining effects where both input effects are used as much as possible.
-- as opposed to 'Control.Applicative.Alternative' where only the "successful" effect is used.
class Also f a where
    -- | An associative binary operation, where both input effects are used as much as possible.
    also :: f a -> f a -> f a
    -- | The identity of 'also'
    alsoZero :: f a

infixr 6 `also` -- like <>

-- | Monoid under 'also'.
-- Mnemonic: 'Als' for 'Also', just like 'Alt' for 'Altenative'
newtype Als f a = Als { getAls :: f a }
    deriving (Generic, Generic1, Read, Show, Eq, Ord, Num, Enum,
                Monad, MonadPlus, Applicative, Alternative, Functor)

instance Newtype (Als f a)

instance Also f a => Semigroup (Als f a) where
    (Als f) <> (Als g) = Als (f `also` g)

instance Also f a => Monoid (Als f a) where
    mempty = Als alsoZero
#if !MIN_VERSION_base(4,11,0)
    (Als f) `mappend` (Als g) = Als (f `also` g)
#endif

-- | Overlappable instance for all Applicatives of Monoids.
#if MIN_VERSION_base(4,11,0)
instance {-# OVERLAPPABLE #-} (Monoid a, Applicative f) => Also f a where
    alsoZero = pure mempty
    f `also` g = liftA2 (<>) f g
#else
instance {-# OVERLAPPABLE #-} (Monoid a, Applicative f) => Also f a where
    alsoZero = pure mempty
    f `also` g = liftA2 mappend f g
#endif

#if MIN_VERSION_base(4,11,0)
instance (Monoid a) => Also Identity a where
    alsoZero = mempty
    a `also` b = a <> b
#else
instance (Monoid a) => Also Identity a where
    alsoZero = mempty
    a `also` b = a `mappend` b
#endif

#if MIN_VERSION_base(4,11,0)
instance (Monoid a) => Also IO a where
    alsoZero = mempty
    a `also` b = a <> b
#else
instance (Monoid a) => Also IO a where
    alsoZero = pure mempty
    a `also` b = liftA2 mappend a b
#endif

instance (Also m a) => Also (IdentityT m) a where
    alsoZero = IdentityT alsoZero
    (IdentityT a) `also` (IdentityT b) = IdentityT $ a `also` b

-- | Combine the monads that returns @r@ not @a@.
instance (Also m r) => Also (ContT r m) a where
    alsoZero = ContT . const $ alsoZero
    (ContT f) `also` (ContT g) =
        ContT $ \k -> (f k) `also` (g k)

instance (Also m a) => Also (ReaderT r m) a where
    alsoZero = ReaderT $ const alsoZero
    (ReaderT f) `also` (ReaderT g) = ReaderT $ \r -> f r `also` g r

instance (Also m (Either e a)) => Also (ExceptT e m) a where
    alsoZero = ExceptT $ alsoZero
    (ExceptT f) `also` (ExceptT g) = ExceptT $ f `also` g

instance (Also m (Maybe a)) => Also (MaybeT m) a where
    alsoZero = MaybeT $ alsoZero
    (MaybeT f) `also` (MaybeT g) = MaybeT $ f `also` g

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Also m a, Monad m) => Also (Lazy.StateT s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Also m a, Monad m) => Also (Strict.StateT s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | Writer instances threads the writer through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Lazy.WriterT w m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | Writer instances threads the writer through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Strict.WriterT w m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Lazy.RWST r w s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y

-- | State instances threads the state through both monad of 'also'
-- in the normal left to right order, and so do not prevent
-- early termination from the left monad (eg if the inner monad was
-- a 'MaybeT' or 'ExceptT'.
-- However, it is able to use the 'also' to combine the return value.
instance (Monoid w, Also m a, Monad m) => Also (Strict.RWST r w s m) a where
    alsoZero = lift alsoZero
    f `also` g = do
        (x, y) <- liftA2 (,) f g
        lift $ pure x `also` pure y
