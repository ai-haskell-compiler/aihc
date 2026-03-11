{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Data.Functor.Logistic
  ( Logistic(..)
  , setters
  ) where

import Data.Distributive
import Data.Functor.Identity
import Data.Functor.Contravariant
import Data.Functor.Compose
import Data.Functor.Product
import Data.Proxy
import Data.Complex
import GHC.Generics

class Functor t => Logistic t where
  deliver :: Contravariant f => f (t a -> t a) -> t (f (a -> a))
  default deliver :: (Generic1 t, Logistic (Rep1 t), Contravariant f) => f (t a -> t a) -> t (f (a -> a))
  deliver f = to1 $ deliver $ contramap (\g -> to1 . g . from1) f

instance Logistic Identity where
  deliver f = Identity (contramap fmap f)

instance Logistic Par1 where
  deliver f = Par1 (contramap fmap f)

instance Logistic f => Logistic (M1 i c f) where
  deliver f = M1 $ deliver $ contramap (\g -> M1 . g . unM1) f

instance Logistic f => Logistic (Rec1 f) where
  deliver f = Rec1 $ deliver $ contramap (\g -> Rec1 . g . unRec1) f

instance Logistic Proxy where
  deliver _ = Proxy

instance Logistic U1 where
  deliver _ = U1

-- | Update only if the argument matches
instance Eq r => Logistic ((->) r) where
  deliver f x = contramap (\u g r -> if r == x then u (g r) else g r) f

instance (Logistic f, Logistic g) => Logistic (Product f g) where
  deliver f = Pair
    (deliver (contramap (\u (Pair a b) -> Pair (u a) b) f))
    (deliver (contramap (\u (Pair a b) -> Pair a (u b)) f))

instance (Logistic f, Logistic g) => Logistic (f :*: g) where
  deliver f
    = deliver (contramap (\u (a :*: b) -> u a :*: b) f)
    :*: deliver (contramap (\u (a :*: b) -> a :*: u b) f)

instance (Logistic f, Logistic g, Applicative f, Traversable g, Distributive g) => Logistic (Compose f g) where
  deliver f = Compose
    $ fmap getCompose
    $ deliver
    $ Compose
    $ deliver
    $ contramap go f
    where
      go p = Compose . sequenceA . p . distribute . getCompose

instance (Logistic f, Logistic g, Applicative f, Traversable g, Distributive g) => Logistic (f :.: g) where
  deliver f = Comp1 $ fmap unComp1 $ deliver $ Comp1 $ deliver $ contramap go f
    where
      go p = Comp1 . sequenceA . p . distribute . unComp1

instance Logistic Complex where
  deliver f
    = contramap (\g (a :+ b) -> g a :+ b) f
    :+ contramap (\g (a :+ b) -> a :+ g b) f

setters :: Logistic t => t ((a -> a) -> t a -> t a)
setters = getOp <$> deliver (Op id)
{-# INLINE setters #-}
