{-# LANGUAGE
    KindSignatures
  #-}

module Data.Singleton.Class where

import Data.Functor.Identity (Identity (..))
import Data.Functor.Compose  (Compose (..))



class Extractable (f :: * -> *) where
  runSingleton :: f a -> a


instance Extractable ((,) e) where
  runSingleton (_,x) = x

instance Extractable ((,,) w s) where
  runSingleton (_,_,x) = x

instance Extractable Identity where
  runSingleton (Identity x) = x


instance (Extractable f, Extractable g) => Extractable (Compose f g) where
  runSingleton (Compose x) = runSingleton (runSingleton x)
