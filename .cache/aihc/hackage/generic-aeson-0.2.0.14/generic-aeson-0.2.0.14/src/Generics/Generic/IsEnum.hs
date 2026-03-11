{-# LANGUAGE
    FlexibleContexts
  , FlexibleInstances
  , ScopedTypeVariables
  , TypeOperators
  #-}
-- | Test if a data type is an enumeration (only zero-argument
-- constructors) generically using "GHC.Generics".
module Generics.Generic.IsEnum
  ( isEnum
  , GIsEnum (..)
  ) where

import Data.Proxy

import GHC.Generics

-- | Class for testing if the functors from "GHC.Generics" are
-- enumerations. You generally don't need to give any custom
-- instances. Just call 'isEnum'.
class GIsEnum f where
  gIsEnum :: Proxy (f a) -> Bool


instance GIsEnum V1 where
  gIsEnum _ = False

instance GIsEnum (K1 i a) where
  gIsEnum _ = False

instance GIsEnum U1 where
  gIsEnum _ = True

instance GIsEnum Par1 where
  gIsEnum _ = False

-- should be K1 R
instance GIsEnum (Rec1 f) where
  gIsEnum _ = False


instance (GIsEnum f, GIsEnum g) => GIsEnum (f :+: g) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a)) && gIsEnum (Proxy :: Proxy (g a))

instance (GIsEnum f, GIsEnum g) => GIsEnum (f :*: g) where
  gIsEnum _ = False

instance GIsEnum f => GIsEnum (M1 C c f) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a))

instance GIsEnum (M1 S c a) where
  gIsEnum _ = False

instance GIsEnum f => GIsEnum (M1 D c f) where
  gIsEnum _ = gIsEnum (Proxy :: Proxy (f a))

-- instance GIsEnum (f :.: g) where

-- | Generically test if a data type is an enumeration.
isEnum :: forall a. (Generic a, GIsEnum (Rep a)) => Proxy a -> Bool
isEnum _ = gIsEnum (Proxy :: Proxy ((Rep a) a))
