{- ORACLE_TEST xfail reason="associated type families with kind signatures followed by default signatures fail to parse" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GenericKAssociatedTypeDefaultSignaturesXFail where

import Data.Kind (Type)

class Generic a where
  type Rep a :: Type
  from :: a -> Rep a
  to :: Rep a -> a

class Conv rep repK x where
  toKindGenerics :: rep -> repK x
  toGhcGenerics :: repK x -> rep

class GenericK (f :: k) where
  type family RepK f :: LoT k -> Type

  fromK :: f :@@: x -> RepK f x
  default
    fromK :: (Generic (f :@@: x), Conv (Rep (f :@@: x)) (RepK f) x)
          => f :@@: x -> RepK f x
  fromK = toKindGenerics . from

  toK :: RepK f x -> f :@@: x
  default
    toK :: (Generic (f :@@: x), Conv (Rep (f :@@: x)) (RepK f) x)
        => RepK f x -> f :@@: x
  toK = to . toGhcGenerics

data LoT k

infixr 0 :@@:
data (:@@:) (f :: k) (x :: LoT k)
