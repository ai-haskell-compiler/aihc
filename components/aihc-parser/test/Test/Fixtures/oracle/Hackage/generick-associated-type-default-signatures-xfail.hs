{- ORACLE_TEST xfail reason="associated type families with kind signatures followed by default signatures fail to parse" -}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module GenericKAssociatedTypeDefaultSignaturesXFail where

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
