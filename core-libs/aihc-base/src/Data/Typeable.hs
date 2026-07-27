module Data.Typeable
  ( Typeable (..),
    TypeRep,
    TyCon,
    cast,
    eqTypeRep,
    typeOf,
    typeRepArgs,
    typeRepTyCon,
    tyConName,
  )
where

import Data.Maybe (Maybe (..))
import Data.Proxy (Proxy (..))
import Type.Reflection
  ( TyCon,
    TypeRep,
    Typeable (..),
    eqTypeRep,
    tyConName,
    typeOf,
    typeRepArgs,
    typeRepTyCon,
  )
import Unsafe.Coerce (unsafeCoerce)
import Prelude (Bool (..))

cast :: (Typeable a, Typeable b) => a -> Maybe b
cast value = castWith value Proxy

castWith :: (Typeable a, Typeable b) => a -> Proxy b -> Maybe b
castWith value target =
  if eqTypeRep (typeOf value) (typeRep target)
    then Just (unsafeCoerce value)
    else Nothing
