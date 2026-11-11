-- | The 'Typeable' class and the type representations that it gives.
--
-- "Data.Typeable.Internal" holds most of this module. Modules below
-- 'Prelude' import it directly. This module adds the fingerprints, which
-- need "GHC.Fingerprint" and thus 'Prelude'.
{-# LANGUAGE GADTs #-}
{-# LANGUAGE PatternSynonyms #-}

module Data.Typeable
  ( Typeable,
    TypeRep,
    TyCon,
    Proxy (..),
    (:~:) (..),
    (:~~:) (..),
    cast,
    eqT,
    heqT,
    gcast,
    gcast1,
    gcast2,
    typeOf,
    typeOf1,
    typeOf2,
    typeOf3,
    typeOf4,
    typeOf5,
    typeOf6,
    typeOf7,
    typeRep,
    showsTypeRep,
    typeRepArgs,
    typeRepTyCon,
    typeRepFingerprint,
    funResultTy,
    mkFunTy,
    splitTyConApp,
    tyConPackage,
    tyConModule,
    tyConName,
    tyConFingerprint,
    rnfTyCon,
    rnfTypeRep,
  )
where

import Data.Type.Equality ((:~~:) (..))
import Data.Typeable.Internal
import GHC.Base (Maybe (..), (++))
import GHC.Err (errorWithoutStackTrace)
import GHC.Fingerprint (Fingerprint)
import GHC.Show (show)
import GHC.Types (Type)
import Type.Reflection (SomeTypeRep (..), pattern Fun)
import Type.Reflection qualified as Reflection
import Type.Reflection.Unsafe (tyConFingerprint)
import Type.Reflection.Unsafe qualified as Unsafe

-- | The fingerprint of a type representation.
typeRepFingerprint :: TypeRep -> Fingerprint
typeRepFingerprint (SomeTypeRep representation) = Unsafe.typeRepFingerprint representation

-- | The result type of a function type applied to an argument type.
-- The result is 'Nothing' unless the first type is a function type and
-- the second type is its argument type.
funResultTy :: TypeRep -> TypeRep -> Maybe TypeRep
funResultTy (SomeTypeRep (Fun parameter result)) (SomeTypeRep argument) =
  case Reflection.eqTypeRep parameter argument of
    Just HRefl -> Just (SomeTypeRep result)
    Nothing -> Nothing
funResultTy _ _ = Nothing

-- | The function type from the first type to the second type. Each type
-- must have the kind 'Type'.
mkFunTy :: TypeRep -> TypeRep -> TypeRep
mkFunTy (SomeTypeRep argument) (SomeTypeRep result) =
  case Reflection.eqTypeRep (Reflection.typeRepKind argument) liftedType of
    Just HRefl ->
      case Reflection.eqTypeRep (Reflection.typeRepKind result) liftedType of
        Just HRefl -> SomeTypeRep (Fun argument result)
        Nothing -> notLifted
    Nothing -> notLifted
  where
    liftedType = Reflection.typeRep :: Reflection.TypeRep Type
    notLifted =
      errorWithoutStackTrace
        ( "mkFunTy: Attempted to construct function type from non-lifted type: arg="
            ++ show argument
            ++ ", res="
            ++ show result
        )
