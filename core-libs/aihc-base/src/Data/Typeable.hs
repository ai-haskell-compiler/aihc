-- | The 'Typeable' class and the type representations that it gives.
--
-- "Data.Typeable.Internal" holds most of this module. Modules below
-- 'Prelude' import it directly. This module adds the fingerprints, which
-- need "GHC.Fingerprint" and thus 'Prelude'.
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
    splitTyConApp,
    tyConPackage,
    tyConModule,
    tyConName,
    tyConFingerprint,
    rnfTyCon,
    rnfTypeRep,
  )
where

import Data.Typeable.Internal
import GHC.Fingerprint (Fingerprint)
import Type.Reflection (SomeTypeRep (..))
import Type.Reflection.Unsafe (tyConFingerprint)
import Type.Reflection.Unsafe qualified as Unsafe

-- | The fingerprint of a type representation.
typeRepFingerprint :: TypeRep -> Fingerprint
typeRepFingerprint (SomeTypeRep representation) = Unsafe.typeRepFingerprint representation
