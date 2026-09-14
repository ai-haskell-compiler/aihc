{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}

module Type.Reflection.Unsafe
  ( TyCon,
    mkTyCon,
    mkTrCon,
    mkTrApp,
    typeRepFingerprint,
    tyConKindArgs,
    tyConKindRep,
    KindRep (..),
    KindBndr,
    TypeLitSort (..),
    pattern KindRepTypeLit,
  )
where

import Data.Maybe (Maybe (..))
import GHC.Base (List (..), String, map, unpackCString#, (++))
import GHC.Fingerprint (Fingerprint, fingerprintFingerprints, fingerprintString)
import GHC.Types (Int, KindBndr, KindRep (..), Module (..), TrName (..), TyCon (..), TypeLitSort (..))
import Type.Reflection.Internal (SomeTypeRep (..), TypeRep, mkTrApp, mkTrCon, splitApps, tyConModule, tyConName, tyConPackage, typeRepKindArguments)

-- | Build a type constructor. The compiler builds the constructors that
-- 'Type.Reflection.Typeable' evidence carries; this is for a caller that
-- deserialises one.
mkTyCon :: String -> String -> String -> Int -> KindRep -> TyCon
mkTyCon package moduleName name =
  TyCon (Module (TrNameD package) (TrNameD moduleName)) (TrNameD name)

-- | The number of kind arguments that the kind representation of a type
-- constructor abstracts over.
tyConKindArgs :: TyCon -> Int
tyConKindArgs (TyCon _ _ kindArgs _) = kindArgs

tyConKindRep :: TyCon -> KindRep
tyConKindRep (TyCon _ _ _ kindRep) = kindRep

-- | A type literal in a kind, whichever way its text is stored. Building
-- one stores the text as a list, as a caller outside the compiler has no
-- string literal to point at.
pattern KindRepTypeLit :: TypeLitSort -> String -> KindRep
pattern KindRepTypeLit sort text <- (kindRepTypeLit -> Just (sort, text))
  where
    KindRepTypeLit sort text = KindRepTypeLitD sort text

kindRepTypeLit :: KindRep -> Maybe (TypeLitSort, String)
kindRepTypeLit (KindRepTypeLitS sort address) = Just (sort, unpackCString# address)
kindRepTypeLit (KindRepTypeLitD sort text) = Just (sort, text)
kindRepTypeLit _ = Nothing

-- | A fingerprint of a type representation.
--
-- GHC stores one in every 'TypeRep' and compares types by it. aihc builds
-- it on demand from the same structure that 'Type.Reflection.eqTypeRep'
-- walks, so it agrees with equality: equal representations fingerprint
-- alike, and distinct ones are only as likely to collide as the hash in
-- "GHC.Fingerprint" allows. Only a hash may depend on it.
typeRepFingerprint :: forall k (a :: k). TypeRep a -> Fingerprint
typeRepFingerprint representation =
  case splitApps representation of
    (constructor, arguments) ->
      fingerprintFingerprints
        ( tyConFingerprint constructor
            : map someTypeRepFingerprint (typeRepKindArguments representation ++ arguments)
        )

someTypeRepFingerprint :: SomeTypeRep -> Fingerprint
someTypeRepFingerprint (SomeTypeRep representation) = typeRepFingerprint representation

-- | A type constructor is named by its package, its module and its own
-- name, which is what 'Type.Reflection.eqTypeRep' compares it by. The
-- separator cannot occur in any of the three.
tyConFingerprint :: TyCon -> Fingerprint
tyConFingerprint constructor =
  fingerprintString (tyConPackage constructor ++ "\NUL" ++ tyConModule constructor ++ "\NUL" ++ tyConName constructor)
