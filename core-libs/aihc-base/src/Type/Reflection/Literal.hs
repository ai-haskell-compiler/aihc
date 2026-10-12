{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Type.Reflection.Literal
  ( typeRepNat,
    typeRepSymbol,
    typeRepChar,
  )
where

import Data.Proxy (Proxy (..))
import GHC.TypeLits (KnownChar, KnownNat, KnownSymbol, Nat, Symbol, charVal, natVal, symbolVal)
import GHC.Types (KindRep (..), TrName (..), TyCon (..))
import Type.Reflection.Internal (TypeRep (..), Typeable (typeRep))
import Prelude (Char, Show (show), String)

-- | Construct a type literal representation from its singleton dictionary.
typeRepNat :: forall (n :: Nat). (KnownNat n) => TypeRep n
typeRepNat = literalTypeRep (show (natVal (Proxy :: Proxy n))) (typeRep :: TypeRep Nat)

typeRepSymbol :: forall (s :: Symbol). (KnownSymbol s) => TypeRep s
typeRepSymbol = literalTypeRep (show (symbolVal (Proxy :: Proxy s))) (typeRep :: TypeRep Symbol)

typeRepChar :: forall (c :: Char). (KnownChar c) => TypeRep c
typeRepChar = literalTypeRep (show (charVal (Proxy :: Proxy c))) (typeRep :: TypeRep Char)

literalTypeRep :: forall k (a :: k). String -> TypeRep k -> TypeRep a
literalTypeRep name (TypeRep kindCon@(TyCon modul _ _ _) _ _) =
  TypeRep (TyCon modul (TrNameD name) 0 (KindRepTyConApp kindCon [])) [] []
