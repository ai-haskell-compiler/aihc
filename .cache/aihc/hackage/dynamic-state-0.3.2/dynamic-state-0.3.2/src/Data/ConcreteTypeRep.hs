{-# LANGUAGE DeriveDataTypeable, GeneralizedNewtypeDeriving, CPP, ViewPatterns #-}

{- |
Copyright (c)2011, Reiner Pope

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

    * Neither the name of Reiner Pope nor the names of other
      contributors may be used to endorse or promote products derived
      from this software without specific prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

This module defines 'Binary' and 'Hashable' instances for 'TypeRep'. These are defined on a newtype of 'TypeRep', namely 'ConcreteTypeRep', for two purposes:

  * to avoid making orphan instances

  * the 'Hashable' instance for 'ConcreteTypeRep' may not be pure enough for some people's tastes.

As usual with 'Typeable', this module will typically be used with some variant of @Data.Dynamic@. Two possible uses of this module are:

  * making hashmaps: @HashMap 'ConcreteTypeRep' Dynamic@

  * serializing @Dynamic@s.

-}

module Data.ConcreteTypeRep (
  ConcreteTypeRep,
  cTypeOf,
  toTypeRep,
  fromTypeRep,
 ) where

#if MIN_VERSION_base(4,10,0)
import Type.Reflection (SomeTypeRep(..))
import Type.Reflection.Unsafe (mkTyCon, mkTrCon, tyConKindArgs, tyConKindRep, KindRep)
#endif
import Data.Typeable
import Data.Hashable
import Data.Binary
import GHC.Fingerprint

-- | Abstract type providing the functionality of 'TypeRep', but additionally supporting hashing and serialization.
--
-- The 'Eq' instance is just the 'Eq' instance for 'TypeRep', so an analogous guarantee holds: @'cTypeOf' a == 'cTypeOf' b@ if and only if @a@ and @b@ have the same type.
-- The hashing and serialization functions preserve this equality.
newtype ConcreteTypeRep = CTR { unCTR :: TypeRep }
    deriving (Eq, Typeable)

-- | \"Concrete\" version of 'typeOf'.
cTypeOf :: Typeable a => a -> ConcreteTypeRep
cTypeOf = fromTypeRep . typeOf

-- | Converts to the underlying 'TypeRep'
toTypeRep :: ConcreteTypeRep -> TypeRep
toTypeRep = unCTR

-- | Converts from the underlying 'TypeRep'
fromTypeRep :: TypeRep -> ConcreteTypeRep
fromTypeRep = CTR

-- show as a normal TypeRep
instance Show ConcreteTypeRep where
  showsPrec i = showsPrec i . unCTR

-- | This instance is guaranteed to be consistent for a single run of the program, but not for multiple runs.
instance Hashable ConcreteTypeRep where
    hashWithSalt salt (CTR (typeRepFingerprint -> Fingerprint w1 w2)) = salt `hashWithSalt` w1 `hashWithSalt` w2

------------- serialization: this uses Gökhan San's construction, from
---- http://www.mail-archive.com/haskell-cafe@haskell.org/msg41134.html
#if MIN_VERSION_base(4,10,0)
type TyConRep = (String, String, String, Int, KindRep)
#else
type TyConRep = (String, String, String)
#endif

toTyConRep :: TyCon -> TyConRep
fromTyConRep :: TyConRep -> TyCon

#if MIN_VERSION_base(4,10,0)
toTyConRep tc = (tyConPackage tc, tyConModule tc, tyConName tc, tyConKindArgs tc, tyConKindRep tc)
#else
toTyConRep tc = (tyConPackage tc, tyConModule tc, tyConName tc)
#endif

#if MIN_VERSION_base(4,10,0)
fromTyConRep (pack, mod', name, ka, kr) = mkTyCon pack mod' name ka kr
#else
fromTyConRep (pack, mod', name) = mkTyCon3 pack mod' name
#endif

newtype SerialRep = SR (TyConRep, [SerialRep])
    deriving (Binary)

toSerial :: ConcreteTypeRep -> SerialRep
toSerial (CTR t) =
  case splitTyConApp t of
    (con, args) -> SR (toTyConRep con, map (toSerial . CTR) args)

fromSerial :: SerialRep -> ConcreteTypeRep
#if MIN_VERSION_base(4,10,0)
fromSerial (SR (con, args)) = CTR . SomeTypeRep $ mkTrCon (fromTyConRep con) (map (unCTR . fromSerial) args)
#else
fromSerial (SR (con, args)) = CTR $ mkTyConApp (fromTyConRep con) (map (unCTR . fromSerial) args)
#endif

instance Binary ConcreteTypeRep where
  put = put . toSerial
  get = fromSerial <$> get
