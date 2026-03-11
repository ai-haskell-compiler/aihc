{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RequiredTypeArguments #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{-# OPTIONS_GHC -Wno-deprecations #-}

{-|
Module:      Data.Proxyless.RequiredTypeArguments
Copyright:   (C) 2024 Ryan Scott
License:     BSD-style (see the file LICENSE)
Maintainer:  Ryan Scott
Stability:   Provisional
Portability: GHC

Remove the 'Proxy', 'Proxy#', and 'undefined' arguments from functions with
'proxyless', 'proxyHashless', and 'undefinedless', respectively, which produce
functions that take type information via GHC's @-XRequiredTypeArguments@
extension.

See also "Data.Proxyless" for a version of this module that leverages
@-XTypeApplications@ instead of @-XRequiredTypeArguments@.

This module is only available with GHC 9.10 or later.

/Since: 0.3.2/
-}
module Data.Proxyless.RequiredTypeArguments (
      -- * 'proxyless', 'proxyHashless', and 'undefinedless'
      proxyless
    , proxyHashless
    , undefinedless
      -- * Proxyless functions
      -- ** "Data.Bits"
    , theBitSize
    , theIsSigned
    , theBitSizeMaybe
    , theFiniteBitSize
      -- ** "Data.Data"
    , theDataTypeOf
      -- ** "Data.Typeable"
    , theTypeNatTypeRep
    , theTypeRep
    , theTypeRep#
    , theTypeSymbolTypeRep
      -- ** "Foreign.Storable"
    , theSizeOf
    , theAlignment
      -- ** "GHC.Generics"
    , theDatatypeName
    , theModuleName
    , theIsNewtype
    , thePackageName
    , theConName
    , theConFixity
    , theConIsRecord
    , theSelName
    , theSelSourceUnpackedness
    , theSelSourceStrictness
    , theSelDecidedStrictness
      -- ** "GHC.OverloadedLabels"
    , theFromLabel
      -- ** "GHC.TypeLits"
    , theNatVal
    , theNatVal'
    , theSameNat
    , theSameSymbol
    , theSomeNat
    , theSomeSymbol
    , theSymbolVal
    , theSymbolVal'
      -- ** "Prelude"
    , theFloatRadix
    , theFloatDigits
    , theFloatRange
      -- ** "Text.Printf"
    , theParseFormat
    ) where

import Data.Bits (Bits(..), FiniteBits(..))
import Data.Data (Data(dataTypeOf), DataType)
import Data.Proxy (Proxy(..))
import Data.Proxyless (proxyless, proxyHashless, undefinedless)
import Data.Type.Equality ((:~:))
import Data.Typeable (Typeable, TypeRep, typeRep)

import Foreign.Storable (Storable(..))

import GHC.Generics
import GHC.OverloadedLabels (IsLabel(..))
import GHC.TypeLits

import Text.Printf (PrintfArg(..), ModifierParser)

-------------------------------------------------------------------------------
-- Data.Bits
-------------------------------------------------------------------------------

-- | @'theBitSize' = 'undefinedless' 'bitSize'@
--
-- /Since: 0.3.2/
theBitSize :: forall a -> Bits a => Int
theBitSize a = undefinedless @a bitSize

-- | @'theIsSigned' = 'undefinedless' 'isSigned'@
--
-- /Since: 0.3.2/
theIsSigned :: forall a -> Bits a => Bool
theIsSigned a = undefinedless @a isSigned

-- | @'theBitSizeMaybe' = 'undefinedless' 'bitSizeMaybe'@
--
-- /Since: 0.3.2/
theBitSizeMaybe :: forall a -> Bits a => Maybe Int
theBitSizeMaybe a = undefinedless @a bitSizeMaybe

-- | @'theFiniteBitSize' = 'undefinedless' 'finiteBitSize'@
--
-- /Since: 0.3.2/
theFiniteBitSize :: forall a -> FiniteBits a => Int
theFiniteBitSize a = undefinedless @a finiteBitSize

-------------------------------------------------------------------------------
-- Data.Data
-------------------------------------------------------------------------------

-- | @'theDataTypeOf' = 'undefinedless' 'dataTypeOf'@
--
-- /Since: 0.3.2/
theDataTypeOf :: forall a -> Data a => DataType
theDataTypeOf a = undefinedless @a dataTypeOf

-------------------------------------------------------------------------------
-- Data.Typeable
-------------------------------------------------------------------------------

-- | A type-specialized version of 'theTypeRep'.
--
-- /Since: 0.3.2/
theTypeNatTypeRep :: forall a -> KnownNat a => TypeRep
theTypeNatTypeRep a = theTypeRep a

-- | @'theTypeRep' = 'proxyless' 'typeRep'@
--
-- /Since: 0.3.2/
theTypeRep :: forall k. forall (a :: k) -> Typeable a => TypeRep
theTypeRep a = proxyless @_ @a typeRep

-- | A type-specialized version of 'theTypeRep'.
--
-- /Since: 0.3.2/
theTypeRep# :: forall k. forall (a :: k) -> Typeable a => TypeRep
theTypeRep# a = theTypeRep a

-- | A type-specialized version of 'theTypeRep'.
--
-- /Since: 0.3.2/
theTypeSymbolTypeRep :: forall a -> KnownSymbol a => TypeRep
theTypeSymbolTypeRep a = theTypeRep a

-------------------------------------------------------------------------------
-- Foreign.Storable
-------------------------------------------------------------------------------

-- | @'theSizeOf' = 'undefinedless' 'sizeOf'@
--
-- /Since: 0.3.2/
theSizeOf :: forall a -> Storable a => Int
theSizeOf a = undefinedless @a sizeOf

-- | @'theAlignment' = 'undefinedless' 'alignment'@
--
-- /Since: 0.3.2/
theAlignment :: forall a -> Storable a => Int
theAlignment a = undefinedless @a alignment

-------------------------------------------------------------------------------
-- GHC.Generics
-------------------------------------------------------------------------------

-- | @'theDatatypeName' = 'datatypeName' 'undefined'@
--
-- /Since: 0.3.2/
theDatatypeName :: forall k. forall (d :: k) -> Datatype d => [Char]
theDatatypeName d = datatypeName @d undefined

-- | @'theModuleName' = 'moduleName' 'undefined'@
--
-- /Since: 0.3.2/
theModuleName :: forall k. forall (d :: k) -> Datatype d => [Char]
theModuleName d = moduleName @d undefined

-- | @'theIsNewtype' = 'isNewtype' 'undefined'@
--
-- /Since: 0.3.2/
theIsNewtype :: forall k. forall (d :: k) -> Datatype d => Bool
theIsNewtype d = isNewtype @d undefined

-- | @'thePackageName' = 'packageName' 'undefined'@
--
-- /Since: 0.3.2/
thePackageName :: forall k. forall (d :: k) -> Datatype d => [Char]
thePackageName d = packageName @d undefined

-- | @'theConName' = 'conName' 'undefined'@
--
-- /Since: 0.3.2/
theConName :: forall k. forall (c :: k) -> Constructor c => [Char]
theConName c = conName @c undefined

-- | @'theConFixity' = 'conFixity' 'undefined'@
--
-- /Since: 0.3.2/
theConFixity :: forall k. forall (c :: k) -> Constructor c => Fixity
theConFixity c = conFixity @c undefined

-- | @'theConIsRecord' = 'conIsRecord' 'undefined'@
--
-- /Since: 0.3.2/
theConIsRecord :: forall k. forall (c :: k) -> Constructor c => Bool
theConIsRecord c = conIsRecord @c undefined

-- | @'theSelName' = 'selName' 'undefined'@
--
-- /Since: 0.3.2/
theSelName :: forall k. forall (s :: k) -> Selector s => [Char]
theSelName s = selName @s undefined

-- | @'theSelSourceUnpackedness' = 'selSourceUnpackedness' 'undefined'@
--
-- /Since: 0.3.2/
theSelSourceUnpackedness :: forall k. forall (s :: k) -> Selector s => SourceUnpackedness
theSelSourceUnpackedness s = selSourceUnpackedness @s undefined

-- | @'theSelSourceStrictness' = 'selSourceStrictness' 'undefined'@
--
-- /Since: 0.3.2/
theSelSourceStrictness :: forall k. forall (s :: k) -> Selector s => SourceStrictness
theSelSourceStrictness s = selSourceStrictness @s undefined

-- | @'theSelDecidedStrictness' = 'selDecidedStrictness' 'undefined'@
--
-- /Since: 0.3.2/
theSelDecidedStrictness :: forall k. forall (s :: k) -> Selector s => DecidedStrictness
theSelDecidedStrictness s = selDecidedStrictness @s undefined

-------------------------------------------------------------------------------
-- GHC.OverloadedLabels
-------------------------------------------------------------------------------

-- | In @base-4.10@ and later, this is simply a synonym for 'fromLabel'.
-- In @base-4.9@, 'theFromLabel' is defined as:
--
-- @'theFromLabel' = 'proxyHashless' 'fromLabel'@
--
-- /Since: 0.3.2/
theFromLabel :: forall x -> forall a. IsLabel x a => a
theFromLabel x = fromLabel @x

-------------------------------------------------------------------------------
-- GHC.TypeLits
-------------------------------------------------------------------------------

-- | @'theNatVal' = 'proxyless' 'natVal'@
--
-- /Since: 0.3.2/
theNatVal :: forall n -> KnownNat n => Integer
theNatVal n = proxyless @_ @n natVal

-- | @`theNatVal'` = 'proxyHashless' `natVal'`@
--
-- /Since: 0.3.2/
theNatVal' :: forall n -> KnownNat n => Integer
theNatVal' n = proxyHashless @_ @n natVal'

-- | @'theSameNat' = 'sameNat' 'Proxy' 'Proxy'@
--
-- /Since: 0.3.2/
theSameNat :: forall a b -> (KnownNat a, KnownNat b) => Maybe (a :~: b)
theSameNat a b = sameNat (Proxy @a) (Proxy @b)

-- | @'theSameSymbol' = 'sameSymbol' 'Proxy' 'Proxy'@
--
-- /Since: 0.3.2/
theSameSymbol :: forall a b -> (KnownSymbol a, KnownSymbol b) => Maybe (a :~: b)
theSameSymbol a b = sameSymbol (Proxy @a) (Proxy @b)

-- | @'theSomeNat' = 'proxyless' 'SomeNat'@
--
-- /Since: 0.3.2/
theSomeNat :: forall n -> KnownNat n => SomeNat
theSomeNat n = proxyless @_ @n SomeNat

-- | @'theSomeSymbol' = 'proxyless' 'SomeSymbol'@
--
-- /Since: 0.3.2/
theSomeSymbol :: forall n -> KnownSymbol n => SomeSymbol
theSomeSymbol n = proxyless @_ @n SomeSymbol

-- | @'theSymbolVal' = 'proxyless' 'symbolVal'@
--
-- /Since: 0.3.2/
theSymbolVal :: forall n -> KnownSymbol n => String
theSymbolVal n = proxyless @_ @n symbolVal

-- | @`theSymbolVal'` = 'proxyHashless' `symbolVal'`@
--
-- /Since: 0.3.2/
theSymbolVal' :: forall n -> KnownSymbol n => String
theSymbolVal' n = proxyHashless @_ @n symbolVal'

-------------------------------------------------------------------------------
-- Prelude
-------------------------------------------------------------------------------

-- | @'theFloatRadix' = 'undefinedless' 'floatRadix'@
--
-- /Since: 0.3.2/
theFloatRadix :: forall a -> RealFloat a => Integer
theFloatRadix a = undefinedless @a floatRadix

-- | @'theFloatDigits' = 'undefinedless' 'floatDigits'@
--
-- /Since: 0.3.2/
theFloatDigits :: forall a -> RealFloat a => Int
theFloatDigits a = undefinedless @a floatDigits

-- | @'theFloatRange' = 'undefinedless' 'floatRange'@
--
-- /Since: 0.3.2/
theFloatRange :: forall a -> RealFloat a => (Int, Int)
theFloatRange a = undefinedless @a floatRange

-------------------------------------------------------------------------------
-- Text.Printf
-------------------------------------------------------------------------------

-- | @'theParseFormat' = 'undefinedless' 'parseFormat'@
--
-- /Since: 0.3.2/
theParseFormat :: forall a -> PrintfArg a => ModifierParser
theParseFormat a = undefinedless @a parseFormat
