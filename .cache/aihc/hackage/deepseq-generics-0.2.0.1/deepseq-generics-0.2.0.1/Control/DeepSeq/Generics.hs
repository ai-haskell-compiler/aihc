{-# LANGUAGE BangPatterns, TypeOperators, FlexibleContexts #-}

-- |
-- Module:  Control.DeepSeq.Generics
-- Copyright:   (c) 2012, Herbert Valerio Riedel
-- License:     BSD-style (see the LICENSE file)
--
-- Maintainer:  Herbert Valerio Riedel <hvr@gnu.org>
-- Stability:   stable
-- Portability: GHC
--
-- __NOTE__: Starting with @deepseq-1.4.0.0@, 'NFData' gained support
-- for generic derivation via @DefaultSignatures@. The new default
-- 'rnf' method implementation is then equivalent to
--
-- @
-- instance NFData MyType where
--   'rnf' = 'genericRnfV1'
-- @
--
-- See documentation of 'rnf' for more details on how to use the new
-- built-in 'Generic' support.

module Control.DeepSeq.Generics
    ( genericRnf
    , genericRnfV1
    ) where

import Control.DeepSeq
import GHC.Generics

-- | "GHC.Generics"-based 'rnf' implementation
--
-- This provides a generic `rnf` implementation for one type at a
-- time. If the type of the value 'genericRnf' is asked to reduce to
-- NF contains values of other types, those types have to provide
-- 'NFData' instances. This also means that recursive types can only
-- be used with 'genericRnf' if a 'NFData' instance has been defined
-- as well (see examples below).
--
-- The typical usage for 'genericRnf' is for reducing boilerplate code
-- when defining 'NFData' instances for ordinary algebraic
-- datatypes. See the code below for some simple usage examples:
--
-- > {-# LANGUAGE DeriveGeneric #-}
-- >
-- > import Control.DeepSeq
-- > import Control.DeepSeq.Generics (genericRnf)
-- > import GHC.Generics
-- >
-- > -- simple record
-- > data Foo = Foo AccountId Name Address
-- >          deriving Generic
-- >
-- > type Address      = [String]
-- > type Name         = String
-- > newtype AccountId = AccountId Int
-- >
-- > instance NFData AccountId
-- > instance NFData Foo where rnf = genericRnf
-- >
-- > -- recursive list-like type
-- > data N = Z | S N deriving Generic
-- >
-- > instance NFData N where rnf = genericRnf
-- >
-- > -- parametric & recursive type
-- > data Bar a = Bar0 | Bar1 a | Bar2 (Bar a)
-- >            deriving Generic
-- >
-- > instance NFData a => NFData (Bar a) where rnf = genericRnf
--
-- __NOTE__: The 'GNFData' type-class showing up in the type-signature is
--           used internally and not exported.

genericRnf :: (Generic a, GNFData (Rep a)) => a -> ()
genericRnf = grnf_ . from
{-# INLINE genericRnf #-}

-- | Hidden internal type-class
--
-- __NOTE__: the 'V1' instance is not provided for 'GNFData' in order to
-- trigger a compile-time error; see 'GNFDataV1' which defers this to
-- a runtime error.
class GNFData f where
    grnf_ :: f a -> ()

instance GNFData U1 where
    grnf_ U1 = ()

instance NFData a => GNFData (K1 i a) where
    grnf_ = rnf . unK1
    {-# INLINEABLE grnf_ #-}

instance GNFData a => GNFData (M1 i c a) where
    grnf_ = grnf_ . unM1
    {-# INLINEABLE grnf_ #-}

instance (GNFData a, GNFData b) => GNFData (a :*: b) where
    grnf_ (x :*: y) = grnf_ x `seq` grnf_ y
    {-# INLINEABLE grnf_ #-}

instance (GNFData a, GNFData b) => GNFData (a :+: b) where
    grnf_ (L1 x) = grnf_ x
    grnf_ (R1 x) = grnf_ x
    {-# INLINEABLE grnf_ #-}

-- | Variant of 'genericRnf' which supports derivation for uninhabited types.
--
-- For instance, the type
--
-- > data TagFoo deriving Generic
--
-- would cause a compile-time error with 'genericRnf', but with
-- 'genericRnfV1' the error is deferred to run-time:
--
-- > Prelude> genericRnf (undefined :: TagFoo)
-- >
-- > <interactive>:1:1:
-- >     No instance for (GNFData V1) arising from a use of `genericRnf'
-- >     Possible fix: add an instance declaration for (GNFData V1)
-- >     In the expression: genericRnf (undefined :: TagFoo)
-- >     In an equation for `it': it = genericRnf (undefined :: TagFoo)
-- >
-- > Prelude> genericRnfV1 (undefined :: TagFoo)
-- > *** Exception: Control.DeepSeq.Generics.genericRnfV1: uninhabited type
--
-- 'genericRnfV1' corresponds to @deepseq-1.4.0.0@'s default @rnf@
-- method implementation.
--
-- __NOTE__: The 'GNFDataV1' type-class showing up in the type-signature is
--           used internally and not exported.
--
-- @since 0.1.1.0
genericRnfV1 :: (Generic a, GNFDataV1 (Rep a)) => a -> ()
genericRnfV1 = grnfV1_ . from
{-# INLINE genericRnfV1 #-}

-- | Variant of 'GNFData' supporting 'V1'
class GNFDataV1 f where
    grnfV1_ :: f a -> ()

instance GNFDataV1 V1 where
    grnfV1_ = error "Control.DeepSeq.Generics.genericRnfV1: uninhabited type"

instance GNFDataV1 U1 where
    grnfV1_ U1 = ()

instance NFData a => GNFDataV1 (K1 i a) where
    grnfV1_ = rnf . unK1
    {-# INLINEABLE grnfV1_ #-}

instance GNFDataV1 a => GNFDataV1 (M1 i c a) where
    grnfV1_ = grnfV1_ . unM1
    {-# INLINEABLE grnfV1_ #-}

instance (GNFDataV1 a, GNFDataV1 b) => GNFDataV1 (a :*: b) where
    grnfV1_ (x :*: y) = grnfV1_ x `seq` grnfV1_ y
    {-# INLINEABLE grnfV1_ #-}

instance (GNFDataV1 a, GNFDataV1 b) => GNFDataV1 (a :+: b) where
    grnfV1_ (L1 x) = grnfV1_ x
    grnfV1_ (R1 x) = grnfV1_ x
    {-# INLINEABLE grnfV1_ #-}
