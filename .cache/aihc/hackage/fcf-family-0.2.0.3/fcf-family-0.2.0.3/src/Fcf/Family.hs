{-# LANGUAGE
  AllowAmbiguousTypes,
  DataKinds,
  PolyKinds,
  RankNTypes,
  ScopedTypeVariables,
  StandaloneKindSignatures,
  TypeAbstractions,
  TypeFamilies,
  TypeOperators,
  UndecidableInstances #-}

-- |
-- Description: The family of type families
--
-- = The family of type families
--
-- /fcf-family/ promotes
-- regular type families to first-class families without requiring new symbols
-- that pollute the type namespace nor a centralized organization to decide
-- where the symbol for each family should be defined.
--
-- All we need is a single symbol 'Family' indexed by a qualified name that
-- uniquely identifies an existing type family: package, module, and base name.
--
-- Example names:
--
-- @
-- -- GHC.TypeNats.+
-- 'MkName' \"base\" \"GHC.TypeNats\" \"+\"
--
-- -- Data.Type.Bool.If
-- 'MkName' \"base\" \"Data.Type.Bool\" \"If\"
-- @
--
-- == Promoting a type family to first-class
--
-- For example, the type family @('GHC.TypeNats.+')@ is promoted using the following
-- 'Eval' instance for 'Family':
--
-- @
-- type (::+) = 'MkName' \"base\" \"GHC.TypeNats\" \"+\" -- 'Name' of (+)
-- type instance 'Eval' ('Family_' (::+) _ '(x, '(y, '())))) = x 'GHC.TypeNats.+' y
-- @
--
-- The necessary instances can be generated using 'Fcf.Family.TH.fcfify' from the
-- module "Fcf.Family.TH".
--
-- @
-- 'Fcf.Family.TH.fcfify' \'\'+
-- @
--
-- The name of the family can be quoted using 'Fcf.Family.TH.familyName'.
--
-- @
-- type (::+) = $(pure ('Fcf.Family.TH.familyName' \'\'+))
-- @
--
-- Two modules may invoke 'Fcf.Family.TH.fcfify' on the same name without conflict.
-- Identical type family instances will be generated, which is allowed.
--
-- Examples:
--
-- @
-- 2 'GHC.TypeNats.+' 3
-- ~
-- 'Eval' ('Family' ('MkName' \"base\" \"GHC.TypeNats\" \"+\") 'P0' '(2, '(3, '())))
-- @
--
-- @
-- 'Data.Type.Bool.If' a b c
-- ~
-- 'Eval' ('Family' ('MkName' \"base\" \"Data.Type.Bool\" \"If\") 'P1' '(a, '(b, '(c, \()))))
-- @
--
-- == Details
--
-- The type of 'Family' is an uncurried version of the original type family:
--
-- @
-- 'Family' (::+) _ :: (Nat, (Nat, ())) -> Exp Nat
-- ('GHC.TypeNats.+') :: Nat -> Nat -> Nat
-- @
--
-- Tuples @(,)@ and @()@ are used as the cons and nil of heterogeneous lists of arguments.
--
-- The signature of the relevant family is encoded by implementing the following
-- type instances:
--
-- @
-- -- Auxiliary instances.
-- type instance 'Params' (::+) = ()
-- type instance 'Args_' (::+) _ = (Nat, (Nat, ()))
-- type instance 'Res_' (::+) _ _ = Nat
-- @
--
-- 'Args_' and 'Res_' denote the types of arguments and result of the given type family.
-- 'Params' denotes the type of implicit parameters (there are none here
-- since @('GHC.TypeNats.+')@ is monomorphic).
-- The type of explicit arguments ('Args_') may depend on the implicit parameters ('Params').
-- The result type ('Res_') may depend on both 'Params' and 'Args_'.
--
-- === Untyped inside, typed outside  #wrappers#
--
-- These families are intended to be very dependent (the type of 'Family' depends on 'Res' depends on 'Args' depends on 'Params').
-- However, the implementation of this library must work around some technical limitations of GHC.
-- 'Args_' and 'Res_' are actually "untyped" to make GHC more lenient in type
-- checking their instances. \"Typed\" wrappers, 'Family', 'Res', 'Args' are
-- then provided to invoke those families with their intended types,
-- which allows type inference to work.
--
-- To recap: define instances of 'Params', 'Args_', 'Res_', 'Family_',
-- but to invoke the latter three, use 'Args', 'Res', and 'Family' instead.
--
-- == Implicit parameters
--
-- An example using implicit parameters is @'Data.Type.Bool.If' :: forall k. Bool -> k -> k -> k@.
--
-- @
-- type If_ = 'MkName' \"base\" \"Data.Type.Bool\" \"If\"
-- type instance 'Eval' ('Family_' If_ _ '(b, '(x, '(y, '())))) = 'Data.Type.Bool.If' b x y
--
-- type instance 'Params' If_ = ('Data.Kind.Type', ())  -- Type of the implicit parameter
-- type instance 'Args_' If_ k = (Bool, (k, (k, ())      -- Types of the explicit arguments
-- type instance 'Res_' If_ k _ = k                      -- Type of the result
-- @
--
-- The second argument of 'Family_' is a proxy that carries the implicit parameters
-- in its type. For example, the type of @'Family' If_@ is really:
--
-- @
-- 'Family' If_ (_ :: Proxy k) :: (Bool, (k, (k, ()))) -> k
-- @
--
-- When using 'Family', apply it to a proxy encoding the number of implicit parameters
-- in unary using 'P0' and 'PS'.
--
-- For example, use 'P0' for the monomorphic @('GHC.TypeNats.+')@
-- and 'P1' (equal to @'PS' 'P0'@) for 'Data.Type.Bool.If'.

module Fcf.Family
  ( -- * Main definitions

    -- ** First-class families
    -- $reexport
    Exp
  , Eval

    -- ** Family of families
  , Name(..)
  , Family
  , Family_
  , NDFamily
  , NDFamily_

    -- ** Encoding type family signatures
    -- $arity
  , Params
  , Args
  , Args_
  , Res
  , Res_

    -- * Implicit parameters
    -- $tuples
  , ParamsProxy
  , P0, P1, P2, P3, P4, P5, P6, P7
  , PS

    -- * Coercions
    -- $hack
  , Coerce, CoerceTo, CoerceFrom
  , Transp
  ) where

import Data.Kind (Type)
import Data.Proxy (Proxy(..))
import Data.Type.Equality ((:~:)(..))
import GHC.Exts (Any)
import GHC.TypeLits (Symbol)

import Fcf.Core

-- $reexport
-- Reexported from <https://hackage.haskell.org/package/first-class-families first-class-families>.

-- | Qualified name of a type family or type synonym.
data Name = MkName
              Symbol  -- ^ Package name
              Symbol  -- ^ Module name
              Symbol  -- ^ Type name

-- | A general defunctionalization symbol for promoted type families.
-- It encodes saturated applications of type families.
--
-- The second argument (@proxy :: 'ParamsProxy' name ks@) is a gadget
-- carrying implicit parameters (if any). When invoking 'Family', it must be applied to
-- a @proxy@ that corresponds to its number of implicit parameters:
-- 'P0', 'P1', 'P2', ..., 'P7', or a unary encoding using 'P0' and 'PS' for larger implicit arities.
-- (This really matters for arity 2 and more.)
--
-- === __Implementation note__
--
-- This module makes heavy use of dependently typed type families.
-- There is <https://gitlab.haskell.org/ghc/ghc/-/issues/12088 a longstanding issue>
-- which severely limits the usability of such families when done naively.
--
-- The workaround used here is to make the type families themselves "untyped",
-- and wrap them within "typed" synonyms.
--
-- Making the families untyped makes GHC more lenient so that it accepts them.
-- Making the wrappers typed recovers the type inference behavior of the original family.
-- For instance, when using 'Data.Type.Bool.If', one would expect to unify
-- the types of its two branches. That is the case when constructing its
-- defunctionalization symbol using 'Family' and not with 'Family_'.
type Family :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> forall (args :: Args name ks) -> Exp (Res name ks args)
type Family name proxy args = Family_ name proxy args

-- | Non-dependently typed family.
type NDFamily :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> Args name ks -> Exp (Res name ks Any)
type NDFamily name proxy = NDFamily_ name proxy Refl

-- | Untyped internals of 'NDFamily'.
data NDFamily_ :: forall (name :: Name) -> forall (ks :: Params name). ParamsProxy name ks -> Res name ks Any :~: r -> Args name ks -> Exp r
type instance Eval (NDFamily_ name proxy e args) = Transp e (Eval (Coerce (Family name proxy args)))

-- | Untyped internals of 'Family'
data Family_ :: Name -> Proxy ks -> args -> Exp res

-- $arity
-- Parameters should be collected in a heterogeneous list made of @()@ and @(,)@.
--
-- - 0: @()@
-- - 1: @(a, ())@
-- - 2: @(a, (b, ()))@
-- - 3: @(a, (b, (c, ())))@
-- - etc.

-- | Type of implicit parameters of the named family.
type family Params (name :: Name) :: Type

-- | Type of explicit parameters of the named family.
type Args (name :: Name) (ks :: Params name) = Args_ name ks

-- | Untyped internals of 'Args'
type family Args_ (name :: Name) (ks :: ksT) :: Type

-- | Type of result of the named family.
type Res (name :: Name) (ks :: Params name) (args :: Args name ks) = Res_ name ks args

-- | Untyped internals of 'Res'
type family Res_ (name :: Name) (ks :: ksT) (args :: argsT) :: Type

-- * Implicit parameters

-- $tuples
-- We want implicit parameters of polykinded type families to remain implicit
-- in their promotion.
--
-- In 'Family', the parameters are collected in a tuple. To help type inference
-- when using a promoted type family, we instantiate the spine of the tuple
-- using the following proxies.

-- | Synonym to make explicit the dependency of the type of the
-- implicit parameters @ks@ on the @name@ of the family.
type ParamsProxy (name :: Name) (ks :: Params name) = Proxy ks

type P0 = ('Proxy :: Proxy '())
type P1 = PS P0
type P2 = PS P1
type P3 = PS P2
type P4 = PS P3
type P5 = PS P4
type P6 = PS P5
type P7 = PS P6

type PS :: forall kk kks (k :: kk) (ks :: kks). Proxy ks -> Proxy '(k, ks)
type PS p = 'Proxy

-- * Coercions

-- $hack These coercions are part of hacks to work around some limitations in GHC.

-- | Sometimes GHC doesn't see that two type-level values are equal when they
-- ought to be equal. 'Coerce' lets us postpone the check to another day.
type Coerce :: forall k l. k -> l
type family Coerce (a :: k) :: l where
  Coerce x = x

-- | 'Coerce' with explicit codomain.
type CoerceTo l (a :: k) = (Coerce a :: l)

-- | 'Coerce' with explicit domain.
type CoerceFrom k (a :: k) = Coerce a

-- | Transport: coercion along an equality.
type Transp :: forall k l. k :~: l -> k -> l
type family Transp (e :: k :~: l) (x :: k) :: l where
  Transp (_ :: k :~: k) x = x
