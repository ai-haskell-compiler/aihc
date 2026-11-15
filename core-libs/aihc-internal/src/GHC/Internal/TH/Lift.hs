-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnboxedSums #-}
{-# LANGUAGE UnboxedTuples #-}
{-# OPTIONS_GHC -fno-warn-inline-rule-shadowing #-}
-- we want users to import Language.Haskell.TH.Syntax instead
{-# OPTIONS_HADDOCK not-home #-}

-- | This module gives the definition of the 'Lift' class.
--
-- This is an internal module.
-- Please import "Language.Haskell.TH" or "Language.Haskell.TH.Syntax" instead!
module GHC.Internal.TH.Lift
  ( Lift (..),

    -- * Generic Lift implementations
    dataToQa,
    dataToExpQ,
    liftData,
    dataToPatQ,

    -- * Wired-in names
    liftString,
    trueName,
    falseName,
    nothingName,
    justName,
    leftName,
    rightName,
    nonemptyName,
  )
where

import Data.Array.Byte (ByteArray)
import Data.Char (isUpper)
import Data.Data hiding (Fixity, Infix)
import Data.Either
import Data.Fixed (Fixed (MkFixed))
import Data.Foldable
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Void
import Data.Word
import GHC.Exts
import GHC.Internal.TH.Syntax
import Numeric.Natural
import Prelude

-- | A 'Lift' instance can have any of its values turned into a Template
-- Haskell expression. This is needed when a value used within a Template
-- Haskell quotation is bound outside the Oxford brackets (@[| ... |]@ or
-- @[|| ... ||]@) but not at the top level. As an example:
--
-- > add1 :: Int -> Code Q Int
-- > add1 x = [|| x + 1 ||]
--
-- Template Haskell has no way of knowing what value @x@ will take on at
-- splice-time, so it requires the type of @x@ to be an instance of 'Lift'.
--
-- A 'Lift' instance must satisfy @$(lift x) ≡ x@ and @$$(liftTyped x) ≡ x@
-- for all @x@, where @$(...)@ and @$$(...)@ are Template Haskell splices.
-- It is additionally expected that @'lift' x ≡ 'unTypeCode' ('liftTyped' x)@.
--
-- 'Lift' instances can be derived automatically by use of the @-XDeriveLift@
-- GHC language extension:
--
-- > {-# LANGUAGE DeriveLift #-}
-- > module Foo where
-- >
-- > import Language.Haskell.TH.Syntax
-- >
-- > data Bar a = Bar1 a (Bar a) | Bar2 String
-- >   deriving Lift
--
-- Representation-polymorphic since /template-haskell-2.16.0.0/.
class Lift (t :: TYPE r) where
  -- | Turn a value into a Template Haskell expression, suitable for use in
  -- a splice.
  lift :: (Quote m) => t -> m Exp
  default lift :: (r ~ ('BoxedRep 'Lifted), Quote m) => t -> m Exp
  lift = unTypeCode . liftTyped

  -- | Turn a value into a Template Haskell typed expression, suitable for use
  -- in a typed splice.
  --
  -- @since template-haskell-2.16.0.0
  liftTyped :: (Quote m) => t -> Code m t

-----------------------------------------------------
--
--      Manual instances for lifting to Literals
--
-----------------------------------------------------

-- If you add any instances here, consider updating test th/TH_Lift
instance Lift Integer where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL x))

instance Lift Int where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

-- | @since template-haskell-2.16.0.0
instance Lift Int# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntPrimL (fromIntegral (I# x))))

instance Lift Int8 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int16 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int32 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Int64 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

-- | @since template-haskell-2.16.0.0
instance Lift Word# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (WordPrimL (fromIntegral (W# x))))

instance Lift Word where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word8 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word16 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word32 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Word64 where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

instance Lift Natural where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (IntegerL (fromIntegral x)))

-- | GHC lifts a byte array through the primitive bytes literal, which this
-- standin has no builder for. The instance is still declared, because a
-- datatype with a byte array field derives its own instance from it.
instance Lift ByteArray where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift _ = error "Template Haskell ByteArray lifting is not available"

instance (Integral a) => Lift (Ratio a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.17.0.0
instance Lift (Fixed a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift (MkFixed units) = AppE (ConE mkFixedName) <$> lift units

mkFixedName :: Name
mkFixedName = mkNameG_d "ghc-internal" "GHC.Internal.Data.Fixed" "MkFixed"

instance Lift Float where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Double where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Char where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (CharL x))

-- | @since template-haskell-2.16.0.0
instance (Lift a) => Lift [a] where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift xs = do xs' <- mapM lift xs; return (ListE xs')

liftString :: (Quote m) => String -> m Exp
-- Used in GHC.Tc.Gen.Expr to short-circuit the lifting for strings
liftString s = return (LitE (StringL s))

-- TH has a special form for literal strings,
-- which we should take advantage of.
-- NB: the lhs of the rule has no args, so that
--     the rule will apply to a 'lift' all on its own
--     which happens to be the way the type checker
--     creates it.
-- SG: This RULE is tested by T3600.
--     In #24983 I advocated defining an overlapping instance
--     to replace this RULE. However, doing so breaks drv023
--     which would need to declare an instance derived from `Lift @[a]` as
--     incoherent. So this RULE it is.

-----------------------------------------------------
--
--      Derived instances for base data types
--
-----------------------------------------------------

instance Lift Bool where
  lift True = return (ConE trueName)
  lift False = return (ConE falseName)
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a) => Lift (Maybe a) where
  lift Nothing = return (ConE nothingName)
  lift (Just x) = AppE (ConE justName) <$> lift x
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b) => Lift (Either a b) where
  lift (Left x) = AppE (ConE leftName) <$> lift x
  lift (Right x) = AppE (ConE rightName) <$> lift x
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.15.0.0
instance (Lift a) => Lift (NonEmpty a) where
  lift (x :| xs) = do
    x' <- lift x
    xs' <- lift xs
    return (AppE (AppE (ConE nonemptyName) x') xs')
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.15.0.0
instance Lift Void where
  lift x = absurd x
  liftTyped x = absurd x

instance Lift () where
  lift () = return (ConE (tupleDataName 0))
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b) => Lift (a, b) where
  lift (a, b) = liftTuple (tupleDataName 2) [lift a, lift b]
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b, Lift c) => Lift (a, b, c) where
  lift (a, b, c) = liftTuple (tupleDataName 3) [lift a, lift b, lift c]
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b, Lift c, Lift d) => Lift (a, b, c, d) where
  lift (a, b, c, d) = liftTuple (tupleDataName 4) [lift a, lift b, lift c, lift d]
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b, Lift c, Lift d, Lift e) => Lift (a, b, c, d, e) where
  lift (a, b, c, d, e) = liftTuple (tupleDataName 5) [lift a, lift b, lift c, lift d, lift e]
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f) => Lift (a, b, c, d, e, f) where
  lift (a, b, c, d, e, f) = liftTuple (tupleDataName 6) [lift a, lift b, lift c, lift d, lift e, lift f]
  liftTyped = unsafeCodeCoerce . lift

instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g) => Lift (a, b, c, d, e, f, g) where
  lift (a, b, c, d, e, f, g) = liftTuple (tupleDataName 7) [lift a, lift b, lift c, lift d, lift e, lift f, lift g]
  liftTyped = unsafeCodeCoerce . lift

liftTuple :: (Quote m) => Name -> [m Exp] -> m Exp
liftTuple constructor arguments = do
  expressions <- sequence arguments
  return (foldl AppE (ConE constructor) expressions)

-- | @since template-haskell-2.16.0.0
instance Lift (# #) where
  lift _ = error "Template Haskell unboxed unit lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a) => Lift (# a #) where
  lift _ = error "Template Haskell unboxed unary tuple lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b) => Lift (# a, b #) where
  lift (# a, b #) = do a' <- lift a; b' <- lift b; return (UnboxedTupE [Just a', Just b'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c) => Lift (# a, b, c #) where
  lift (# a, b, c #) = do a' <- lift a; b' <- lift b; c' <- lift c; return (UnboxedTupE [Just a', Just b', Just c'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d) => Lift (# a, b, c, d #) where
  lift (# a, b, c, d #) = do a' <- lift a; b' <- lift b; c' <- lift c; d' <- lift d; return (UnboxedTupE [Just a', Just b', Just c', Just d'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e) => Lift (# a, b, c, d, e #) where
  lift (# a, b, c, d, e #) = do a' <- lift a; b' <- lift b; c' <- lift c; d' <- lift d; e' <- lift e; return (UnboxedTupE [Just a', Just b', Just c', Just d', Just e'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f) => Lift (# a, b, c, d, e, f #) where
  lift (# a, b, c, d, e, f #) = do a' <- lift a; b' <- lift b; c' <- lift c; d' <- lift d; e' <- lift e; f' <- lift f; return (UnboxedTupE [Just a', Just b', Just c', Just d', Just e', Just f'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g) => Lift (# a, b, c, d, e, f, g #) where
  lift (# a, b, c, d, e, f, g #) = do a' <- lift a; b' <- lift b; c' <- lift c; d' <- lift d; e' <- lift e; f' <- lift f; g' <- lift g; return (UnboxedTupE [Just a', Just b', Just c', Just d', Just e', Just f', Just g'])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b) => Lift (# a | b #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c) => Lift (# a | b | c #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d) => Lift (# a | b | c | d #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e) => Lift (# a | b | c | d | e #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f) => Lift (# a | b | c | d | e | f #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g) => Lift (# a | b | c | d | e | f | g #) where
  lift _ = error "Template Haskell sum lifting is not available"
  liftTyped value = unsafeCodeCoerce (lift value)

trueName, falseName :: Name
trueName = mkNameG_d "ghc-prim" "GHC.Types" "True"
falseName = mkNameG_d "ghc-prim" "GHC.Types" "False"

nothingName, justName :: Name
nothingName = mkNameG_d "ghc-internal" "GHC.Internal.Maybe" "Nothing"
justName = mkNameG_d "ghc-internal" "GHC.Internal.Maybe" "Just"

leftName, rightName :: Name
leftName = mkNameG_d "ghc-internal" "GHC.Internal.Data.Either" "Left"
rightName = mkNameG_d "ghc-internal" "GHC.Internal.Data.Either" "Right"

nonemptyName :: Name
nonemptyName = mkNameG_d "ghc-internal" "GHC.Internal.Base" ":|"

-----------------------------------------------------
--
--              Lifting the TH AST
--
-----------------------------------------------------

-- | @since template-haskell-2.22.1.0
instance Lift Loc where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DocLoc where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift ModName where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift GHC.Internal.TH.Syntax.Module where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameSpace where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NamespaceSpecifier where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PkgName where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameFlavour where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift OccName where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Name where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameIs where
  lift Alone = return (ConE (thSyntaxDataName "Alone"))
  lift Applied = return (ConE (thSyntaxDataName "Applied"))
  lift Infix = return (ConE (thSyntaxDataName "Infix"))
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Specificity where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift BndrVis where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance (Lift a) => Lift (TyVarBndr a) where
  lift (PlainTV name flag) = do
    name' <- lift name
    flag' <- lift flag
    return (AppE (AppE (ConE (thSyntaxDataName "PlainTV")) name') flag')
  lift (KindedTV name flag kind) = do
    name' <- lift name
    flag' <- lift flag
    kind' <- lift kind
    return (AppE (AppE (AppE (ConE (thSyntaxDataName "KindedTV")) name') flag') kind')
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift TyLit where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Type where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Bytes where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift _ = error "Template Haskell Bytes lifting is not available"

-- | @since template-haskell-2.22.1.0
instance Lift Lit where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Pat where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Clause where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DerivClause where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DerivStrategy where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Overlap where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FunDep where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Safety where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Callconv where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Foreign where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift ForeignSrcLang where
  lift = liftEnumeration
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FixityDirection where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Fixity where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Inline where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift RuleMatch where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Phases where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift RuleBndr where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift AnnTarget where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Pragma where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift SourceStrictness where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift SourceUnpackedness where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DecidedStrictness where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Bang where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Con where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift TySynEqn where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FamilyResultSig where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift InjectivityAnn where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift TypeFamilyHead where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Role where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PatSynArgs where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PatSynDir where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Dec where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Range where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Exp where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift (TExp a) where
  lift (TExp expression) = AppE (ConE (thSyntaxDataName "TExp")) <$> lift expression
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Match where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Guard where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Stmt where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Body where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Info where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift AnnLookup where
  lift = liftSyntax
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Extension where
  lift = liftEnumeration
  liftTyped = unsafeCodeCoerce . lift

-- | The name of a data constructor of "GHC.Internal.TH.Syntax". The package
-- of a global name is the package id of the compiler, which has a version.
thSyntaxDataName :: String -> Name
thSyntaxDataName = mkNameG_d (tyConPackage syntaxTyCon) (tyConModule syntaxTyCon)
  where
    syntaxTyCon = typeRepTyCon (typeRep (Proxy :: Proxy Exp))

-- | Lift a value of the syntax tree of Template Haskell. GHC derives these
-- instances. Here, the generic lift of 'Data' gives the same expression:
-- each constructor is a global name of its package and module, applied to
-- its lifted fields.
liftSyntax :: (Quote m, Data a) => a -> m Exp
liftSyntax = dataToExpQ (\value -> fmap lift (cast value :: Maybe Bytes))

-- | Lift a value of an enumeration type that has no 'Data' instance. The
-- derived 'Show' instance gives the name of the constructor.
liftEnumeration :: forall m a. (Quote m, Show a, Typeable a) => a -> m Exp
liftEnumeration value = return (ConE (mkNameG_d (tyConPackage tyCon) (tyConModule tyCon) (show value)))
  where
    tyCon = typeRepTyCon (typeRep (Proxy :: Proxy a))

-----------------------------------------------------
--
--              Generic Lift implementations
--
-----------------------------------------------------

-- | 'dataToQa' is an internal utility function for constructing generic
-- conversion functions from types with 'Data' instances to various
-- quasi-quoting representations.  See the source of 'dataToExpQ' and
-- 'dataToPatQ' for two example usages: @mkCon@, @mkLit@
-- and @appQ@ are overloadable to account for different syntax for
-- expressions and patterns; @antiQ@ allows you to override type-specific
-- cases, a common usage is just @const Nothing@, which results in
-- no overloading.
dataToQa ::
  forall m a k q.
  (Quote m, Data a) =>
  (Name -> k) ->
  (Lit -> m q) ->
  (k -> [m q] -> m q) ->
  (forall b. (Data b) => b -> Maybe (m q)) ->
  a ->
  m q
dataToQa mkCon mkLit appCon antiQ t =
  case antiQ t of
    Just y -> y
    Nothing ->
      case constrRep constr of
        AlgConstr _ -> appCon (mkCon (constructorName t constr)) (gmapQ (dataToQa mkCon mkLit appCon antiQ) t)
        IntConstr n -> mkLit (IntegerL n)
        FloatConstr n -> mkLit (RationalL n)
        CharConstr c -> mkLit (CharL c)
  where
    constr = toConstr t

-- | The name of a constructor of a value. The list constructors and the
-- tuple constructors are built-in syntax, so their names have no module.
-- Other names come from the package and the module of the type of the
-- value. A constructor that starts with a lower-case letter or a symbol
-- is an ordinary function, as for the abstract types in
-- Note [Data for non-algebraic types].
constructorName :: (Data a) => a -> Constr -> Name
constructorName t constr =
  case showConstr constr of
    "(:)" -> mkNameS ":"
    spelling@('(' : _) -> mkNameS spelling
    spelling@"[]" -> mkNameS spelling
    spelling@(first : _)
      | first /= ':' && not (isUpper first) -> mkNameG_v package home spelling
    spelling -> mkNameG_d package home spelling
  where
    tyCon = typeRepTyCon (typeOf t)
    package = tyConPackage tyCon
    home = tyConModule tyCon

-- | Whether a constructor is a constructor of a list.
isListConstr :: Constr -> Bool
isListConstr constr = showConstr constr == "(:)" || showConstr constr == "[]"

-- | Whether a constructor is a constructor of a boxed tuple or of the unit.
isTupleConstr :: Constr -> Bool
isTupleConstr constr =
  case showConstr constr of
    '(' : rest -> all (== ',') (takeWhile (/= ')') rest)
    _ -> False

-- | The elements of a list, each with a function applied.
listElements :: (Data a) => (forall b. (Data b) => b -> r) -> a -> [r]
listElements f list
  | showConstr (toConstr list) == "(:)" = gmapQi 0 f list : gmapQi 1 (listElements f) list
  | otherwise = []

{- Note [Data for non-algebraic types]
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Class Data was originally intended for algebraic data types.  But
it is possible to use it for abstract types too.  For example, in
package `text` we find

  instance Data Text where
    ...
    toConstr _ = packConstr

  packConstr :: Constr
  packConstr = mkConstr textDataType "pack" [] Prefix

Here `packConstr` isn't a real data constructor, it's an ordinary
function.  Two complications

\* In such a case, we must take care to build the Name using
  mkNameG_v (for values), not mkNameG_d (for data constructors).
  See #10796.

\* The pseudo-constructor is named only by its string, here "pack".
  But 'dataToQa' needs the TyCon of its defining module, and has
  to assume it's defined in the same module as the TyCon itself.
  But nothing enforces that; #12596 shows what goes wrong if
  "pack" is defined in a different module than the data type "Text".
  -}

-- | 'dataToExpQ' converts a value to a 'Exp' representation of the
-- same value, in the SYB style. It is generalized to take a function
-- override type-specific cases; see 'liftData' for a more commonly
-- used variant.
dataToExpQ ::
  forall m a.
  (Quote m, Data a) =>
  (forall b. (Data b) => b -> Maybe (m Exp)) ->
  a ->
  m Exp
dataToExpQ antiQ = go
  where
    go :: forall b. (Data b) => b -> m Exp
    go t =
      case antiQ t of
        Just y -> y
        Nothing ->
          case constrRep constr of
            AlgConstr _
              | isListConstr constr -> ListE <$> sequence (listElements go t)
              | isTupleConstr constr -> TupE . map Just <$> sequence (gmapQ go t)
              | otherwise -> foldl (\left right -> AppE <$> left <*> right) (return (varOrConE (constructorName t constr))) (gmapQ go t)
            IntConstr n -> return (LitE (IntegerL n))
            FloatConstr n -> return (LitE (RationalL n))
            CharConstr c -> return (LitE (CharL c))
          where
            constr = toConstr t
    varOrConE name =
      case nameSpace name of
        Just DataName -> ConE name
        Nothing | isConstructorName name -> ConE name
        _ -> VarE name
    isConstructorName name =
      case nameBase name of
        first : _ -> first == ':' || isUpper first || first == '(' || first == '['
        [] -> False

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: (Quote m, Data a) => a -> m Exp
liftData = dataToExpQ (const Nothing)

-- | 'dataToPatQ' converts a value to a 'Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ ::
  forall m a.
  (Quote m, Data a) =>
  (forall b. (Data b) => b -> Maybe (m Pat)) ->
  a ->
  m Pat
dataToPatQ antiQ = go
  where
    go :: forall b. (Data b) => b -> m Pat
    go t =
      case antiQ t of
        Just y -> y
        Nothing ->
          case constrRep constr of
            AlgConstr _
              | isListConstr constr -> ListP <$> sequence (listElements go t)
              | isTupleConstr constr -> TupP <$> sequence (gmapQ go t)
              | otherwise -> ConP (constructorName t constr) [] <$> sequence (gmapQ go t)
            IntConstr n -> return (LitP (IntegerL n))
            FloatConstr n -> return (LitP (RationalL n))
            CharConstr c -> return (LitP (CharL c))
          where
            constr = toConstr t
