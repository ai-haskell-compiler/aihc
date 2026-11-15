-- SPDX-License-Identifier: BSD-3-Clause
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveLift #-}
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
deriving instance Lift Loc

-- | @since template-haskell-2.22.1.0
deriving instance Lift DocLoc

-- | @since template-haskell-2.22.1.0
deriving instance Lift ModName

-- | @since template-haskell-2.22.1.0
deriving instance Lift GHC.Internal.TH.Syntax.Module

-- | @since template-haskell-2.22.1.0
deriving instance Lift NameSpace

-- | @since template-haskell-2.22.1.0
deriving instance Lift NamespaceSpecifier

-- | @since template-haskell-2.22.1.0
deriving instance Lift PkgName

-- | @since template-haskell-2.22.1.0
deriving instance Lift NameFlavour

-- | @since template-haskell-2.22.1.0
deriving instance Lift OccName

-- | @since template-haskell-2.22.1.0
deriving instance Lift Name

-- | @since template-haskell-2.22.1.0
instance Lift NameIs where
  lift Alone = return (ConE (thSyntaxDataName "Alone"))
  lift Applied = return (ConE (thSyntaxDataName "Applied"))
  lift Infix = return (ConE (thSyntaxDataName "Infix"))
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
deriving instance Lift Specificity

-- | @since template-haskell-2.22.1.0
deriving instance Lift BndrVis

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
deriving instance Lift TyLit

-- | @since template-haskell-2.22.1.0
deriving instance Lift Type

-- | @since template-haskell-2.22.1.0
instance Lift Bytes where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift _ = error "Template Haskell Bytes lifting is not available"

-- | @since template-haskell-2.22.1.0
deriving instance Lift Lit

-- | @since template-haskell-2.22.1.0
deriving instance Lift Pat

-- | @since template-haskell-2.22.1.0
deriving instance Lift Clause

-- | @since template-haskell-2.22.1.0
deriving instance Lift DerivClause

-- | @since template-haskell-2.22.1.0
deriving instance Lift DerivStrategy

-- | @since template-haskell-2.22.1.0
deriving instance Lift Overlap

-- | @since template-haskell-2.22.1.0
deriving instance Lift FunDep

-- | @since template-haskell-2.22.1.0
deriving instance Lift Safety

-- | @since template-haskell-2.22.1.0
deriving instance Lift Callconv

-- | @since template-haskell-2.22.1.0
deriving instance Lift Foreign

-- | @since template-haskell-2.22.1.0
deriving instance Lift ForeignSrcLang

-- | @since template-haskell-2.22.1.0
deriving instance Lift FixityDirection

-- | @since template-haskell-2.22.1.0
deriving instance Lift Fixity

-- | @since template-haskell-2.22.1.0
deriving instance Lift Inline

-- | @since template-haskell-2.22.1.0
deriving instance Lift RuleMatch

-- | @since template-haskell-2.22.1.0
deriving instance Lift Phases

-- | @since template-haskell-2.22.1.0
deriving instance Lift RuleBndr

-- | @since template-haskell-2.22.1.0
deriving instance Lift AnnTarget

-- | @since template-haskell-2.22.1.0
deriving instance Lift Pragma

-- | @since template-haskell-2.22.1.0
deriving instance Lift SourceStrictness

-- | @since template-haskell-2.22.1.0
deriving instance Lift SourceUnpackedness

-- | @since template-haskell-2.22.1.0
deriving instance Lift DecidedStrictness

-- | @since template-haskell-2.22.1.0
deriving instance Lift Bang

-- | @since template-haskell-2.22.1.0
deriving instance Lift Con

-- | @since template-haskell-2.22.1.0
deriving instance Lift TySynEqn

-- | @since template-haskell-2.22.1.0
deriving instance Lift FamilyResultSig

-- | @since template-haskell-2.22.1.0
deriving instance Lift InjectivityAnn

-- | @since template-haskell-2.22.1.0
deriving instance Lift TypeFamilyHead

-- | @since template-haskell-2.22.1.0
deriving instance Lift Role

-- | @since template-haskell-2.22.1.0
deriving instance Lift PatSynArgs

-- | @since template-haskell-2.22.1.0
deriving instance Lift PatSynDir

-- | @since template-haskell-2.22.1.0
deriving instance Lift Dec

-- | @since template-haskell-2.22.1.0
deriving instance Lift Range

-- | @since template-haskell-2.22.1.0
deriving instance Lift Exp

-- | @since template-haskell-2.22.1.0
instance Lift (TExp a) where
  lift (TExp expression) = AppE (ConE (thSyntaxDataName "TExp")) <$> lift expression
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
deriving instance Lift Match

-- | @since template-haskell-2.22.1.0
deriving instance Lift Guard

-- | @since template-haskell-2.22.1.0
deriving instance Lift Stmt

-- | @since template-haskell-2.22.1.0
deriving instance Lift Body

-- | @since template-haskell-2.22.1.0
deriving instance Lift Info

-- | @since template-haskell-2.22.1.0
deriving instance Lift AnnLookup

-- | @since template-haskell-2.22.1.0
deriving instance Lift Extension

thSyntaxDataName :: String -> Name
thSyntaxDataName = mkNameG_d "aihc-internal" "GHC.Internal.TH.Syntax"

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
dataToQa _ _ _ _ _ = error "Generic Template Haskell lifting is not available"

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
  (Quote m, Data a) =>
  (forall b. (Data b) => b -> Maybe (m Exp)) ->
  a ->
  m Exp
dataToExpQ _ _ = error "Generic Template Haskell lifting is not available"

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: (Quote m, Data a) => a -> m Exp
liftData _ = error "Generic Template Haskell lifting is not available"

-- | 'dataToPatQ' converts a value to a 'Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ ::
  (Quote m, Data a) =>
  (forall b. (Data b) => b -> Maybe (m Pat)) ->
  a ->
  m Pat
dataToPatQ _ _ = error "Generic Template Haskell lifting is not available"
