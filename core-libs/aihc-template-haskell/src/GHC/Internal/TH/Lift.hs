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

import Data.Char (ord)
import Data.Data hiding (Fixity, Infix)
import Data.Either
import Data.Foldable
import Data.Int
import Data.List.NonEmpty (NonEmpty (..))
import Data.Ratio
import Data.Typeable
import Data.Void
import Data.Word
import Foreign.ForeignPtr
import GHC.Exts
import GHC.Internal.Lexeme (startsVarId, startsVarSym)
import GHC.Internal.TH.Syntax
import Numeric.Natural
import Prelude hiding (Module, Type)

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

instance (Integral a) => Lift (Ratio a) where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

instance Lift Float where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Float# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (FloatPrimL (toRational (F# x))))

instance Lift Double where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (RationalL (toRational x)))

-- | @since template-haskell-2.16.0.0
instance Lift Double# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (DoublePrimL (toRational (D# x))))

instance Lift Char where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (CharL x))

-- | @since template-haskell-2.16.0.0
instance Lift Char# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x = return (LitE (CharPrimL (C# x)))

-- | Produces an 'Addr#' literal from the NUL-terminated C-string starting at
-- the given memory address.
--
-- @since template-haskell-2.16.0.0
instance Lift Addr# where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift x =
    return (LitE (StringPrimL (map (fromIntegral . ord) (unpackCString# x))))

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
{-# RULES "TH:liftString" lift = liftString #-}

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
  lift (# #) = return (UnboxedTupE [])
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a) => Lift (# a #) where
  lift (# a #) = UnboxedTupE . pure . Just <$> lift a
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
  lift value = case value of
    (# a | #) -> liftSum a 1 2
    (# | b #) -> liftSum b 2 2
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c) => Lift (# a | b | c #) where
  lift value = case value of
    (# a | | #) -> liftSum a 1 3
    (# | b | #) -> liftSum b 2 3
    (# | | c #) -> liftSum c 3 3
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d) => Lift (# a | b | c | d #) where
  lift value = case value of
    (# a | | | #) -> liftSum a 1 4
    (# | b | | #) -> liftSum b 2 4
    (# | | c | #) -> liftSum c 3 4
    (# | | | d #) -> liftSum d 4 4
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e) => Lift (# a | b | c | d | e #) where
  lift value = case value of
    (# a | | | | #) -> liftSum a 1 5
    (# | b | | | #) -> liftSum b 2 5
    (# | | c | | #) -> liftSum c 3 5
    (# | | | d | #) -> liftSum d 4 5
    (# | | | | e #) -> liftSum e 5 5
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f) => Lift (# a | b | c | d | e | f #) where
  lift value = case value of
    (# a | | | | | #) -> liftSum a 1 6
    (# | b | | | | #) -> liftSum b 2 6
    (# | | c | | | #) -> liftSum c 3 6
    (# | | | d | | #) -> liftSum d 4 6
    (# | | | | e | #) -> liftSum e 5 6
    (# | | | | | f #) -> liftSum f 6 6
  liftTyped value = unsafeCodeCoerce (lift value)

-- | @since template-haskell-2.16.0.0
instance (Lift a, Lift b, Lift c, Lift d, Lift e, Lift f, Lift g) => Lift (# a | b | c | d | e | f | g #) where
  lift value = case value of
    (# a | | | | | | #) -> liftSum a 1 7
    (# | b | | | | | #) -> liftSum b 2 7
    (# | | c | | | | #) -> liftSum c 3 7
    (# | | | d | | | #) -> liftSum d 4 7
    (# | | | | e | | #) -> liftSum e 5 7
    (# | | | | | f | #) -> liftSum f 6 7
    (# | | | | | | g #) -> liftSum g 7 7
  liftTyped value = unsafeCodeCoerce (lift value)

liftSum :: (Lift a, Quote m) => a -> SumAlt -> SumArity -> m Exp
liftSum value alternative arity = do
  expression <- lift value
  return (UnboxedSumE expression alternative arity)

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
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DocLoc where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift ModName where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift GHC.Internal.TH.Syntax.Module where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameSpace where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NamespaceSpecifier where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PkgName where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameFlavour where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift OccName where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Name where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift NameIs where
  lift Alone = return (ConE (thSyntaxDataName "Alone"))
  lift Applied = return (ConE (thSyntaxDataName "Applied"))
  lift Infix = return (ConE (thSyntaxDataName "Infix"))
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Specificity where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift BndrVis where
  lift = liftData
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
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Type where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Bytes where
  liftTyped x = unsafeCodeCoerce (lift x)
  lift bytes@Bytes {} =
    return
      ( RecConE
          (thSyntaxDataName "Bytes")
          [ ( thSyntaxValueName "bytesPtr",
              AppE
                (AppE (ConE foreignPtrName) (LitE (BytesPrimL bytes)))
                (ConE finalPtrName)
            ),
            (thSyntaxValueName "bytesOffset", LitE (IntegerL 0)),
            (thSyntaxValueName "bytesSize", LitE (IntegerL (fromIntegral (bytesSize bytes))))
          ]
      )

-- | @since template-haskell-2.22.1.0
instance Lift Lit where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Pat where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Clause where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DerivClause where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DerivStrategy where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Overlap where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FunDep where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Safety where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Callconv where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Foreign where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift ForeignSrcLang where
  lift = liftEnumConstructor "GHC.Internal.ForeignSrcLang"
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FixityDirection where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Fixity where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Inline where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift RuleMatch where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Phases where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift RuleBndr where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift AnnTarget where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Pragma where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift SourceStrictness where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift SourceUnpackedness where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift DecidedStrictness where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Bang where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Con where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift TySynEqn where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift FamilyResultSig where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift InjectivityAnn where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift TypeFamilyHead where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Role where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PatSynArgs where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift PatSynDir where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Dec where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Range where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Exp where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift (TExp a) where
  lift (TExp expression) = AppE (ConE (thSyntaxDataName "TExp")) <$> lift expression
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Match where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Guard where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Stmt where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Body where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Info where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift AnnLookup where
  lift = liftData
  liftTyped = unsafeCodeCoerce . lift

-- | @since template-haskell-2.22.1.0
instance Lift Extension where
  lift = liftEnumConstructor "GHC.Internal.LanguageExtensions"
  liftTyped = unsafeCodeCoerce . lift

thSyntaxDataName :: String -> Name
thSyntaxDataName = mkNameG_d "aihc-template-haskell" "GHC.Internal.TH.Syntax"

thSyntaxValueName :: String -> Name
thSyntaxValueName = mkNameG_v "aihc-template-haskell" "GHC.Internal.TH.Syntax"

foreignPtrName :: Name
foreignPtrName = mkNameG_d "ghc-internal" "GHC.Internal.ForeignPtr" "ForeignPtr"

finalPtrName :: Name
finalPtrName = mkNameG_d "ghc-internal" "GHC.Internal.ForeignPtr" "FinalPtr"

liftEnumConstructor :: (Quote m, Show a) => String -> a -> m Exp
liftEnumConstructor moduleName value =
  return (ConE (mkNameG_d "aihc-template-haskell" moduleName (show value)))

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
    Nothing ->
      case constrRep constr of
        AlgConstr _ ->
          appCon (mkCon funOrConName) conArgs
          where
            funOrConName :: Name
            funOrConName =
              case showConstr constr of
                "(:)" ->
                  Name
                    (mkOccName ":")
                    ( NameG
                        DataName
                        (mkPkgName "ghc-prim")
                        (mkModName "GHC.Types")
                    )
                con@"[]" ->
                  Name
                    (mkOccName con)
                    ( NameG
                        DataName
                        (mkPkgName "ghc-prim")
                        (mkModName "GHC.Types")
                    )
                con@('(' : _) ->
                  Name
                    (mkOccName con)
                    ( NameG
                        DataName
                        (mkPkgName "ghc-prim")
                        (mkModName "GHC.Tuple")
                    )
                -- Tricky case: see Note [Data for non-algebraic types]
                fun@(x : _)
                  | startsVarSym x || startsVarId x ->
                      mkNameG_v tyconPkg tyconMod fun
                con -> mkNameG_d tyconPkg tyconMod con
              where
                tycon :: TyCon
                tycon = (typeRepTyCon . typeOf) t

                tyconPkg, tyconMod :: String
                tyconPkg = tyConPackage tycon
                tyconMod = tyConModule tycon

            conArgs :: [m q]
            conArgs = gmapQ (dataToQa mkCon mkLit appCon antiQ) t
        IntConstr n ->
          mkLit $ IntegerL n
        FloatConstr n ->
          mkLit $ RationalL n
        CharConstr c ->
          mkLit $ CharL c
      where
        constr :: Constr
        constr = toConstr t
    Just y -> y

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
dataToExpQ = dataToQa varOrConE litE (foldl appE)
  where
    -- Make sure that VarE is used if the Constr value relies on a
    -- function underneath the surface (instead of a constructor).
    -- See #10796.
    varOrConE s =
      case nameSpace s of
        Just VarName -> return (VarE s)
        Just (FldName {}) -> return (VarE s)
        Just DataName -> return (ConE s)
        _ ->
          error $
            "Can't construct an expression from name "
              ++ showName s
    appE x y = do a <- x; b <- y; return (AppE a b)
    litE c = return (LitE c)

-- | 'liftData' is a variant of 'lift' in the 'Lift' type class which
-- works for any type with a 'Data' instance.
liftData :: (Quote m, Data a) => a -> m Exp
liftData = dataToExpQ (const Nothing)

-- | 'dataToPatQ' converts a value to a 'Pat' representation of the same
-- value, in the SYB style. It takes a function to handle type-specific cases,
-- alternatively, pass @const Nothing@ to get default behavior.
dataToPatQ ::
  (Quote m, Data a) =>
  (forall b. (Data b) => b -> Maybe (m Pat)) ->
  a ->
  m Pat
dataToPatQ = dataToQa id litP conP
  where
    litP l = return (LitP l)
    conP n ps =
      case nameSpace n of
        Just DataName -> do
          ps' <- sequence ps
          return (ConP n [] ps')
        _ ->
          error $
            "Can't construct a pattern from name "
              ++ showName n
