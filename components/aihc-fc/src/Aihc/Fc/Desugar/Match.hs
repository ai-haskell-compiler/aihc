{-# LANGUAGE OverloadedStrings #-}

-- | Pattern match desugaring for System FC Core.
--
-- Translates surface patterns into Core case alternative constructors
-- and binder lists. Also handles data declaration desugaring.
--
-- Functions in this module are pure (no monadic state) where possible,
-- or take explicit arguments for fresh name generation.
module Aihc.Fc.Desugar.Match
  ( dsPatternPure,
    dsDataConPure,
  )
where

import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( DataConDecl (..),
    Name (..),
    Pattern (..),
    TupleFlavor (..),
    UnqualifiedName (..),
  )
import Data.Text (Text)
import Data.Text qualified as T

-- | Desugar a surface pattern into a Core alt constructor, pure version.
--
-- Returns the constructor and the names of sub-pattern binders.
-- The caller is responsible for creating proper 'Var' values.
dsPatternPure :: Pattern -> (FcAltCon, [Text])
dsPatternPure (PCon name _typeArgs subPats) =
  let conName = nameToText name
      binderNames = map subPatName subPats
   in (DataAlt conName, binderNames)
dsPatternPure (PVar uname) =
  (DefaultAlt, [unqualifiedNameText uname])
dsPatternPure PWildcard =
  (DefaultAlt, [])
dsPatternPure (PAnn _ann inner) = dsPatternPure inner
dsPatternPure (PParen inner) = dsPatternPure inner
dsPatternPure (PLit _lit) =
  (DefaultAlt, [])
dsPatternPure _ = (DefaultAlt, [])

-- | Extract a name from a sub-pattern.
subPatName :: Pattern -> Text
subPatName (PVar uname) = unqualifiedNameText uname
subPatName PWildcard = "_"
subPatName (PAnn _ann inner) = subPatName inner
subPatName (PParen inner) = subPatName inner
subPatName _ = "_pat"

-- | Desugar a data constructor declaration (pure).
--
-- Returns @(constructor name, number of fields)@.
dsDataConPure :: DataConDecl -> (Text, Int)
dsDataConPure (DataConAnn _ inner) = dsDataConPure inner
dsDataConPure (PrefixCon _docs _ctx conName args) =
  (unqualifiedNameText conName, length args)
dsDataConPure (InfixCon _docs _ctx _lhs conName _rhs) =
  (unqualifiedNameText conName, 2)
dsDataConPure (RecordCon _docs _ctx conName _fields) =
  (unqualifiedNameText conName, 0)
dsDataConPure (TupleCon _docs _ctx flavor fields) =
  (tupleConstructorText flavor (length fields), length fields)
dsDataConPure (ListCon {}) =
  ("[]", 0)
dsDataConPure (UnboxedSumCon _docs _ctx pos arity _field) =
  (unboxedSumConstructorText pos arity, 1)
dsDataConPure (GadtCon {}) = ("<gadt>", 0)

tupleConstructorText :: TupleFlavor -> Int -> Text
tupleConstructorText flavor arity =
  case flavor of
    Boxed -> "(" <> T.replicate (max 0 (arity - 1)) "," <> ")"
    Unboxed -> "(#" <> T.replicate (max 0 (arity - 1)) "," <> "#)"

unboxedSumConstructorText :: Int -> Int -> Text
unboxedSumConstructorText pos arity =
  "(#"
    <> T.intercalate " " [if i == pos then "x" else "|" | i <- [1 .. arity]]
    <> "#)"

-- | Convert a Name to Text.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n
