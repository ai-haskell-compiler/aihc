{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

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
    numericRuntimeRep,
  )
where

import Aihc.Fc.Syntax
import Aihc.Parser.Syntax
  ( DataConDecl (..),
    FieldDecl (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    TupleFlavor (..),
    UnqualifiedName (..),
    fromAnnotation,
  )
import Aihc.Parser.Syntax qualified as Surface
import Aihc.Resolve (PackageId (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations (TcAnnotation (..))
import Aihc.Tc.Types
  ( TcType,
    pattern Int16Rep,
    pattern Int32Rep,
    pattern Int64Rep,
    pattern Int8Rep,
    pattern IntRep,
    pattern Word16Rep,
    pattern Word32Rep,
    pattern Word64Rep,
    pattern Word8Rep,
    pattern WordRep,
  )
import Control.Applicative ((<|>))
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

-- | Desugar a surface pattern into a Core alt constructor, pure version.
--
-- Returns the constructor and the names of sub-pattern binders.
-- The caller is responsible for creating proper 'Var' values.
dsPatternPure :: PackageId -> Pattern -> (FcAltCon, [Text])
dsPatternPure _ (PCon name _typeArgs subPats) =
  (DataAlt (constructorOrigin name), map subPatName subPats)
dsPatternPure packageId (PList []) =
  (DataAlt (primitiveOrigin packageId "GHC.Types" "[]"), [])
dsPatternPure packageId (PList (_ : _)) =
  (DataAlt (primitiveOrigin packageId "GHC.Types" ":"), ["_head", "_tail"])
dsPatternPure _ (PInfix lhs op rhs) =
  (DataAlt (constructorOrigin op), [subPatName lhs, subPatName rhs])
dsPatternPure packageId (PTuple flavor subPats) =
  (DataAlt (primitiveOrigin packageId tupleModule (tupleConText flavor (length subPats))), map subPatName subPats)
  where
    tupleModule = case flavor of
      Boxed -> "GHC.Tuple"
      Unboxed -> "GHC.Types"
dsPatternPure _ (PVar uname) =
  (DefaultAlt, [unqualifiedNameText uname])
dsPatternPure _ PWildcard =
  (DefaultAlt, [])
dsPatternPure packageId (PAnn _ann inner) = dsPatternPure packageId inner
dsPatternPure packageId (PParen inner) = dsPatternPure packageId inner
dsPatternPure _ (PLit lit) =
  (dsLiteralAlt lit, [])
dsPatternPure _ _ = (DefaultAlt, [])

primitiveOrigin :: PackageId -> Text -> Text -> FcConstructorId
primitiveOrigin = FcConstructorId

constructorOrigin :: Name -> FcConstructorId
constructorOrigin name =
  fromMaybe (missingConstructorIdentity name) $ do
    resolution <-
      listToMaybe
        [ annotation
        | annotation <- mapMaybe fromAnnotation (nameAnns name),
          resolutionNamespace annotation == ResolutionNamespaceTerm
        ]
    case resolutionTarget resolution of
      ResolvedTopLevel packageId resolved ->
        Just
          ( FcConstructorId
              packageId
              (fromMaybe "" (nameQualifier resolved))
              (nameText resolved)
          )
      ResolvedBuiltin {} -> Nothing
      ResolvedLocal {} -> Nothing
      ResolvedError {} -> Nothing

missingConstructorIdentity :: Name -> FcConstructorId
missingConstructorIdentity name =
  error ("constructor does not have a complete identity: " <> T.unpack (nameToText name))

dsLiteralAlt :: Surface.Literal -> FcAltCon
dsLiteralAlt = go Nothing
  where
    go maybeType lit =
      case lit of
        Surface.LitAnn ann inner -> go (tcAnnType <$> fromAnnotation ann <|> maybeType) inner
        Surface.LitInt n numericType _ -> typed maybeType (LitInt (numericRuntimeRep numericType) n)
        Surface.LitChar c _ -> typed maybeType (LitChar WordRep c)
        Surface.LitCharHash c _ -> typed maybeType (LitChar WordRep c)
        _ -> error ("unsupported checked literal pattern: " <> show lit)
    typed maybeType literal =
      case maybeType of
        Just ty -> LitAlt literal ty
        Nothing -> error ("literal pattern does not have a checked type: " <> show literal)

numericRuntimeRep :: NumericType -> TcType
numericRuntimeRep numericType =
  case numericType of
    TInteger -> IntRep
    TIntHash -> IntRep
    TWordHash -> WordRep
    TInt8Hash -> Int8Rep
    TInt16Hash -> Int16Rep
    TInt32Hash -> Int32Rep
    TInt64Hash -> Int64Rep
    TWord8Hash -> Word8Rep
    TWord16Hash -> Word16Rep
    TWord32Hash -> Word32Rep
    TWord64Hash -> Word64Rep

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
dsDataConPure (RecordCon _docs _ctx conName fields) =
  (unqualifiedNameText conName, sum (map (length . fieldNames) fields))
dsDataConPure (GadtCon {}) = ("<gadt>", 0)
dsDataConPure (TupleCon _docs _ctx flavor fields) =
  (tupleConText flavor (length fields), length fields)
dsDataConPure (UnboxedSumCon _docs _ctx pos arity _field) =
  (unboxedSumConText pos arity, 1)
dsDataConPure (ListCon {}) = ("[]", 0)

tupleConText :: TupleFlavor -> Int -> Text
tupleConText flavor fieldCount =
  case flavor of
    Boxed -> "(" <> T.replicate (max 0 (fieldCount - 1)) "," <> ")"
    Unboxed -> "(#" <> T.replicate (max 0 (fieldCount - 1)) "," <> "#)"

unboxedSumConText :: Int -> Int -> Text
unboxedSumConText pos arity =
  let leftBars = T.replicate (max 0 (pos - 1)) "| "
      rightBars = T.replicate (max 0 (arity - pos)) " |"
   in "(# " <> leftBars <> "_" <> rightBars <> " #)"

-- | Convert a Name to Text.
nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n
