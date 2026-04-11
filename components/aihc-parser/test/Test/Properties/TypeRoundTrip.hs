{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Type (canonicalContextType, canonicalTopLevelType)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr, span0)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
      expected = normalizeType (canonicalTopLevelType ty)
   in checkCoverage $
        withMaxShrinks 100 $
          assertCtorCoverage ["TAnn", "TContext", "TImplicitParam"] ty $
            counterexample (T.unpack source) $
              case parseType typeConfig source of
                ParseErr err ->
                  counterexample (MPE.errorBundlePretty err) False
                ParseOk parsed ->
                  let actual = normalizeType parsed
                   in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TImplicitParam _ name inner -> TImplicitParam span0 name (normalizeType inner)
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TWildcard _ -> TWildcard span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 (map normalizeTyVarBinder binders) (normalizeType inner)
    TApp _ f x -> TApp span0 (normalizeType f) (normalizeType x)
    TFun _ a b -> TFun span0 (normalizeType a) (normalizeType b)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeType elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeType elems)
    TParen _ inner -> TParen span0 (normalizeType inner)
    TKindSig _ ty' kind -> TKindSig span0 (normalizeType ty') (normalizeType kind)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeType elems)
    TContext _ constraints inner -> canonicalContextType (map normalizeType constraints) (normalizeType inner)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TAnn ann sub -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

