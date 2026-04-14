{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Syntax
import Data.Maybe (isJust)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Type (canonicalTopLevelType)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr, span0)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, DataKinds, ImplicitParams, KindSignatures, ExplicitForAll, RankNTypes, StarIsType]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
      expected = normalizeType (canonicalTopLevelType ty)
      hasKindedInferredBinder = containsKindedInferredBinder ty
   in checkCoverage $
        withMaxShrinks 100 $
          cover 1 hasKindedInferredBinder "kinded inferred forall binder" $
            assertCtorCoverage ["TAnn"] ty $
              counterexample (T.unpack source) $
                case parseType typeConfig source of
                  ParseErr err ->
                    counterexample (MPE.errorBundlePretty err) False
                  ParseOk parsed ->
                    let actual = normalizeType parsed
                     in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

-- | Normalize a type by stripping source spans.
-- TParen nodes are preserved as-is, except TParen around TKindSig is
-- stripped because the parser absorbs @(ty :: kind)@ as @TKindSig ty kind@
-- without a TParen wrapper.
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
    -- Strip TParen around TKindSig: the parser absorbs (ty :: kind) parens
    -- into the TKindSig node itself. Normalize the inner first to collapse
    -- any chain of TParens, then strip if TKindSig is underneath.
    TParen _ inner ->
      case normalizeType inner of
        result@(TKindSig {}) -> result
        result -> TParen span0 result
    TKindSig _ ty' kind -> TKindSig span0 (normalizeType ty') (normalizeType kind)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeType elems)
    TContext _ constraints inner -> TContext span0 (map normalizeType constraints) (normalizeType inner)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TAnn ann sub -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

containsKindedInferredBinder :: Type -> Bool
containsKindedInferredBinder ty =
  case ty of
    TForall _ binders inner -> any isKindedInferredBinder binders || containsKindedInferredBinder inner
    TImplicitParam _ _ inner -> containsKindedInferredBinder inner
    TApp _ f x -> containsKindedInferredBinder f || containsKindedInferredBinder x
    TFun _ a b -> containsKindedInferredBinder a || containsKindedInferredBinder b
    TTuple _ _ _ elems -> any containsKindedInferredBinder elems
    TList _ _ elems -> any containsKindedInferredBinder elems
    TParen _ inner -> containsKindedInferredBinder inner
    TKindSig _ ty' kind -> containsKindedInferredBinder ty' || containsKindedInferredBinder kind
    TUnboxedSum _ elems -> any containsKindedInferredBinder elems
    TContext _ constraints inner -> any containsKindedInferredBinder constraints || containsKindedInferredBinder inner
    TSplice _ _ -> False
    TAnn _ sub -> containsKindedInferredBinder sub
    _ -> False

isKindedInferredBinder :: TyVarBinder -> Bool
isKindedInferredBinder binder =
  tyVarBinderSpecificity binder == TyVarBInferred && isJust (tyVarBinderKind binder)
