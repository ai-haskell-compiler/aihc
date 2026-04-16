{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Test.Properties.TypeRoundTrip
  ( prop_typePrettyRoundTrip,
  )
where

import Aihc.Parser
import Aihc.Parser.Parens (addTypeParens)
import Aihc.Parser.Syntax
import Data.Maybe (isJust)
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr, span0)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

typeConfig :: ParserConfig
typeConfig =
  defaultConfig
    { parserExtensions = effectiveExtensions GHC2024Edition [EnableExtension BlockArguments, EnableExtension UnboxedTuples, EnableExtension UnboxedSums, EnableExtension TemplateHaskell, EnableExtension ImplicitParams]
    }

prop_typePrettyRoundTrip :: Type -> Property
prop_typePrettyRoundTrip ty =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty ty))
      expected = normalizeType (addTypeParens ty)
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
normalizeType :: Type -> Type
normalizeType ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TImplicitParam name inner -> TImplicitParam name (normalizeType inner)
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TWildcard -> TWildcard
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall telescope inner ->
      TForall
        (telescope {forallTelescopeBinders = map normalizeTyVarBinder (forallTelescopeBinders telescope)})
        (normalizeType inner)
    TApp f x -> TApp (normalizeType f) (normalizeType x)
    TFun a b -> TFun (normalizeType a) (normalizeType b)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeType elems)
    TList promoted elems -> TList promoted (map normalizeType elems)
    TParen inner -> TParen (normalizeType inner)
    TKindSig ty' kind -> TKindSig (normalizeType ty') (normalizeType kind)
    TUnboxedSum elems -> TUnboxedSum (map normalizeType elems)
    TContext constraints inner -> TContext (map normalizeType constraints) (normalizeType inner)
    TSplice body -> TSplice (normalizeExpr body)
    TAnn ann sub
      | Just _ <- fromAnnotation @SourceSpan ann -> normalizeType sub
      | otherwise -> TAnn ann (normalizeType sub)

normalizeTyVarBinder :: TyVarBinder -> TyVarBinder
normalizeTyVarBinder tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeType (tyVarBinderKind tvb)
    }

containsKindedInferredBinder :: Type -> Bool
containsKindedInferredBinder ty =
  case ty of
    TForall telescope inner -> any isKindedInferredBinder (forallTelescopeBinders telescope) || containsKindedInferredBinder inner
    TImplicitParam _name inner -> containsKindedInferredBinder inner
    TApp f x -> containsKindedInferredBinder f || containsKindedInferredBinder x
    TFun a b -> containsKindedInferredBinder a || containsKindedInferredBinder b
    TTuple _tupleFlavor _promoted elems -> any containsKindedInferredBinder elems
    TList _promoted elems -> any containsKindedInferredBinder elems
    TParen inner -> containsKindedInferredBinder inner
    TKindSig ty' kind -> containsKindedInferredBinder ty' || containsKindedInferredBinder kind
    TUnboxedSum elems -> any containsKindedInferredBinder elems
    TContext constraints inner -> any containsKindedInferredBinder constraints || containsKindedInferredBinder inner
    TSplice _body -> False
    TAnn _ sub -> containsKindedInferredBinder sub
    _ -> False

isKindedInferredBinder :: TyVarBinder -> Bool
isKindedInferredBinder binder =
  tyVarBinderSpecificity binder == TyVarBInferred && isJust (tyVarBinderKind binder)
