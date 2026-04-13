{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parsePattern)
import Aihc.Parser.Syntax
import Data.Text qualified as T
import Prettyprinter (Pretty (..), defaultLayoutOptions, layoutPretty)
import Prettyprinter.Render.Text (renderStrict)
import Test.Properties.Arb.Pattern ()
import Test.Properties.Coverage (assertCtorCoverage)
import Test.Properties.ExprHelpers (normalizeExpr, span0)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, MagicHash, OverloadedLabels, TypeApplications, MultiWayIf, RecursiveDo, TupleSections]
    }

prop_patternPrettyRoundTrip :: Pattern -> Property
prop_patternPrettyRoundTrip pat =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = normalizePattern pat
   in checkCoverage $
        assertCtorCoverage ["PAnn"] pat $
          counterexample (T.unpack source) $
            case parsePattern patternConfig source of
              ParseErr err ->
                counterexample (MPE.errorBundlePretty err) False
              ParseOk parsed ->
                let actual = normalizePattern parsed
                 in counterexample ("expected: " <> show expected <> "\nactual: " <> show actual) (expected == actual)

normalizePattern :: Pattern -> Pattern
normalizePattern pat =
  case pat of
    PAnn _ sub -> normalizePattern sub
    PVar _ name -> PVar span0 name
    PWildcard _ -> PWildcard span0
    PLit _ lit -> PLit span0 (normalizeLiteral lit)
    PQuasiQuote _ quoter body -> PQuasiQuote span0 quoter body
    PTuple _ tupleFlavor elems -> PTuple span0 tupleFlavor (map normalizePattern elems)
    PList _ elems -> PList span0 (map normalizePattern elems)
    PCon _ con args -> PCon span0 con (map normalizePattern args)
    PInfix _ lhs op rhs -> PInfix span0 (normalizePattern lhs) op (normalizePattern rhs)
    PView _ expr inner -> PView span0 (normalizeExpr expr) (normalizePattern inner)
    PAs _ name inner -> PAs span0 name (normalizeAsInner inner)
    PStrict _ inner -> PStrict span0 (normalizeUnaryInner inner)
    PIrrefutable _ inner -> PIrrefutable span0 (normalizeUnaryInner inner)
    PNegLit _ lit -> PNegLit span0 (normalizeLiteral lit)
    PParen _ inner -> PParen span0 (normalizePattern inner)
    PUnboxedSum _ altIdx arity inner -> PUnboxedSum span0 altIdx arity (normalizePattern inner)
    PRecord _ con fields rwc -> PRecord span0 con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields] rwc
    PTypeSig _ inner ty -> PTypeSig span0 (normalizePattern inner) (normalizeTypeSpan ty)
    PSplice _ body -> PSplice span0 (normalizeExpr body)

-- | Normalize source spans in a type (reset to noSourceSpan).
normalizeTypeSpan :: Type -> Type
normalizeTypeSpan ty =
  case ty of
    TVar _ name -> TVar span0 name
    TCon _ name promoted -> TCon span0 name promoted
    TImplicitParam _ name inner -> TImplicitParam span0 name (normalizeTypeSpan inner)
    TTypeLit _ lit -> TTypeLit span0 lit
    TStar _ -> TStar span0
    TQuasiQuote _ quoter body -> TQuasiQuote span0 quoter body
    TForall _ binders inner -> TForall span0 (map normalizeTyVarBinderSpan binders) (normalizeTypeSpan inner)
    TApp _ lhs rhs -> TApp span0 (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TFun _ lhs rhs -> TFun span0 (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TTuple _ tupleFlavor promoted elems -> TTuple span0 tupleFlavor promoted (map normalizeTypeSpan elems)
    TList _ promoted elems -> TList span0 promoted (map normalizeTypeSpan elems)
    TParen _ inner -> TParen span0 (normalizeTypeSpan inner)
    TKindSig _ inner kind -> TKindSig span0 (normalizeTypeSpan inner) (normalizeTypeSpan kind)
    TContext _ constraints inner -> TContext span0 (map normalizeTypeSpan constraints) (normalizeTypeSpan inner)
    TUnboxedSum _ elems -> TUnboxedSum span0 (map normalizeTypeSpan elems)
    TSplice _ body -> TSplice span0 (normalizeExpr body)
    TWildcard _ -> TWildcard span0
    TAnn ann sub -> TAnn ann (normalizeTypeSpan sub)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case lit of
    LitInt _ value repr -> LitInt span0 value repr
    LitIntHash _ value repr -> LitIntHash span0 value repr
    LitIntBase _ value repr -> LitIntBase span0 value repr
    LitIntBaseHash _ value repr -> LitIntBaseHash span0 value repr
    LitFloat _ value repr -> LitFloat span0 value repr
    LitFloatHash _ value repr -> LitFloatHash span0 value repr
    LitChar _ value repr -> LitChar span0 value repr
    LitCharHash _ value repr -> LitCharHash span0 value repr
    LitString _ value repr -> LitString span0 value repr
    LitStringHash _ value repr -> LitStringHash span0 value repr

normalizeUnaryInner :: Pattern -> Pattern
normalizeUnaryInner pat =
  case normalizePattern pat of
    PParen _ inner@(PCon {}) -> inner
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PStrict {}) -> inner
    PParen _ inner@(PIrrefutable {}) -> inner
    other -> other

-- | Normalize the inner pattern of an as-pattern.
-- The pretty-printer adds parens around negative literals after @ for safety (a@-0 is invalid),
-- and around strict/irrefutable patterns to avoid lexing @!/@~ as symbolic operators,
-- so we strip those parens to get the canonical form.
normalizeAsInner :: Pattern -> Pattern
normalizeAsInner pat =
  case normalizePattern pat of
    PParen _ inner@(PCon {}) -> inner
    PParen _ inner@(PNegLit {}) -> inner
    PParen _ inner@(PStrict {}) -> inner
    PParen _ inner@(PIrrefutable {}) -> inner
    other -> other

normalizeTyVarBinderSpan :: TyVarBinder -> TyVarBinder
normalizeTyVarBinderSpan tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeTypeSpan (tyVarBinderKind tvb)
    }
