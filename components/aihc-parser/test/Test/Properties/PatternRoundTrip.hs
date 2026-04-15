{-# LANGUAGE OverloadedStrings #-}

module Test.Properties.PatternRoundTrip
  ( prop_patternPrettyRoundTrip,
  )
where

import Aihc.Parser (ParseResult (..), ParserConfig (..), defaultConfig, parsePattern)
import Aihc.Parser.Parens (addPatternParens)
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
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, MagicHash, OverloadedLabels, TypeApplications, MultiWayIf, RecursiveDo, TupleSections, ImplicitParams]
    }

prop_patternPrettyRoundTrip :: Pattern -> Property
prop_patternPrettyRoundTrip pat =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = normalizePattern (addPatternParens pat)
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
    PVar name -> patternAnnSpan span0 (PVar name)
    PWildcard -> patternAnnSpan span0 PWildcard
    PLit lit -> patternAnnSpan span0 (PLit (normalizeLiteral lit))
    PQuasiQuote quoter body -> patternAnnSpan span0 (PQuasiQuote quoter body)
    PTuple tupleFlavor elems -> patternAnnSpan span0 (PTuple tupleFlavor (map normalizePattern elems))
    PList elems -> patternAnnSpan span0 (PList (map normalizePattern elems))
    PCon con args -> patternAnnSpan span0 (PCon con (map normalizePattern args))
    PInfix lhs op rhs -> patternAnnSpan span0 (PInfix (normalizePattern lhs) op (normalizePattern rhs))
    PView expr inner -> patternAnnSpan span0 (PView (normalizeExpr expr) (normalizePattern inner))
    PAs name inner -> patternAnnSpan span0 (PAs name (normalizeAsInner inner))
    PStrict inner -> patternAnnSpan span0 (PStrict (normalizeUnaryInner inner))
    PIrrefutable inner -> patternAnnSpan span0 (PIrrefutable (normalizeUnaryInner inner))
    PNegLit lit -> patternAnnSpan span0 (PNegLit (normalizeLiteral lit))
    PParen (PNegLit lit) -> patternAnnSpan span0 (PNegLit (normalizeLiteral lit))
    PParen inner -> patternAnnSpan span0 (PParen (normalizePattern inner))
    PUnboxedSum altIdx arity inner -> patternAnnSpan span0 (PUnboxedSum altIdx arity (normalizePattern inner))
    PRecord con fields rwc -> patternAnnSpan span0 (PRecord con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields] rwc)
    PTypeSig inner ty -> patternAnnSpan span0 (PTypeSig (normalizePattern inner) (normalizeTypeSpan ty))
    PSplice body -> patternAnnSpan span0 (PSplice (normalizeExpr body))

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
  case peelLiteralAnn lit of
    LitInt value repr -> literalAnnSpan span0 (LitInt value repr)
    LitIntHash value repr -> literalAnnSpan span0 (LitIntHash value repr)
    LitIntBase value repr -> literalAnnSpan span0 (LitIntBase value repr)
    LitIntBaseHash value repr -> literalAnnSpan span0 (LitIntBaseHash value repr)
    LitFloat value repr -> literalAnnSpan span0 (LitFloat value repr)
    LitFloatHash value repr -> literalAnnSpan span0 (LitFloatHash value repr)
    LitChar value repr -> literalAnnSpan span0 (LitChar value repr)
    LitCharHash value repr -> literalAnnSpan span0 (LitCharHash value repr)
    LitString value repr -> literalAnnSpan span0 (LitString value repr)
    LitStringHash value repr -> literalAnnSpan span0 (LitStringHash value repr)
    LitAnn {} -> error "unreachable"

normalizeUnaryInner :: Pattern -> Pattern
normalizeUnaryInner pat =
  case normalizePattern pat of
    PParen inner@(PCon {}) -> inner
    PParen inner@(PNegLit {}) -> inner
    PParen inner@(PStrict {}) -> inner
    PParen inner@(PIrrefutable {}) -> inner
    other -> other

-- | Normalize the inner pattern of an as-pattern.
-- The pretty-printer adds parens around negative literals after @ for safety (a@-0 is invalid),
-- and around strict/irrefutable patterns to avoid lexing @!/@~ as symbolic operators,
-- so we strip those parens to get the canonical form.
normalizeAsInner :: Pattern -> Pattern
normalizeAsInner pat =
  case normalizePattern pat of
    PParen inner@(PCon {}) -> inner
    PParen inner@(PNegLit {}) -> inner
    PParen inner@(PStrict {}) -> inner
    PParen inner@(PIrrefutable {}) -> inner
    other -> other

normalizeTyVarBinderSpan :: TyVarBinder -> TyVarBinder
normalizeTyVarBinderSpan tvb =
  tvb
    { tyVarBinderSpan = span0,
      tyVarBinderKind = fmap normalizeTypeSpan (tyVarBinderKind tvb)
    }
