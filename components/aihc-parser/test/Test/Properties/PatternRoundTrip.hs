{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

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
import Test.Properties.ExprHelpers (normalizeExpr)
import Test.QuickCheck
import Text.Megaparsec.Error qualified as MPE

patternConfig :: ParserConfig
patternConfig =
  defaultConfig
    { parserExtensions = [BlockArguments, UnboxedTuples, UnboxedSums, TemplateHaskell, MagicHash, OverloadedLabels, TypeApplications, MultiWayIf, RecursiveDo, TupleSections, ImplicitParams, ExplicitNamespaces, TypeAbstractions, RequiredTypeArguments, LambdaCase]
    }

prop_patternPrettyRoundTrip :: Pattern -> Property
prop_patternPrettyRoundTrip pat =
  let source = renderStrict (layoutPretty defaultLayoutOptions (pretty pat))
      expected = normalizePattern (addPatternParens pat)
   in checkCoverage $
        assertCtorCoverage ["PAnn", "PTypeBinder", "PTypeSyntax"] pat $
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
    PVar name -> PVar name
    PTypeBinder binder -> PTypeBinder (normalizeTyVarBinderSpan binder)
    PTypeSyntax form ty -> PTypeSyntax form (normalizeTypeSpan ty)
    PWildcard -> PWildcard
    PLit lit -> PLit (normalizeLiteral lit)
    PQuasiQuote quoter body -> PQuasiQuote quoter body
    PTuple tupleFlavor elems -> PTuple tupleFlavor (map normalizePattern elems)
    PList elems -> PList (map normalizePattern elems)
    PCon con typeArgs args -> PCon con (map normalizeTypeSpan typeArgs) (map normalizePattern args)
    PInfix lhs op rhs -> PInfix (normalizePattern lhs) op (normalizePattern rhs)
    PView expr inner -> PView (normalizeExpr expr) (normalizePattern inner)
    PAs name inner -> PAs name (normalizeAsInner inner)
    PStrict inner -> PStrict (normalizeUnaryInner inner)
    PIrrefutable inner -> PIrrefutable (normalizeUnaryInner inner)
    PNegLit lit -> PNegLit (normalizeLiteral lit)
    PParen inner -> PParen (normalizePattern inner)
    PUnboxedSum altIdx arity inner -> PUnboxedSum altIdx arity (normalizePattern inner)
    PRecord con fields rwc -> PRecord con [(fieldName, normalizePattern fieldPat) | (fieldName, fieldPat) <- fields] rwc
    PTypeSig inner ty -> PTypeSig (normalizePattern inner) (normalizeTypeSpan ty)
    PSplice body -> PSplice (normalizeExpr body)

-- | Normalize source spans in a type (reset to noSourceSpan).
normalizeTypeSpan :: Type -> Type
normalizeTypeSpan ty =
  case ty of
    TVar name -> TVar name
    TCon name promoted -> TCon name promoted
    TImplicitParam name inner -> TImplicitParam name (normalizeTypeSpan inner)
    TTypeLit lit -> TTypeLit lit
    TStar -> TStar
    TQuasiQuote quoter body -> TQuasiQuote quoter body
    TForall telescope inner -> TForall (normalizeForallTelescope telescope) (normalizeTypeSpan inner)
    TApp lhs rhs -> TApp (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TTypeApp lhs rhs -> TTypeApp (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TInfix lhs op promoted rhs -> TInfix (normalizeTypeSpan lhs) op promoted (normalizeTypeSpan rhs)
    TFun lhs rhs -> TFun (normalizeTypeSpan lhs) (normalizeTypeSpan rhs)
    TTuple tupleFlavor promoted elems -> TTuple tupleFlavor promoted (map normalizeTypeSpan elems)
    TList promoted elems -> TList promoted (map normalizeTypeSpan elems)
    TParen inner -> TParen (normalizeTypeSpan inner)
    TKindSig inner kind -> TKindSig (normalizeTypeSpan inner) (normalizeTypeSpan kind)
    TContext constraints inner -> TContext (map normalizeTypeSpan constraints) (normalizeTypeSpan inner)
    TUnboxedSum elems -> TUnboxedSum (map normalizeTypeSpan elems)
    TSplice body -> TSplice (normalizeExpr body)
    TWildcard -> TWildcard
    TAnn ann sub
      | Just _ <- fromAnnotation @SourceSpan ann -> normalizeTypeSpan sub
      | otherwise -> TAnn ann (normalizeTypeSpan sub)

normalizeLiteral :: Literal -> Literal
normalizeLiteral lit =
  case peelLiteralAnn lit of
    LitInt value nt repr -> LitInt value nt repr
    LitFloat value ft repr -> LitFloat value ft repr
    LitChar value repr -> LitChar value repr
    LitCharHash value repr -> LitCharHash value repr
    LitString value repr -> LitString value repr
    LitStringHash value repr -> LitStringHash value repr
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
    { tyVarBinderAnns = [],
      tyVarBinderKind = fmap normalizeTypeSpan (tyVarBinderKind tvb)
    }

normalizeForallTelescope :: ForallTelescope -> ForallTelescope
normalizeForallTelescope telescope =
  telescope
    { forallTelescopeBinders = map normalizeTyVarBinderSpan (forallTelescopeBinders telescope)
    }
