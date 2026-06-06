{-# LANGUAGE OverloadedStrings #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Parser.Pretty ()
import Aihc.Parser.Syntax (Annotation, Module, fromAnnotation, moduleName)
import Aihc.Tc (Kind (..), Pred (..), TcAnnotation (..), TcBindingAnnotation (..), TcDiagnostic (..), TcErrorKind (..), TcModuleResult (..), TcSeverity (..), TcType (..), TyCon (..), Unique (..), renderTcType)
import Aihc.Tc.Constraint (CtOrigin (..), EqProvenance (..), TypeOrigin (..), TypeRole (..), TypeTrace (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Testing.AnnotatedModule (renderAnnotatedModuleSources)
import Control.Applicative ((<|>))
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, pretty, (<+>))

renderAnnotatedTcResults :: [Text] -> [TcModuleResult] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than type-checker results"
    GT -> error "renderAnnotatedTcResults: more source texts than type-checker results"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . tcmModule . snd) (zip sources results)
       in renderAnnotatedModuleSources renderedTcLabelDoc (map fst moduleSources) (map (tcmModule . snd) moduleSources)

moduleDisplayName :: Module -> Text
moduleDisplayName modu =
  fromMaybe "<unnamed>" (moduleName modu)

renderedTcLabelDoc :: Annotation -> Maybe (Doc ann)
renderedTcLabelDoc annotation =
  bindingAnnotationDoc annotation
    <|> elaborationAnnotationDoc annotation
    <|> diagnosticAnnotationDoc annotation

bindingAnnotationDoc :: Annotation -> Maybe (Doc ann)
bindingAnnotationDoc annotation = do
  TcBindingAnnotation name ty <- fromAnnotation annotation
  pure (pretty name <+> "∷" <+> pretty (renderTcType ty))

elaborationAnnotationDoc :: Annotation -> Maybe (Doc ann)
elaborationAnnotationDoc annotation = do
  tcAnn <- fromAnnotation annotation
  pretty <$> renderElaboration tcAnn

diagnosticAnnotationDoc :: Annotation -> Maybe (Doc ann)
diagnosticAnnotationDoc annotation = do
  diagnostic <- fromAnnotation annotation
  pure (pretty (renderDiagnostic diagnostic))

renderElaboration :: TcAnnotation -> Maybe String
renderElaboration ann =
  case extras of
    [] -> Nothing
    _ -> Just (intercalate "; " extras)
  where
    extras =
      typeArgs
        <> evidenceTerms
        <> termArgTypes
    typeArgs =
      case tcAnnTypeArgs ann of
        [] -> []
        tys -> ["type-args: " <> intercalate ", " (map renderTcType tys)]
    evidenceTerms =
      case tcAnnEvidenceTerms ann of
        [] -> []
        evs -> ["evidence: " <> intercalate ", " (map renderEvTerm evs)]
    termArgTypes =
      case tcAnnTermArgTypes ann of
        [] -> []
        tys -> ["term-args: " <> intercalate ", " (map renderTcType tys)]

renderDiagnostic :: TcDiagnostic -> String
renderDiagnostic diagnostic =
  severityPrefix (diagSeverity diagnostic) <> ": " <> renderDiagnosticKind (diagKind diagnostic)

severityPrefix :: TcSeverity -> String
severityPrefix TcError = "error"
severityPrefix TcWarning = "warning"

renderDiagnosticKind :: TcErrorKind -> String
renderDiagnosticKind kind =
  case kind of
    UnificationError left right _ maybeProvenance ->
      case maybeProvenance of
        Just provenance -> renderTypeMismatch provenance
        Nothing -> "couldn't match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError unique ty ->
      "occurs check failed: " <> renderUnique unique <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderKind expected <> ", got " <> renderKind actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    OtherError message ->
      message

renderTypeMismatch :: EqProvenance -> String
renderTypeMismatch provenance =
  typeRoleNoun (typeTraceRole actual)
    <> " has type "
    <> renderTcType (typeTraceType actual)
    <> ", but expected "
    <> renderTcType (typeTraceType expected)
    <> renderExpectedOrigin (typeTraceOrigin expected)
  where
    actual = eqActualTrace provenance
    expected = eqExpectedTrace provenance

renderExpectedOrigin :: TypeOrigin -> String
renderExpectedOrigin origin =
  case origin of
    ListElementTypeOrigin _ -> " from an earlier list element"
    TypeSignatureOrigin name _ -> " from the type signature for " <> T.unpack name
    ConstraintTypeOrigin ctOrigin -> " from " <> renderOrigin ctOrigin
    _ -> ""

typeRoleNoun :: TypeRole -> String
typeRoleNoun role =
  case role of
    ActualType -> "expression"
    ExpectedType -> "expected type"
    RequiredType -> "required type"
    InferredType -> "inferred type"

renderOrigin :: CtOrigin -> String
renderOrigin origin =
  case origin of
    OccurrenceOf name -> "the occurrence of " <> T.unpack name
    AppOrigin {} -> "an application"
    LambdaOrigin {} -> "a lambda expression"
    LetOrigin {} -> "a let binding"
    LitOrigin {} -> "a literal"
    SigOrigin {} -> "a type signature"
    CaseBranchOrigin {} -> "a case branch"
    InstOrigin name -> "the instance " <> T.unpack name
    UnifyOrigin {} -> "a unification constraint"

renderKind :: Kind -> String
renderKind kind =
  case kind of
    KType -> "*"
    KConstraint -> "Constraint"
    KFun arg result -> renderKindArg arg <> " -> " <> renderKind result
    KMeta unique -> renderUnique unique

renderKindArg :: Kind -> String
renderKindArg kind =
  case kind of
    KFun {} -> "(" <> renderKind kind <> ")"
    _ -> renderKind kind

renderUnique :: Unique -> String
renderUnique (Unique unique) = "?" <> show unique

renderEvTerm :: EvTerm -> String
renderEvTerm ev =
  case ev of
    EvVarTerm evVar -> renderEvVar evVar
    EvGiven pred' -> "given " <> renderPred pred'
    EvDict name typeArgs evidence ->
      T.unpack name
        <> renderTypeArgs typeArgs
        <> renderEvidenceArgs evidence
    EvCoercion coercion -> renderCoercion coercion
    EvSuperClass evidence index -> "super[" <> show index <> "](" <> renderEvTerm evidence <> ")"
    EvCast evidence coercion -> "cast(" <> renderEvTerm evidence <> ", " <> renderCoercion coercion <> ")"

renderTypeArgs :: [TcType] -> String
renderTypeArgs [] = ""
renderTypeArgs tys = " @" <> intercalate " @" (map renderTcType tys)

renderEvidenceArgs :: [EvTerm] -> String
renderEvidenceArgs [] = ""
renderEvidenceArgs evs = " (" <> intercalate ", " (map renderEvTerm evs) <> ")"

renderCoercion :: Coercion -> String
renderCoercion coercion =
  case coercion of
    CoVar evVar -> renderEvVar evVar
    Refl ty -> "refl " <> renderTcType ty
    Sym co -> "sym (" <> renderCoercion co <> ")"
    Trans left right -> "trans (" <> renderCoercion left <> ") (" <> renderCoercion right <> ")"
    TyConAppCo tyCon args -> T.unpack (tyConName tyCon) <> " " <> unwords (map renderCoercion args)
    AxiomInstCo name tys -> T.unpack name <> renderTypeArgs tys

renderEvVar :: EvVar -> String
renderEvVar (EvVar (Unique unique)) = "ev" <> show unique

renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred cls args -> T.unpack cls <> concatMap ((" " <>) . renderTcType) args
    EqPred left right -> renderTcType left <> " ~ " <> renderTcType right
