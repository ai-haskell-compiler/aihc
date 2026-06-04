{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    CaseAlt (..),
    ClassDecl (..),
    ClassDeclItem (..),
    CompStmt (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr (..),
    ForeignDecl (..),
    GuardQualifier (..),
    GuardedRhs (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    Match (..),
    Module (..),
    Name (..),
    NameType (..),
    NewtypeDecl (..),
    Pattern (..),
    RecordField (..),
    Rhs (..),
    SourceSpan (..),
    TypeSynDecl (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    mkAnnotation,
    moduleName,
  )
import Aihc.Tc (TcModuleResult (..), TcType (..), renderTcSignature, renderTcType)
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
  )
import Aihc.Tc.Constraint (CtOrigin (..), EqProvenance (..), TypeOrigin (..), TypeRole (..), TypeTrace (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Types (Kind (..), Pred (..), TyCon (..), Unique (..))
import Aihc.Testing.AnnotatedModule (renderAnnotatedModuleSources)
import Control.Applicative ((<|>))
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, pretty)

renderAnnotatedTcResults :: [Text] -> [TcModuleResult] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than type-checker results"
    GT -> error "renderAnnotatedTcResults: more source texts than type-checker results"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . tcmModule . snd) (zip sources results)
       in renderAnnotatedModuleSources renderedTcLabelDoc (map fst moduleSources) (map annotatedModule moduleSources)
  where
    annotatedModule (_, result) =
      attachRenderedLabels (collectTcLabels result) (tcmModule result)

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe "<unnamed>" (moduleName modu)

newtype RenderedTcLabel = RenderedTcLabel String
  deriving (Eq, Show)

renderedTcLabelDoc :: Annotation -> Maybe (Doc ann)
renderedTcLabelDoc annotation = do
  RenderedTcLabel label <- fromAnnotation annotation
  pure (pretty label)

data TcLabel = TcLabel
  { labelSpan :: !SourceSpan,
    labelText :: !String
  }
  deriving (Eq, Show)

sourceLabel :: SourceSpan -> String -> TcLabel
sourceLabel = TcLabel

attachRenderedLabels :: [TcLabel] -> Module -> Module
attachRenderedLabels labels modu =
  modu {moduleDecls = attachToDecls (moduleDecls modu)}
  where
    concreteLabels =
      [ label
      | label <- labels,
        labelSpan label /= NoSourceSpan
      ]

    attachToDecls [] = []
    attachToDecls (decl : decls) = foldr attachLabel decl concreteLabels : decls

    attachLabel label decl =
      DeclAnn
        (mkAnnotation (RenderedTcLabel (labelText label)))
        (DeclAnn (mkAnnotation (labelSpan label)) decl)

collectTcLabels :: TcModuleResult -> [TcLabel]
collectTcLabels result =
  sortOn labelKey $
    -- The annotated AST is the source of truth: it preserves source placement
    -- and distinct binders that a flat binding table can conflate.
    concatMap (declLabels Nothing) (moduleDecls (tcmModule result))
      <> diagnosticLabels (tcmDiagnostics result)

labelKey :: TcLabel -> (Int, Int, String)
labelKey label =
  case labelSpan label of
    SourceSpan _ startLine startCol _ _ _ _ -> (startLine, startCol, labelText label)
    NoSourceSpan -> (maxBound, maxBound, labelText label)

diagnosticLabels :: [TcDiagnostic] -> [TcLabel]
diagnosticLabels =
  concatMap diagnosticLabelsFor

diagnosticLabelsFor :: TcDiagnostic -> [TcLabel]
diagnosticLabelsFor diagnostic =
  case diagnosticSpan diagnostic of
    NoSourceSpan -> []
    sp -> sourceLabel sp (renderDiagnostic diagnostic) : diagnosticRelatedLabels diagnostic

diagnosticRelatedLabels :: TcDiagnostic -> [TcLabel]
diagnosticRelatedLabels diagnostic =
  case diagKind diagnostic of
    UnificationError _ _ _ (Just provenance) ->
      mapMaybe (typeOriginNote "note" Nothing) (eqContextOrigins provenance)
    _ -> []
  where
    typeOriginNote prefix maybeTy origin =
      case origin of
        TypeSignatureOrigin name sp ->
          Just $
            sourceLabel sp $
              prefix
                <> ": type signature for "
                <> T.unpack name
                <> " provides expected type"
                <> maybe "" ((" " <>) . renderTcType) maybeTy
        ConstraintTypeOrigin ctOrigin ->
          case originSpan (Just ctOrigin) of
            NoSourceSpan -> Nothing
            sp -> Just (sourceLabel sp (prefix <> ": related type information came from " <> renderOrigin ctOrigin))
        _ -> Nothing

diagnosticSpan :: TcDiagnostic -> SourceSpan
diagnosticSpan diagnostic =
  case diagLoc diagnostic of
    NoSourceSpan ->
      case diagKind diagnostic of
        UnificationError _ _ _ (Just provenance) -> eqPrimarySpan provenance
        _ -> originSpan (diagnosticOrigin diagnostic)
    sp -> sp

diagnosticOrigin :: TcDiagnostic -> Maybe CtOrigin
diagnosticOrigin diagnostic =
  case diagKind diagnostic of
    UnificationError _ _ origin _ -> Just origin
    UnsolvedWanted _ origin -> Just origin
    _ -> Nothing

originSpan :: Maybe CtOrigin -> SourceSpan
originSpan origin =
  case origin of
    Just (AppOrigin sp) -> sp
    Just (LambdaOrigin sp) -> sp
    Just (LetOrigin sp) -> sp
    Just (LitOrigin sp) -> sp
    Just (SigOrigin sp) -> sp
    Just (CaseBranchOrigin sp) -> sp
    Just (UnifyOrigin sp) -> sp
    _ -> NoSourceSpan

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
        Just provenance ->
          renderTypeMismatch provenance
        Nothing ->
          "couldn't match " <> renderTcType left <> " with " <> renderTcType right
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
    ListElementTypeOrigin _ ->
      " from an earlier list element"
    TypeSignatureOrigin name _ ->
      " from the type signature for " <> T.unpack name
    ConstraintTypeOrigin ctOrigin ->
      " from " <> renderOrigin ctOrigin
    _ -> ""

typeRoleNoun :: TypeRole -> String
typeRoleNoun role =
  case role of
    ActualType -> "expression"
    ExpectedType -> "expected type"
    RequiredType -> "required type"
    InferredType -> "inferred type"

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

declLabels :: Maybe SourceSpan -> Decl -> [TcLabel]
declLabels ambient decl =
  case decl of
    DeclAnn ann inner ->
      let ambient' = fromAnnotation @SourceSpan ann <|> ambient
          labels = case fromAnnotation @TcAnnotation ann of
            Just tcAnn -> bindingAnnotationLabels ambient' tcAnn inner
            _ -> []
          classLabels = case fromAnnotation @TcClassAnnotation ann of
            Just tcAnn -> classAnnotationLabels ambient' tcAnn inner
            Nothing -> []
          instanceLabels = case fromAnnotation @TcInstanceAnnotation ann of
            Just tcAnn -> instanceAnnotationLabels ambient' tcAnn inner
            Nothing -> []
       in labels <> classLabels <> instanceLabels <> declLabels ambient' inner
    DeclValue valueDecl -> valueDeclLabels ambient valueDecl
    DeclData dataDecl -> concatMap (dataConDeclLabels ambient) (dataDeclConstructors dataDecl)
    DeclNewtype newtypeDecl -> maybe [] (dataConDeclLabels ambient) (newtypeDeclConstructor newtypeDecl)
    DeclInstance instanceDecl -> concatMap (instanceItemLabels ambient) (instanceDeclItems instanceDecl)
    _ -> []

bindingAnnotationLabels :: Maybe SourceSpan -> TcAnnotation -> Decl -> [TcLabel]
bindingAnnotationLabels ambient tcAnn decl =
  case decl of
    DeclValue valueDecl -> valueBindingLabels ambient (tcAnnType tcAnn) valueDecl
    DeclData dataDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (dataDeclHead dataDecl)]
    DeclNewtype newtypeDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (newtypeDeclHead newtypeDecl)]
    DeclTypeSyn typeSynDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (typeSynHead typeSynDecl)]
    DeclForeign foreignDecl -> declBinderLabels ambient (tcAnnType tcAnn) [foreignName foreignDecl]
    _ -> []

declBinderLabels :: Maybe SourceSpan -> TcType -> [UnqualifiedName] -> [TcLabel]
declBinderLabels ambient ty binders =
  [ sourceLabel sp (renderTcSignature (renderBinderName binder) ty)
  | binder <- binders,
    Just sp <- [spanFromUnqualifiedName binder <|> ambient]
  ]

dataConDeclLabels :: Maybe SourceSpan -> DataConDecl -> [TcLabel]
dataConDeclLabels ambient dataConDecl =
  case dataConDecl of
    DataConAnn ann inner ->
      let ambient' = fromAnnotation @SourceSpan ann <|> ambient
          own = case fromAnnotation @TcAnnotation ann of
            Just tcAnn ->
              [ sourceLabel sp (renderTcSignature displayName (tcAnnType tcAnn))
              | (displayName, maybeSpan) <- dataConBinders ambient' inner,
                Just sp <- [maybeSpan <|> ambient']
              ]
            Nothing -> []
       in own <> dataConDeclLabels ambient' inner
    _ -> []

classAnnotationLabels :: Maybe SourceSpan -> TcClassAnnotation -> Decl -> [TcLabel]
classAnnotationLabels ambient (TcClassAnnotation methods) decl =
  case decl of
    DeclClass classDecl ->
      [ sourceLabel sp (renderTcSignature methodName methodTy)
      | method <- methods,
        let methodName = tcClassMethodName method,
        let methodTy = tcClassMethodType method,
        Just sp <- [classMethodSpan methodName classDecl <|> ambient]
      ]
    _ -> []

instanceAnnotationLabels :: Maybe SourceSpan -> TcInstanceAnnotation -> Decl -> [TcLabel]
instanceAnnotationLabels ambient tcAnn _decl =
  [ sourceLabel sp (renderTcSignature (tcInstanceDictName tcAnn) (tcInstanceDictType tcAnn))
  | Just sp <- [ambient]
  ]

instanceItemLabels :: Maybe SourceSpan -> InstanceDeclItem -> [TcLabel]
instanceItemLabels ambient item =
  case item of
    InstanceItemAnn ann inner ->
      let ambient' = fromAnnotation @SourceSpan ann <|> ambient
          methodLabels = case fromAnnotation @TcInstanceMethodAnnotation ann of
            Just tcAnn ->
              [ sourceLabel sp (renderTcSignature (tcInstanceMethodName tcAnn) (tcInstanceMethodType tcAnn))
              | Just sp <- [instanceMethodSpan (tcInstanceMethodName tcAnn) inner <|> ambient']
              ]
            Nothing -> []
          argLabels = case fromAnnotation @TcInstanceMethodAnnotation ann of
            Just tcAnn -> instanceItemArgBindingLabels ambient' (tcInstanceMethodType tcAnn) inner
            Nothing -> []
       in methodLabels <> argLabels <> instanceItemLabels ambient' inner
    InstanceItemBind valueDecl -> valueDeclLabels ambient valueDecl
    _ -> []

valueDeclLabels :: Maybe SourceSpan -> ValueDecl -> [TcLabel]
valueDeclLabels ambient valueDecl =
  case valueDecl of
    FunctionBind _ matches -> concatMap (matchLabels ambient) matches
    PatternBind _ pat rhs -> patternLabels ambient pat <> rhsLabels ambient rhs

matchLabels :: Maybe SourceSpan -> Match -> [TcLabel]
matchLabels ambient match =
  let ambient' = spanFromAnnotations (matchAnns match) <|> ambient
   in concatMap (patternLabels ambient') (matchPats match) <> rhsLabels ambient' (matchRhs match)

rhsLabels :: Maybe SourceSpan -> Rhs Expr -> [TcLabel]
rhsLabels ambient rhs =
  case rhs of
    UnguardedRhs anns expr maybeDecls ->
      let ambient' = spanFromAnnotations anns <|> ambient
       in exprLabels ambient' expr <> maybe [] (concatMap (declLabels ambient')) maybeDecls
    GuardedRhss anns guarded maybeDecls ->
      let ambient' = spanFromAnnotations anns <|> ambient
       in concatMap (guardedRhsLabels ambient') guarded <> maybe [] (concatMap (declLabels ambient')) maybeDecls

guardedRhsLabels :: Maybe SourceSpan -> GuardedRhs Expr -> [TcLabel]
guardedRhsLabels ambient guarded =
  let ambient' = spanFromAnnotations (guardedRhsAnns guarded) <|> ambient
   in concatMap (guardQualifierLabels ambient') (guardedRhsGuards guarded)
        <> exprLabels ambient' (guardedRhsBody guarded)

guardQualifierLabels :: Maybe SourceSpan -> GuardQualifier -> [TcLabel]
guardQualifierLabels ambient qual =
  case qual of
    GuardAnn ann inner -> guardQualifierLabels (fromAnnotation @SourceSpan ann <|> ambient) inner
    GuardExpr expr -> exprLabels ambient expr
    GuardPat pat expr -> patternLabels ambient pat <> exprLabels ambient expr
    GuardLet decls -> concatMap (declLabels ambient) decls

exprLabels :: Maybe SourceSpan -> Expr -> [TcLabel]
exprLabels ambient expr =
  case expr of
    EAnn ann inner ->
      let ambient' = fromAnnotation @SourceSpan ann <|> ambient
          own = case fromAnnotation @TcAnnotation ann of
            Just tcAnn -> elaborationLabel ambient' tcAnn
            Nothing -> []
       in own <> exprLabels ambient' inner
    EIf cond thenE elseE -> exprLabels ambient cond <> exprLabels ambient thenE <> exprLabels ambient elseE
    ELambdaPats pats body -> concatMap (patternLabels ambient) pats <> exprLabels ambient body
    EInfix lhs op rhs -> exprLabels ambient lhs <> nameElaborationLabels ambient op <> exprLabels ambient rhs
    ENegate inner -> exprLabels ambient inner
    ESectionL lhs _ -> exprLabels ambient lhs
    ESectionR _ rhs -> exprLabels ambient rhs
    ELetDecls decls body -> concatMap (declLabels ambient) decls <> exprLabels ambient body
    ECase scrut alts -> exprLabels ambient scrut <> concatMap (caseAltLabels ambient) alts
    EListComp body stmts -> exprLabels ambient body <> concatMap (compStmtLabels ambient) stmts
    EListCompParallel body stmtGroups -> exprLabels ambient body <> concatMap (concatMap (compStmtLabels ambient)) stmtGroups
    ERecordCon _ fields _ -> concatMap (exprLabels ambient . recordFieldValue) fields
    ERecordUpd base fields -> exprLabels ambient base <> concatMap (exprLabels ambient . recordFieldValue) fields
    EGetField base _ -> exprLabels ambient base
    ETypeSig inner _ -> exprLabels ambient inner
    EParen inner -> exprLabels ambient inner
    EList elems -> concatMap (exprLabels ambient) elems
    ETuple _ elems -> concatMap (maybe [] (exprLabels ambient)) elems
    EUnboxedSum _ _ inner -> exprLabels ambient inner
    ETypeApp fun _ -> exprLabels ambient fun
    EApp fun arg -> exprLabels ambient fun <> exprLabels ambient arg
    ETHExpQuote inner -> exprLabels ambient inner
    ETHTypedQuote inner -> exprLabels ambient inner
    ETHDeclQuote decls -> concatMap (declLabels ambient) decls
    _ -> []

caseAltLabels :: Maybe SourceSpan -> CaseAlt Expr -> [TcLabel]
caseAltLabels ambient (CaseAlt anns pat rhs) =
  let ambient' = spanFromAnnotations anns <|> ambient
   in patternLabels ambient' pat <> rhsLabels ambient' rhs

compStmtLabels :: Maybe SourceSpan -> CompStmt -> [TcLabel]
compStmtLabels ambient stmt =
  case stmt of
    CompAnn ann inner -> compStmtLabels (fromAnnotation @SourceSpan ann <|> ambient) inner
    CompGen pat expr ->
      let bindingLabels =
            maybe [] (\elemTy -> patternBindingLabels ambient elemTy pat) (exprListElementType expr)
       in bindingLabels <> patternLabels ambient pat <> exprLabels ambient expr
    CompGuard expr -> exprLabels ambient expr
    CompLetDecls decls -> concatMap (declLabels ambient) decls
    CompThen expr -> exprLabels ambient expr
    CompThenBy f byExpr -> exprLabels ambient f <> exprLabels ambient byExpr
    CompGroupUsing expr -> exprLabels ambient expr
    CompGroupByUsing byExpr usingExpr -> exprLabels ambient byExpr <> exprLabels ambient usingExpr

patternLabels :: Maybe SourceSpan -> Pattern -> [TcLabel]
patternLabels ambient pat =
  case pat of
    PAnn ann inner ->
      let ambient' = fromAnnotation @SourceSpan ann <|> ambient
          own = case fromAnnotation @TcAnnotation ann of
            Just tcAnn -> patternAnnotationLabels ambient' (tcAnnType tcAnn) inner
            Nothing -> []
       in own <> patternLabels ambient' inner
    PParen inner -> patternLabels ambient inner
    PAs _ inner -> patternLabels ambient inner
    PStrict inner -> patternLabels ambient inner
    PIrrefutable inner -> patternLabels ambient inner
    PList items -> concatMap (patternLabels ambient) items
    PTuple _ items -> concatMap (patternLabels ambient) items
    PUnboxedSum _ _ inner -> patternLabels ambient inner
    PInfix lhs _ rhs -> patternLabels ambient lhs <> patternLabels ambient rhs
    PView expr inner -> exprLabels ambient expr <> patternLabels ambient inner
    PCon _ _ pats -> concatMap (patternLabels ambient) pats
    PRecord _ fields _ -> concatMap (patternLabels ambient . recordFieldValue) fields
    PTypeSig inner _ -> patternLabels ambient inner
    PSplice expr -> exprLabels ambient expr
    _ -> []

patternAnnotationLabels :: Maybe SourceSpan -> TcType -> Pattern -> [TcLabel]
patternAnnotationLabels ambient ty pat =
  case pat of
    PVar name -> binderLabel ambient ty name
    PParen inner -> patternAnnotationLabels ambient ty inner
    PAs name _ -> binderLabel ambient ty name
    PStrict inner -> patternAnnotationLabels ambient ty inner
    PIrrefutable inner -> patternAnnotationLabels ambient ty inner
    PTypeSig inner _ -> patternAnnotationLabels ambient ty inner
    _ -> []

elaborationLabel :: Maybe SourceSpan -> TcAnnotation -> [TcLabel]
elaborationLabel maybeSpan ann =
  case (maybeSpan, renderElaboration ann) of
    (Just sp, Just label) -> [sourceLabel sp label]
    _ -> []

nameElaborationLabels :: Maybe SourceSpan -> Name -> [TcLabel]
nameElaborationLabels ambient name =
  let maybeSpan = spanFromAnnotations (nameAnns name) <|> ambient
   in [ label
      | ann <- nameAnns name,
        Just tcAnn <- [fromAnnotation @TcAnnotation ann],
        label <- elaborationLabel maybeSpan tcAnn
      ]

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

valueDeclBinders :: ValueDecl -> [UnqualifiedName]
valueDeclBinders valueDecl =
  case valueDecl of
    FunctionBind binder _ -> [binder]
    PatternBind _ pat _ -> patternBinders pat

patternBinders :: Pattern -> [UnqualifiedName]
patternBinders pat =
  case pat of
    PAnn _ inner -> patternBinders inner
    PVar name -> [name]
    PParen inner -> patternBinders inner
    PAs name inner -> name : patternBinders inner
    PStrict inner -> patternBinders inner
    PIrrefutable inner -> patternBinders inner
    PList items -> concatMap patternBinders items
    PTuple _ items -> concatMap patternBinders items
    PUnboxedSum _ _ inner -> patternBinders inner
    PInfix lhs _ rhs -> patternBinders lhs <> patternBinders rhs
    PView _ inner -> patternBinders inner
    PCon _ _ pats -> concatMap patternBinders pats
    PRecord _ fields _ -> concatMap (patternBinders . recordFieldValue) fields
    PTypeSig inner _ -> patternBinders inner
    _ -> []

valueBindingLabels :: Maybe SourceSpan -> TcType -> ValueDecl -> [TcLabel]
valueBindingLabels ambient ty valueDecl =
  signatureLabels <> valueArgBindingLabels ambient valueDecl ty
  where
    signatureLabels =
      [ sourceLabel sp (renderTcSignature (renderBinderName binder) ty)
      | binder <- valueDeclBinders valueDecl,
        Just sp <- [spanFromUnqualifiedName binder <|> ambient]
      ]

valueArgBindingLabels :: Maybe SourceSpan -> ValueDecl -> TcType -> [TcLabel]
valueArgBindingLabels ambient valueDecl ty =
  case valueDecl of
    FunctionBind _ matches -> concatMap (matchArgBindingLabels ambient ty) matches
    PatternBind {} -> []

matchArgBindingLabels :: Maybe SourceSpan -> TcType -> Match -> [TcLabel]
matchArgBindingLabels ambient ty match =
  concat (zipWith (patternBindingLabels ambient) (functionArgTypes (length (matchPats match)) ty) (matchPats match))

instanceItemArgBindingLabels :: Maybe SourceSpan -> TcType -> InstanceDeclItem -> [TcLabel]
instanceItemArgBindingLabels ambient ty item =
  case item of
    InstanceItemAnn ann inner -> instanceItemArgBindingLabels (fromAnnotation @SourceSpan ann <|> ambient) ty inner
    InstanceItemBind valueDecl -> valueArgBindingLabels ambient valueDecl ty
    _ -> []

patternBindingLabels :: Maybe SourceSpan -> TcType -> Pattern -> [TcLabel]
patternBindingLabels ambient ty pat =
  case pat of
    PAnn ann inner -> patternBindingLabels (fromAnnotation @SourceSpan ann <|> ambient) ty inner
    PVar name -> binderLabel ambient ty name
    PParen inner -> patternBindingLabels ambient ty inner
    PAs name inner -> binderLabel ambient ty name <> patternBindingLabels ambient ty inner
    PStrict inner -> patternBindingLabels ambient ty inner
    PIrrefutable inner -> patternBindingLabels ambient ty inner
    PList items ->
      maybe [] (\elemTy -> concatMap (patternBindingLabels ambient elemTy) items) (listElementType ty)
    PTuple _ items -> concat (zipWith (patternBindingLabels ambient) (tupleElementTypes ty) items)
    PUnboxedSum _ _ inner -> patternBindingLabels ambient ty inner
    PInfix lhs op rhs
      | unqualifiedNameTextFromPatternOperator op == ":" ->
          maybe [] (\elemTy -> patternBindingLabels ambient elemTy lhs <> patternBindingLabels ambient ty rhs) (listElementType ty)
      | otherwise -> []
    PView _ inner -> patternBindingLabels ambient ty inner
    PRecord {} -> []
    PTypeSig inner _ -> patternBindingLabels ambient ty inner
    _ -> []

binderLabel :: Maybe SourceSpan -> TcType -> UnqualifiedName -> [TcLabel]
binderLabel ambient ty name =
  [ sourceLabel sp (renderTcSignature (renderBinderName name) ty)
  | Just sp <- [spanFromUnqualifiedName name <|> ambient]
  ]

functionArgTypes :: Int -> TcType -> [TcType]
functionArgTypes wanted ty =
  take wanted (go (qualifiedBody ty))
  where
    go (TcFunTy arg rest) = arg : go rest
    go _ = []

qualifiedBody :: TcType -> TcType
qualifiedBody ty =
  case ty of
    TcForAllTy _ body -> qualifiedBody body
    TcQualTy _ body -> qualifiedBody body
    _ -> ty

listElementType :: TcType -> Maybe TcType
listElementType ty =
  case ty of
    TcTyCon (TyCon "[]" 1) [elemTy] -> Just elemTy
    _ -> Nothing

exprListElementType :: Expr -> Maybe TcType
exprListElementType expr =
  case expr of
    EAnn ann inner ->
      (fromAnnotation @TcAnnotation ann >>= listElementType . tcAnnType) <|> exprListElementType inner
    EParen inner -> exprListElementType inner
    _ -> Nothing

tupleElementTypes :: TcType -> [TcType]
tupleElementTypes ty =
  case ty of
    TcTyCon (TyCon _ arity) elemTys
      | arity == length elemTys -> elemTys
    _ -> []

unqualifiedNameTextFromPatternOperator :: Name -> Text
unqualifiedNameTextFromPatternOperator = nameText

dataConBinders :: Maybe SourceSpan -> DataConDecl -> [(Text, Maybe SourceSpan)]
dataConBinders ambient dataConDecl =
  case dataConDecl of
    DataConAnn ann inner -> dataConBinders (fromAnnotation @SourceSpan ann <|> ambient) inner
    PrefixCon _ _ name _ -> [binderInfoWithAmbient ambient name]
    InfixCon _ _ _ name _ -> [binderInfoWithAmbient ambient name]
    RecordCon _ _ name _ -> [binderInfoWithAmbient ambient name]
    GadtCon _ _ names _ -> map (binderInfoWithAmbient ambient) names
    TupleCon {} -> []
    UnboxedSumCon {} -> []
    ListCon {} -> []

binderInfo :: UnqualifiedName -> (Text, Maybe SourceSpan)
binderInfo name = (renderBinderName name, spanFromUnqualifiedName name)

binderInfoWithAmbient :: Maybe SourceSpan -> UnqualifiedName -> (Text, Maybe SourceSpan)
binderInfoWithAmbient ambient name =
  let (displayName, maybeSpan) = binderInfo name
   in (displayName, maybeSpan <|> ambient)

classMethodSpan :: Text -> ClassDecl -> Maybe SourceSpan
classMethodSpan methodName classDecl =
  listToMaybe (concatMap (classItemMethodSpans methodName Nothing) (classDeclItems classDecl))

classItemMethodSpans :: Text -> Maybe SourceSpan -> ClassDeclItem -> [SourceSpan]
classItemMethodSpans methodName ambient item =
  case item of
    ClassItemAnn ann inner -> classItemMethodSpans methodName (fromAnnotation @SourceSpan ann <|> ambient) inner
    ClassItemTypeSig names _ -> matchingNameSpans methodName ambient names
    ClassItemDefaultSig name _ -> matchingNameSpans methodName ambient [name]
    ClassItemDefault valueDecl ->
      [ sp
      | binder <- valueDeclBinders valueDecl,
        unqualifiedNameText binder == methodName,
        Just sp <- [spanFromUnqualifiedName binder <|> ambient]
      ]
    _ -> []

matchingNameSpans :: Text -> Maybe SourceSpan -> [UnqualifiedName] -> [SourceSpan]
matchingNameSpans methodName ambient names =
  [ sp
  | name <- names,
    unqualifiedNameText name == methodName,
    Just sp <- [spanFromUnqualifiedName name <|> ambient]
  ]

instanceMethodSpan :: Text -> InstanceDeclItem -> Maybe SourceSpan
instanceMethodSpan methodName item =
  case item of
    InstanceItemBind valueDecl ->
      listToMaybe
        [ sp
        | binder <- valueDeclBinders valueDecl,
          unqualifiedNameText binder == methodName,
          Just sp <- [spanFromUnqualifiedName binder]
        ]
    InstanceItemAnn ann inner -> instanceMethodSpan methodName inner <|> fromAnnotation @SourceSpan ann
    _ -> Nothing

spanFromUnqualifiedName :: UnqualifiedName -> Maybe SourceSpan
spanFromUnqualifiedName = spanFromAnnotations . unqualifiedNameAnns

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations = listToMaybe . mapMaybe (fromAnnotation @SourceSpan)

renderBinderName :: UnqualifiedName -> Text
renderBinderName name =
  case unqualifiedNameType name of
    NameVarSym -> "(" <> unqualifiedNameText name <> ")"
    NameConSym -> "(" <> unqualifiedNameText name <> ")"
    _ -> unqualifiedNameText name

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
