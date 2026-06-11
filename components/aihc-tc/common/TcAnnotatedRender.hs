{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    ClassDecl (..),
    ClassDeclItem (..),
    DataConDecl (..),
    DataDecl (..),
    Decl (..),
    Expr,
    ForeignDecl (..),
    InstanceDeclItem (..),
    Match (..),
    Module,
    NameType (..),
    NewtypeDecl (..),
    Pattern (..),
    RecordField (..),
    SourceSpan (..),
    TypeSynDecl (..),
    UnqualifiedName (..),
    ValueDecl (..),
    binderHeadName,
    fromAnnotation,
    moduleName,
    nameText,
    unqualifiedNameAnns,
    unqualifiedNameText,
    unqualifiedNameType,
  )
import Aihc.Tc (TcType (..), renderTcSignature, renderTcType)
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
import Aihc.Testing.AnnotatedModule (AnnotationLabel (..), renderAnnotatedModuleSourcesWithLabels)
import Control.Applicative ((<|>))
import Data.Data (Data, cast)
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (pretty)

renderAnnotatedTcResults :: [Text] -> [Module] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than modules"
    GT -> error "renderAnnotatedTcResults: more source texts than modules"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . snd) (zip sources results)
       in renderAnnotatedModuleSourcesWithLabels tcAnnotationLabels (map fst moduleSources) (map snd moduleSources)

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe "<unnamed>" (moduleName modu)

tcAnnotationLabels :: (Data carrier) => SourceSpan -> Maybe SourceSpan -> carrier -> Annotation -> [AnnotationLabel ann]
tcAnnotationLabels span' ambient carrier annotation =
  concat
    [ maybe [] (tcTypeAnnotationLabels span' ambient carrier) (fromAnnotation @TcAnnotation annotation),
      maybe [] (tcClassAnnotationLabels span' ambient carrier) (fromAnnotation @TcClassAnnotation annotation),
      maybe [] (tcInstanceAnnotationLabels span' ambient carrier) (fromAnnotation @TcInstanceAnnotation annotation),
      maybe [] (tcInstanceMethodAnnotationLabels span' ambient carrier) (fromAnnotation @TcInstanceMethodAnnotation annotation),
      maybe [] diagnosticLabelsFor (fromAnnotation @TcDiagnostic annotation)
    ]

tcTypeAnnotationLabels :: (Data carrier) => SourceSpan -> Maybe SourceSpan -> carrier -> TcAnnotation -> [AnnotationLabel ann]
tcTypeAnnotationLabels span' ambient carrier tcAnn =
  fromMaybe [] $
    (bindingAnnotationLabels wrapperSpan tcAnn <$> (cast carrier :: Maybe Decl))
      <|> (dataConAnnotationLabels wrapperSpan tcAnn <$> (cast carrier :: Maybe DataConDecl))
      <|> (patternAnnotationLabels wrapperSpan (tcAnnType tcAnn) <$> (cast carrier :: Maybe Pattern))
      <|> (elaborationLabel wrapperSpan tcAnn <$ (cast carrier :: Maybe Expr))
      <|> (elaborationLabel ownSpan tcAnn <$ (cast carrier :: Maybe [Annotation]))
  where
    ownSpan = concreteSpan span'
    wrapperSpan = ambient <|> ownSpan

tcClassAnnotationLabels :: (Data carrier) => SourceSpan -> Maybe SourceSpan -> carrier -> TcClassAnnotation -> [AnnotationLabel ann]
tcClassAnnotationLabels span' ambient carrier tcAnn =
  maybe [] (classAnnotationLabels (ambient <|> concreteSpan span') tcAnn) (cast carrier :: Maybe Decl)

tcInstanceAnnotationLabels :: (Data carrier) => SourceSpan -> Maybe SourceSpan -> carrier -> TcInstanceAnnotation -> [AnnotationLabel ann]
tcInstanceAnnotationLabels span' ambient carrier tcAnn =
  maybe [] (instanceAnnotationLabels (ambient <|> concreteSpan span') tcAnn) (cast carrier :: Maybe Decl)

tcInstanceMethodAnnotationLabels :: (Data carrier) => SourceSpan -> Maybe SourceSpan -> carrier -> TcInstanceMethodAnnotation -> [AnnotationLabel ann]
tcInstanceMethodAnnotationLabels span' ambient carrier tcAnn =
  maybe [] (instanceMethodAnnotationLabels (ambient <|> concreteSpan span') tcAnn) (cast carrier :: Maybe InstanceDeclItem)

sourceLabel :: SourceSpan -> String -> AnnotationLabel ann
sourceLabel sp label =
  AnnotationLabel sp (pretty label) Nothing

elaborationSourceLabel :: SourceSpan -> String -> AnnotationLabel ann
elaborationSourceLabel sp label =
  AnnotationLabel sp (pretty label) (Just "elaboration")

diagnosticLabelsFor :: TcDiagnostic -> [AnnotationLabel ann]
diagnosticLabelsFor diagnostic =
  case diagnosticSpan diagnostic of
    NoSourceSpan -> []
    sp -> sourceLabel sp (renderDiagnostic diagnostic) : diagnosticRelatedLabels diagnostic

diagnosticRelatedLabels :: TcDiagnostic -> [AnnotationLabel ann]
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
    Nothing ->
      case diagKind diagnostic of
        UnificationError _ _ _ (Just provenance) -> eqPrimarySpan provenance
        _ -> originSpan (diagnosticOrigin diagnostic)
    Just sp -> sp

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

bindingAnnotationLabels :: Maybe SourceSpan -> TcAnnotation -> Decl -> [AnnotationLabel ann]
bindingAnnotationLabels ambient tcAnn decl =
  case decl of
    DeclValue valueDecl -> valueBindingLabels ambient (tcAnnType tcAnn) valueDecl
    DeclData dataDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (dataDeclHead dataDecl)]
    DeclNewtype newtypeDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (newtypeDeclHead newtypeDecl)]
    DeclTypeSyn typeSynDecl -> declBinderLabels ambient (tcAnnType tcAnn) [binderHeadName (typeSynHead typeSynDecl)]
    DeclForeign foreignDecl -> declBinderLabels ambient (tcAnnType tcAnn) [foreignName foreignDecl]
    _ -> []

declBinderLabels :: Maybe SourceSpan -> TcType -> [UnqualifiedName] -> [AnnotationLabel ann]
declBinderLabels ambient ty binders =
  [ sourceLabel sp (renderTcSignature (renderBinderName binder) ty)
  | binder <- binders,
    Just sp <- [spanFromUnqualifiedName binder <|> ambient]
  ]

dataConAnnotationLabels :: Maybe SourceSpan -> TcAnnotation -> DataConDecl -> [AnnotationLabel ann]
dataConAnnotationLabels ambient tcAnn dataConDecl =
  [ sourceLabel sp (renderTcSignature displayName (tcAnnType tcAnn))
  | (displayName, maybeSpan) <- dataConBinders ambient dataConDecl,
    Just sp <- [maybeSpan <|> ambient]
  ]

classAnnotationLabels :: Maybe SourceSpan -> TcClassAnnotation -> Decl -> [AnnotationLabel ann]
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

instanceAnnotationLabels :: Maybe SourceSpan -> TcInstanceAnnotation -> Decl -> [AnnotationLabel ann]
instanceAnnotationLabels ambient tcAnn _decl =
  [ sourceLabel (lineStartSpan sp) (renderTcSignature (tcInstanceDictName tcAnn) (tcInstanceDictType tcAnn))
  | Just sp <- [ambient]
  ]

instanceMethodAnnotationLabels :: Maybe SourceSpan -> TcInstanceMethodAnnotation -> InstanceDeclItem -> [AnnotationLabel ann]
instanceMethodAnnotationLabels ambient tcAnn item =
  methodLabels <> argLabels
  where
    methodLabels =
      [ sourceLabel sp (renderTcSignature (tcInstanceMethodName tcAnn) (tcInstanceMethodType tcAnn))
      | Just sp <- [instanceMethodSpan (tcInstanceMethodName tcAnn) item <|> ambient]
      ]
    argLabels = instanceItemArgBindingLabels ambient (tcInstanceMethodType tcAnn) item

patternAnnotationLabels :: Maybe SourceSpan -> TcType -> Pattern -> [AnnotationLabel ann]
patternAnnotationLabels ambient ty pat =
  case pat of
    PVar name -> binderLabel ambient ty name
    PParen inner -> patternAnnotationLabels ambient ty inner
    PAs name _ -> binderLabel ambient ty name
    PStrict inner -> patternAnnotationLabels ambient ty inner
    PIrrefutable inner -> patternAnnotationLabels ambient ty inner
    PTypeSig inner _ -> patternAnnotationLabels ambient ty inner
    _ -> []

elaborationLabel :: Maybe SourceSpan -> TcAnnotation -> [AnnotationLabel ann]
elaborationLabel maybeSpan ann =
  case (maybeSpan, renderElaboration ann) of
    (Just sp, Just label) -> [elaborationSourceLabel sp label]
    _ -> []

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

valueBindingLabels :: Maybe SourceSpan -> TcType -> ValueDecl -> [AnnotationLabel ann]
valueBindingLabels ambient ty valueDecl =
  signatureLabels <> valueArgBindingLabels ambient valueDecl ty
  where
    signatureLabels =
      [ sourceLabel sp (renderTcSignature (renderBinderName binder) ty)
      | binder <- valueDeclBinders valueDecl,
        Just sp <- [spanFromUnqualifiedName binder <|> ambient]
      ]

valueArgBindingLabels :: Maybe SourceSpan -> ValueDecl -> TcType -> [AnnotationLabel ann]
valueArgBindingLabels ambient valueDecl ty =
  case valueDecl of
    FunctionBind _ matches -> concatMap (matchArgBindingLabels ambient ty) matches
    PatternBind {} -> []

matchArgBindingLabels :: Maybe SourceSpan -> TcType -> Match -> [AnnotationLabel ann]
matchArgBindingLabels ambient ty match =
  concat (zipWith (patternBindingLabels ambient) (functionArgTypes (length (matchPats match)) ty) (matchPats match))

instanceItemArgBindingLabels :: Maybe SourceSpan -> TcType -> InstanceDeclItem -> [AnnotationLabel ann]
instanceItemArgBindingLabels ambient ty item =
  case item of
    InstanceItemAnn ann inner -> instanceItemArgBindingLabels (fromAnnotation @SourceSpan ann <|> ambient) ty inner
    InstanceItemBind valueDecl -> valueArgBindingLabels ambient valueDecl ty
    _ -> []

patternBindingLabels :: Maybe SourceSpan -> TcType -> Pattern -> [AnnotationLabel ann]
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
      | nameText op == ":" ->
          maybe [] (\elemTy -> patternBindingLabels ambient elemTy lhs <> patternBindingLabels ambient ty rhs) (listElementType ty)
      | otherwise -> []
    PView _ inner -> patternBindingLabels ambient ty inner
    PRecord {} -> []
    PTypeSig inner _ -> patternBindingLabels ambient ty inner
    _ -> []

binderLabel :: Maybe SourceSpan -> TcType -> UnqualifiedName -> [AnnotationLabel ann]
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

tupleElementTypes :: TcType -> [TcType]
tupleElementTypes ty =
  case ty of
    TcTyCon (TyCon _ arity) elemTys
      | arity == length elemTys -> elemTys
    _ -> []

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

concreteSpan :: SourceSpan -> Maybe SourceSpan
concreteSpan NoSourceSpan = Nothing
concreteSpan sp = Just sp

lineStartSpan :: SourceSpan -> SourceSpan
lineStartSpan NoSourceSpan = NoSourceSpan
lineStartSpan sp@SourceSpan {sourceSpanStartCol} =
  sp {sourceSpanStartCol = 1, sourceSpanEndCol = max 1 (sourceSpanEndCol sp - sourceSpanStartCol + 1)}

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
