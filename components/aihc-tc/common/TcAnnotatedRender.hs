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
import Aihc.Tc.Constraint (CtOrigin (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Evidence (Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Types (Kind (..), Pred (..), TyCon (..), Unique (..))
import Control.Applicative ((<|>))
import Data.List (intercalate, isPrefixOf, sortOn)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T

renderAnnotatedTcResults :: [Text] -> [TcModuleResult] -> [String]
renderAnnotatedTcResults sources results =
  map renderOne (sortOn (moduleDisplayName . tcmModule . snd) (zip sources results))
  where
    renderOne (source, result) =
      renderAnnotatedSource source (collectTcLabels result)

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe "<unnamed>" (moduleName modu)

data TcLabel = TcLabel
  { labelSpan :: !SourceSpan,
    labelText :: !String
  }
  deriving (Eq, Show)

sourceLabel :: SourceSpan -> String -> TcLabel
sourceLabel = TcLabel

collectTcLabels :: TcModuleResult -> [TcLabel]
collectTcLabels result =
  sortOn labelKey $
    -- The annotated AST is the source of truth: it preserves source placement
    -- and distinct binders that a flat binding table can conflate.
    concatMap (declLabels Nothing) (moduleDecls (tcmModule result))
      <> diagnosticLabels (tcmDiagnostics result)

diagnosticLabels :: [TcDiagnostic] -> [TcLabel]
diagnosticLabels =
  mapMaybe diagnosticLabel

diagnosticLabel :: TcDiagnostic -> Maybe TcLabel
diagnosticLabel diagnostic =
  case diagnosticSpan diagnostic of
    NoSourceSpan -> Nothing
    sp -> Just (sourceLabel sp (renderDiagnostic diagnostic))

diagnosticSpan :: TcDiagnostic -> SourceSpan
diagnosticSpan diagnostic =
  case diagLoc diagnostic of
    NoSourceSpan -> originSpan (diagnosticOrigin diagnostic)
    sp -> sp

diagnosticOrigin :: TcDiagnostic -> Maybe CtOrigin
diagnosticOrigin diagnostic =
  case diagKind diagnostic of
    UnificationError _ _ origin -> Just origin
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

renderDiagnostic :: TcDiagnostic -> String
renderDiagnostic diagnostic =
  severityPrefix (diagSeverity diagnostic) <> ": " <> renderDiagnosticKind (diagKind diagnostic)

severityPrefix :: TcSeverity -> String
severityPrefix TcError = "error"
severityPrefix TcWarning = "warning"

renderDiagnosticKind :: TcErrorKind -> String
renderDiagnosticKind kind =
  case kind of
    UnificationError left right _ ->
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
        renderBinderName binder == methodName,
        Just sp <- [spanFromUnqualifiedName binder <|> ambient]
      ]
    _ -> []

matchingNameSpans :: Text -> Maybe SourceSpan -> [UnqualifiedName] -> [SourceSpan]
matchingNameSpans methodName ambient names =
  [ sp
  | name <- names,
    renderBinderName name == methodName,
    Just sp <- [spanFromUnqualifiedName name <|> ambient]
  ]

instanceMethodSpan :: Text -> InstanceDeclItem -> Maybe SourceSpan
instanceMethodSpan methodName item =
  case item of
    InstanceItemBind valueDecl ->
      listToMaybe
        [ sp
        | binder <- valueDeclBinders valueDecl,
          renderBinderName binder == methodName,
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

renderAnnotatedSource :: Text -> [TcLabel] -> String
renderAnnotatedSource source labels =
  let sourceLines = T.lines source
      grouped = groupByLine labels
   in intercalate "\n" (concatMap (renderSourceLine grouped) (zip [1 ..] sourceLines))

groupByLine :: [TcLabel] -> Map.Map Int [TcLabel]
groupByLine = foldr insertLabel Map.empty
  where
    insertLabel label acc =
      case labelSpan label of
        SourceSpan _ startLine _ _ _ _ _ ->
          Map.insertWith (<>) startLine [label] acc
        NoSourceSpan -> acc

renderSourceLine :: Map.Map Int [TcLabel] -> (Int, Text) -> [String]
renderSourceLine grouped (lineNum, srcLine) =
  let labels = maybe [] (sortOn labelStartCol) (Map.lookup lineNum grouped)
   in T.unpack srcLine : renderAnnotationLines labels

labelStartCol :: TcLabel -> Int
labelStartCol label =
  case labelSpan label of
    SourceSpan _ _ startCol _ _ _ _ -> startCol
    NoSourceSpan -> maxBound

labelKey :: TcLabel -> (Int, Int, String)
labelKey label =
  case labelSpan label of
    SourceSpan _ startLine startCol _ _ _ _ -> (startLine, startCol, labelText label)
    NoSourceSpan -> (maxBound, maxBound, labelText label)

type AnnotationItem = (Int, String)

renderAnnotationLines :: [TcLabel] -> [String]
renderAnnotationLines [] = []
renderAnnotationLines labels =
  layoutAnnotationLines [(labelStartCol label - 1, labelText label) | label <- labels]

layoutAnnotationLines :: [AnnotationItem] -> [String]
layoutAnnotationLines [] = []
layoutAnnotationLines items =
  let reversed = reverse items
      (currentLine, deferred) = packLine reversed
   in renderAnnotationLine currentLine deferred : layoutAnnotationLines deferred

packLine :: [AnnotationItem] -> ([AnnotationItem], [AnnotationItem])
packLine [] = ([], [])
packLine (rightmost : rest) =
  let lineHasElaboration = annotationItemIsElaboration rightmost
      go _minCol [] fitted deferred = (fitted, deferred)
      go minCol (item@(col, label) : remaining) fitted deferred =
        let annotEnd = col + 3 + length label
            fitsBeforePlaced = annotEnd < minCol
            crossesDeferred = any ((\d -> d > col && d < annotEnd) . fst) deferred
            sameLabelKind = annotationItemIsElaboration item == lineHasElaboration
         in if sameLabelKind && fitsBeforePlaced && not crossesDeferred
              then go col remaining (item : fitted) deferred
              else go minCol remaining fitted (item : deferred)
   in go (fst rightmost) rest [rightmost] []

annotationItemIsElaboration :: AnnotationItem -> Bool
annotationItemIsElaboration (_, label) =
  any (`isPrefixOf` label) ["type-args:", "evidence:", "term-args:"]

renderAnnotationLine :: [AnnotationItem] -> [AnnotationItem] -> String
renderAnnotationLine placedOnLine deferredItems =
  let deferredCols = map fst deferredItems
      buildLine _ [] [] = ""
      buildLine pos placed deferred =
        case (placed, deferred) of
          ((col, label) : restPlaced, _)
            | pos == col ->
                "\x2514\x2500 " <> label <> buildLine (pos + 3 + length label) restPlaced (filter (>= pos + 3 + length label) deferred)
          (_, d : restDeferred)
            | pos == d ->
                "\x2502" <> buildLine (pos + 1) placed restDeferred
          _ ->
            let nextPos = case (placed, deferred) of
                  ((col, _) : _, d : _) -> min col d
                  ((col, _) : _, []) -> col
                  ([], d : _) -> d
                padding = nextPos - pos
             in replicate padding ' ' <> buildLine nextPos placed deferred
   in buildLine 0 (sortOn fst placedOnLine) (sortOn id deferredCols)
