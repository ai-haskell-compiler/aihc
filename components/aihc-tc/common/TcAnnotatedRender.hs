{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

-- | Human-readable inline rendering for type-checker annotations.
module TcAnnotatedRender
  ( renderAnnotatedTcResults,
  )
where

import Aihc.Tc (FunDep (..), TypeFamilyInstanceInfo (..), renderFunDepNames, renderTcSignature, renderTcType, renderTcTypeInModule, renderTyLit)
import Aihc.Tc.Annotations
  ( TcAnnotation (..),
    TcClassAnnotation (..),
    TcClassMethodAnnotation (..),
    TcDerivingContext (..),
    TcDerivingPlan (..),
    TcDerivingStrategy (..),
    TcInstanceAnnotation (..),
    TcInstanceMethodAnnotation (..),
    TcRecStmt (..),
    typeAnnotation,
  )
import Aihc.Tc.Constraint (CtOrigin (..), EqProvenance (..), TypeOrigin (..), TypeRole (..), TypeTrace (..))
import Aihc.Tc.Error (TcDiagnostic (..), TcErrorKind (..), TcSeverity (..))
import Aihc.Tc.Evidence (CallSite (..), Coercion (..), EvTerm (..), EvVar (..))
import Aihc.Tc.Syntax
  ( Annotation,
    ArithSeq (..),
    ClassDecl (..),
    ClassDeclItem (..),
    Cmd (..),
    CompStmt (..),
    DataConDecl (..),
    DataDecl (..),
    DataFamilyInst (..),
    Decl (..),
    DerivingClause (..),
    DoBindMethods (..),
    DoRecMethods (..),
    DoStmt (..),
    ExportSpec (..),
    Expr (..),
    ExprTc (..),
    GuardQualifier (..),
    ImportItem (..),
    InstanceDecl (..),
    InstanceDeclItem (..),
    ListMatch (..),
    Literal (..),
    LiteralMatch (..),
    LiteralSyntax (..),
    Module (..),
    NPlusKMatch (..),
    Name (..),
    NewtypeDecl (..),
    OverloadedLiteralMatch (..),
    Pattern (..),
    PatternTc (..),
    SourceSpan,
    StandaloneDerivingDecl (..),
    Type (..),
    annotationSpan,
    arithSeqMethod,
    doBindMethodsNames,
    doRecMethodsNames,
    listMatchNames,
    literalMatchNames,
    literalSyntaxNames,
    mapArithSeqMethod,
    moduleName,
    nPlusKMatchNames,
  )
import Aihc.Tc.Syntax.Traverse (Walk (..), idWalk, walk)
import Aihc.Tc.Types
import Aihc.Testing.AnnotatedModule (renderLabeledSource)
import Control.Applicative ((<|>))
import Control.Monad (void)
import Control.Monad.Trans.State.Strict (State, execState, gets, modify')
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, mapMaybe, maybeToList)
import Data.Text (Text)
import Data.Text qualified as T
import Prettyprinter (Doc, pretty)

renderAnnotatedTcResults :: [Text] -> [Module] -> [String]
renderAnnotatedTcResults sources results =
  case compare (length sources) (length results) of
    LT -> error "renderAnnotatedTcResults: fewer source texts than modules"
    GT -> error "renderAnnotatedTcResults: more source texts than modules"
    EQ ->
      let moduleSources = sortOn (moduleDisplayName . snd) (zip sources results)
       in concatMap renderModule moduleSources
  where
    renderModule (source, modu) =
      [renderLabeledSource source (moduleLabels modu)]

moduleDisplayName :: Module -> Text
moduleDisplayName modu = fromMaybe "<unnamed>" (moduleName modu)

-- | The labels of a module, each at the span of its node.
--
-- The labels of type checker information go to the span of the node that
-- holds the information. A node that has leading wrappers gets the first
-- span of its wrappers. A node that holds its information in a list, such
-- as a name, gets the first span of its annotation list. A located
-- diagnostic goes to its span, and an unlocated diagnostic goes to the
-- span of the module.
moduleLabels :: Module -> [(SourceSpan, Doc ann)]
moduleLabels modu =
  reverse (labelsDone (execState (walk labelWalk modu) (Labels [] Nothing currentModule)))
    <> concatMap diagnosticLabel (concat (moduleTcDiagnostics modu))
  where
    currentModule = moduleName modu
    diagnosticLabel diagnostic =
      [ (placedAt "diagnostic" (diagLoc diagnostic <|> spanFromAnnotations (moduleAnns modu)), pretty (renderDiagnostic diagnostic))
      ]

data Labels ann = Labels
  { labelsDone :: [(SourceSpan, Doc ann)],
    -- | The span of the annotation list that the walk visited last. The
    -- walk visits the information list of a node right after its
    -- annotation list.
    labelsListSpan :: Maybe SourceSpan,
    labelsModule :: Maybe Text
  }

labelWalk :: Walk (State (Labels ann))
labelWalk =
  idWalk
    { walkAnnotationList = \children anns -> do
        modify' (\labels -> labels {labelsListSpan = spanFromAnnotations anns})
        children anns,
      walkTcAnnotation = \info -> do
        span' <- gets labelsListSpan
        addInfoLabels span' [LabelInfo info]
        pure info,
      walkCheckedType = \ty -> do
        span' <- gets labelsListSpan
        addInfoLabels span' [LabelType ty]
        pure ty,
      walkExpr = exprLabels,
      walkPattern = patternLabels,
      walkType = wrapped peelType,
      walkDecl = wrappedFields peelDecl declFields,
      walkDataConDecl = wrappedOwn peelDataCon dataConOwn,
      walkLiteral = wrapped peelLiteral,
      walkGuardQualifier = wrapped peelGuard,
      walkDoStmt = doLabels,
      walkCompStmt = wrappedOwn peelComp compStmtOwn,
      walkArithSeq = arithSeqLabels,
      walkClassDeclItem = wrappedFields peelClassItem classItemFields,
      walkInstanceDeclItem = wrappedFields peelInstanceItem instanceItemFields,
      walkCmd = wrapped peelCmd,
      walkExportSpec = wrapped peelExport,
      walkImportItem = wrapped peelImport
    }
  where
    -- Label the information of the leading wrappers at their first span.
    -- Then walk the children of the node below the wrappers.
    wrapped :: (node -> Maybe (Maybe Annotation, Maybe TcAnnotation, node)) -> (node -> State (Labels ann) node) -> node -> State (Labels ann) node
    wrapped peel children node = do
      let (anns, infos, base) = peelLeading peel node
      addInfoLabels (spanFromAnnotations anns) (map LabelInfo infos)
      _ <- children base
      pure node

    -- The information in the typed fields of a node below its wrappers
    -- gets the first span of the wrappers, before the information of the
    -- wrappers.
    wrappedFields :: (node -> Maybe (Maybe Annotation, Maybe TcAnnotation, node)) -> (node -> [Label]) -> (node -> State (Labels ann) node) -> node -> State (Labels ann) node
    wrappedFields peel fields children node = do
      let (anns, infos, base) = peelLeading peel node
      addInfoLabels (spanFromAnnotations anns) (fields base <> map LabelInfo infos)
      _ <- children base
      pure node

    -- The information of a node below its wrappers gets the first span
    -- of the wrappers, after the information of the wrappers.
    wrappedOwn :: (node -> Maybe (Maybe Annotation, Maybe TcAnnotation, node)) -> (node -> ([Label], node)) -> (node -> State (Labels ann) node) -> node -> State (Labels ann) node
    wrappedOwn peel own children node = do
      let (anns, infos, base) = peelLeading peel node
          (ownInfos, base') = own base
      addInfoLabels (spanFromAnnotations anns) (map LabelInfo infos <> ownInfos)
      _ <- children base'
      pure node

    -- The syntax names of an expression, such as @negate@ for @-x@, get
    -- the span of the expression, after the information of its wrappers.
    exprLabels children node = do
      let (anns, infos, base) = peelLeading peelExpr node
          (ownType, typeless) = exprOwnType base
          (syntaxInfos, base') = exprSyntax typeless
      addInfoLabels (spanFromAnnotations anns) (map LabelInfo infos <> ownType <> map LabelInfo syntaxInfos)
      _ <- children base'
      pure node

    -- The method names of a statement get the span of the statement,
    -- before the information of its wrappers.
    doLabels :: (DoStmt body -> State (Labels ann) (DoStmt body)) -> DoStmt body -> State (Labels ann) (DoStmt body)
    doLabels children node = do
      let (anns, infos, base) = peelLeading peelDo node
          (methodInfos, base') = doStmtMethods base
      addInfoLabels (spanFromAnnotations anns) (map LabelInfo (methodInfos <> infos))
      _ <- children base'
      pure node

    -- The method name of an arithmetic sequence gets the span of the
    -- sequence, before the information of its wrappers.
    arithSeqLabels children node = do
      let (anns, infos, base) = peelLeading peelArithSeq node
          methodInfos = maybeToList (arithSeqMethod base >>= nameTc)
          base' = mapArithSeqMethod (\name -> name {nameTc = Nothing}) base
      addInfoLabels (spanFromAnnotations anns) (map LabelInfo (methodInfos <> infos))
      _ <- children base'
      pure node

    -- The parser gives no span to a literal in a nested pattern. The
    -- literal has the same source extent as its pattern, so it gets the
    -- pattern span. The names of the match of a pattern also get the
    -- pattern span.
    patternLabels children node = do
      let (anns, infos, base) = peelLeading peelPattern node
          patternSpan = spanFromAnnotations anns
          (matchInfos, base') = patternMatch base
      addInfoLabels patternSpan (map LabelInfo (infos <> matchInfos))
      case base' of
        PLit _ literal -> do
          let (literalAnns, literalInfos, _) = peelLeading peelLiteral literal
          addInfoLabels (spanFromAnnotations literalAnns <|> patternSpan) (map LabelInfo literalInfos)
        _ -> void (children base')
      pure node

-- | The information of the names of the match of a pattern, and the
-- pattern with no information in these names.
patternMatch :: Pattern -> ([TcAnnotation], Pattern)
patternMatch pat =
  case pat of
    PLit match literal -> (literalInfos match, PLit (literalWithout match) literal)
    PNegLit match literal -> (literalInfos match, PNegLit (literalWithout match) literal)
    PNPlusK match name literal ->
      ( mapMaybe nameTc (nPlusKMatchNames match),
        PNPlusK (NPlusKMatch (without <$> nPlusKType match) (without (nPlusKFromInteger match)) (without (nPlusKGreaterEqual match)) (without (nPlusKMinus match))) name literal
      )
    PList (Just match) items ->
      (mapMaybe nameTc (listMatchNames match), PList (Just (ListMatch (without (listMatchToList match)) (without (listMatchFromListN match)))) items)
    _ -> ([], pat)
  where
    without name = name {nameTc = Nothing}
    literalInfos = mapMaybe nameTc . literalMatchNames
    literalWithout match =
      case match of
        LiteralMatchFixed -> LiteralMatchFixed
        LiteralMatchPrimitive typeName -> LiteralMatchPrimitive (without typeName)
        LiteralMatchOverloaded (OverloadedLiteralMatch literalType conversion negation equality) ->
          LiteralMatchOverloaded (OverloadedLiteralMatch (without <$> literalType) (without conversion) (without <$> negation) (without equality))

-- | The information of the method names of a statement, and the statement
-- with no information in these names. The methods go from the outside in:
-- @>>=@ before @fail@, and @mfix@, @return@, and @>>=@ for a @rec@
-- statement.
doStmtMethods :: DoStmt body -> ([TcAnnotation], DoStmt body)
doStmtMethods stmt =
  case stmt of
    DoBind (Just methods@(DoBindMethods method failure)) pat body ->
      (mapMaybe nameTc (doBindMethodsNames methods), DoBind (Just (DoBindMethods (without method) (without <$> failure))) pat body)
    DoExpr (Just method) body -> (maybeToList (nameTc method), DoExpr (Just (without method)) body)
    DoRecStmt tc (Just methods@(DoRecMethods mfix return' bind)) stmts ->
      (mapMaybe nameTc (doRecMethodsNames methods) <> recInfos tc, DoRecStmt Nothing (Just (DoRecMethods (without mfix) (without return') (without bind))) stmts)
    DoRecStmt tc Nothing stmts -> (recInfos tc, DoRecStmt Nothing Nothing stmts)
    _ -> ([], stmt)
  where
    without name = name {nameTc = Nothing}
    -- The tuple annotation of a @rec@ statement, then the occurrence
    -- annotation of each knot variable.
    recInfos = maybe [] (\checked -> tcRecTuple checked : map snd (tcRecKnotOccurrences checked))

-- | The label of the checked type of a constructor declaration, and the
-- declaration with no checked type.
dataConOwn :: DataConDecl -> ([Label], DataConDecl)
dataConOwn decl =
  case decl of
    PrefixCon tc a b c d -> (typeLabel tc, PrefixCon Nothing a b c d)
    InfixCon tc a b c d e -> (typeLabel tc, InfixCon Nothing a b c d e)
    RecordCon tc a b c d -> (typeLabel tc, RecordCon Nothing a b c d)
    GadtCon tc a b c d -> (typeLabel tc, GadtCon Nothing a b c d)
    TupleCon tc a b c d -> (typeLabel tc, TupleCon Nothing a b c d)
    UnboxedSumCon tc a b c d e -> (typeLabel tc, UnboxedSumCon Nothing a b c d e)
    ListCon tc a b c -> (typeLabel tc, ListCon Nothing a b c)
    DataConAnn _ _ -> ([], decl)
  where
    typeLabel = maybe [] (\ty -> [LabelType ty])

-- | The information of a comprehension statement, and the statement with
-- no information.
compStmtOwn :: CompStmt -> ([Label], CompStmt)
compStmtOwn stmt =
  case stmt of
    CompGen tc pat expr -> (label tc, CompGen Nothing pat expr)
    CompGuard tc expr -> (label tc, CompGuard Nothing expr)
    CompLetDecls tc decls -> (label tc, CompLetDecls Nothing decls)
    CompThen tc function -> (label tc, CompThen Nothing function)
    CompThenBy tc function key -> (label tc, CompThenBy Nothing function key)
    CompGroupUsing tc function -> (label tc, CompGroupUsing Nothing function)
    CompGroupByUsing tc key function -> (label tc, CompGroupByUsing Nothing key function)
    CompAnn _ _ -> ([], stmt)
  where
    label = maybe [] (\annotation -> [LabelInfo annotation])

-- | The labels of the typed fields of a class item.
classItemFields :: ClassDeclItem -> [Label]
classItemFields item =
  case item of
    ClassItemDefault tc _ -> [LabelText (renderInstanceMethodAnnotation annotation) | Just annotation <- [tc]]
    _ -> []

-- | The labels of the typed fields of an instance item.
instanceItemFields :: InstanceDeclItem -> [Label]
instanceItemFields item =
  case item of
    InstanceItemBind tc _ -> [LabelText (renderInstanceMethodAnnotation annotation) | Just annotation <- [tc]]
    _ -> []

-- | The label of the type that an expression holds in a field of its own,
-- and the expression with no such type.
exprOwnType :: Expr -> ([Label], Expr)
exprOwnType expr =
  case expr of
    EIf ty method condition thenExpr elseExpr -> (typeLabel ty, EIf Nothing method condition thenExpr elseExpr)
    EMultiWayIf ty alternatives -> (typeLabel ty, EMultiWayIf Nothing alternatives)
    ELambdaPats ty pats body -> (typeLabel ty, ELambdaPats Nothing pats body)
    ELambdaCase ty alternatives -> (typeLabel ty, ELambdaCase Nothing alternatives)
    ELambdaCases ty alternatives -> (typeLabel ty, ELambdaCases Nothing alternatives)
    ECase ty scrutinee alternatives -> (typeLabel ty, ECase Nothing scrutinee alternatives)
    EDo ty statements flavor -> (typeLabel ty, EDo Nothing statements flavor)
    EArithSeq ty method sequence' -> (typeLabel ty, EArithSeq Nothing method sequence')
    EList tc syntax method items -> (annotationLabel tc, EList Nothing syntax method items)
    ETuple tc syntax flavor items -> (annotationLabel tc, ETuple Nothing syntax flavor items)
    EUnboxedSum tc position arity inner -> (annotationLabel tc, EUnboxedSum Nothing position arity inner)
    EListComp tc body statements -> (annotationLabel tc, EListComp Nothing body statements)
    EListCompParallel tc body branches -> (annotationLabel tc, EListCompParallel Nothing body branches)
    _ -> ([], expr)
  where
    typeLabel = maybe [] (\ty -> [LabelType ty])
    annotationLabel = maybe [] (\annotation -> [LabelInfo annotation])

-- | The information of the syntax names of an expression, and the
-- expression with no information in these names.
exprSyntax :: Expr -> ([TcAnnotation], Expr)
exprSyntax expr =
  case expr of
    EInt syntax value numericType text -> (literalInfos syntax, EInt (literalWithout syntax) value numericType text)
    EFloat syntax value floatType text -> (literalInfos syntax, EFloat (literalWithout syntax) value floatType text)
    ECharHash ty value text -> (maybeToList (nameTc ty), ECharHash (without ty) value text)
    EString conversion value text -> (maybeToList (conversion >>= nameTc), EString (without <$> conversion) value text)
    EStringHash ty value text -> (maybeToList (nameTc ty), EStringHash (without ty) value text)
    EIf ty method condition thenExpr elseExpr -> (maybeToList (method >>= nameTc), EIf ty (without <$> method) condition thenExpr elseExpr)
    ENegate method inner -> (maybeToList (nameTc method), ENegate (without method) inner)
    EArithSeq ty method sequence' -> (maybeToList (method >>= nameTc), EArithSeq ty (without <$> method) sequence')
    EGetField method record field -> (maybeToList (nameTc method), EGetField (without method) record field)
    EGetFieldProjection method fields -> (maybeToList (nameTc method), EGetFieldProjection (without method) fields)
    -- The conversion of an overloaded list goes before the ordinary list.
    EList tc syntax method items -> (maybeToList (method >>= nameTc) <> maybeToList (nameTc syntax), EList tc (without syntax) (without <$> method) items)
    ETuple tc syntax flavor items -> (maybeToList (nameTc syntax), ETuple tc (without syntax) flavor items)
    _ -> ([], expr)
  where
    without name = name {nameTc = Nothing}
    literalInfos = mapMaybe nameTc . literalSyntaxNames
    literalWithout syntax =
      case syntax of
        LiteralSyntaxPrimitive ty -> LiteralSyntaxPrimitive (without ty)
        LiteralSyntaxOverloaded ty conversion -> LiteralSyntaxOverloaded (without <$> ty) (without conversion)

-- | One label of a node: an item of information, or the text of the
-- information in a typed field.
data Label
  = LabelInfo TcAnnotation
  | -- | A checked type with no elaboration.
    LabelType TcType
  | LabelText String

-- | The labels of the typed fields of a declaration.
declFields :: Decl -> [Label]
declFields decl =
  case decl of
    DeclClass classDecl -> [LabelText (renderClassAnnotation annotation) | Just annotation <- [classDeclTc classDecl]]
    DeclInstance instanceDecl -> [LabelText (renderInstanceAnnotation annotation) | Just annotation <- [instanceDeclTc instanceDecl]]
    DeclData dataDecl -> derivingLabels (concatMap derivingPlans (dataDeclDeriving dataDecl))
    DeclNewtype newtypeDecl -> derivingLabels (concatMap derivingPlans (newtypeDeclDeriving newtypeDecl))
    DeclDataFamilyInst familyInst -> derivingLabels (concatMap derivingPlans (dataFamilyInstDeriving familyInst))
    DeclStandaloneDeriving derivingDecl -> derivingLabels (maybeToList (standaloneDerivingPlan derivingDecl))
    _ -> []
  where
    derivingLabels plans = [LabelText (renderDerivingPlans plans) | not (null plans)]

addInfoLabels :: Maybe SourceSpan -> [Label] -> State (Labels ann) ()
addInfoLabels span' infos = do
  currentModule <- gets labelsModule
  let labelText label =
        case label of
          LabelInfo info -> Just (renderTypeAnnotation currentModule info)
          LabelType ty -> Just (renderTypeAnnotation currentModule (typeAnnotation ty))
          LabelText text -> Just text
  case [label | info <- infos, Just label <- [labelText info]] of
    [] -> pure ()
    rendered ->
      modify' (\labels -> labels {labelsDone = reverse [(placedAt label span', pretty label) | label <- rendered] <> labelsDone labels})

placedAt :: String -> Maybe SourceSpan -> SourceSpan
placedAt label =
  fromMaybe (error ("renderAnnotatedTcResults: a label has no source span: " <> show label))

peelLeading :: (node -> Maybe (Maybe Annotation, Maybe TcAnnotation, node)) -> node -> ([Annotation], [TcAnnotation], node)
peelLeading peel =
  go [] []
  where
    go anns infos node =
      case peel node of
        Just (ann, info, inner) -> go (maybe anns (: anns) ann) (maybe infos (: infos) info) inner
        Nothing -> (reverse anns, reverse infos, node)

peelExpr :: Expr -> Maybe (Maybe Annotation, Maybe TcAnnotation, Expr)
peelExpr (EAnn ann inner) = Just (Just ann, Nothing, inner)
peelExpr (ETc (ExprType annotation) inner) = Just (Nothing, Just annotation, inner)
peelExpr (ETc (ExprCast _) inner) = Just (Nothing, Nothing, inner)
peelExpr _ = Nothing

peelPattern :: Pattern -> Maybe (Maybe Annotation, Maybe TcAnnotation, Pattern)
peelPattern (PAnn ann inner) = Just (Just ann, Nothing, inner)
peelPattern (PTc (PatternType annotation) inner) = Just (Nothing, Just annotation, inner)
peelPattern (PTc _ inner) = Just (Nothing, Nothing, inner)
peelPattern _ = Nothing

peelType :: Type -> Maybe (Maybe Annotation, Maybe TcAnnotation, Type)
peelType (TAnn ann inner) = Just (Just ann, Nothing, inner)
peelType _ = Nothing

peelDecl :: Decl -> Maybe (Maybe Annotation, Maybe TcAnnotation, Decl)
peelDecl (DeclAnn ann inner) = Just (Just ann, Nothing, inner)
peelDecl (DeclTc annotation inner) = Just (Nothing, Just annotation, inner)
peelDecl (DeclError _ inner) = Just (Nothing, Nothing, inner)
peelDecl _ = Nothing

peelDataCon :: DataConDecl -> Maybe (Maybe Annotation, Maybe TcAnnotation, DataConDecl)
peelDataCon (DataConAnn ann inner) = Just (Just ann, Nothing, inner)
peelDataCon _ = Nothing

peelLiteral :: Literal -> Maybe (Maybe Annotation, Maybe TcAnnotation, Literal)
peelLiteral (LitAnn ann inner) = Just (Just ann, Nothing, inner)
peelLiteral (LitTc annotation inner) = Just (Nothing, Just annotation, inner)
peelLiteral _ = Nothing

peelGuard :: GuardQualifier -> Maybe (Maybe Annotation, Maybe TcAnnotation, GuardQualifier)
peelGuard (GuardAnn ann inner) = Just (Just ann, Nothing, inner)
peelGuard _ = Nothing

peelDo :: DoStmt body -> Maybe (Maybe Annotation, Maybe TcAnnotation, DoStmt body)
peelDo (DoAnn ann inner) = Just (Just ann, Nothing, inner)
peelDo (DoRecKnot _ inner) = Just (Nothing, Nothing, inner)
peelDo _ = Nothing

peelComp :: CompStmt -> Maybe (Maybe Annotation, Maybe TcAnnotation, CompStmt)
peelComp (CompAnn ann inner) = Just (Just ann, Nothing, inner)
peelComp _ = Nothing

peelArithSeq :: ArithSeq -> Maybe (Maybe Annotation, Maybe TcAnnotation, ArithSeq)
peelArithSeq (ArithSeqAnn ann inner) = Just (Just ann, Nothing, inner)
peelArithSeq _ = Nothing

peelClassItem :: ClassDeclItem -> Maybe (Maybe Annotation, Maybe TcAnnotation, ClassDeclItem)
peelClassItem (ClassItemAnn ann inner) = Just (Just ann, Nothing, inner)
peelClassItem (ClassItemError _ inner) = Just (Nothing, Nothing, inner)
peelClassItem _ = Nothing

peelInstanceItem :: InstanceDeclItem -> Maybe (Maybe Annotation, Maybe TcAnnotation, InstanceDeclItem)
peelInstanceItem (InstanceItemAnn ann inner) = Just (Just ann, Nothing, inner)
peelInstanceItem (InstanceItemError _ inner) = Just (Nothing, Nothing, inner)
peelInstanceItem _ = Nothing

peelCmd :: Cmd -> Maybe (Maybe Annotation, Maybe TcAnnotation, Cmd)
peelCmd (CmdAnn ann inner) = Just (Just ann, Nothing, inner)
peelCmd _ = Nothing

peelExport :: ExportSpec -> Maybe (Maybe Annotation, Maybe TcAnnotation, ExportSpec)
peelExport (ExportAnn ann inner) = Just (Just ann, Nothing, inner)
peelExport _ = Nothing

peelImport :: ImportItem -> Maybe (Maybe Annotation, Maybe TcAnnotation, ImportItem)
peelImport (ImportAnn ann inner) = Just (Just ann, Nothing, inner)
peelImport (ImportItemError _ inner) = Just (Nothing, Nothing, inner)
peelImport _ = Nothing

spanFromAnnotations :: [Annotation] -> Maybe SourceSpan
spanFromAnnotations =
  foldr ((<|>) . annotationSpan) Nothing

renderTypeAnnotation :: Maybe Text -> TcAnnotation -> String
renderTypeAnnotation currentModule ann =
  intercalate "; " ("type: " <> renderTcTypeInModule currentModule (tcAnnType ann) : renderElaboration ann)

renderClassAnnotation :: TcClassAnnotation -> String
renderClassAnnotation classAnnotation =
  intercalate "; " (renderedMethods : renderedFunDeps)
  where
    renderedMethods =
      "class methods:" <> case tcClassMethods classAnnotation of
        [] -> ""
        methods -> " " <> intercalate ", " (map renderClassMethod methods)
    renderedFunDeps =
      case tcClassFunDeps classAnnotation of
        [] -> []
        dependencies -> ["fundeps: " <> intercalate ", " (map renderFunDep dependencies)]
    renderFunDep dependency =
      unwords (map classParamName (fdDeterminers dependency))
        <> " → "
        <> unwords (map classParamName (fdDetermined dependency))
    classParamName index =
      case drop index (tcClassTyVars classAnnotation <> tcClassKindTyVars classAnnotation) of
        variable : _ -> T.unpack (tvName variable)
        [] -> "?"

renderClassMethod :: TcClassMethodAnnotation -> String
renderClassMethod method =
  renderTcSignature (tcClassMethodName method) (tcClassMethodType method)

renderDerivingPlans :: [TcDerivingPlan] -> String
renderDerivingPlans plans =
  "deriving plans: " <> intercalate ", " (map renderDerivingPlan plans)

renderDerivingPlan :: TcDerivingPlan -> String
renderDerivingPlan plan =
  renderDerivingStrategy (tcDerivingStrategy plan)
    <> " "
    <> renderTcType (TcTyCon (tcDerivingClassTyCon plan) (tcDerivingHeadTypes plan))
    <> renderDerivingContext (tcDerivingContext plan)
    <> renderDerivingMethods (tcDerivingClassMethods plan)

renderDerivingStrategy :: TcDerivingStrategy -> String
renderDerivingStrategy strategy =
  case strategy of
    TcDerivingStock -> "stock"
    TcDerivingNewtype -> "newtype"
    TcDerivingAnyclass -> "anyclass"
    TcDerivingVia viaType -> "via " <> renderTcType viaType

renderDerivingContext :: TcDerivingContext -> String
renderDerivingContext context =
  case context of
    TcDerivingInferContext -> " [context: infer]"
    TcDerivingExplicitContext [] -> " [context: ()]"
    TcDerivingExplicitContext predicates -> " [context: " <> intercalate ", " (map renderPred predicates) <> "]"

renderDerivingMethods :: [TcClassMethodAnnotation] -> String
renderDerivingMethods [] = ""
renderDerivingMethods methods = " [methods: " <> intercalate ", " (map (T.unpack . tcClassMethodName) methods) <> "]"

renderInstanceAnnotation :: TcInstanceAnnotation -> String
renderInstanceAnnotation ann =
  renderTcSignature (tcInstanceDictName ann) (tcInstanceDictType ann)
    <> case tcInstanceSuperClasses ann of
      [] -> ""
      superClasses -> " [superclasses: " <> intercalate ", " (map (renderEvTerm . snd) superClasses) <> "]"
    <> case tcInstanceAssociatedTypes ann of
      [] -> ""
      equations -> " [associated types: " <> intercalate ", " (map renderTypeFamilyEquation equations) <> "]"

renderTypeFamilyEquation :: TypeFamilyInstanceInfo -> String
renderTypeFamilyEquation equation =
  renderTcType (tfiiLeft equation) <> " = " <> renderTcType (tfiiRight equation)

renderInstanceMethodAnnotation :: TcInstanceMethodAnnotation -> String
renderInstanceMethodAnnotation ann =
  renderTcSignature (tcInstanceMethodName ann) (tcInstanceMethodType ann)

renderElaboration :: TcAnnotation -> [String]
renderElaboration ann =
  typeArgs
    <> evidenceTerms
    <> termArgTypes
  where
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
        Just provenance ->
          renderTypeMismatch provenance
        Nothing ->
          "couldn't match " <> renderTcType left <> " with " <> renderTcType right
    OccursCheckError variable ty ->
      "occurs check failed: " <> renderTcType variable <> " occurs in " <> renderTcType ty
    UnboundVariable name ->
      "unbound variable " <> name
    KindMismatch expected actual ->
      "kind mismatch: expected " <> renderTcType expected <> ", got " <> renderTcType actual
    UnsolvedWanted pred' _ ->
      "unsolved constraint " <> renderPred pred'
    TopLevelUnliftedBinding name ty ->
      "top-level binding " <> T.unpack name <> " has unlifted type " <> renderTcType ty
    RepresentationPolymorphicFunctionArgument name ty ->
      "function argument " <> T.unpack name <> " has type " <> renderTcType ty <> " without a fixed runtime representation"
    FunDepUnknownTyVar className name ->
      "the functional dependency of class " <> T.unpack className <> " names " <> T.unpack name <> ", which is not a parameter of the class"
    InstanceFunDepCoverage predicate determiners determined ->
      "instance " <> renderPred predicate <> " does not determine " <> unwords (map T.unpack determined) <> " from " <> unwords (map T.unpack determiners)
    InstanceFunDepConflict predicate other determiners determined ->
      "instance " <> renderPred predicate <> " conflicts with instance " <> renderPred other <> " under the functional dependency " <> renderFunDepNames determiners determined
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
    ImplicitParamOrigin name -> "the implicit parameter " <> T.unpack name

renderEvTerm :: EvTerm -> String
renderEvTerm ev =
  case ev of
    -- After finalization, an evidence variable is only the placeholder for a
    -- reported unsolved constraint. Its unique is not stable, so hide it.
    EvVarTerm _ -> "unsolved"
    EvGiven pred' -> "given " <> renderPred pred'
    EvDict _ name typeArgs evidence ->
      T.unpack name
        <> renderTypeArgs typeArgs
        <> renderEvidenceArgs evidence
    EvEqualityDict _ left right _ -> "equality @" <> renderTcType left <> " @" <> renderTcType right
    EvCoercible _ left right -> "coercible @" <> renderTcType left <> " @" <> renderTcType right
    EvCoercion coercion -> renderCoercion coercion
    EvSuperClass evidence _ _ _ index -> "super[" <> show index <> "](" <> renderEvTerm evidence <> ")"
    EvCast evidence coercion -> "cast(" <> renderEvTerm evidence <> ", " <> renderCoercion coercion <> ")"
    EvTypeable _ ty _ _ arguments ->
      "typeable @" <> renderTcType ty <> renderEvidenceArgs arguments
    EvTypeableLiteral _ ty _ evidence ->
      "typeable @" <> renderTcType ty <> renderEvidenceArgs [evidence]
    EvTypeableApp _ function argument functionEvidence argumentEvidence ->
      "typeable @(" <> renderTcType function <> " " <> renderTcType argument <> ")" <> renderEvidenceArgs [functionEvidence, argumentEvidence]
    EvWithDict _ arguments _ target _ _ ->
      "withdict " <> T.unpack (tyConName target) <> renderTypeArgs arguments
    EvTypeLit _ ty literal ->
      "knownliteral @" <> renderTcType ty <> " " <> renderTyLit literal
    EvHasField _ _ recordType _ label _ ->
      "hasfield " <> T.unpack label <> " @" <> renderTcType recordType
    EvTypeLam variable body ->
      "Λ" <> T.unpack (tvName variable) <> ". " <> renderEvTerm body
    EvDictLam predicate _ body ->
      "λ(_ ∷ " <> renderPred predicate <> "). " <> renderEvTerm body
    EvTypeApp function argument ->
      "(" <> renderEvTerm function <> ") @" <> renderTcType argument
    EvDictApp function argument ->
      "(" <> renderEvTerm function <> ") (" <> renderEvTerm argument <> ")"
    EvCallStackPush _ function site parent ->
      "push(" <> T.unpack function <> "@" <> show (callSiteStartLine site) <> ":" <> show (callSiteStartColumn site) <> ", " <> renderEvTerm parent <> ")"
    EvCallStackEmpty _ -> "emptyCallStack"
    EvExceptionContextEmpty _ -> "emptyExceptionContext"
    EvRecursive predicate _ body ->
      "rec(" <> renderPred predicate <> "). " <> renderEvTerm body

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
    GivenCo predicate -> "given " <> renderPred predicate
    Refl ty -> "refl " <> renderTcType ty
    Sym co -> "sym (" <> renderCoercion co <> ")"
    Trans left right -> "trans (" <> renderCoercion left <> ") (" <> renderCoercion right <> ")"
    AppCo function argument -> "app (" <> renderCoercion function <> ") (" <> renderCoercion argument <> ")"
    FunCo domain range -> "fun (" <> renderCoercion domain <> ") (" <> renderCoercion range <> ")"
    ForAllCo tyVar body -> "forall " <> T.unpack (tvName tyVar) <> ". (" <> renderCoercion body <> ")"
    NthCo index proof -> "nth[" <> show index <> "](" <> renderCoercion proof <> ")"
    LeftCo proof -> "left(" <> renderCoercion proof <> ")"
    RightCo proof -> "right(" <> renderCoercion proof <> ")"
    EvidenceCo _ evidence -> renderEvTerm evidence
    TyConAppCo tyCon _ args _ -> T.unpack (tyConName tyCon) <> " " <> unwords (map renderCoercion args)
    AxiomInstCo key tys -> show key <> renderTypeArgs tys

renderEvVar :: EvVar -> String
renderEvVar (EvVar (Unique unique)) = "ev" <> show unique

renderPred :: Pred -> String
renderPred pred' =
  case pred' of
    ClassPred cls args -> T.unpack (tyConName cls) <> concatMap ((" " <>) . renderTcType) args
    EqPred left right -> renderTcType left <> " ~ " <> renderTcType right
    IParamPred name payload -> T.unpack name <> " ∷ " <> renderTcType payload
    IrredPred constraint -> renderTcType constraint
    QuantifiedPred variables antecedents consequent ->
      "forall "
        <> unwords (map (T.unpack . tvName) variables)
        <> ". "
        <> (if null antecedents then "" else "(" <> intercalate ", " (map renderPred antecedents) <> ") => ")
        <> renderPred consequent
