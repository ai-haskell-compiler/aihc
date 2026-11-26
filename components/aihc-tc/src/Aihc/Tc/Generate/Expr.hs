{-# LANGUAGE OverloadedStrings #-}

-- | Constraint generation for expressions.
--
-- This module implements bidirectional type inference/checking for the
-- surface expression language. It walks the surface AST and returns the same
-- expression with pending type-checker annotations attached at the exact sites
-- that produced them.
module Aihc.Tc.Generate.Expr
  ( inferExpr,
    inferExprAt,
    checkExpr,
    checkRhs,
  )
where

import Aihc.Resolve (RecKnotVariables (..), Resolved (..), displayIdentifier, resolvedOf)
import Aihc.Tc.Annotations (TcAnnotation (..), TcRecStmt (..), pendingAnnotation, pendingTypeLambdaAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (DataConFieldInfo (..), DataConInfo (..), DataTypeInfo (..), PatSynDirection (..), PatSynInfo (..), RecordHead (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..), EvVar)
import Aihc.Tc.Generate.Bind (checkGuardedRhss, inferGuardedRhss, inferLocalDecls)
import Aihc.Tc.Generate.Pattern
import Aihc.Tc.Generate.PatternBranch (solvePatternBranch)
import Aihc.Tc.Generate.Record (lookupRecordHead, orderRecordFields, recordFieldLabel, recordHeadNameSyntax, recordUpdateHeads, synthesizedRecordLocal)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (checkSurfaceType, explicitForallNames, scopedSigTyVars, sigToScheme, tcTypeKind, unboxedSumType)
import Aihc.Tc.Monad
import Aihc.Tc.QuickLook (quickLookUnify)
import Aihc.Tc.Solve.Dict (DictResult (..), solveDictWithGivens)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Solve.Family (isTypeFamilyTyCon, reduceTypeFamilies)
import Aihc.Tc.Solve.FunDep (improveFunDeps)
import Aihc.Tc.Syntax
  ( Annotation,
    ArithSeq (..),
    CaseAlt (..),
    CompStmt (..),
    Decl (..),
    DoBindMethods (..),
    DoFlavor (..),
    DoRecMethods (..),
    DoStmt (..),
    Expr (..),
    ExprTc (..),
    FloatType (..),
    GuardedRhs (..),
    LambdaCaseAlt (..),
    LiteralSyntax (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    PatternTc (..),
    Pragma,
    RecordField (..),
    Rhs (..),
    SourceSpan,
    TupleFlavor (..),
    Type (..),
    UnqualifiedName (..),
    addNameCast,
    addNameTc,
    annotateBranchCast,
    annotateExprCast,
    annotateFunCast,
    annotateRhsCast,
    annotateSigCast,
    annotationSpan,
    arithSeqMethod,
    mapArithSeqMethod,
    mkSpanAnnotation,
    peelExprAnn,
    qualifyName,
    setCompStmtTc,
  )
import Aihc.Tc.Syntax.Traverse (Walk (..), Walkable, collected, idWalk, runCollect, walk)
import Aihc.Tc.TypeScheme (schemeToType)
import Aihc.Tc.Types
import Aihc.Tc.Unify (unifyDeferring, unifyDeferringUnderGivens)
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad (forM_, mfilter, when, zipWithM)
import Data.Bifunctor qualified as Bifunctor
import Data.Char (isLower, isUpper)
import Data.IntSet (IntSet)
import Data.IntSet qualified as IntSet
import Data.List (partition)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isNothing, listToMaybe, mapMaybe, maybeToList)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Infer the type of an expression.
--
-- Returns the pending-annotated expression, the inferred type, and wanted
-- constraints.
inferExpr :: Expr -> TcM (Expr, TcType, [Ct])
inferExpr = inferExprAt Nothing

inferExprAt :: Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferExprAt ambient expr = case expr of
  EInt (LiteralSyntaxOverloaded integerType conversion) _ TInteger _
    | Just resolution <- syntaxResolution conversion ->
        inferOverloadedLiteral ambient "fromInteger" (maybeToList (integerType >>= syntaxResolution)) resolution expr
  EFloat (LiteralSyntaxOverloaded _ conversion) _ TFractional _
    | Just resolution <- syntaxResolution conversion ->
        inferOverloadedLiteral ambient "fromRational" [] resolution expr
  EString (Just conversion) _ _
    | Just resolution <- syntaxResolution conversion ->
        inferOverloadedLiteral ambient "fromString" [] resolution expr
  EList syntax (Just method) items
    | Just resolution <- syntaxResolution method ->
        inferOverloadedList (resolutionSpan resolution <|> ambient) syntax method resolution items
  EArithSeq (Just method) arithSeq
    | Just resolution <- syntaxResolution method ->
        inferOverloadedSequence (resolutionSpan resolution <|> ambient) method resolution arithSeq
  _
    | Just typeName <- primitiveLiteralTypeName expr,
      Just resolution <- syntaxResolution typeName ->
        inferPrimitiveLiteral resolution expr
  EIf _ (Just method) cond thenE elseE
    | Just resolution <- syntaxResolution method ->
        inferRebindableIf (resolutionSpan resolution <|> ambient) method resolution cond thenE elseE
  ENegate method inner
    | Just resolution <- syntaxResolution method ->
        inferNegate (resolutionSpan resolution <|> ambient) method resolution inner
  EGetField method record field
    | Just resolution <- syntaxResolution method ->
        inferGetField (resolutionSpan resolution <|> ambient) method resolution record field
  EGetFieldProjection method fields
    | Just resolution <- syntaxResolution method ->
        inferGetFieldProjection (resolutionSpan resolution <|> ambient) method resolution fields
  EVar name ->
    inferVar (exprSpan expr <|> ambient) name
  EImplicitParam name ->
    inferImplicitParam (exprSpan expr <|> ambient) name
  EInt {} ->
    abortTc "integer literal is missing its resolver type annotation"
  EFloat {} ->
    abortTc "fractional literal is missing its resolver type annotation"
  EChar _ _ ->
    literalResult expr charType
  ECharHash {} ->
    abortTc "primitive character literal is missing its resolver type annotation"
  EString {} ->
    literalResult expr stringType
  EStringHash {} ->
    abortTc "primitive string literal is missing its resolver type annotation"
  ELambdaPats _ pats body ->
    inferLambda (exprSpan expr <|> ambient) pats body
  ELambdaCase _ alts ->
    inferLambdaCase (exprSpan expr <|> ambient) alts
  ELambdaCases _ alts ->
    inferLambdaCases (exprSpan expr <|> ambient) alts
  EApp {} ->
    inferApplicationSpine ambient expr
  ETypeApp {} ->
    inferApplicationSpine ambient expr
  EInfix {} ->
    inferApplicationSpine ambient expr
  ESectionL inner op ->
    inferSectionL (exprSpan expr <|> ambient) inner op
  ESectionR op inner ->
    inferSectionR (exprSpan expr <|> ambient) op inner
  EIf _ method cond thenE elseE ->
    inferIf (exprSpan expr <|> ambient) method cond thenE elseE
  EMultiWayIf _ alternatives ->
    inferMultiWayIf (exprSpan expr <|> ambient) alternatives
  ECase _ scrutinee alts ->
    inferCase (exprSpan expr <|> ambient) scrutinee alts
  ERecordCon name fields wildcard ->
    inferRecordCon (sourceSpanFromAnns (nameAnns name) <|> ambient) name fields wildcard
  ERecordUpd record fields ->
    inferRecordUpdate (exprSpan expr <|> ambient) record fields
  ELetDecls decls body -> do
    (decls', body', bodyTy, cts) <- inferLocalDecls inferExpr decls (inferExpr body)
    pure (ELetDecls decls' body', bodyTy, cts)
  EParen inner -> do
    (inner', ty, cts) <- inferExprAt (exprSpan expr <|> ambient) inner
    pure (EParen inner', ty, cts)
  -- An expression pragma such as SCC does not change the type. The
  -- compiler ignores pragmas and keeps the wrapped expression.
  EPragma pragma inner -> do
    (inner', ty, cts) <- inferExprAt ambient inner
    pure (EPragma pragma inner', ty, cts)
  ETypeSig inner tyAnn -> do
    inferTypeSig (exprSpan expr <|> ambient) inner tyAnn
  ENegate method inner -> do
    (inner', innerTy, cs) <- inferExpr inner
    pure (ENegate method inner', innerTy, cs)
  EAnn ann inner -> do
    (inner', ty, cts) <- inferExprAt (annotationSpan ann <|> ambient) inner
    pure (EAnn ann inner', ty, cts)
  ETc ann inner -> do
    (inner', ty, cts) <- inferExprAt ambient inner
    pure (ETc ann inner', ty, cts)
  ETuple syntax flavor elems ->
    inferTuple (exprSpan expr <|> ambient) syntax flavor elems
  EUnboxedSum alternative arity inner -> do
    (inner', innerType, constraints) <- inferExprAt ambient inner
    types <- mapM (\index -> if index == alternative then pure innerType else freshMetaTv) [0 .. arity - 1]
    sumType <- unboxedSumType types
    let pending = pendingAnnotation sumType types [] []
    pure (annotatePendingExprAt ambient pending (EUnboxedSum alternative arity inner'), sumType, constraints)
  EList syntax _ elems ->
    inferList (exprSpan expr <|> ambient) syntax elems
  EListComp body quals ->
    inferListComp (exprSpan expr <|> ambient) body quals
  EListCompParallel body branches ->
    inferListCompParallel (exprSpan expr <|> ambient) body branches
  EArithSeq _ arithSeq ->
    inferArithSeq (exprSpan expr <|> ambient) arithSeq
  EDo _ stmts flavor ->
    inferDo (exprSpan expr <|> ambient) flavor stmts
  -- A Template Haskell quote or a quasi-quote compiles to a runtime
  -- error, so it has any type the context wants. The splices in an
  -- untyped quote must use the monad of the quote.
  _ | isTemplateHaskellQuote expr -> inferTemplateHaskellQuote (exprSpan expr <|> ambient) expr
  EQuasiQuote {} -> literalResult expr freshMetaTv
  other -> do
    emitError (exprSpan expr <|> ambient) (OtherError ("unsupported expression form in TC MVP: " ++ take 50 (show other)))
    ty <- freshMetaTv
    pure (expr, ty, [])

-- | A Template Haskell quote such as @[| e |]@, @[t| ty |]@, or @'name@.
isTemplateHaskellQuote :: Expr -> Bool
isTemplateHaskellQuote expr =
  case expr of
    ETHExpQuote {} -> True
    ETHTypedQuote {} -> True
    ETHDeclQuote {} -> True
    ETHTypeQuote {} -> True
    ETHPatQuote {} -> True
    ETHNameQuote {} -> True
    ETHTypeNameQuote {} -> True
    _ -> False

-- | Infer the type of a Template Haskell quote.
--
-- The type checker does not check the body of a quote. But each splice in
-- the body of an untyped quote is an expression at the level of the quote.
-- GHC gives the quote the type @m t@ and gives each splice a type @m s@ with
-- the same @m@. Thus a splice can fix the monad of a local binding that only
-- the splice uses.
inferTemplateHaskellQuote :: Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferTemplateHaskellQuote sp expr =
  case untypedQuoteSplices expr of
    [] -> literalResult expr freshMetaTv
    splices -> do
      monad <- freshMetaTv
      quoteTy <- TcAppTy monad <$> freshMetaTv
      spliceCts <- concat <$> mapM (spliceConstraints monad) splices
      pure (annotatePendingExpr (pendingAnnotation quoteTy [] [] []) expr, quoteTy, spliceCts)
  where
    spliceConstraints monad splice = do
      (_, spliceTy, cts) <- inferExprAt sp splice
      resultTy <- freshMetaTv
      ev <- freshEvVar
      pure (mkWantedCt (EqPred spliceTy (TcAppTy monad resultTy)) ev (AppOrigin sp) sp : cts)

-- | The splice expressions in the body of an untyped quote.
--
-- The search does not go into a nested quote, because the splices of a
-- nested quote are at a different level.
untypedQuoteSplices :: Expr -> [Expr]
untypedQuoteSplices expr =
  case peelExprAnn expr of
    ETHExpQuote body -> quoteBodySplices body
    ETHDeclQuote decls -> concatMap quoteBodySplices decls
    ETHTypeQuote ty -> quoteBodySplices ty
    ETHPatQuote pat -> quoteBodySplices pat
    _ -> []

quoteBodySplices :: (Walkable a) => a -> [Expr]
quoteBodySplices =
  runCollect
    ( walk
        idWalk
          { walkExpr = \children expr ->
              case expr of
                ETHSplice inner -> collected [inner]
                _
                  | isTemplateHaskellQuote expr -> collected []
                  | otherwise -> children expr,
            walkType = \children ty ->
              case ty of
                TSplice inner -> collected [inner]
                _ -> children ty,
            walkPattern = \children pat ->
              case pat of
                PSplice inner -> collected [inner]
                _ -> children pat,
            walkDecl = \children decl ->
              case decl of
                DeclSplice inner -> collected [inner]
                _ -> children decl
          }
    )

literalResult :: Expr -> TcM TcType -> TcM (Expr, TcType, [Ct])
literalResult expr typeAction = do
  ty <- typeAction
  pure (annotatePendingExpr (pendingAnnotation ty [] [] []) expr, ty, [])

-- | Infer the type of a variable reference.
inferVar :: Maybe SourceSpan -> Name -> TcM (Expr, TcType, [Ct])
inferVar ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let expr =
        case mPending of
          Just pending -> annotatePendingExprAt (sourceSpanFromAnns (nameAnns nameSyntax)) pending (EVar nameSyntax)
          Nothing -> EVar nameSyntax
  pure (expr, ty, cts)

-- | Infer the type of an implicit-parameter use such as @?x@.
--
-- The use has a fresh type and wants @?x@ at that type. The solver connects
-- the wanted constraint to a binding, and the evidence is the bound value.
inferImplicitParam :: Maybe SourceSpan -> Text -> TcM (Expr, TcType, [Ct])
inferImplicitParam sp name = do
  ty <- freshMetaTv
  ev <- freshEvVar
  let ct = mkWantedCt (IParamPred name ty) ev (ImplicitParamOrigin name) sp
      expr = annotatePendingExprAt sp (pendingAnnotation ty [] [ev] []) (EImplicitParam name)
  pure (expr, ty, [ct])

inferOperator :: Maybe SourceSpan -> Name -> TcM (Name, TcType, [Ct])
inferOperator ambient nameSyntax = do
  (mPending, ty, cts) <- inferNameOccurrence ambient nameSyntax
  let name' =
        case mPending of
          Just pending -> annotatePendingName pending nameSyntax
          Nothing -> nameSyntax
  pure (name', ty, cts)

inferNameOccurrence :: Maybe SourceSpan -> Name -> TcM (Maybe TcAnnotation, TcType, [Ct])
inferNameOccurrence ambient nameSyntax = do
  let sp = sourceSpanFromAnns (nameAnns nameSyntax) <|> ambient
      name = nameToText nameSyntax
  target <- resolvedTermTarget nameSyntax
  mBinder <- lookupResolvedTerm name target
  rejectUnidirectionalPatSyn sp name target
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp name) (instPreds inst)
      let typeArgs = instTypeArgs inst
          inferredCount = length typeArgs - length (instSpecifiedArgs inst)
          evidenceVars = map ctEvVar cts
          pending = occurrenceAnnotation (instType inst) typeArgs inferredCount evidenceVars
      pure (pending, instType inst, cts)
    Just (TcMonoIdBinder ty) -> do
      (instantiatedTy, typeArgs, predicates) <- instantiateSigmaType ty
      cts <- mapM (predToCt sp name) predicates
      let evidenceVars = map ctEvVar cts
      pure (occurrenceAnnotation instantiatedTy typeArgs 0 evidenceVars, instantiatedTy, cts)
    Nothing -> do
      -- A record field whose type has an existential variable has no
      -- selector function. Record construction, record patterns, and
      -- record wildcards can still name it, but a use as a function is an
      -- error, as in GHC.
      existentialField <- isExistentialRecordField name target
      if existentialField
        then do
          emitError sp (OtherError ("Cannot use record selector " <> T.unpack name <> " as a function due to escaped type variables"))
          ty <- freshMetaTv
          pure (Nothing, ty, [])
        else abortTc ("resolved term missing from type environment: " <> show name <> " resolved as " <> show target)

-- | Whether a term is a record field of a constructor of the same module
-- that has no selector function, because its type has an existential
-- variable.
isExistentialRecordField :: Text -> Entity -> TcM Bool
isExistentialRecordField name target =
  case target of
    EntityGlobal global -> do
      dataTypes <- getModuleDataTypes (globalNameModule global, globalNamePackage global)
      pure $
        or
          [ True
          | dataType <- dataTypes,
            constructor <- dtiConstructors dataType,
            snd (dciOrigin constructor) == globalNameModule global,
            field <- dciFields constructor,
            dcfiLabel field == Just name || dcfiLabel field == Just (globalNameText global)
          ]
    _ -> pure False

-- | A unidirectional pattern synonym has no builder. An expression cannot
-- use it.
rejectUnidirectionalPatSyn :: Maybe SourceSpan -> Text -> Entity -> TcM ()
rejectUnidirectionalPatSyn sp name target = do
  mPatSyn <- lookupPatSynTarget target
  case mPatSyn of
    Just info
      | psiDirection info == PatSynUnidirectionalInfo ->
          emitError sp (OtherError ("unidirectional pattern synonym " <> T.unpack name <> " cannot be used as an expression"))
    _ -> pure ()

occurrenceAnnotation :: TcType -> [TcType] -> Int -> [EvVar] -> Maybe TcAnnotation
occurrenceAnnotation ty typeArgs inferredCount evidenceVars
  | null typeArgs && null evidenceVars = Nothing
  | otherwise = Just (pendingAnnotation ty typeArgs evidenceVars []) {tcAnnInferredTypeArgs = inferredCount}

-- | Quantify free variables in an expression signature. Each name denotes
-- one variable. Keep variables from the enclosing scope unchanged.
-- 'sigToScheme' also permits a result of any @TYPE r@ kind.
inferTypeSig :: Maybe SourceSpan -> Expr -> Type -> TcM (Expr, TcType, [Ct])
inferTypeSig sp inner tyAnn = do
  sigTy <- schemeToType <$> sigToScheme tyAnn
  if isPolyType sigTy
    then inferPolyTypeSig sp inner tyAnn sigTy
    else inferMonoTypeSig sp inner tyAnn sigTy

-- | An expression signature that is a polytype cannot be equated with the
-- inferred type of the expression: the expression is checked against the
-- skolemized signature, with its context as givens and its explicit
-- @forall@ variables in scope, exactly as a higher-rank argument is. The
-- signature is then instantiated again, because the expression itself
-- stands where a monotype is expected.
inferPolyTypeSig :: Maybe SourceSpan -> Expr -> Type -> TcType -> TcM (Expr, TcType, [Ct])
inferPolyTypeSig sp inner tyAnn sigTy = do
  boundary <- getUniqueBoundary
  skolemized@(skolems, _, sigBody) <- skolemizeSigmaType sigTy
  let scope = scopedSigTyVars (explicitForallNames tyAnn) skolems
  (inner', innerTy, innerCts) <-
    withScopedTyVars scope $
      if checksExpectedResult inner
        then checkExpr sigBody inner
        else inferExprAt sp inner
  (annotatedInner, residualCts) <-
    finishHigherRankArgument sp boundary sigTy skolemized inner' innerTy innerCts
  (instantiatedTy, typeArgs, predicates) <- instantiateSigmaType sigTy
  instantiationCts <- mapM (predToCt sp "<expression>") predicates
  let pending = pendingAnnotation instantiatedTy typeArgs (map ctEvVar instantiationCts) []
  pure
    ( annotatePendingExprAt sp pending (ETypeSig annotatedInner tyAnn),
      instantiatedTy,
      residualCts <> instantiationCts
    )

inferMonoTypeSig :: Maybe SourceSpan -> Expr -> Type -> TcType -> TcM (Expr, TcType, [Ct])
inferMonoTypeSig sp inner tyAnn sigTy = do
  (inner', innerTy, cts) <- inferExprAt sp inner
  ev <- freshEvVar
  let sigCt =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = innerTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin sp
            }
          TypeTrace
            { typeTraceType = sigTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = TypeSignatureOrigin "<expression>" sp
            }
          ev
          (SigOrigin sp)
          sp
  -- A given equality can be what proves the signature, and then FC needs
  -- the cast the proof carries. The annotation is dropped again when the
  -- proof is reflexivity.
  pure (annotateSigCast sigTy ev (ETypeSig inner' tyAnn), sigTy, cts <> [sigCt])

-- | Infer an overloaded literal that a class method converts.
--
-- @literalResolutions@ holds the resolution of the type of the argument of
-- the method, when the resolver gives it. The argument type of the method
-- must then equal that type. An integer literal is
-- @fromInteger (n :: Integer)@.
--
-- The literal gets one annotation: the result type, with the type
-- arguments and the evidence of the method.
inferOverloadedLiteral :: Maybe SourceSpan -> Text -> [Resolved] -> Resolved -> Expr -> TcM (Expr, TcType, [Ct])
inferOverloadedLiteral ambient methodName literalResolutions resolution literalExpr = do
  let sp = resolutionSpan resolution <|> ambient
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  resultTy <- freshMetaTv
  ev <- freshEvVar
  literalArgTy <-
    case methodTy of
      TcFunTy argumentTy _ -> pure argumentTy
      _ -> abortTc (T.unpack methodName <> " does not have a function type")
  literalCts <- concat <$> mapM (\literalResolution -> resolvedLiteralCts sp literalResolution literalArgTy) literalResolutions
  let expectedMethodTy = TcFunTy literalArgTy resultTy
      methodEq =
        mkWantedEqCt
          TypeTrace
            { typeTraceType = methodTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf methodName)
            }
          TypeTrace
            { typeTraceType = expectedMethodTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
            }
          ev
          (LitOrigin sp)
          sp
      pending =
        pendingAnnotation
          resultTy
          typeArgs
          (map ctEvVar methodCts)
          []
  pure (annotatePendingExprAt sp pending literalExpr, resultTy, methodCts <> [methodEq] <> literalCts)

inferPrimitiveLiteral :: Resolved -> Expr -> TcM (Expr, TcType, [Ct])
inferPrimitiveLiteral resolution literalExpr = do
  maybeInfo <- lookupResolvedTypeSyntax resolution
  case maybeInfo of
    Just info ->
      literalResult literalExpr (pure (TcTyCon (tciTyCon info) []))
    Nothing ->
      abortTc ("resolved primitive literal type missing from type environment: " <> show (resolutionTarget resolution))

-- | The name of the type of a primitive literal expression.
primitiveLiteralTypeName :: Expr -> Maybe Name
primitiveLiteralTypeName expr =
  case expr of
    EInt (LiteralSyntaxPrimitive typeName) _ _ _ -> Just typeName
    EFloat (LiteralSyntaxPrimitive typeName) _ _ _ -> Just typeName
    ECharHash typeName _ _ -> Just typeName
    EStringHash typeName _ _ -> Just typeName
    _ -> Nothing

-- | The resolution of a syntax name, when the resolver found it.
syntaxResolution :: Name -> Maybe Resolved
syntaxResolution name = nameResolution name >>= resolvedOf

annotatePendingExpr :: TcAnnotation -> Expr -> Expr
annotatePendingExpr ann =
  ETc (ExprType ann)

-- | Give an expression its source span, if it has one.
atSpan :: Maybe SourceSpan -> Expr -> Expr
atSpan Nothing = id
atSpan (Just sp) = EAnn (mkSpanAnnotation sp)

annotatePendingExprAt :: Maybe SourceSpan -> TcAnnotation -> Expr -> Expr
annotatePendingExprAt Nothing ann =
  annotatePendingExpr ann
annotatePendingExprAt (Just sp) ann =
  EAnn (mkSpanAnnotation sp) . annotatePendingExpr ann

annotatePendingName :: TcAnnotation -> Name -> Name
annotatePendingName ann name =
  addNameTc ann name

-- | Give the method name of a do statement its checked type, and the cast
-- onto the type that the statement uses it at. The cast goes first. The
-- method is equated as @method ~ (action -> continuation -> block)@, so
-- the proof runs forwards. A given equality can be what makes the method
-- fit the statement.
annotateDoMethodName :: TcType -> EvVar -> TcAnnotation -> Name -> Name
annotateDoMethodName ty evidence pending name =
  addNameTc pending (addNameCast ty evidence name)

-- | Cast the operator of a section onto the arrow type that the section
-- uses. The operator is equated as @actual ~ arrow@, so the proof runs
-- forwards. A given equality can prove it: with the given @v ~ Diff p@,
-- the operator of @(.-. z)@ has the result type @Diff p@, but the
-- section can have the result type @v@.
annotateOperatorCast :: TcType -> EvVar -> Name -> Name
annotateOperatorCast ty evidence name =
  addNameCast ty evidence name

-- | Convert a predicate to a wanted constraint.
predToCt :: Maybe SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name p = do
  ev <- freshEvVar
  pure $
    mkWantedCt p ev (OccurrenceOf name) sp

-- | Infer the type of a lambda expression.
inferLambda :: Maybe SourceSpan -> [Pattern] -> Expr -> TcM (Expr, TcType, [Ct])
inferLambda sp pats body = do
  argTys <- mapM (const freshMetaTv) pats
  patCheck <- checkFunctionPatterns sp (zip pats argTys)
  (body', bodyTy, bodyCts) <- withPatternScope patCheck (inferExpr body)
  remainingCts <- solvePatternBranch sp patCheck bodyTy bodyCts
  let funTy = foldr TcFunTy bodyTy argTys
      pats' = zipWith (annotateLambdaPattern (pcBindings patCheck)) argTys (pcPatterns patCheck)
      lambda = atSpan sp (ELambdaPats (Just funTy) pats' body')
  pure (lambda, funTy, remainingCts)

annotateLambdaPattern :: [(UnqualifiedName, TcType)] -> TcType -> Pattern -> Pattern
annotateLambdaPattern bindings argTy pat =
  let annotated = annotatePatternBindings bindings pat
   in if lambdaPatternCarriesBinderType annotated
        then annotated
        else PTc (PatternType (pendingAnnotation argTy [] [] [])) annotated

lambdaPatternCarriesBinderType :: Pattern -> Bool
lambdaPatternCarriesBinderType (PAnn _ inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PTc _ inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PVar {} = True
lambdaPatternCarriesBinderType (PParen inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType PAs {} = True
lambdaPatternCarriesBinderType (PStrict inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PIrrefutable inner) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType (PTypeSig inner _) = lambdaPatternCarriesBinderType inner
lambdaPatternCarriesBinderType _ = False

inferLambdaCase :: Maybe SourceSpan -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferLambdaCase sp alts = do
  argTy <- freshMetaTv
  resTy <- freshMetaTv
  lambdaCaseWith sp argTy resTy alts

-- | Check a lambda-case with known argument and result types. The
-- alternatives then see the skolems of the expected type, and a GADT
-- pattern can refine them with its givens.
lambdaCaseWith :: Maybe SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
lambdaCaseWith sp argTy resTy alts = do
  (alts', cts) <- inferCaseAlts sp argTy resTy alts
  -- The desugarer reads the type of the function from this annotation. A
  -- lambda-case with no alternative has no other source of it.
  let funTy = TcFunTy argTy resTy
  pure (atSpan sp (ELambdaCase (Just funTy) alts'), funTy, cts)

-- | Pass the expected result into case branches before equality refinement.
checkExpr :: TcType -> Expr -> TcM (Expr, TcType, [Ct])
checkExpr expected expression = case expression of
  EAnn annotation inner | checksExpectedResult inner -> do
    (inner', ty, constraints) <- checkExpr expected inner
    pure (EAnn annotation inner', ty, constraints)
  ETc annotation inner | checksExpectedResult inner -> do
    (inner', ty, constraints) <- checkExpr expected inner
    pure (ETc annotation inner', ty, constraints)
  EParen inner -> do
    (inner', ty, constraints) <- checkExpr expected inner
    pure (EParen inner', ty, constraints)
  -- A pragma does not change the type, so the expected type goes through it.
  EPragma pragma inner -> do
    (inner', ty, constraints) <- checkExpr expected inner
    pure (EPragma pragma inner', ty, constraints)
  ELambdaPats _ patterns body -> checkLambda expected (exprSpan expression) patterns body
  ELambdaCase _ alternatives -> do
    expectedParts <- splitExpectedFunction 1 expected
    case expectedParts of
      Just ([argumentType], resultType) -> lambdaCaseWith (exprSpan expression) argumentType resultType alternatives
      _ -> inferExpr expression
  ELambdaCases _ alternatives -> do
    expectedParts <- splitExpectedFunction (lambdaCasesArity alternatives) expected
    case expectedParts of
      Just (argumentTypes, resultType) -> lambdaCasesWith (exprSpan expression) argumentTypes resultType alternatives
      Nothing -> inferExpr expression
  EList syntax Nothing elements -> inferListWithExpected (Just expected) (exprSpan expression) syntax elements
  ECase _ scrutinee alternatives -> do
    (scrutinee', scrutineeType, constraints) <- inferExpr scrutinee
    prepareScrutinee constraints
    (alternatives', branchConstraints) <- inferCaseAlts (exprSpan expression) scrutineeType expected alternatives
    pure (atSpan (exprSpan expression) (ECase (Just expected) scrutinee' alternatives'), expected, constraints <> branchConstraints)
  EDo _ statements flavor | isMonadicDoFlavor flavor -> do
    (statements', ty, constraints) <- inferDoStmtsWith (Just expected) (exprSpan expression) statements
    pure (atSpan (exprSpan expression) (EDo (Just ty) statements' flavor), ty, constraints)
  ELetDecls declarations body -> do
    (declarations', body', ty, constraints) <- inferLocalDecls inferExpr declarations (checkExpr expected body)
    pure (ELetDecls declarations' body', ty, constraints)
  _
    | isAnnotatedApplication expression -> checkApplicationAt Nothing expression
    | otherwise -> inferExpr expression
  where
    -- The annotations keep their source spans for the spine, as
    -- 'inferExprAt' keeps them.
    checkApplicationAt ambient expr = case expr of
      EAnn annotation inner -> do
        (inner', ty, constraints) <- checkApplicationAt (annotationSpan annotation <|> ambient) inner
        pure (EAnn annotation inner', ty, constraints)
      ETc annotation inner -> do
        (inner', ty, constraints) <- checkApplicationAt ambient inner
        pure (ETc annotation inner', ty, constraints)
      EParen inner -> do
        (inner', ty, constraints) <- checkApplicationAt ambient inner
        pure (EParen inner', ty, constraints)
      EPragma pragma inner -> do
        (inner', ty, constraints) <- checkApplicationAt ambient inner
        pure (EPragma pragma inner', ty, constraints)
      _ -> checkApplicationSpine expected ambient expr
    isAnnotatedApplication expr = case expr of
      EAnn _ inner -> isAnnotatedApplication inner
      ETc _ inner -> isAnnotatedApplication inner
      EParen inner -> isAnnotatedApplication inner
      EPragma _ inner -> isAnnotatedApplication inner
      EApp {} -> True
      ETypeApp {} -> True
      EInfix {} -> True
      _ -> False

-- | Give lambda parameters their expected types before the body check.
checkLambda :: TcType -> Maybe SourceSpan -> [Pattern] -> Expr -> TcM (Expr, TcType, [Ct])
checkLambda expected sp patterns body = do
  expectedParts <- splitExpected expected patterns
  case expectedParts of
    Nothing -> inferLambda sp patterns body
    Just (argumentTypes, resultType) -> do
      patternCheck <- checkFunctionPatterns sp (zip patterns argumentTypes)
      (body', bodyType, bodyConstraints) <-
        withPatternScope patternCheck (checkExpr resultType body)
      equality <- wantedDoEq sp resultType bodyType
      constraints <- solvePatternBranch sp patternCheck resultType (bodyConstraints <> [equality])
      let functionType = foldr TcFunTy resultType argumentTypes
          patterns' = zipWith (annotateLambdaPattern (pcBindings patternCheck)) argumentTypes (pcPatterns patternCheck)
          lambda = atSpan sp (ELambdaPats (Just functionType) patterns' (annotateExprCast resultType (ctEvVar equality) body'))
      pure (lambda, functionType, constraints)
  where
    splitExpected ty [] = pure (Just ([], ty))
    splitExpected ty (_ : rest) = do
      zonked <- zonkType ty
      case zonked of
        TcFunTy argument result -> do
          remaining <- splitExpected result rest
          pure (fmap (Bifunctor.first (argument :)) remaining)
        _ -> pure Nothing

checkRhs :: TcType -> Rhs Expr -> TcM (Rhs Expr, TcType, [Ct])
checkRhs expected rhs = case rhs of
  UnguardedRhs annotations rhsInfos expression Nothing -> do
    (expression', ty, constraints) <- checkExpr expected expression
    pure (UnguardedRhs annotations rhsInfos expression' Nothing, ty, constraints)
  UnguardedRhs annotations rhsInfos expression (Just declarations) -> do
    (declarations', expression', ty, constraints) <- inferLocalDecls inferExpr declarations (checkExpr expected expression)
    pure (UnguardedRhs annotations rhsInfos expression' (Just declarations'), ty, constraints)
  -- A guarded body gets the expected type, as an unguarded body does.
  GuardedRhss annotations rhsInfos guardedRhss Nothing -> do
    (guardedRhss', ty, constraints) <- checkGuardedRhss inferExpr (checkExpr expected) expected guardedRhss
    pure (GuardedRhss annotations rhsInfos guardedRhss' Nothing, ty, constraints)
  GuardedRhss annotations rhsInfos guardedRhss (Just declarations) -> do
    (declarations', guardedRhss', ty, constraints) <-
      inferLocalDecls inferExpr declarations (checkGuardedRhss inferExpr (checkExpr expected) expected guardedRhss)
    pure (GuardedRhss annotations rhsInfos guardedRhss' (Just declarations'), ty, constraints)

-- | Whether 'checkExpr' gives the expected type to this expression.
-- Keep this list of constructors the same as the list in 'checkExpr'.
checksExpectedResult :: Expr -> Bool
checksExpectedResult expression = case expression of
  EAnn _ inner -> checksExpectedResult inner
  ETc _ inner -> checksExpectedResult inner
  EParen inner -> checksExpectedResult inner
  EPragma _ inner -> checksExpectedResult inner
  ECase {} -> True
  ELetDecls {} -> True
  EDo _ _ flavor -> isMonadicDoFlavor flavor
  ELambdaPats {} -> True
  ELambdaCase {} -> True
  ELambdaCases {} -> True
  -- An overloaded list does not give the expected type to its list.
  EList _ method _ -> isNothing method
  _ -> False

-- | Split an expected type into the given number of argument types and a
-- result type. The result is 'Nothing' if the zonked type has fewer arrows.
splitExpectedFunction :: Int -> TcType -> TcM (Maybe ([TcType], TcType))
splitExpectedFunction count ty
  | count <= 0 = pure (Just ([], ty))
  | otherwise = do
      zonked <- zonkType ty
      case zonked of
        TcFunTy argument result -> do
          remaining <- splitExpectedFunction (count - 1) result
          pure (fmap (Bifunctor.first (argument :)) remaining)
        _ -> pure Nothing

-- | Connect application results before constructor patterns refine their indices.
prepareScrutinee :: [Ct] -> TcM ()
prepareScrutinee = mapM_ prepare
  where
    prepare constraint = case ctPred constraint of
      EqPred {} -> do
        _ <- solveEquality constraint
        pure ()
      _ -> pure ()

inferCase :: Maybe SourceSpan -> Expr -> [CaseAlt Expr] -> TcM (Expr, TcType, [Ct])
inferCase sp scrutinee alts = do
  (scrutinee', scrutTy, scrutCts) <- inferExpr scrutinee
  prepareScrutinee scrutCts
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts sp scrutTy resTy alts
  pure (atSpan sp (ECase (Just resTy) scrutinee' alts'), resTy, scrutCts ++ altCts)

-- | Record construction is constructor application with the arguments in
-- field declaration order.
inferRecordCon :: Maybe SourceSpan -> Name -> [RecordField Expr] -> Bool -> TcM (Expr, TcType, [Ct])
inferRecordCon sp name fields wildcard = do
  -- The resolver expands a record wildcard into puns, so one that survives
  -- means the constructor's fields were not in scope.
  when wildcard $
    abortTc ("the fields of the record wildcard construction of " <> T.unpack (nameText name) <> " are not in scope at " <> show sp)
  -- A record pattern synonym builds through its builder, which is bound
  -- under the synonym's own name, so both heads expand the same way.
  head' <- lookupRecordHead name
  args <- orderRecordFields sp head' fields missingField
  inferExprAt sp (foldl EApp (EVar name) args)
  where
    -- GHC rejects a construction that leaves out a strict field. A lazy
    -- field that the construction leaves out is bottom. An empty case
    -- expression is bottom of each type, and the FC modules give it a
    -- runtime failure.
    missingField label strict
      | strict =
          abortTc ("record construction of " <> T.unpack (nameText name) <> " does not give the strict field " <> show (fromMaybe "<positional>" label) <> " at " <> show sp)
      | otherwise =
          pure (ECase Nothing (EChar 'x' "'x'") [])

-- | A record update is a case expression. Each alternative matches one
-- head that has every updated field and rebuilds it with the new field
-- values. A record pattern synonym is one such head, and it has exactly
-- one alternative.
inferRecordUpdate :: Maybe SourceSpan -> Expr -> [RecordField Expr] -> TcM (Expr, TcType, [Ct])
inferRecordUpdate sp record fields = do
  (record', recordTy, recordCts) <- inferExprAt sp record
  zonked <- zonkType recordTy
  heads <- recordUpdateHeads sp (Just zonked) fields
  alts <- mapM updateAlternative heads
  resTy <- freshMetaTv
  (alts', altCts) <- inferCaseAlts sp recordTy resTy alts
  pure (atSpan sp (ECase (Just resTy) record' alts'), resTy, recordCts ++ altCts)
  where
    updateAlternative :: RecordHead -> TcM (CaseAlt Expr)
    updateAlternative head' = do
      let labels = rhFields head'
      -- A binder takes the label of its field, so the code is easier to read.
      -- The binder is resolved by its unique, so it does not hide a variable
      -- with the same name in an updated value.
      binders <- zipWithM (\index label -> synthesizedRecordLocal (fromMaybe ("$field" <> T.pack (show index)) (mfilter isVariableLabel label))) [1 :: Int ..] labels
      let conSyntax = recordHeadNameSyntax head'
          argument label binder =
            case [recordFieldValue occurrence | occurrence <- fields, Just (recordFieldLabel occurrence) == label] of
              value : _ -> value
              [] -> EVar (qualifyName Nothing binder)
          body = foldl EApp (EVar conSyntax) (zipWith argument labels binders)
      pure (CaseAlt [] (PCon conSyntax [] (map PVar binders)) (UnguardedRhs [] [] body Nothing))

-- | Whether a record label is a variable identifier, and not an operator.
isVariableLabel :: Text -> Bool
isVariableLabel label =
  case T.uncons label of
    Just (first, _) -> isLower first || first == '_'
    Nothing -> False

inferLambdaCases :: Maybe SourceSpan -> [LambdaCaseAlt] -> TcM (Expr, TcType, [Ct])
inferLambdaCases sp alts = do
  argTys <- mapM (const freshMetaTv) [1 .. lambdaCasesArity alts]
  resTy <- freshMetaTv
  lambdaCasesWith sp argTys resTy alts

lambdaCasesArity :: [LambdaCaseAlt] -> Int
lambdaCasesArity alts = maximum (0 : map (length . lambdaCaseAltPats) alts)

-- | Check a @\\cases@ expression with known argument and result types.
lambdaCasesWith :: Maybe SourceSpan -> [TcType] -> TcType -> [LambdaCaseAlt] -> TcM (Expr, TcType, [Ct])
lambdaCasesWith sp argTys resTy alts = do
  results <- mapM (inferLambdaCaseAlt sp argTys resTy) alts
  let alts' = map fst results
      cts = concatMap snd results
  pure (ELambdaCases Nothing alts', foldr TcFunTy resTy argTys, cts)

inferCaseAlts :: Maybe SourceSpan -> TcType -> TcType -> [CaseAlt Expr] -> TcM ([CaseAlt Expr], [Ct])
inferCaseAlts _sp _scrutTy _resTy [] = pure ([], [])
inferCaseAlts sp scrutTy resTy alternatives = do
  results <- mapM inferAlt alternatives
  pure (map fst results, concatMap snd results)
  where
    inferAlt (CaseAlt altAnns pat rhs) = do
      let altSp = sourceSpanFromAnns altAnns
          branchSp = (<|>) altSp sp
      patCheck <- checkPattern branchSp pat scrutTy
      (rhs', rhsTy, rhsCts) <- withPatternScope patCheck (checkRhs resTy rhs)
      resultEv <- freshEvVar
      let rhsSp = rhsExprSpan rhs <|> branchSp
          resultCt =
            mkWantedEqCt
              TypeTrace
                { typeTraceType = rhsTy,
                  typeTraceRole = ActualType,
                  typeTraceOrigin = ExpressionTypeOrigin rhsSp
                }
              TypeTrace
                { typeTraceType = resTy,
                  typeTraceRole = ExpectedType,
                  typeTraceOrigin = ConstraintTypeOrigin (CaseBranchOrigin branchSp)
                }
              resultEv
              (CaseBranchOrigin rhsSp)
              rhsSp
          pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch branchSp patCheck resTy (rhsCts <> [resultCt])
      -- A constructor pattern can refine the result through its givens, so
      -- the alternative casts its body like a function equation does.
      pure (CaseAlt altAnns pat' (annotateRhsCast resTy resultEv rhs'), remainingCts)

inferLambdaCaseAlt :: Maybe SourceSpan -> [TcType] -> TcType -> LambdaCaseAlt -> TcM (LambdaCaseAlt, [Ct])
inferLambdaCaseAlt sp argTys resTy alt = do
  let pats = lambdaCaseAltPats alt
      rhs = lambdaCaseAltRhs alt
  patCheck <- checkFunctionPatterns sp (zip pats argTys)
  (rhs', rhsTy, rhsCts) <- withPatternScope patCheck (checkRhs resTy rhs)
  ev <- freshEvVar
  let pats' = map (annotatePatternBindings (pcBindings patCheck)) (pcPatterns patCheck)
      rhsCt = mkWantedCt (EqPred rhsTy resTy) ev (AppOrigin sp) sp
  remainingCts <- solvePatternBranch sp patCheck resTy (rhsCts <> [rhsCt])
  pure (alt {lambdaCaseAltPats = pats', lambdaCaseAltRhs = annotateRhsCast resTy ev rhs'}, remainingCts)

sourceSpanFromAnns :: [Annotation] -> Maybe SourceSpan
sourceSpanFromAnns anns =
  listToMaybe (mapMaybe annotationSpan anns)

-- | An application spine is a head and the frames applied to it, from the
-- head outwards. A source span, a parenthesis, or a pragma between the
-- head and an argument is a frame, so @(f x) y@ is one spine and the
-- quick look at @y@ can use what @x@ revealed.
data SpineFrame
  = SpineValueArg (Maybe SourceSpan) Expr
  | -- | The right operand of an infix operator. The frame rebuilds the
    -- infix node, so the partial application cannot be instantiated.
    SpineInfixRhs (Maybe SourceSpan) Expr
  | SpineTypeArg (Maybe SourceSpan) Type
  | SpineParen
  | SpinePragma Pragma
  | SpineAnn Annotation

data SpineHead
  = SpineHeadExpr Expr
  | -- | An infix operator. Its operands are the first two frames.
    SpineHeadOperator (Maybe SourceSpan) Name

collectSpine :: Maybe SourceSpan -> Expr -> (SpineHead, [SpineFrame])
collectSpine = go []
  where
    go frames sp expr =
      case expr of
        EAnn ann inner
          | Just nodeSpan <- annotationSpan ann ->
              go (SpineAnn ann : frames) (Just nodeSpan) inner
        EApp fun arg -> go (SpineValueArg sp arg : frames) sp fun
        ETypeApp fun tyArg -> go (SpineTypeArg sp tyArg : frames) sp fun
        EParen inner -> go (SpineParen : frames) sp inner
        EPragma pragma inner -> go (SpinePragma pragma : frames) sp inner
        EInfix lhs op rhs ->
          (SpineHeadOperator sp op, SpineValueArg sp lhs : SpineInfixRhs sp rhs : frames)
        _ -> (SpineHeadExpr expr, frames)

-- | Whether an argument's type is known without inferring it: the
-- argument is a variable, an application of a variable, or an expression
-- with a type signature. The quick look reads such an argument before any
-- argument is checked, so a later rank-2 argument sees the instantiation
-- it fixes: in @gdeciding (Proxy :: Proxy Eq) (\b -> b == b)@ the given
-- @q b@ of the lambda is @Eq b@ only after the signature fixes @q@.
isGuardedArgument :: Expr -> Bool
isGuardedArgument expr =
  case expr of
    EVar {} -> True
    ETypeSig {} -> True
    EApp fun _ -> isGuardedArgument fun
    ETypeApp fun _ -> isGuardedArgument fun
    EInfix {} -> True
    ESectionL {} -> True
    ESectionR {} -> True
    EParen inner -> isGuardedArgument inner
    EPragma _ inner -> isGuardedArgument inner
    EAnn ann inner
      | Just _ <- annotationSpan ann -> isGuardedArgument inner
    _ -> False

-- | A value argument after the quick look, before it is checked.
data ArgState
  = ArgDeferred Expr
  | -- | The argument was inferred by the quick look. The unique boundary
    -- was taken before the inference, for the escape check.
    ArgInferred Unique Expr TcType [Ct]

data ArgPlan = ArgPlan
  { argPlanSpan :: Maybe SourceSpan,
    -- | The instantiation of the function's polymorphic type before this
    -- argument, with its wanted constraints.
    argPlanInstantiation :: Maybe (TcAnnotation, [Ct]),
    -- | The wanted equality that gives an unknown function type an arrow.
    argPlanArrowCts :: [Ct],
    -- | The arrow type the function is cast to, and the evidence of the
    -- equality that proves the cast. Only set when the function's type was
    -- not already an arrow.
    argPlanArrowCast :: Maybe (TcType, EvVar),
    argPlanExpected :: TcType,
    argPlanArgument :: ArgState,
    argPlanIsInfixRhs :: Bool
  }

data SpineStep
  = StepArg ArgPlan
  | StepTypeArg Type [Ct]
  | StepParen
  | StepPragma Pragma
  | StepAnn Annotation

-- | Infer an application spine.
--
-- The head is instantiated once. Its fresh meta-variables are the
-- instantiation variables of the spine. A first pass over the frames
-- (the plan) walks the function type, infers the guarded arguments, and
-- quick-look-unifies their types with the expected argument types, which
-- may bind an instantiation variable to a polytype. A second pass checks
-- every argument against its zonked expected type: an argument whose
-- expected type is a polytype is checked against it, any other argument
-- is inferred and equated. The checked nodes rebuild the source shape.
inferApplicationSpine :: Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
inferApplicationSpine = applicationSpine Nothing

-- | Check an application spine against the type that its place expects.
--
-- An argument of a qualified type, such as @c => r@, is checked under its
-- givens. The result type of the spine can fix the types in the argument
-- type. Then the quick look binds them from the expected type before the
-- arguments are checked. If it did not, the argument would fix them
-- outside of the givens.
checkApplicationSpine :: TcType -> Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
checkApplicationSpine expected = applicationSpine (Just expected)

applicationSpine :: Maybe TcType -> Maybe SourceSpan -> Expr -> TcM (Expr, TcType, [Ct])
applicationSpine expected ambient expr = do
  let (spineHead, frames) = collectSpine ambient expr
  (headExpr, headTy, headCts, headTypeArgs) <-
    case spineHead of
      SpineHeadExpr headSyntax -> do
        (headExpr, headTy, headCts) <- inferExprAt ambient headSyntax
        pure (headExpr, headTy, headCts, pendingTypeArgs headExpr)
      SpineHeadOperator sp op -> do
        (op', opTy, opCts) <- inferOperator sp op
        pure (EVar op', opTy, opCts, nameTypeArgs op')
  let instantiationVariables = IntSet.fromList (map uniqueKey (concatMap typeMetaVariables headTypeArgs))
  -- Fix the result before a guarded argument can fix it outside the
  -- givens of a later lambda or do block.
  case (expected, spineResultType headTy frames) of
    (Just expectedTy, Just resultType)
      | not (isPolyType expectedTy),
        any frameNeedsExpectedType frames -> do
          quickLookUnify instantiationVariables expectedTy resultType
          -- A GADT constructor relates its result to its fields by an
          -- equality: @(:*) :: (ys ~ (x ': xs)) => f x -> NP f xs -> NP f
          -- ys@. The result type only fixes @ys@. A look at the equality
          -- then fixes @x@ and @xs@ before a field is checked.
          forM_ [(left, right) | ct <- headCts, EqPred left right <- [ctPred ct]] $
            uncurry (quickLookUnify instantiationVariables)
    _ -> pure ()
  (steps, resultTy, spineVariables) <- planSpine instantiationVariables headTypeArgs headTy frames
  case expected of
    Just expectedTy -> do
      qualified <- anyM qualifiedArgument steps
      let guided = any guidedArgument steps
      when (qualified || (guided && not (isPolyType expectedTy))) (quickLookUnify spineVariables expectedTy resultTy)
    Nothing -> pure ()
  (expr', stepCts) <- checkSpineSteps headExpr steps
  -- An application with a polymorphic result, for example a record field
  -- of higher rank applied to its record, is instantiated like a variable.
  -- A quick look at it as an argument then binds an instantiation variable
  -- to the instantiated type, not to the polytype.
  zonkedResultTy <- zonkType resultTy
  if isPolyType zonkedResultTy
    then do
      let sp = exprSpan expr <|> ambient
      (instantiated, typeArgs, predicates) <- instantiateSigmaType zonkedResultTy
      cts <- mapM (predToCt sp "<application>") predicates
      let pending = pendingAnnotation instantiated typeArgs (map ctEvVar cts) []
      pure (annotatePendingExprAt sp pending expr', instantiated, headCts <> stepCts <> cts)
    else pure (expr', resultTy, headCts <> stepCts)

guidedArgument :: SpineStep -> Bool
guidedArgument (StepArg plan) = case argPlanArgument plan of
  ArgDeferred argument -> checksExpectedResult argument
  ArgInferred {} -> False
guidedArgument _ = False

frameNeedsExpectedType :: SpineFrame -> Bool
frameNeedsExpectedType (SpineValueArg _ argument) = containsCheckedArgument argument
frameNeedsExpectedType (SpineInfixRhs _ argument) = containsCheckedArgument argument
frameNeedsExpectedType _ = False

-- | Whether an argument checks against its expected type, or is an
-- application that has such an argument. In @Comp . (\ns -> case ns of Z
-- fx -> Some fx) . unK@, the operand @(\ns -> ...) . unK@ is an
-- application, but its lambda checks a case under the givens of @Z@.
-- Thus the result type must fix the types before that lambda.
containsCheckedArgument :: Expr -> Bool
containsCheckedArgument expression
  | checksExpectedResult expression = True
  | otherwise = case expression of
      EInfix lhs _ rhs -> containsCheckedArgument lhs || containsCheckedArgument rhs
      EApp function argument -> containsCheckedArgument function || containsCheckedArgument argument
      EParen inner -> containsCheckedArgument inner
      EPragma _ inner -> containsCheckedArgument inner
      EAnn _ inner -> containsCheckedArgument inner
      ETc _ inner -> containsCheckedArgument inner
      _ -> False

spineResultType :: TcType -> [SpineFrame] -> Maybe TcType
spineResultType ty [] = Just ty
spineResultType ty (frame : rest) = case frame of
  SpineTypeArg {} -> Nothing
  SpineValueArg {} -> resultAfterArgument
  SpineInfixRhs {} -> resultAfterArgument
  _ -> spineResultType ty rest
  where
    resultAfterArgument = case ty of
      TcFunTy _ result -> spineResultType result rest
      _ -> Nothing

-- | Whether an argument gives its fresh type variables to the quick look of
-- the spine: a bare data constructor, as @Comp@, or an operator section,
-- as @(<$ ctx)@. In @(<$ ctx) <$> case accum of Add w -> ...@ of
-- @fused-effects@, the section fixes the element type of @<$>@ at the
-- type @a@ of the result before the GADT branches are checked. The name
-- of a data constructor starts with an upper-case letter or a colon.
isConstructorArgument :: Expr -> Bool
isConstructorArgument expr =
  case expr of
    EVar name -> case T.uncons (nameText name) of
      Just (first, _) -> isUpper first || first == ':'
      Nothing -> False
    ESectionL {} -> True
    ESectionR {} -> True
    EParen inner -> isConstructorArgument inner
    EPragma _ inner -> isConstructorArgument inner
    EAnn _ inner -> isConstructorArgument inner
    ETc _ inner -> isConstructorArgument inner
    _ -> False

-- | Whether an argument is an operator section.
isSectionArgument :: Expr -> Bool
isSectionArgument expr =
  case expr of
    ESectionL {} -> True
    ESectionR {} -> True
    EParen inner -> isSectionArgument inner
    EPragma _ inner -> isSectionArgument inner
    EAnn _ inner -> isSectionArgument inner
    ETc _ inner -> isSectionArgument inner
    _ -> False

-- | Whether a type family application is part of a type.
mentionsFamily :: (TyCon -> Bool) -> TcType -> Bool
mentionsFamily family = go
  where
    go ty = case ty of
      TcTyCon tyCon arguments -> family tyCon || any go arguments
      TcFunTy argument result -> go argument || go result
      TcAppTy function argument -> go function || go argument
      TcForAllTy _ body -> go body
      TcQualTy _ body -> go body
      _ -> False

-- | Whether a polytype is part of a type.
mentionsPolyType :: TcType -> Bool
mentionsPolyType ty = case ty of
  TcForAllTy {} -> True
  TcQualTy {} -> True
  TcFunTy argument result -> mentionsPolyType argument || mentionsPolyType result
  TcAppTy function argument -> mentionsPolyType function || mentionsPolyType argument
  TcTyCon _ arguments -> any mentionsPolyType arguments
  _ -> False

-- | Whether a step is an argument whose expected type has givens.
qualifiedArgument :: SpineStep -> TcM Bool
qualifiedArgument step =
  case step of
    StepArg plan -> hasGivens <$> zonkType (argPlanExpected plan)
    _ -> pure False
  where
    hasGivens ty =
      case ty of
        TcForAllTy _ body -> hasGivens body
        TcQualTy (_ : _) _ -> True
        _ -> False

uniqueKey :: Unique -> Int
uniqueKey (Unique key) = key

nameTypeArgs :: Name -> [TcType]
nameTypeArgs name =
  case nameTc name of
    Just pending -> tcAnnTypeArgs pending
    Nothing -> []

-- | The first pass over the frames. The remaining type arguments are the
-- instantiation's type arguments that visible type applications have not
-- consumed yet; a value argument ends them. The result holds the
-- instantiation variables of the whole spine.
planSpine :: IntSet -> [TcType] -> TcType -> [SpineFrame] -> TcM ([SpineStep], TcType, IntSet)
planSpine = go
  where
    go instantiationVariables _ funTy [] = pure ([], funTy, instantiationVariables)
    go instantiationVariables remainingTypeArgs funTy (frame : frames) =
      case frame of
        SpineAnn ann -> continue (StepAnn ann) instantiationVariables remainingTypeArgs funTy frames
        SpineParen -> continue StepParen instantiationVariables remainingTypeArgs funTy frames
        SpinePragma pragma -> continue (StepPragma pragma) instantiationVariables remainingTypeArgs funTy frames
        SpineTypeArg sp tyArg -> do
          scoped <- getScopedTyVars
          case remainingTypeArgs of
            inferredTy : rest -> do
              expectedKind <- tcTypeKind inferredTy
              explicitTy <- checkSurfaceType scoped tyArg expectedKind
              -- The explicit type may be a polytype. The quick look binds
              -- the instantiation variable to it; the wanted equality
              -- then checks the two agree.
              quickLookUnify instantiationVariables inferredTy explicitTy
              ev <- freshEvVar
              let origin = InstOrigin "visible type application"
                  eqCt =
                    mkWantedEqCt
                      TypeTrace
                        { typeTraceType = inferredTy,
                          typeTraceRole = InferredType,
                          typeTraceOrigin = ConstraintTypeOrigin origin
                        }
                      TypeTrace
                        { typeTraceType = explicitTy,
                          typeTraceRole = RequiredType,
                          typeTraceOrigin = ConstraintTypeOrigin origin
                        }
                      ev
                      origin
                      sp
              continue (StepTypeArg tyArg [eqCt]) instantiationVariables rest funTy frames
            [] -> do
              emitError sp (OtherError "visible type application requires a polymorphic expression")
              continue (StepTypeArg tyArg []) instantiationVariables [] funTy frames
        SpineValueArg sp arg -> valueArg sp arg False instantiationVariables funTy frames
        SpineInfixRhs sp arg -> valueArg sp arg True instantiationVariables funTy frames

    continue step instantiationVariables remainingTypeArgs funTy frames = do
      (steps, resultTy, spineVariables) <- go instantiationVariables remainingTypeArgs funTy frames
      pure (step : steps, resultTy, spineVariables)

    valueArg sp arg isInfixRhs instantiationVariables funTy frames = do
      zonkedFunTy <- zonkType funTy
      -- A function with a polymorphic type, for example a record field
      -- with a higher-rank type, is instantiated at its application. Its
      -- fresh meta-variables are instantiation variables too. This also
      -- applies to the right operand of an infix node, as in
      -- @c `unC` k@ when @unC@ selects a higher-rank field.
      (instantiation, instantiatedFunTy, instantiationVariables') <-
        if isPolyType zonkedFunTy
          then do
            (instantiated, typeArgs, predicates) <- instantiateSigmaType zonkedFunTy
            cts <- mapM (predToCt sp "<application>") predicates
            let pending = pendingAnnotation instantiated typeArgs (map ctEvVar cts) []
                fresh = IntSet.fromList (map uniqueKey (concatMap typeMetaVariables typeArgs))
            pure (Just (pending, cts), instantiated, IntSet.union instantiationVariables fresh)
          else pure (Nothing, zonkedFunTy, instantiationVariables)
      (expectedArgTy, resultTy, arrowCts, arrowCast) <-
        case instantiatedFunTy of
          TcFunTy expectedArgTy resultTy -> pure (expectedArgTy, resultTy, [], Nothing)
          _ -> do
            argTy <- freshMetaTv
            resTy <- freshMetaTv
            let arrowTy = TcFunTy argTy resTy
            quickLookUnify instantiationVariables' instantiatedFunTy arrowTy
            ev <- freshEvVar
            -- A given equality can be what gives the function an arrow
            -- type, and then FC needs the cast the proof carries. The
            -- annotation is dropped again when the proof is reflexivity.
            pure (argTy, resTy, [mkWantedCt (EqPred instantiatedFunTy arrowTy) ev (AppOrigin sp) sp], Just (arrowTy, ev))
      (argState, instantiationVariables'') <-
        if isGuardedArgument arg
          then do
            boundary <- getUniqueBoundary
            -- The expected type only guides an application argument: its
            -- type is still inferred, and the quick look below and the
            -- equality of the second pass relate the two.
            (arg', argTy, argCts) <- checkExpr expectedArgTy arg
            -- The instantiation of the argument's own head, as @a@ of
            -- @Oracle :: (forall t. t -> Answer t a) -> Oracle a@ in
            -- @Oracle $ \c -> ...@, gives instantiation variables of this
            -- spine too, as in GHC's quick look. The result type can then
            -- fix them before a later argument is checked under givens.
            -- Only an argument with a polytype in its type needs this. Any
            -- other argument leaves its variables to the solver, which
            -- reduces a type family before it binds one.
            --
            -- A data constructor without a type family application in
            -- either type also gives its variables. In @Comp . (\ns -> case
            -- ns of Z fx -> Some fx)@, the result type then fixes @Comp@ at
            -- @f (g x)@. Thus the branch @Z fx@, checked later under its
            -- givens, does not fix the open @x@ of @Comp@ at the
            -- existential of @Z@. A variable keeps its old behavior: its
            -- type can relate to a given of the enclosing scope.
            zonkedArgTy <- zonkType argTy
            zonkedExpectedArgTy <- zonkType expectedArgTy
            family <- isTypeFamilyTyCon
            let Unique boundaryKey = boundary
                familyFree =
                  isConstructorArgument arg
                    && not (any (mentionsFamily family) (zonkedArgTy : zonkedExpectedArgTy : concat [[left, right] | (left, right) <- sectionEqualities]))
                sectionEqualities = [(left, right) | isSectionArgument arg, ct <- argCts, EqPred left right <- [ctPred ct]]
                argumentVariables
                  | mentionsPolyType zonkedArgTy || familyFree =
                      IntSet.fromList
                        [ key
                        | Unique key <- typeMetaVariables zonkedArgTy <> concat [typeMetaVariables left <> typeMetaVariables right | (left, right) <- sectionEqualities],
                          key >= boundaryKey
                        ]
                  | otherwise = IntSet.empty
                extended = IntSet.union instantiationVariables' argumentVariables
            quickLookUnify extended expectedArgTy argTy
            -- An operator section gets its type from equality wanteds, as
            -- @(<$ ctx)@ has the type @t1 -> t2@ with @f t1 ~ t2@. A look
            -- at them gives the structure of that type.
            when familyFree $
              forM_ sectionEqualities (uncurry (quickLookUnify extended))
            pure (ArgInferred boundary arg' argTy argCts, extended)
          else pure (ArgDeferred arg, instantiationVariables')
      let plan =
            ArgPlan
              { argPlanSpan = sp,
                argPlanInstantiation = instantiation,
                argPlanArrowCts = arrowCts,
                argPlanArrowCast = arrowCast,
                argPlanExpected = expectedArgTy,
                argPlanArgument = argState,
                argPlanIsInfixRhs = isInfixRhs
              }
      continue (StepArg plan) instantiationVariables'' [] resultTy frames

-- | The second pass: check each argument and rebuild the nodes.
--
-- An infix node has the operator as its head and the two operands as its
-- first two steps. The rebuild of the node needs the operator and the
-- checked left operand. The walk therefore keeps them from the step of the
-- left operand: the function of that step is the head of the spine, thus it
-- is the operator itself and no annotation hides it. An annotation that the
-- left operand gets is dropped with its application node, because the infix
-- node replaces that node. An infix operator has its own type, which a
-- given never refines, thus the annotation holds no necessary evidence.
checkSpineSteps :: Expr -> [SpineStep] -> TcM (Expr, [Ct])
checkSpineSteps = go Nothing
  where
    go _ fun [] = pure (fun, [])
    go operand fun (step : steps) =
      case step of
        StepAnn ann -> go operand (EAnn ann fun) steps
        StepParen -> go operand (EParen fun) steps
        StepPragma pragma -> go operand (EPragma pragma fun) steps
        StepTypeArg tyArg cts -> do
          (expr', moreCts) <- go operand (ETypeApp fun tyArg) steps
          pure (expr', cts <> moreCts)
        StepArg plan -> do
          let -- The parser gives an infix node no span of its own. An
              -- instantiation of the right operand's function then gets
              -- the span of the operator.
              sp = argPlanSpan plan
              operatorSpan = operand >>= sourceSpanFromAnns . nameAnns . fst
              instantiationSpan = sp <|> (if argPlanIsInfixRhs plan then operatorSpan else Nothing)
              -- A prefix application reads the instantiation of a
              -- variable from an expression annotation, not from the name.
              prefixFun = if argPlanIsInfixRhs plan then prefixOperatorHead fun else fun
              instantiated = maybe prefixFun (\(pending, _) -> annotatePendingExprAt instantiationSpan pending prefixFun) (argPlanInstantiation plan)
              -- An infix node is rebuilt by matching on the function, so
              -- an annotation there would hide the operator. An infix
              -- operator's type is its own, never refined by a given.
              -- But the application of the operator to its left operand
              -- can have a polymorphic type. The instantiation of that
              -- type needs an annotation, thus the node becomes two
              -- prefix applications.
              rebuildsInfix = argPlanIsInfixRhs plan && isNothing (argPlanInstantiation plan)
              fun' = case argPlanArrowCast plan of
                Just (arrowTy, ev) | not rebuildsInfix -> annotateFunCast arrowTy ev instantiated
                _ -> instantiated
              instantiationCts = maybe [] snd (argPlanInstantiation plan)
          expectedArgTy <- zonkType (argPlanExpected plan)
          (arg', argCts) <-
            if isPolyType expectedArgTy
              then case argPlanArgument plan of
                ArgDeferred arg -> checkHigherRankArgument sp expectedArgTy arg
                ArgInferred boundary arg' argTy cts -> checkInferredHigherRankArgument sp boundary expectedArgTy arg' argTy cts
              else do
                (arg', argTy, cts) <-
                  case argPlanArgument plan of
                    ArgDeferred arg -> checkExpr expectedArgTy arg
                    ArgInferred _ arg' argTy cts -> pure (arg', argTy, cts)
                ev <- freshEvVar
                let eqCt = mkWantedCt (EqPred expectedArgTy argTy) ev (AppOrigin sp) sp
                -- A given equality can be what makes the argument fit, and
                -- then FC needs the cast the proof carries. The annotation
                -- is dropped again when the proof is reflexivity.
                pure (annotateExprCast expectedArgTy ev arg', cts <> [eqCt])
          node <-
            if rebuildsInfix
              then case operand of
                Just (op', lhs') -> pure (EInfix lhs' op' arg')
                Nothing -> abortTc ("infix operator application lost its operator node at " <> show sp)
              else pure (EApp fun' arg')
          let operand' = case fun of
                EVar op' -> Just (op', arg')
                _ -> Nothing
          (expr', moreCts) <- go operand' node steps
          pure (expr', instantiationCts <> argPlanArrowCts plan <> argCts <> moreCts)

-- | Give the operator at the head of an application the form of a prefix
-- variable. The instantiation annotation of the operator name moves to an
-- expression annotation on the variable.
prefixOperatorHead :: Expr -> Expr
prefixOperatorHead expr =
  case expr of
    EApp fun arg -> EApp (prefixOperatorHead fun) arg
    EAnn ann inner -> EAnn ann (prefixOperatorHead inner)
    ETc ann inner -> ETc ann (prefixOperatorHead inner)
    EVar name ->
      case nameTc name of
        Just pending ->
          let plainName = name {nameTc = Nothing}
           in annotatePendingExprAt (sourceSpanFromAnns (nameAnns name)) pending (EVar plainName)
        Nothing -> expr
    _ -> expr

-- | Check an argument against a polytype. The argument is inferred and
-- its type unified with the skolemized polytype.
--
-- An expression that takes the expected type (a @do@ block, a @case@)
-- is checked against the skolemized body instead: the result type of a
-- @do@ block that the polytype fixes must reach its statements, or a
-- type family application over the monad is stuck while they are
-- checked and a meta variable is solved the wrong way.
checkHigherRankArgument :: Maybe SourceSpan -> TcType -> Expr -> TcM (Expr, [Ct])
checkHigherRankArgument sp expectedTy arg
  | checksExpectedResult arg = do
      boundary <- getUniqueBoundary
      skolemized@(_, predicates, expectedBody) <- skolemizeSigmaType expectedTy
      (arg', actualTy, argCts) <- withGivenPredicates predicates (checkExpr expectedBody arg)
      finishHigherRankArgument sp boundary expectedTy skolemized arg' actualTy argCts
  | otherwise = do
      boundary <- getUniqueBoundary
      (arg', actualTy, argCts) <- inferExpr arg
      checkInferredHigherRankArgument sp boundary expectedTy arg' actualTy argCts

-- | Whether a given is an equality. Only an equality can prove that two
-- types that do not unify are equal.
isEqualityGiven :: Pred -> Bool
isEqualityGiven predicate =
  case predicate of
    EqPred {} -> True
    _ -> False

-- | Check an already inferred argument against a polytype. The boundary
-- was taken before the inference: a meta-variable older than it must not
-- mention the skolems.
checkInferredHigherRankArgument :: Maybe SourceSpan -> Unique -> TcType -> Expr -> TcType -> [Ct] -> TcM (Expr, [Ct])
checkInferredHigherRankArgument sp boundary expectedTy arg' actualTy argCts = do
  skolemized <- skolemizeSigmaType expectedTy
  finishHigherRankArgument sp boundary expectedTy skolemized arg' actualTy argCts

-- | Tie an argument of a higher-rank application to the skolemized
-- polytype it was checked or inferred against.
finishHigherRankArgument :: Maybe SourceSpan -> Unique -> TcType -> ([TyVarId], [Pred], TcType) -> Expr -> TcType -> [Ct] -> TcM (Expr, [Ct])
finishHigherRankArgument sp boundary expectedTy (skolems, predicates, expectedBody) arg' actualTy argCts = do
  -- An equality that a stuck type family application leaves undecided
  -- becomes a wanted: the meta variable that blocks the reduction may
  -- only be solved by a later part of the enclosing binding.
  --
  -- A given of the enclosing scope can also be necessary. In @instance
  -- (pty ~ Basic) => Mask (Program pty n)@, the argument @f :: forall b.
  -- Program Basic n b -> ...@ of @mb :: (forall a. Program pty n a -> ...)
  -- -> ...@ needs the given @pty ~ Basic@.
  outerGivens <- getGivenPredicates
  let underGivens = not (null predicates) || any isEqualityGiven outerGivens
  deferred <-
    if underGivens
      then unifyDeferringUnderGivens sp actualTy expectedBody
      else unifyDeferring sp (AppOrigin sp) actualTy expectedBody
  -- Under givens, a deferred equality can need them, as @StM m a ~ a@
  -- needs the given @Pure m a@. Then the argument gets a cast to the
  -- expected type, and the proof of the cast can use the givens.
  (castArg, deferredCts) <-
    if not underGivens || null deferred
      then (arg',) <$> mapM deferredConstraint deferred
      else do
        evidence <- freshEvVar
        pure (annotateExprCast expectedBody evidence arg', [mkWantedCt (EqPred expectedBody actualTy) evidence (AppOrigin sp) sp])
  rejectEscapingHigherRankMetas sp boundary skolems actualTy
  givenCts <- mapM makeGiven predicates
  let (equalityCts, dictionaryCts) = partition isEqualityConstraint (argCts <> deferredCts)
  residualEqualities <- concat <$> mapM (solveEqualityConstraint predicates) equalityCts
  -- A functional dependency of a local given can fix a meta variable of
  -- the given: in @coerce (Magic k)@ of the reflection trick, the given
  -- @Reifies Skolem a0@ of the field of @Magic@ and the wanted @Reifies
  -- Skolem a@ of @k@ give @a0 := a@ by the dependency @s -> a@.
  _ <- improveFunDeps predicates dictionaryCts
  -- The improvement can give a meta variable of the enclosing scope a type
  -- with a skolem of the polytype: the given @Reifies Skolem a0@ and the
  -- wanted @Reifies Skolem (Proxy s)@ give @a0 := Proxy s@. The skolem
  -- then escapes, as GHC reports. Thus the escape check runs again, also
  -- over the givens, where such a meta variable can be.
  rejectEscapingHigherRankMetas sp boundary skolems (TcQualTy predicates actualTy)
  residualDictionaries <- concat <$> mapM (solveDictionary predicates) dictionaryCts
  -- A wanted that the local givens do not solve can need them and the
  -- givens of the enclosing scope together, as an instance context does.
  -- A given can also wait on a meta variable that a later argument
  -- solves, as the @c@ of @(c => r) -> e -> r@ waits on @e@. The wanted
  -- leaves with the local givens, as a wanted that leaves a pattern
  -- branch does.
  let withArgumentGivens ct = ct {ctBranchGivens = givenCts <> ctBranchGivens ct}
      annotatedArg = annotatePendingExprAt sp (pendingTypeLambdaAnnotation expectedTy skolems (map ctEvVar givenCts)) castArg
  pure (annotatedArg, map withArgumentGivens (residualEqualities <> residualDictionaries))
  where
    deferredConstraint (left, right) = do
      evidence <- freshEvVar
      pure (mkWantedCt (EqPred left right) evidence (AppOrigin sp) sp)

    makeGiven predicate = do
      evidence <- freshEvVar
      bindEvidence evidence (EvGiven predicate)
      pure ((mkWantedCt predicate evidence (AppOrigin sp) sp) {ctFlavor = Given})

    isEqualityConstraint ct =
      case ctPred ct of
        EqPred {} -> True
        _ -> False

    solveEqualityConstraint givens ct = do
      result <- withGivenPredicates givens (solveEquality ct)
      pure $ case result of
        EqSolved -> []
        EqStuck stuck -> [stuck]
        EqError err -> [err]

    solveDictionary givens ct = do
      result <- solveDictWithGivens givens ct
      pure $ case result of
        DictSolved -> []
        DictStuck stuck -> [stuck]

instantiateSigmaType :: TcType -> TcM (TcType, [TcType], [Pred])
instantiateSigmaType = go []
  where
    go arguments (TcForAllTy binder body) = do
      argument <- freshMetaTvOfKind (tvKind binder)
      go (arguments <> [argument]) (applySubst (Map.singleton (tvUnique binder) argument) body)
    -- Consecutive contexts are one context: @Pro p => Str p => t@, which a
    -- synonym such as @type Optic_ k p = Constraints k p => ...@ gives
    -- under another context, is @(Pro p, Str p) => t@. The dictionaries
    -- are arguments in the same order in both forms.
    go arguments (TcQualTy predicates body) = pure (inner, arguments, predicates <> nested)
      where
        (nested, inner) = consecutiveContexts body
    go arguments ty = pure (ty, arguments, [])
    consecutiveContexts (TcQualTy predicates body) =
      let (nested, inner) = consecutiveContexts body in (predicates <> nested, inner)
    consecutiveContexts ty = ([], ty)

skolemizeSigmaType :: TcType -> TcM ([TyVarId], [Pred], TcType)
skolemizeSigmaType = go [] []
  where
    go skolems predicates (TcForAllTy binder body) = do
      skolem <- setTyVarKind (tvKind binder) <$> freshSkolemTv (tvName binder)
      go (skolems <> [skolem]) predicates (applySubst (Map.singleton (tvUnique binder) (TcTyVar skolem)) body)
    go skolems predicates (TcQualTy morePredicates body) =
      go skolems (predicates <> morePredicates) body
    go skolems predicates ty = pure (skolems, predicates, ty)

rejectEscapingHigherRankMetas :: Maybe SourceSpan -> Unique -> [TyVarId] -> TcType -> TcM ()
rejectEscapingHigherRankMetas sp (Unique boundaryInt) skolems actualTy = do
  let olderMetas = filter (isOlderThan boundaryInt) (typeMetaVariables actualTy)
  escaped <- anyM (metaMentionsAnySkolem skolems) olderMetas
  when escaped $
    emitError sp (OtherError "higher-rank type variable escapes its argument")
  where
    isOlderThan threshold (Unique metaInt) = metaInt < threshold

metaMentionsAnySkolem :: [TyVarId] -> Unique -> TcM Bool
metaMentionsAnySkolem skolems meta = do
  ty <- zonkType (TcMetaTv meta)
  pure (any (`typeMentionsTyVar` ty) skolems)

anyM :: (a -> TcM Bool) -> [a] -> TcM Bool
anyM _ [] = pure False
anyM predicate (value : values) = do
  matches <- predicate value
  if matches then pure True else anyM predicate values

typeMetaVariables :: TcType -> [Unique]
typeMetaVariables ty =
  case ty of
    TcTyVar {} -> []
    TcArrowTy -> []
    TcTyLit {} -> []
    TcMetaTv meta -> [meta]
    TcKindedTyCon _ kindArguments -> concatMap typeMetaVariables kindArguments
    TcTyConApp _ kindArguments arguments -> concatMap typeMetaVariables kindArguments <> concatMap typeMetaVariables arguments
    TcFunTy argument result -> typeMetaVariables argument <> typeMetaVariables result
    TcForAllTy _ body -> typeMetaVariables body
    TcQualTy predicates body -> concatMap predicateMetaVariables predicates <> typeMetaVariables body
    TcAppTy function argument -> typeMetaVariables function <> typeMetaVariables argument

predicateMetaVariables :: Pred -> [Unique]
predicateMetaVariables predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeMetaVariables arguments
    EqPred left right -> typeMetaVariables left <> typeMetaVariables right
    IParamPred _ payload -> typeMetaVariables payload
    IrredPred constraint -> typeMetaVariables constraint
    QuantifiedPred variables antecedents consequent ->
      concatMap (typeMetaVariables . tvKind) variables
        <> concatMap predicateMetaVariables antecedents
        <> predicateMetaVariables consequent

pendingTypeArgs :: Expr -> [TcType]
pendingTypeArgs expr =
  case expr of
    EAnn _ inner -> pendingTypeArgs inner
    ETc info inner ->
      case info of
        ExprType pending -> drop (tcAnnInferredTypeArgs pending) (tcAnnTypeArgs pending)
        ExprCast _ -> pendingTypeArgs inner
    ETypeApp fun _ -> pendingTypeArgs fun
    EParen inner -> pendingTypeArgs inner
    EPragma _ inner -> pendingTypeArgs inner
    _ -> []

-- | Without PostfixOperators, the left section @(e op)@ is the function
-- @\\y -> op e y@. Its annotation records the type of @y@.
--
-- With PostfixOperators, the left section is the application @op e@. Its
-- annotation records no argument type, so the desugarer applies @op@ to
-- @e@ only.
inferSectionL :: Maybe SourceSpan -> Expr -> Name -> TcM (Expr, TcType, [Ct])
inferSectionL sp inner op = do
  postfix <- tcPostfixOperators
  if postfix then inferPostfixSection sp inner op else inferBinarySectionL sp inner op

inferPostfixSection :: Maybe SourceSpan -> Expr -> Name -> TcM (Expr, TcType, [Ct])
inferPostfixSection sp inner op = do
  (op', opTy, opCts) <- inferOperator sp op
  (inner', innerTy, innerCts) <- inferExpr inner
  resultTy <- freshMetaTv
  evidence <- freshEvVar
  let operatorTy = TcFunTy innerTy resultTy
      pending = pendingAnnotation resultTy [] [] []
      wanted = mkWantedCt (EqPred opTy operatorTy) evidence (AppOrigin sp) sp
      op'' = annotateOperatorCast operatorTy evidence op'
  pure (annotatePendingExprAt sp pending (ESectionL inner' op''), resultTy, opCts <> innerCts <> [wanted])

-- | A section operand of a polymorphic parameter type, such as a lens
-- in @(.^ l)@, is checked against that polytype like an application
-- argument. Inference would instantiate the operand.
inferBinarySectionL :: Maybe SourceSpan -> Expr -> Name -> TcM (Expr, TcType, [Ct])
inferBinarySectionL sp inner op = do
  (op', opTy, opCts) <- inferOperator sp op
  zonkedOpTy <- zonkType opTy
  case zonkedOpTy of
    TcFunTy innerExpected sectionTy@(TcFunTy argumentTy _)
      | isPolyType innerExpected -> do
          (inner', innerCts) <- checkHigherRankArgument sp innerExpected inner
          let pending = pendingAnnotation sectionTy [] [] [argumentTy]
          pure (annotatePendingExprAt sp pending (ESectionL inner' op'), sectionTy, opCts <> innerCts)
    _ -> do
      (inner', innerTy, innerCts) <- inferExpr inner
      argumentTy <- freshMetaTv
      resultTy <- freshMetaTv
      evidence <- freshEvVar
      let sectionTy = TcFunTy argumentTy resultTy
          operatorTy = TcFunTy innerTy sectionTy
          pending = pendingAnnotation sectionTy [] [] [argumentTy]
          wanted = mkWantedCt (EqPred opTy operatorTy) evidence (AppOrigin sp) sp
          op'' = annotateOperatorCast operatorTy evidence op'
      pure (annotatePendingExprAt sp pending (ESectionL inner' op''), sectionTy, opCts <> innerCts <> [wanted])

inferSectionR :: Maybe SourceSpan -> Name -> Expr -> TcM (Expr, TcType, [Ct])
inferSectionR sp op inner = do
  (op', opTy, opCts) <- inferOperator sp op
  zonkedOpTy <- zonkType opTy
  case zonkedOpTy of
    TcFunTy argumentTy (TcFunTy innerExpected resultTy)
      | isPolyType innerExpected -> do
          (inner', innerCts) <- checkHigherRankArgument sp innerExpected inner
          let sectionTy = TcFunTy argumentTy resultTy
              pending = pendingAnnotation sectionTy [] [] [argumentTy]
          pure (annotatePendingExprAt sp pending (ESectionR op' inner'), sectionTy, opCts <> innerCts)
    _ -> do
      (inner', innerTy, innerCts) <- inferExpr inner
      argumentTy <- freshMetaTv
      resultTy <- freshMetaTv
      evidence <- freshEvVar
      let sectionTy = TcFunTy argumentTy resultTy
          operatorTy = TcFunTy argumentTy (TcFunTy innerTy resultTy)
          pending = pendingAnnotation sectionTy [] [] [argumentTy]
          wanted = mkWantedCt (EqPred opTy operatorTy) evidence (AppOrigin sp) sp
          op'' = annotateOperatorCast operatorTy evidence op'
      pure (annotatePendingExprAt sp pending (ESectionR op'' inner'), sectionTy, opCts <> innerCts <> [wanted])

inferIf :: Maybe SourceSpan -> Maybe Name -> Expr -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferIf sp method cond thenE elseE = do
  (cond', condTy, condCts) <- inferExpr cond
  (thenE', thenTy, thenCts) <- inferExpr thenE
  (elseE', elseTy, elseCts) <- inferExpr elseE
  resultTy <- freshMetaTv
  condEv <- freshEvVar
  expectedBoolTy <- boolType
  let condCt = mkWantedCt (EqPred condTy expectedBoolTy) condEv (AppOrigin sp) sp
  thenEv <- freshEvVar
  elseEv <- freshEvVar
  let thenCt = mkWantedCt (EqPred thenTy resultTy) thenEv (AppOrigin sp) sp
      elseCt = mkWantedCt (EqPred elseTy resultTy) elseEv (AppOrigin sp) sp
      cond'' = annotateBranchCast expectedBoolTy condEv cond'
      thenE'' = annotateBranchCast resultTy thenEv thenE'
      elseE'' = annotateBranchCast resultTy elseEv elseE'
  pure (atSpan sp (EIf (Just resultTy) method cond'' thenE'' elseE''), resultTy, condCts ++ thenCts ++ elseCts ++ [condCt, thenCt, elseCt])

-- | A multi-way if is a guarded right-hand side without a binding. Each
-- alternative has the result type.
inferMultiWayIf :: Maybe SourceSpan -> [GuardedRhs Expr] -> TcM (Expr, TcType, [Ct])
inferMultiWayIf sp alternatives = do
  (alternatives', resultTy, cts) <- inferGuardedRhss inferExpr alternatives
  pure (atSpan sp (EMultiWayIf (Just resultTy) alternatives'), resultTy, cts)

-- | RebindableSyntax gives an if expression the in-scope ifThenElse.
--
-- The method name gets the method annotation, and the expression gets the
-- result annotation. The desugarer applies the method to the condition and
-- the two branches.
inferRebindableIf :: Maybe SourceSpan -> Name -> Resolved -> Expr -> Expr -> Expr -> TcM (Expr, TcType, [Ct])
inferRebindableIf sp method resolution cond thenE elseE = do
  (cond', condTy, condCts) <- inferExpr cond
  (thenE', thenTy, thenCts) <- inferExpr thenE
  (elseE', elseTy, elseCts) <- inferExpr elseE
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "ifThenElse" resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy condTy (TcFunTy thenTy (TcFunTy elseTy resultTy))
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "ifThenElse") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
  pure
    ( atSpan sp (EIf (Just resultTy) (Just (annotatePendingName methodPending method)) cond' thenE' elseE'),
      resultTy,
      condCts <> thenCts <> elseCts <> methodCts <> [methodEquality]
    )

-- | Negation applies the resolved negate method to its operand.
--
-- The method name gets the method annotation, and the expression gets the
-- result annotation. The desugarer applies the method to the operand.
inferNegate :: Maybe SourceSpan -> Name -> Resolved -> Expr -> TcM (Expr, TcType, [Ct])
inferNegate sp method resolution inner = do
  (inner', innerTy, innerCts) <- inferExpr inner
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "negate" resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy innerTy resultTy
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "negate") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      resultPending = pendingAnnotation resultTy [] [] []
  pure
    ( annotatePendingExprAt sp resultPending (ENegate (annotatePendingName methodPending method) inner'),
      resultTy,
      innerCts <> methodCts <> [methodEquality]
    )

-- | Record dot syntax applies the getField syntax term at the label of the
-- field, as GHC does. The expression @e.x@ is @getField \@"x" e@.
inferGetField :: Maybe SourceSpan -> Name -> Resolved -> Expr -> Name -> TcM (Expr, TcType, [Ct])
inferGetField sp method resolution record field = do
  (record', recordTy, recordCts) <- inferExpr record
  (field', resultTy, fieldCts) <- inferFieldSelection sp resolution recordTy field
  let pending = pendingAnnotation resultTy [] [] []
  pure
    ( annotatePendingExprAt sp pending (EGetField method record' field'),
      resultTy,
      recordCts <> fieldCts
    )

-- | A record dot projection such as @(.x.y)@ is a function. It applies
-- getField at each label in turn, first @x@ and then @y@. The checked
-- argument type of the annotation is the record type.
inferGetFieldProjection :: Maybe SourceSpan -> Name -> Resolved -> [Name] -> TcM (Expr, TcType, [Ct])
inferGetFieldProjection sp method resolution fields = do
  argumentTy <- freshMetaTv
  (fields', resultTy, cts) <- selectFields argumentTy fields
  let projectionTy = TcFunTy argumentTy resultTy
      pending = pendingAnnotation projectionTy [] [] [argumentTy]
  pure
    ( annotatePendingExprAt sp pending (EGetFieldProjection method fields'),
      projectionTy,
      cts
    )
  where
    selectFields recordTy [] = pure ([], recordTy, [])
    selectFields recordTy (field : rest) = do
      (field', fieldTy, fieldCts) <- inferFieldSelection sp resolution recordTy field
      (rest', resultTy, restCts) <- selectFields fieldTy rest
      pure (field' : rest', resultTy, fieldCts <> restCts)

-- | Apply getField at the label of one field to a value of the record type.
--
-- The first specified type argument of getField is the label, as in
-- @getField \@"x"@. The label is a type-level symbol. The field name gets
-- the occurrence annotation of getField, with its type arguments and its
-- evidence. The result is the type of the field.
inferFieldSelection :: Maybe SourceSpan -> Resolved -> TcType -> Name -> TcM (Name, TcType, [Ct])
inferFieldSelection ambient resolution recordTy field = do
  let sp = sourceSpanFromAnns (nameAnns field) <|> ambient
      label = TcTyLit (TyLitSymbol (nameText field))
  mBinder <- lookupResolvedTerm "getField" (resolutionTarget resolution)
  (methodTy, typeArgs, specifiedArgs, predicates) <-
    case mBinder of
      Just (TcIdBinder scheme _) -> do
        inst <- instantiateWithArgs scheme
        pure (instType inst, instTypeArgs inst, instSpecifiedArgs inst, instPreds inst)
      Just (TcMonoIdBinder ty) -> pure (ty, [], [], [])
      Nothing -> abortTc ("resolved getField is missing from the type environment: " <> show (resolutionTarget resolution))
  methodCts <- mapM (predToCt sp "getField") predicates
  labelCts <-
    case specifiedArgs of
      labelArg : _ -> do
        labelKind <- tcTypeKind labelArg
        symbolKind <- tcTypeKind label
        kindDeferred <- unifyDeferring sp (OccurrenceOf "getField") labelKind symbolKind
        labelDeferred <- unifyDeferring sp (OccurrenceOf "getField") labelArg label
        mapM (deferredEquality sp) (kindDeferred <> labelDeferred)
      [] -> do
        emitError sp (OtherError "getField has no type argument for the label of a record field")
        pure []
  resultTy <- freshMetaTv
  evidence <- freshEvVar
  let methodEquality = mkWantedCt (EqPred methodTy (TcFunTy recordTy resultTy)) evidence (OccurrenceOf "getField") sp
      pending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
  pure (annotatePendingName pending field, resultTy, methodCts <> labelCts <> [methodEquality])
  where
    deferredEquality sp (left, right) = do
      evidence <- freshEvVar
      pure (mkWantedCt (EqPred left right) evidence (OccurrenceOf "getField") sp)

inferTuple :: Maybe SourceSpan -> Name -> TupleFlavor -> [Maybe Expr] -> TcM (Expr, TcType, [Ct])
inferTuple sp syntax flavor elems = do
  results <- mapM inferElem elems
  let elems' = map (\(expr, _, _) -> expr) results
      tys = map (\(_, ty, _) -> ty) results
      cts = concatMap (\(_, _, elemCts) -> elemCts) results
      n = length tys
  wired <- wiredTupleTyCon flavor n
  -- The wiring gives the full identity of the tuple type constructor.
  -- A bare name lookup can find a different constructor with the same name.
  tc <- registeredWiredTyCon wired
  -- A tuple section such as @(0,)@ is a function of its missing fields.
  let tupleTy = TcTyCon tc tys
      missingTys = [ty | (Nothing, ty, _) <- results]
      sectionTy = foldr TcFunTy tupleTy missingTys
      pending = pendingAnnotation sectionTy tys [] []
  pure (annotatePendingExprAt sp pending (ETuple syntax flavor elems'), sectionTy, cts)
  where
    inferElem Nothing = do
      ty <- freshMetaTv
      pure (Nothing, ty, [])
    inferElem (Just e) = do
      (e', ty, cts) <- inferExpr e
      pure (Just e', ty, cts)

-- | An overloaded list applies fromListN to its length and an ordinary list.
--
-- The list syntax name gets the annotation of the ordinary list. The
-- method name gets the method annotation, and the expression gets the
-- result annotation.
inferOverloadedList :: Maybe SourceSpan -> Name -> Name -> Resolved -> [Expr] -> TcM (Expr, TcType, [Ct])
inferOverloadedList sp syntax method resolution items = do
  (items', listPending, listTy, listCts) <- inferListParts Nothing sp items
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "fromListN" resolution
  kinds <- getKinds
  intTyCon <- wiredTyCon tcWiringIntTyCon (typeKind kinds)
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy (TcTyCon intTyCon []) (TcFunTy listTy resultTy)
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "fromListN") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      resultPending = pendingAnnotation resultTy [] [] []
      list = EList (annotatePendingName listPending syntax) (Just (annotatePendingName methodPending method)) items'
  pure (annotatePendingExprAt sp resultPending list, resultTy, listCts <> methodCts <> [methodEquality])

-- | An arithmetic sequence under OverloadedLists applies fromList to an
-- ordinary list.
--
-- The method name gets the method annotation, and the expression gets the
-- result annotation.
inferOverloadedSequence :: Maybe SourceSpan -> Name -> Resolved -> ArithSeq -> TcM (Expr, TcType, [Ct])
inferOverloadedSequence sp method resolution arithSeq = do
  (arithSeq', listTy, listCts) <- inferArithSeqNode sp arithSeq
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp "fromList" resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = TcFunTy listTy resultTy
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf "fromList") sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      resultPending = pendingAnnotation resultTy [] [] []
      sequence' = EArithSeq (Just (annotatePendingName methodPending method)) arithSeq'
  pure (annotatePendingExprAt sp resultPending sequence', resultTy, listCts <> methodCts <> [methodEquality])

inferList :: Maybe SourceSpan -> Name -> [Expr] -> TcM (Expr, TcType, [Ct])
inferList = inferListWithExpected Nothing

inferListWithExpected :: Maybe TcType -> Maybe SourceSpan -> Name -> [Expr] -> TcM (Expr, TcType, [Ct])
inferListWithExpected expected sp syntax elems = do
  (elems', pending, listTy, cts) <- inferListParts expected sp elems
  pure (annotatePendingExprAt sp pending (EList syntax Nothing elems'), listTy, cts)

-- | Infer an ordinary list. The result has the checked elements, the
-- annotation of the list, the list type, and the constraints.
inferListParts :: Maybe TcType -> Maybe SourceSpan -> [Expr] -> TcM ([Expr], TcAnnotation, TcType, [Ct])
inferListParts expected sp elems = do
  (nilCon, nilInstantiation) <- instantiateListConstructor sp tcWiringNilDataCon
  nilCts <- mapM (predToCt sp (tyConName nilCon)) (instPreds nilInstantiation)
  case elems of
    [] -> do
      let listTy = instType nilInstantiation
          pending = pendingAnnotation listTy (instTypeArgs nilInstantiation) (map ctEvVar nilCts) []
      pure ([], pending, listTy, nilCts)
    _ -> do
      (consCon, consInstantiation) <- instantiateListConstructor sp tcWiringConsDataCon
      consPredicateCts <- mapM (predToCt sp (tyConName consCon)) (instPreds consInstantiation)
      case instType consInstantiation of
        TcFunTy sourceElemTy (TcFunTy sourceTailTy sourceResultTy) -> do
          case expected of
            Just expectedTy -> do
              let variables = IntSet.fromList (map uniqueKey (concatMap typeMetaVariables (instTypeArgs consInstantiation)))
              reducedExpected <- zonkType expectedTy >>= reduceTypeFamilies
              quickLookUnify variables reducedExpected sourceResultTy
            Nothing -> pure ()
          results <- mapM (inferElem sourceElemTy) elems
          let elems' = map (\(element, _, _, _) -> element) results
              elemCts = concatMap (\(_, _, cts, _) -> cts) results
              (firstElemTy, firstElemSp) = case results of
                (_, ty, _, elemSp) : _ -> (ty, elemSp)
                [] -> (sourceElemTy, sp)
              pending = pendingAnnotation sourceResultTy [sourceElemTy] [] []
          firstConstructorCt <- constructorEqualityCt firstElemSp firstElemTy sourceElemTy
          nilConstructorCt <- constructorEqualityCt sp (instType nilInstantiation) sourceTailTy
          resultConstructorCt <- constructorEqualityCt sp sourceResultTy sourceTailTy
          elementEqualityCts <- mapM (elementEqualityCt firstElemSp firstElemTy) (drop 1 results)
          let constructorCts = nilCts <> consPredicateCts <> [firstConstructorCt, nilConstructorCt, resultConstructorCt]
          pure (elems', pending, sourceResultTy, elemCts <> elementEqualityCts <> constructorCts)
        _ -> abortTc "the wired list cons constructor has an invalid type"
  where
    inferElem expectedElemTy elemExpr = case expected of
      Just _ -> do
        (elemExpr', elemTy, elemCts) <- checkExpr expectedElemTy elemExpr
        -- A given equality can be the only proof that the element has the
        -- expected element type, so the element takes the cast of that proof.
        equality <- wantedDoEq elemSp expectedElemTy elemTy
        pure (annotateExprCast expectedElemTy (ctEvVar equality) elemExpr', expectedElemTy, elemCts <> [equality], elemSp)
      Nothing -> do
        (elemExpr', elemTy, elemCts) <- inferExpr elemExpr
        pure (elemExpr', elemTy, elemCts, elemSp)
      where
        elemSp = exprSpan elemExpr <|> sp
    constructorEqualityCt loc left right = do
      ev <- freshEvVar
      pure (mkWantedCt (EqPred left right) ev (AppOrigin loc) loc)
    elementEqualityCt firstElemSp firstElemTy (_, elemTy, _, elemSp) = do
      ev <- freshEvVar
      pure $
        mkWantedEqCt
          TypeTrace
            { typeTraceType = elemTy,
              typeTraceRole = ActualType,
              typeTraceOrigin = ExpressionTypeOrigin elemSp
            }
          TypeTrace
            { typeTraceType = firstElemTy,
              typeTraceRole = ExpectedType,
              typeTraceOrigin = ListElementTypeOrigin firstElemSp
            }
          ev
          (AppOrigin elemSp)
          elemSp

inferArithSeq :: Maybe SourceSpan -> ArithSeq -> TcM (Expr, TcType, [Ct])
inferArithSeq sp arithSeq = do
  (arithSeq', resultTy, cts) <- inferArithSeqNode sp arithSeq
  let pending = pendingAnnotation resultTy [] [] []
  pure (annotatePendingExprAt sp pending (EArithSeq Nothing arithSeq'), resultTy, cts)

inferArithSeqNode :: Maybe SourceSpan -> ArithSeq -> TcM (ArithSeq, TcType, [Ct])
inferArithSeqNode sp arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> do
      (inner', resultTy, cts) <- inferArithSeqNode sp inner
      pure (ArithSeqAnn ann inner', resultTy, cts)
    _
      | Just resolution <- syntaxResolution =<< arithSeqMethod arithSeq ->
          inferResolvedArithSeq sp resolution arithSeq
      | otherwise -> abortTc "arithmetic sequence is missing its resolved method"

-- | Check an arithmetic sequence with its enumeration method. The method
-- name gets the checked type of the method.
inferResolvedArithSeq :: Maybe SourceSpan -> Resolved -> ArithSeq -> TcM (ArithSeq, TcType, [Ct])
inferResolvedArithSeq sp resolution arithSeq = do
  (arithSeq', argumentTypes, argumentCts) <- inferArithSeqForm arithSeq
  let methodName = displayIdentifier (resolutionIdentifier resolution)
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  resultTy <- freshMetaTv
  equalityEvidence <- freshEvVar
  let expectedMethodTy = foldr TcFunTy resultTy argumentTypes
      methodEquality = mkWantedCt (EqPred methodTy expectedMethodTy) equalityEvidence (OccurrenceOf methodName) sp
      methodPending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
      annotated = mapArithSeqMethod (annotatePendingName methodPending) arithSeq'
  pure (annotated, resultTy, argumentCts <> methodCts <> [methodEquality])

-- | Instantiate a resolved syntax method such as enumFrom or ifThenElse.
inferResolvedSyntaxMethod :: Maybe SourceSpan -> Text -> Resolved -> TcM (TcType, [TcType], [Ct])
inferResolvedSyntaxMethod sp methodName resolution = do
  mBinder <- lookupResolvedTerm methodName (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp methodName) (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) -> pure (ty, [], [])
    Nothing ->
      abortTc ("resolved " <> T.unpack methodName <> " is missing from the type environment: " <> show (resolutionTarget resolution))

inferArithSeqForm :: ArithSeq -> TcM (ArithSeq, [TcType], [Ct])
inferArithSeqForm arithSeq =
  case arithSeq of
    ArithSeqAnn ann inner -> do
      (inner', types, cts) <- inferArithSeqForm inner
      pure (ArithSeqAnn ann inner', types, cts)
    ArithSeqFrom method from -> inferForm (ArithSeqFrom method) [from]
    ArithSeqFromThen method from thenExpr -> inferForm2 (ArithSeqFromThen method) from thenExpr
    ArithSeqFromTo method from to -> inferForm2 (ArithSeqFromTo method) from to
    ArithSeqFromThenTo method from thenExpr to -> do
      results <- mapM inferExpr [from, thenExpr, to]
      case results of
        [(from', fromTy, fromCts), (thenExpr', thenTy, thenCts), (to', toTy, toCts)] ->
          pure (ArithSeqFromThenTo method from' thenExpr' to', [fromTy, thenTy, toTy], fromCts <> thenCts <> toCts)
        _ -> abortTc "arithmetic sequence has an invalid argument count"
  where
    inferForm constructor expressions = do
      results <- mapM inferExpr expressions
      case results of
        [(expression', ty, cts)] -> pure (constructor expression', [ty], cts)
        _ -> abortTc "arithmetic sequence has an invalid argument count"
    inferForm2 constructor first second = do
      (first', firstTy, firstCts) <- inferExpr first
      (second', secondTy, secondCts) <- inferExpr second
      pure (constructor first' second', [firstTy, secondTy], firstCts <> secondCts)

-- | The identity and an instantiation of one wired list constructor.
instantiateListConstructor :: Maybe SourceSpan -> (TcWiring -> TyCon) -> TcM (TyCon, Instantiation)
instantiateListConstructor sp select = do
  wired <- wiredTyConIdentity select
  maybeBinder <- lookupWiredTerm wired
  let name = tyConName wired
  case maybeBinder of
    Just (TcIdBinder scheme _) -> (,) wired <$> instantiateWithArgs scheme
    Just TcMonoIdBinder {} ->
      abortTc ("the wired list constructor is monomorphic at " <> show sp <> ": " <> show name)
    Nothing ->
      abortTc ("the wired list constructor is missing at " <> show sp <> ": " <> show name)

inferListComp :: Maybe SourceSpan -> Expr -> [CompStmt] -> TcM (Expr, TcType, [Ct])
inferListComp sp body quals = do
  listTyCon' <- resolvedListTyCon
  (quals', body', bodyTy, cts) <- inferCompQuals listTyCon' sp quals (inferExpr body)
  let resultTy = TcTyCon listTyCon' [bodyTy]
      pending = pendingAnnotation resultTy [bodyTy] [] []
  pure (annotatePendingExprAt sp pending (EListComp body' quals'), resultTy, cts)

-- | Check a parallel list comprehension. Its desugaring is
--
-- > [e | (vs1, vs2) <- zip [vs1 | quals1] [vs2 | quals2]]
--
-- where @vsi@ is the tuple of the variables of branch @i@ in the order of
-- 'compStmtBinderNames'. A branch with one variable uses the variable and
-- not a tuple. Each branch sees only the outer scope. The variables have
-- monomorphic types in the body.
--
-- The first statement of each branch gets a tuple annotation. The tuple
-- annotation has the tuple type and the types of the variables as type
-- arguments.
inferListCompParallel :: Maybe SourceSpan -> Expr -> [[CompStmt]] -> TcM (Expr, TcType, [Ct])
inferListCompParallel sp body branches = do
  listTyCon' <- resolvedListTyCon
  checked <- mapM (inferBranch listTyCon') branches
  let binders = concat [branchBinders | (_, branchBinders, _) <- checked]
  (body', bodyTy, bodyCts) <-
    foldr (\(key, ty) -> extendTermEnv key (TcMonoIdBinder ty)) (inferExpr body) binders
  let resultTy = TcTyCon listTyCon' [bodyTy]
      pending = pendingAnnotation resultTy [bodyTy] [] []
      branches' = [quals' | (quals', _, _) <- checked]
      branchCts = concat [cts | (_, _, cts) <- checked]
  pure (annotatePendingExprAt sp pending (EListCompParallel body' branches'), resultTy, branchCts <> bodyCts)
  where
    inferBranch listTyCon' quals = do
      keys <- mapM resolvedUnqualifiedTermKey (compStmtBinderNames quals)
      binderTys <- mapM (const freshMetaTv) keys
      tupleTy <- recTupleType binderTys
      let branchBinders = zip keys binderTys
      (quals', _, _, cts) <- inferCompQuals listTyCon' sp quals (finishBranch tupleTy branchBinders)
      let tupleAnn = pendingAnnotation tupleTy binderTys [] []
          annotated =
            case quals' of
              first : rest -> setCompStmtTc tupleAnn first : rest
              [] -> []
      pure (annotated, branchBinders, cts)
    -- After the last statement of a branch, equate the type of each
    -- variable of the branch with its tuple field type.
    finishBranch tupleTy branchBinders = do
      cts <- mapM equate branchBinders
      pure (body, tupleTy, concat cts)
    equate (key, fieldTy) = do
      binder <- lookupTermKey key
      case binder of
        Just (TcMonoIdBinder boundTy) -> pure <$> wantedDoEq sp boundTy fieldTy
        Just (TcIdBinder (ForAll [] [] boundTy) _) -> pure <$> wantedDoEq sp boundTy fieldTy
        _ -> do
          emitError sp (OtherError "a variable of a parallel list comprehension branch must have a monomorphic type")
          pure []

-- | Check the statements of a list comprehension, and then run the action
-- in the scope of their variables.
inferCompQuals :: TyCon -> Maybe SourceSpan -> [CompStmt] -> TcM (Expr, TcType, [Ct]) -> TcM ([CompStmt], Expr, TcType, [Ct])
inferCompQuals listTyCon' ambient0 quals0 action = go [] ambient0 quals0
  where
    -- The first argument has the statements before the current statement,
    -- in reverse order. A transform statement uses their variables.
    go _ _ [] = do
      (body', bodyTy, bodyCts) <- action
      pure ([], body', bodyTy, bodyCts)
    go seen ambient (qual : rest) =
      case qual of
        CompAnn ann inner -> do
          (stmts', body', bodyTy, cts) <- go seen (compStmtSpan qual <|> ambient) (inner : rest)
          case stmts' of
            inner' : rest' -> pure (CompAnn ann inner' : rest', body', bodyTy, cts)
            [] -> pure ([], body', bodyTy, cts)
        CompGen tc pat src -> do
          elemTy <- freshMetaTv
          (src', srcTy, srcCts) <- inferExpr src
          patCheck <- checkPattern ambient pat elemTy
          ev <- freshEvVar
          let srcSp = exprSpan src <|> ambient
              srcListCt = mkWantedCt (EqPred srcTy (TcTyCon listTyCon' [elemTy])) ev (AppOrigin srcSp) srcSp
          (rest', body', bodyTy, bodyCts) <- withPatternScope patCheck (go (qual : seen) ambient rest)
          remainingCts <- solvePatternBranch ambient patCheck bodyTy bodyCts
          pure (CompGen tc (annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)) src' : rest', body', bodyTy, srcCts ++ [srcListCt] ++ remainingCts)
        CompGuard tc guard -> do
          (guard', guardTy, guardCts) <- inferExpr guard
          ev <- freshEvVar
          expectedBoolTy <- boolType
          let guardSp = exprSpan guard <|> ambient
              guardCt = mkWantedCt (EqPred guardTy expectedBoolTy) ev (AppOrigin guardSp) guardSp
          (rest', body', bodyTy, bodyCts) <- go (qual : seen) ambient rest
          pure (CompGuard tc guard' : rest', body', bodyTy, guardCts ++ [guardCt] ++ bodyCts)
        CompLetDecls tc decls -> do
          (decls', (rest', body'), bodyTy, bodyCts) <-
            inferLocalDecls inferExpr decls $ do
              (rest', body', bodyTy, bodyCts) <- go (qual : seen) ambient rest
              pure ((rest', body'), bodyTy, bodyCts)
          pure (CompLetDecls tc decls' : rest', body', bodyTy, bodyCts)
        CompThen tc function -> transform seen ambient qual rest False Nothing function (\_ function' -> CompThen tc function')
        CompThenBy tc function key -> transform seen ambient qual rest False (Just key) function (\key' function' -> CompThenBy tc function' (fromMaybe key key'))
        CompGroupUsing tc function -> transform seen ambient qual rest True Nothing function (\_ function' -> CompGroupUsing tc function')
        CompGroupByUsing tc key function -> transform seen ambient qual rest True (Just key) function (CompGroupByUsing tc . fromMaybe key)

    -- Check a transform statement of TransformListComp. Let @t@ be the
    -- tuple type of the variables of the statements before it. The
    -- function has the type @[t] -> [t]@, or @[t] -> [[t]]@ for a group
    -- statement. A key expression @e@ adds the argument @t -> te@ in
    -- front. After a group statement, each variable has a list type.
    --
    -- The statement gets a tuple annotation. The annotation has the tuple
    -- type, the variable types as type arguments, and the key type as
    -- term argument type.
    transform seen ambient qual rest isGroup maybeKey function rebuild = do
      let qualSp = compStmtSpan qual <|> ambient
          listOf ty = TcTyCon listTyCon' [ty]
      keys <- mapM resolvedUnqualifiedTermKey (compStmtBinderNames (reverse seen))
      binderTys <- mapM (transformBinderType qualSp) keys
      tupleTy <- recTupleType binderTys
      keyPart <- traverse inferExpr maybeKey
      (function', functionTy, functionCts) <- inferExpr function
      let resultTy
            | isGroup = listOf (listOf tupleTy)
            | otherwise = listOf tupleTy
          keyArgTys = [TcFunTy tupleTy keyTy | (_, keyTy, _) <- maybe [] pure keyPart]
          expectedTy = foldr TcFunTy (TcFunTy (listOf tupleTy) resultTy) keyArgTys
          functionSp = exprSpan function <|> qualSp
      functionCt <- wantedDoEq functionSp functionTy expectedTy
      let rebind action'
            | isGroup = foldr (\(key, ty) -> rebindTermEnv key (TcMonoIdBinder (listOf ty))) action' (zip keys binderTys)
            | otherwise = action'
      (rest', body', bodyTy, bodyCts) <- rebind (go (qual : seen) ambient rest)
      let keyTys = [keyTy | (_, keyTy, _) <- maybe [] pure keyPart]
          keyCts = maybe [] (\(_, _, cts) -> cts) keyPart
          tupleAnn = pendingAnnotation tupleTy binderTys [] keyTys
          qual' = setCompStmtTc tupleAnn (rebuild (fmap (\(key', _, _) -> key') keyPart) function')
      pure (qual' : rest', body', bodyTy, keyCts ++ functionCts ++ [functionCt] ++ bodyCts)

    transformBinderType sp key = do
      binder <- lookupTermKey key
      case binder of
        Just (TcMonoIdBinder ty) -> pure ty
        Just (TcIdBinder (ForAll [] [] ty) _) -> pure ty
        _ -> do
          emitError sp (OtherError "a variable before a TransformListComp statement must have a monomorphic type")
          freshMetaTv

resolvedListTyCon :: TcM TyCon
resolvedListTyCon = listTyConOfWiring

inferDo :: Maybe SourceSpan -> DoFlavor -> [DoStmt Expr] -> TcM (Expr, TcType, [Ct])
inferDo sp flavor stmts
  | isMonadicDoFlavor flavor = do
      (stmts', resultTy, cts) <- inferDoStmts sp stmts
      pure (atSpan sp (EDo (Just resultTy) stmts' flavor), resultTy, cts)
  | otherwise = do
      emitError sp (OtherError ("unsupported do flavor in TC MVP: " ++ show flavor))
      resultTy <- freshMetaTv
      pure (EDo Nothing stmts flavor, resultTy, [])

-- | Whether a do block uses the built-in monad methods. The resolver puts
-- each recursive segment of an @mdo@ block into a @rec@ statement, so an
-- @mdo@ block is the same as a plain do block here.
isMonadicDoFlavor :: DoFlavor -> Bool
isMonadicDoFlavor flavor =
  case flavor of
    DoPlain -> True
    DoMdo -> True
    _ -> False

inferDoStmts :: Maybe SourceSpan -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmts = inferDoStmtsWith Nothing

-- | What comes after the statements of a do block.
data DoEnd
  = -- | The last statement is the result of the block.
    DoEndLast
  | -- | The statements are the body of a recursive group. After the last
    -- statement, this action gives the type of the block and the
    -- constraints that connect the group variables with the knot.
    DoEndRec (TcM (TcType, [Ct]))

inferDoStmtsWith :: Maybe TcType -> Maybe SourceSpan -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmtsWith = inferDoStmtsEnd DoEndLast

inferDoStmtsEnd :: DoEnd -> Maybe TcType -> Maybe SourceSpan -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmtsEnd end expected sp stmts =
  case (stmts, end) of
    ([], DoEndRec finish) -> do
      (resultTy, cts) <- finish
      pure ([], resultTy, cts)
    ([], DoEndLast) -> do
      emitError sp (OtherError "empty do block in TC MVP")
      resultTy <- freshMetaTv
      pure ([], resultTy, [])
    ([stmt], DoEndLast) -> inferLastDoStmt expected sp stmt
    (stmt : rest, _) -> inferDoStmt end expected sp stmt rest

inferLastDoStmt :: Maybe TcType -> Maybe SourceSpan -> DoStmt Expr -> TcM ([DoStmt Expr], TcType, [Ct])
inferLastDoStmt expected ambient stmt =
  case stmt of
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferLastDoStmt expected (doStmtSpan stmt <|> ambient) inner
      case stmts' of
        [inner'] -> pure ([DoAnn ann inner'], resultTy, cts)
        _ -> pure (stmts', resultTy, cts)
    DoRecKnot knot inner -> inferLastWrapped (DoRecKnot knot) inner
    DoExpr method body -> do
      (body', bodyTy, cts) <- maybe (inferExprAt ambient body) (`checkExpr` body) expected
      case expected of
        Nothing -> pure ([DoExpr method body'], bodyTy, cts)
        Just expectedTy -> do
          -- The continuation must have the type that the do method requires.
          equality <- wantedDoEq ambient expectedTy bodyTy
          pure ([DoExpr method (annotateExprCast expectedTy (ctEvVar equality) body')], expectedTy, cts <> [equality])
    _ -> do
      emitError ambient (OtherError "the last statement in a do block must be an expression")
      resultTy <- freshMetaTv
      pure ([stmt], resultTy, [])
  where
    inferLastWrapped wrap inner = do
      (stmts', resultTy, cts) <- inferLastDoStmt expected ambient inner
      case stmts' of
        [inner'] -> pure ([wrap inner'], resultTy, cts)
        _ -> pure (stmts', resultTy, cts)

inferDoStmt :: DoEnd -> Maybe TcType -> Maybe SourceSpan -> DoStmt Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferDoStmt end expected ambient stmt rest =
  case stmt of
    _
      | Just parts <- recStmtParts stmt ->
          inferRecStmt end expected ambient parts rest
    DoBind (Just (DoBindMethods method failure)) pat action
      | Just resolution <- syntaxResolution method ->
          inferResolvedDoBind end expected ambient method resolution (resolvedFailure =<< failure) pat action rest
    DoExpr (Just method) action
      | Just resolution <- syntaxResolution method ->
          inferResolvedDoExpr end expected ambient method resolution action rest
    DoAnn ann inner -> do
      (stmts', resultTy, cts) <- inferDoStmt end expected (doStmtSpan stmt <|> ambient) inner rest
      case stmts' of
        inner' : rest' -> pure (DoAnn ann inner' : rest', resultTy, cts)
        [] -> pure ([], resultTy, cts)
    DoRecKnot knot inner -> inferWrapped (DoRecKnot knot) inner
    DoBind methods pat action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      patCheck <- checkPattern ambient pat itemTy
      (rest', resultTy, restCts) <-
        withPatternScope patCheck (inferDoStmtsEnd end expected ambient rest)
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      remainingCts <- solvePatternBranch ambient patCheck resultTy restCts
      pure
        ( DoBind methods pat' action' : rest',
          resultTy,
          actionCts <> remainingCts <> [actionEq, resultEq, monadCt]
        )
    DoExpr method action -> do
      monadTy <- freshMetaTv
      itemTy <- freshMetaTv
      resultItemTy <- freshMetaTv
      (action', actionTy, actionCts) <- inferExprAt ambient action
      (rest', resultTy, restCts) <- inferDoStmtsEnd end expected ambient rest
      actionEq <- wantedDoEq ambient actionTy (TcAppTy monadTy itemTy)
      resultEq <- wantedDoEq ambient resultTy (TcAppTy monadTy resultItemTy)
      monadCt <- wantedMonad ambient monadTy
      pure
        ( DoExpr method action' : rest',
          resultTy,
          actionCts <> restCts <> [actionEq, resultEq, monadCt]
        )
    DoLetDecls decls -> do
      (decls', rest', resultTy, cts) <-
        inferLocalDecls inferExpr decls $ do
          (rest', resultTy, restCts) <- inferDoStmtsEnd end expected ambient rest
          pure (rest', resultTy, restCts)
      pure (DoLetDecls decls' : rest', resultTy, cts)
    DoRecStmt _ _ _ -> do
      emitError ambient (OtherError "a rec statement without its resolved methods")
      (rest', resultTy, cts) <- inferDoStmtsEnd end expected ambient rest
      pure (stmt : rest', resultTy, cts)
  where
    resolvedFailure failure = (,) failure <$> syntaxResolution failure
    inferWrapped wrap inner = do
      (stmts', resultTy, cts) <- inferDoStmt end expected ambient inner rest
      case stmts' of
        inner' : rest' -> pure (wrap inner' : rest', resultTy, cts)
        [] -> pure ([], resultTy, cts)

-- | The parts of a @rec@ statement that the resolver gives: the @mfix@,
-- @return@, and @>>=@ methods of its desugaring, the knot variables of a
-- segment of an @mdo@ block, and its statements.
data RecStmtParts = RecStmtParts
  { recMfix :: !(Name, Resolved),
    recReturn :: !(Name, Resolved),
    recBind :: !(Name, Resolved),
    recKnot :: !(Maybe (RecKnotVariables, Set.Set Entity)),
    recStmts :: ![DoStmt Expr]
  }

recStmtParts :: DoStmt Expr -> Maybe RecStmtParts
recStmtParts stmt =
  case stmt of
    DoRecKnot variablesKnot@(RecKnotVariables variables) (DoRecStmt _ (Just methods) stmts) ->
      parts (Just (variablesKnot, variables)) methods stmts
    DoRecStmt _ (Just methods) stmts -> parts Nothing methods stmts
    _ -> Nothing
  where
    parts knot (DoRecMethods mfix return' bind) stmts =
      RecStmtParts
        <$> method mfix
        <*> method return'
        <*> method bind
        <*> pure knot
        <*> pure stmts
    method name = (,) name <$> syntaxResolution name

-- | Check a @rec@ statement. Its desugaring is
--
-- > vs <- mfix (\ ~vs -> do { stmts; return vs })
--
-- where @vs@ is the tuple of the knot variables in the order of
-- 'recStmtBinderNames'. A group with one variable uses the variable and not
-- a tuple. The knot variables are all the group variables, but a segment
-- of an @mdo@ block has a 'RecKnotVariables' annotation that can give
-- fewer. The knot variables have monomorphic types in the group and after
-- it.
-- A let statement of the group can generalize a variable. Then the
-- statements after the let statement can use it at more than one type,
-- and the knot takes one instance of it, as in GHC.
--
-- The method names of the statement get the checked types of the @mfix@,
-- @return@, and @>>=@ methods. The statement gets a tuple annotation. The
-- tuple annotation has the tuple type, the types of
-- the variables as type arguments, and the type of the statement block as
-- its only term argument type. Last, each generalized variable has the
-- occurrence annotation of the variable at the knot. The 'RecKnotVariables' annotation stays next
-- to the statements.
inferRecStmt :: DoEnd -> Maybe TcType -> Maybe SourceSpan -> RecStmtParts -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferRecStmt end expected ambient parts rest = do
  let groupNames = recStmtBinderNames (recStmts parts)
  groupKeys <- mapM resolvedUnqualifiedTermKey groupNames
  let inKnot key = maybe True (Set.member key . snd) (recKnot parts)
      (names, keys) = unzip (filter (inKnot . snd) (zip groupNames groupKeys))
  binderTys <- mapM (const freshMetaTv) keys
  monadTy <- freshMetaTv
  tupleTy <- recTupleType binderTys
  let (mfixName, mfixResolution) = recMfix parts
      (returnName, returnResolution) = recReturn parts
      (bindName, bindResolution) = recBind parts
      innerTy = TcAppTy monadTy tupleTy
      knotTy = TcFunTy tupleTy innerTy
      mfixTy = TcFunTy knotTy innerTy
  (mfixPending, mfixEv, mfixCts) <- inferDoMethod ambient "mfix" mfixResolution mfixTy
  (returnPending, returnEv, returnCts) <- inferDoMethod ambient "return" returnResolution knotTy
  prepareScrutinee (mfixCts <> returnCts)
  (stmts', _, innerCts) <-
    withRecBinders (zip keys binderTys) $
      inferDoStmtsEnd (DoEndRec (finishRecGroup ambient innerTy (zip3 names keys binderTys))) (Just innerTy) ambient (recStmts parts)
  knotOccurrences <- takeRecKnotOccurrences keys
  restExpected <- freshMetaTv
  blockTy <- maybe freshMetaTv pure expected
  let bindTy = TcFunTy innerTy (TcFunTy (TcFunTy tupleTy restExpected) blockTy)
  (bindPending, bindEv, bindCts) <- inferDoMethod ambient ">>=" bindResolution bindTy
  prepareScrutinee bindCts
  (rest', restTy, restCts) <-
    withLocalKeys [(key, TcMonoIdBinder ty) | (key, ty) <- zip keys binderTys] $
      inferDoStmtsEnd end (Just restExpected) ambient rest
  resultEquality <- wantedDoEq ambient restTy restExpected
  let tuplePending = pendingAnnotation tupleTy binderTys [] [innerTy]
      methods' =
        DoRecMethods
          (annotateDoMethodName mfixTy mfixEv mfixPending mfixName)
          (annotateDoMethodName knotTy returnEv returnPending returnName)
          (annotateDoMethodName bindTy bindEv bindPending bindName)
      recTc = TcRecStmt tuplePending knotOccurrences
      stmt' = maybe id (DoRecKnot . fst) (recKnot parts) (DoRecStmt (Just recTc) (Just methods') stmts')
  pure
    ( stmt' : rest',
      blockTy,
      mfixCts <> returnCts <> innerCts <> bindCts <> restCts <> [resultEquality]
    )
  where
    withLocalKeys binders action = foldr (uncurry extendTermEnv) action binders

-- | The type of the knot of a recursive group: the unit type for no
-- variable, the type of the variable for one variable, and a tuple type for
-- more variables.
recTupleType :: [TcType] -> TcM TcType
recTupleType tys =
  case tys of
    [ty] -> pure ty
    _ -> do
      kinds <- getKinds
      wired <- wiredTupleTyCon Boxed (length tys)
      tc <- mkWiredTyCon wired (foldr (KFun . const (typeKind kinds)) (typeKind kinds) tys)
      pure (TcTyCon tc tys)

-- | After the last statement of a recursive group, equate the type of each
-- group variable that the statements bound with its knot type. A let
-- statement can generalize a variable. Then the knot takes one instance of
-- the variable, and 'inferRecStmt' keeps the occurrence annotation of that
-- instance.
finishRecGroup :: Maybe SourceSpan -> TcType -> [(UnqualifiedName, Entity, TcType)] -> TcM (TcType, [Ct])
finishRecGroup sp innerTy binders = do
  cts <- mapM equate binders
  pure (innerTy, concat cts)
  where
    equate (name, key, knotTy) = do
      binder <- lookupTermKey key
      case binder of
        Just (TcMonoIdBinder boundTy) -> pure <$> wantedDoEq sp boundTy knotTy
        Just (TcIdBinder (ForAll [] [] boundTy) _) -> pure <$> wantedDoEq sp boundTy knotTy
        Just TcIdBinder {} -> do
          (pending, boundTy, cts) <- inferNameOccurrence sp (qualifyName Nothing name)
          mapM_ (recordRecKnotOccurrence key) pending
          equality <- wantedDoEq sp boundTy knotTy
          pure (cts <> [equality])
        Nothing -> abortTc ("rec group variable missing from type environment: " <> show key)

-- | Check an expression statement with its @>>@ method.
inferResolvedDoExpr :: DoEnd -> Maybe TcType -> Maybe SourceSpan -> Name -> Resolved -> Expr -> [DoStmt Expr] -> TcM ([DoStmt Expr], TcType, [Ct])
inferResolvedDoExpr end expected ambient method resolution action rest = do
  restExpected <- freshMetaTv
  blockTy <- maybe freshMetaTv pure expected
  (action', actionTy, actionCts) <- inferExprAt ambient action
  let thenTy = TcFunTy actionTy (TcFunTy restExpected blockTy)
  (pending, methodEv, methodCts) <- inferDoMethod ambient ">>" resolution thenTy
  prepareScrutinee (actionCts <> methodCts)
  (rest', restTy, restCts) <- inferDoStmtsEnd end (Just restExpected) ambient rest
  resultEquality <- wantedDoEq ambient restTy restExpected
  let stmt' = DoExpr (Just (annotateDoMethodName thenTy methodEv pending method)) action'
  pure (stmt' : rest', blockTy, actionCts <> restCts <> methodCts <> [resultEquality])

-- | Check a bind statement. The resolver gives the @fail@ method when the
-- pattern can fail as far as its syntax tells. When the constructors of
-- the pattern make it irrefutable, the method is dropped. Otherwise the
-- desugaring calls it in the default alternative, so its result type is
-- the type of the rest of the block.
inferResolvedDoBind ::
  DoEnd ->
  Maybe TcType ->
  Maybe SourceSpan ->
  Name ->
  Resolved ->
  Maybe (Name, Resolved) ->
  Pattern ->
  Expr ->
  [DoStmt Expr] ->
  TcM ([DoStmt Expr], TcType, [Ct])
inferResolvedDoBind end expected ambient method resolution failMethod pat action rest = do
  itemTy <- freshMetaTv
  restExpected <- freshMetaTv
  blockTy <- maybe freshMetaTv pure expected
  (action', actionTy, actionCts) <- inferExprAt ambient action
  let bindTy = TcFunTy actionTy (TcFunTy (TcFunTy itemTy restExpected) blockTy)
  (pending, methodEv, methodCts) <- inferDoMethod ambient ">>=" resolution bindTy
  prepareScrutinee (actionCts <> methodCts)
  patCheck <- checkPattern ambient pat itemTy
  (rest', restTy, restCts) <-
    withPatternScope patCheck (inferDoStmtsEnd end (Just restExpected) ambient rest)
  resultEquality <- wantedDoEq ambient restTy restExpected
  failure <-
    case failMethod of
      Nothing -> pure Nothing
      Just (failName, failResolution) -> do
        canFail <- patternCanFail pat
        if canFail
          then do
            messageTy <- stringType
            let failTy = TcFunTy messageTy restExpected
            (failPending, failEv, failCts) <- inferDoMethod ambient "fail" failResolution failTy
            pure (Just (annotateDoMethodName failTy failEv failPending failName, failCts))
          else pure Nothing
  let pat' = annotatePatternBindings (pcBindings patCheck) (checkedPattern patCheck)
      methods' = DoBindMethods (annotateDoMethodName bindTy methodEv pending method) (fst <$> failure)
      failCts = maybe [] snd failure
      stmt' = DoBind (Just methods') pat' action'
  remainingCts <- solvePatternBranch ambient patCheck restExpected (restCts <> [resultEquality])
  pure (stmt' : rest', blockTy, actionCts <> remainingCts <> methodCts <> failCts)

-- | The constraint that equates the argument of a literal method with the
-- resolved type of the literal.
--
-- The list is empty when the built-in scope does not give that type.
resolvedLiteralCts :: Maybe SourceSpan -> Resolved -> TcType -> TcM [Ct]
resolvedLiteralCts sp literalResolution argumentTy = do
  maybeInfo <- lookupResolvedTypeSyntax literalResolution
  case maybeInfo of
    Nothing -> pure []
    Just info -> do
      ev <- freshEvVar
      pure [mkWantedCt (EqPred argumentTy (TcTyCon (tciTyCon info) [])) ev (LitOrigin sp) sp]

-- | Instantiate the method that sequences a do statement and equate it with
-- the expected type. The evidence of that equality is the cast the
-- desugarer puts on the method: a given equality can be what makes the
-- method fit the statement.
inferDoMethod :: Maybe SourceSpan -> Text -> Resolved -> TcType -> TcM (TcAnnotation, EvVar, [Ct])
inferDoMethod sp methodName resolution expectedTy = do
  (methodTy, typeArgs, methodCts) <- inferResolvedSyntaxMethod sp methodName resolution
  ev <- freshEvVar
  let methodEq = mkWantedCt (EqPred methodTy expectedTy) ev (OccurrenceOf methodName) sp
      pending = pendingAnnotation methodTy typeArgs (map ctEvVar methodCts) []
  pure (pending, ev, methodCts <> [methodEq])

wantedDoEq :: Maybe SourceSpan -> TcType -> TcType -> TcM Ct
wantedDoEq sp actual expected = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred actual expected) ev (AppOrigin sp) sp)

wantedMonad :: Maybe SourceSpan -> TcType -> TcM Ct
wantedMonad sp monadTy = do
  ev <- freshEvVar
  maybeMonad <- lookupTyCon "Monad"
  case maybeMonad of
    Just monadInfo -> pure (mkWantedCt (ClassPred (tciTyCon monadInfo) [monadTy]) ev (AppOrigin sp) sp)
    Nothing -> abortTc "missing checked type constructor for Monad"

compStmtSpan :: CompStmt -> Maybe SourceSpan
compStmtSpan compStmt =
  case compStmt of
    CompAnn ann _ -> annotationSpan ann
    _ -> Nothing

doStmtSpan :: DoStmt body -> Maybe SourceSpan
doStmtSpan stmt =
  case stmt of
    DoAnn ann _ -> annotationSpan ann
    _ -> Nothing

rhsExprSpan :: Rhs Expr -> Maybe SourceSpan
rhsExprSpan rhs =
  case rhs of
    UnguardedRhs anns _ expr _ -> exprSpan expr <|> sourceSpanFromAnns anns
    GuardedRhss anns _ _ _ -> sourceSpanFromAnns anns

exprSpan :: Expr -> Maybe SourceSpan
exprSpan expr =
  case expr of
    EAnn ann inner ->
      annotationSpan ann <|> exprSpan inner
    ETc _ inner -> exprSpan inner
    _ -> Nothing

nameToText :: Name -> Text
nameToText n = case nameQualifier n of
  Nothing -> nameText n
  Just q -> q <> "." <> nameText n
