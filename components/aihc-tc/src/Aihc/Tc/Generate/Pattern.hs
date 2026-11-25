{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared type-checking support for term patterns.
module Aihc.Tc.Generate.Pattern
  ( PatternCheck (..),
    PatternTyVar (..),
    bindPatternTyVar,
    withPatternTyVars,
    annotatePatternBindings,
    reannotatePatternBinders,
    checkPattern,
    checkPatterns,
    checkFunctionPatterns,
    splitMatchType,
    checkedPattern,
    patternBinderNames,
    patternCanFail,
    recStmtBinderNames,
    compStmtBinderNames,
    withPatternBindings,
    withPatternScope,
  )
where

import Aihc.Resolve (Resolved (..), resolvedOf)
import Aihc.Tc.Annotations (CastDirection (..), PendingTcAnnotation (..), PendingTcCastAnnotation (..), PendingTcMatchArityCast (..), pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (DataConInfo (..), DataFamilyInstanceInfo (..), DataTypeInfo (..), PatSynInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import {-# SOURCE #-} Aihc.Tc.Generate.Expr (inferExprAt)
import Aihc.Tc.Generate.Record (lookupRecordHead, orderRecordFields)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (checkSurfaceType, freeTypeVars, freshKindMeta, tcTypeKind, unboxedSumType)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Solve.Equality (EqResult (..), solveEquality)
import Aihc.Tc.Syntax.Pending
  ( Annotation,
    BuiltinCon (..),
    CompStmt (..),
    Decl (..),
    DoStmt (..),
    Expr (..),
    FloatType (..),
    ListMatch (..),
    Literal (..),
    LiteralMatch (..),
    Match (..),
    NPlusKMatch (..),
    Name (..),
    NumericType (..),
    OverloadedLiteralMatch (..),
    Pattern (..),
    PatternTc (..),
    RecordField (..),
    SourceSpan,
    TcInfo (..),
    TupleFlavor (..),
    Type,
    UnqualifiedName (..),
    ValueDecl (..),
    annotationSpan,
    emptyPatternTc,
    fillSlot,
    infoHasType,
    infoIsPending,
    mkSpanAnnotation,
    modifyPatternTc,
    nameText,
    patternTc,
    patternTcHasType,
    peelDeclAnn,
    peelLiteralAnn,
    peelPatternAnn,
    resolvedBinderEntity,
  )
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (get, put)
import Data.List qualified as List
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | The variables that a pattern binds, in source order. Every pattern form
-- that can hold a sub-pattern is walked, so a tuple, list, record, view,
-- or signature pattern in a binding position lists the same binders as a
-- constructor pattern.
patternBinderNames :: Pattern -> [UnqualifiedName]
patternBinderNames pat =
  case pat of
    PAnn _ inner -> patternBinderNames inner
    PVar _ name -> [name]
    PTypeBinder _ _ -> []
    PTypeSyntax _ _ _ -> []
    PWildcard _ -> []
    PLit _ _ _ -> []
    PQuasiQuote _ _ _ -> []
    PTuple _ _ items -> concatMap patternBinderNames items
    PUnboxedSum _ _ _ inner -> patternBinderNames inner
    PList _ _ items -> concatMap patternBinderNames items
    PCon _ _ _ pats -> concatMap patternBinderNames pats
    PBuiltinCon _ _ _ pats -> concatMap patternBinderNames pats
    PInfix _ lhs _ rhs -> patternBinderNames lhs <> patternBinderNames rhs
    PView _ _ inner -> patternBinderNames inner
    PAs _ name inner -> name : patternBinderNames inner
    PStrict _ inner -> patternBinderNames inner
    PIrrefutable _ inner -> patternBinderNames inner
    PNegLit _ _ _ -> []
    PNPlusK _ _ name _ -> [name]
    PParen _ inner -> patternBinderNames inner
    PRecord _ _ fields _ -> concatMap (patternBinderNames . recordFieldValue) fields
    PTypeSig _ inner _ -> patternBinderNames inner
    PSplice _ _ -> []

-- | The variables that the statements of a recursive @do@ group bind, in
-- source order and without repeats. The knot of the group is a tuple of
-- these variables. The type checker and the desugarer both use this order.
recStmtBinderNames :: [DoStmt Expr] -> [UnqualifiedName]
recStmtBinderNames =
  List.nubBy (\left right -> unqualifiedNameText left == unqualifiedNameText right) . concatMap stmtBinders
  where
    stmtBinders stmt =
      case stmt of
        DoAnn _ inner -> stmtBinders inner
        DoRecKnot _ inner -> stmtBinders inner
        DoBind _ pat _ -> patternBinderNames pat
        DoLetDecls decls -> concatMap declBinders decls
        DoExpr {} -> []
        DoRecStmt _ _ stmts -> concatMap stmtBinders stmts
    declBinders decl =
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []

-- | The variables that the statements of one branch of a parallel list
-- comprehension bind, in source order and without repeats. The branch
-- gives a list of tuples of these variables. The type checker and the
-- desugarer both use this order.
compStmtBinderNames :: [CompStmt] -> [UnqualifiedName]
compStmtBinderNames =
  List.nubBy (\left right -> unqualifiedNameText left == unqualifiedNameText right) . concatMap stmtBinders
  where
    stmtBinders stmt =
      case stmt of
        CompAnn _ inner -> stmtBinders inner
        CompGen _ pat _ -> patternBinderNames pat
        CompLetDecls _ decls -> concatMap declBinders decls
        _ -> []
    declBinders decl =
      case peelDeclAnn decl of
        DeclValue (FunctionBind name _) -> [name]
        DeclValue (PatternBind _ pat _) -> patternBinderNames pat
        _ -> []

data PatternCheck = PatternCheck
  { pcBindings :: ![(UnqualifiedName, TcType)],
    pcWantedCts :: ![Ct],
    pcGivenCts :: ![Ct],
    pcSkolems :: ![TyVarId],
    -- | The type variables that pattern signatures bind. They scope over
    -- the patterns that follow and over the body.
    pcTyVars :: ![PatternTyVar],
    pcPatterns :: ![Pattern]
  }
  deriving (Show)

-- | A type variable that a pattern signature binds, as in @(x :: Arr a)@
-- when @a@ is not in scope. Its uses stand for 'ptvType', a meta variable
-- that the signature equality unifies with the type the signature matched.
data PatternTyVar = PatternTyVar
  { ptvTyVar :: !TyVarId,
    ptvType :: !TcType
  }
  deriving (Show)

instance Semigroup PatternCheck where
  left <> right =
    PatternCheck
      { pcBindings = pcBindings left <> pcBindings right,
        pcWantedCts = pcWantedCts left <> pcWantedCts right,
        pcGivenCts = pcGivenCts left <> pcGivenCts right,
        pcSkolems = pcSkolems left <> pcSkolems right,
        pcTyVars = pcTyVars left <> pcTyVars right,
        pcPatterns = pcPatterns left <> pcPatterns right
      }

instance Monoid PatternCheck where
  mempty = PatternCheck [] [] [] [] [] []

checkFunctionPatterns :: Maybe SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkFunctionPatterns sp arguments = do
  mapM_ (checkFunctionArgument sp) arguments
  checkPatterns sp arguments

checkFunctionArgument :: Maybe SourceSpan -> (Pattern, TcType) -> TcM ()
checkFunctionArgument ambient (pat, ty) = do
  kind <- tcTypeKind ty >>= zonkType
  case runtimeRepFromKind kind of
    -- A representation that is still a meta-variable comes from a
    -- partial-signature wildcard; the checked body fixes it, and
    -- generalization defaults whatever the body leaves open.
    Right (TcMetaTv _) -> pure ()
    Right representation
      | not (isFixedRuntimeRep representation) ->
          emitError
            (patternOwnSpan pat <|> ambient)
            (RepresentationPolymorphicFunctionArgument (functionArgumentName pat) ty)
    _ -> pure ()

functionArgumentName :: Pattern -> Text
functionArgumentName pat =
  case pat of
    PAnn _ inner -> functionArgumentName inner
    PVar _ name -> unqualifiedNameText name
    PParen _ inner -> functionArgumentName inner
    PAs _ name _ -> unqualifiedNameText name
    PStrict _ inner -> functionArgumentName inner
    PIrrefutable _ inner -> functionArgumentName inner
    PTypeSig _ inner _ -> functionArgumentName inner
    _ -> "<pattern>"

-- | Check patterns left to right. A view pattern's expression sees the
-- binders of the patterns before it, across a function's arguments and
-- inside one constructor or tuple pattern alike.
checkPatterns :: Maybe SourceSpan -> [(Pattern, TcType)] -> TcM PatternCheck
checkPatterns sp = go mempty
  where
    go done [] = pure done
    go done ((pat, ty) : rest) = do
      check <- withPatternTyVars (pcTyVars done) (withEarlierPatternBindings (pcBindings done) (checkPattern sp pat ty))
      go (done <> check) rest

-- | Bring the binders of the patterns checked so far into scope for the
-- patterns that follow, so a view pattern's expression can name them. A
-- binder that a placeholder already stands for keeps the placeholder, and
-- the module-level binders of a pattern binding are in the environment
-- already.
withEarlierPatternBindings :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withEarlierPatternBindings [] action = action
withEarlierPatternBindings ((name, ty) : rest) action =
  case localBinderKey name of
    Nothing -> withEarlierPatternBindings rest action
    Just key -> do
      bound <- lookupTermKey key
      case bound of
        Just _ -> withEarlierPatternBindings rest action
        Nothing -> extendTermEnv key (TcMonoIdBinder ty) (withEarlierPatternBindings rest action)

localBinderKey :: UnqualifiedName -> Maybe Entity
localBinderKey name =
  case resolvedBinderEntity name of
    Just local@EntityLocal {} -> Just local
    _ -> Nothing

checkPattern :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPattern sp pat scrutTy
  | isSigmaType scrutTy,
    patternNeedsInstantiation pat = do
      -- A constructor matches a value of the instantiated type, as in GHC.
      -- The annotation gives the type arguments and the evidence that the
      -- desugarer applies the scrutinee to.
      (body, typeArgs, predicates) <- instantiatePatternType scrutTy
      wanted <- mapM (predToCt sp "pattern") predicates
      check <- checkPattern sp pat body
      let instantiation = pendingAnnotation body typeArgs (map ctEvVar wanted) []
          annotate = atPatternSpan sp . modifyPatternTc (\tc -> tc {patternInstantiation = fillSlot "pattern instantiation" (patternInstantiation tc) instantiation})
      pure check {pcWantedCts = wanted <> pcWantedCts check, pcPatterns = map annotate (pcPatterns check)}
  where
    isSigmaType ty = case ty of
      TcForAllTy {} -> True
      TcQualTy {} -> True
      _ -> False
checkPattern sp pat scrutTy = do
  check <- case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing -> checkPatternCore sp pat scrutTy
  pure check {pcPatterns = map (checkedPatternType sp scrutTy) (pcPatterns check)}

-- | Instantiate the outer type variables of a sigma type with fresh meta
-- variables. The result is the body after the first context, the type
-- arguments, and the predicates of that context.
instantiatePatternType :: TcType -> TcM (TcType, [TcType], [Pred])
instantiatePatternType = go []
  where
    go arguments (TcForAllTy binder body) = do
      argument <- freshMetaTvOfKind (tvKind binder)
      go (arguments <> [argument]) (applySubst (Map.singleton (tvUnique binder) argument) body)
    go arguments (TcQualTy predicates body) = pure (body, arguments, predicates)
    go arguments ty = pure (ty, arguments, [])

-- | A variable keeps the constrained value. A constructor uses it.
patternNeedsInstantiation :: Pattern -> Bool
patternNeedsInstantiation pattern' = case pattern' of
  PAnn _ inner -> patternNeedsInstantiation inner
  PParen _ inner -> patternNeedsInstantiation inner
  PVar {} -> False
  PWildcard _ -> False
  PAs _ _ inner -> patternNeedsInstantiation inner
  PIrrefutable _ inner -> patternNeedsInstantiation inner
  -- A bang forces the value as it is: @f !_ = ..@ for an argument of type
  -- @forall x. C x => x -> x@ needs no @C@ instance.
  PStrict _ inner -> patternNeedsInstantiation inner
  _ -> True

checkPatternWithoutResultType :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternWithoutResultType sp pat scrutTy =
  case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing ->
      case pat of
        PAnn ann inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
        PParen tc inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PParen tc (checkedPattern innerCheck)]}
        PStrict tc inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PStrict tc (checkedPattern innerCheck)]}
        PIrrefutable tc inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PIrrefutable tc (checkedPattern innerCheck)]}
        _ -> checkPatternCore sp pat scrutTy

checkedPatternType :: Maybe SourceSpan -> TcType -> Pattern -> Pattern
checkedPatternType sp ty pat
  | patternUsesBinderAnnotation pat = pat
  | not (patternNeedsCheckedType pat) = pat
  | patternHasPendingType pat = pat
  | otherwise = annotatePendingPatternAt (patternOwnSpan pat <|> sp) (pendingAnnotation ty [] [] []) pat

annotatePendingPatternAt :: Maybe SourceSpan -> PendingTcAnnotation -> Pattern -> Pattern
annotatePendingPatternAt sp pending =
  atPatternSpan sp . modifyPatternTc (\tc -> tc {patternType = fillSlot "pattern type" (patternType tc) pending})

-- | Put a pattern below a span annotation, if there is a span.
atPatternSpan :: Maybe SourceSpan -> Pattern -> Pattern
atPatternSpan sp pat = maybe pat (\sp' -> PAnn (mkSpanAnnotation sp') pat) sp

-- | Set the cast of the scrutinee of a pattern.
castPatternScrutinee :: PendingTcCastAnnotation -> Pattern -> Pattern
castPatternScrutinee cast =
  modifyPatternTc (\tc -> tc {patternCast = fillSlot "pattern cast" (patternCast tc) cast})

patternUsesBinderAnnotation :: Pattern -> Bool
patternUsesBinderAnnotation pat =
  case pat of
    PAnn _ inner -> patternUsesBinderAnnotation inner
    PParen _ inner -> patternUsesBinderAnnotation inner
    PVar {} -> True
    PAs {} -> True
    PStrict _ inner -> patternUsesBinderAnnotation inner
    PIrrefutable _ inner -> patternUsesBinderAnnotation inner
    PTypeSig _ inner _ -> patternUsesBinderAnnotation inner
    _ -> False

patternNeedsCheckedType :: Pattern -> Bool
patternNeedsCheckedType pat =
  case pat of
    PAnn _ inner -> patternNeedsCheckedType inner
    PParen _ inner -> patternNeedsCheckedType inner
    PLit {} -> False
    PNegLit {} -> False
    PNPlusK {} -> False
    PStrict _ inner -> patternNeedsCheckedType inner
    PIrrefutable _ inner -> patternNeedsCheckedType inner
    PTypeSig _ inner _ -> patternNeedsCheckedType inner
    _ -> True

patternHasPendingType :: Pattern -> Bool
patternHasPendingType pat =
  case pat of
    _ | patternTcHasType (patternTc pat) -> True
    PAnn _ inner -> patternHasPendingType inner
    PParen _ inner -> patternHasPendingType inner
    PLit _ _ literal -> literalHasPendingType literal
    PStrict _ inner -> patternHasPendingType inner
    PIrrefutable _ inner -> patternHasPendingType inner
    PTypeSig _ inner _ -> patternHasPendingType inner
    _ -> False

literalHasPendingType :: Literal -> Bool
literalHasPendingType literal =
  case literal of
    LitAnn _ inner -> literalHasPendingType inner
    LitTc info inner -> infoHasType info || literalHasPendingType inner
    _ -> False

sourceSpanFromAnnotations :: [Annotation] -> Maybe SourceSpan
sourceSpanFromAnnotations = listToMaybe . mapMaybe annotationSpan

patternOwnSpan :: Pattern -> Maybe SourceSpan
patternOwnSpan pat =
  case pat of
    PAnn ann inner -> annotationSpan ann <|> patternOwnSpan inner
    PVar _ name -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PParen _ inner -> patternOwnSpan inner
    PAs _ name _ -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PStrict _ inner -> patternOwnSpan inner
    PIrrefutable _ inner -> patternOwnSpan inner
    PCon _ name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PInfix _ _ name _ -> sourceSpanFromAnnotations (nameAnns name)
    PRecord _ name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PTypeSig _ inner _ -> patternOwnSpan inner
    PView _ expr inner -> viewExprSpan expr <|> patternOwnSpan inner
    _ -> Nothing

-- | The span of a view pattern function. The parser gives spans to names
-- and to annotated expressions only.
viewExprSpan :: Expr -> Maybe SourceSpan
viewExprSpan expr =
  case expr of
    EAnn ann inner -> annotationSpan ann <|> viewExprSpan inner
    EVar _ name -> sourceSpanFromAnnotations (nameAnns name)
    EParen inner -> viewExprSpan inner
    EPragma _ inner -> viewExprSpan inner
    EApp _ function _ -> viewExprSpan function
    _ -> Nothing

checkPatternCore :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternCore sp pat scrutTy =
  case pat of
    PVar _ name ->
      pure (checkedOnly pat) {pcBindings = [(name, scrutTy)]}
    PAnn ann inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
    PParen tc inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PParen tc (checkedPattern innerCheck)]}
    PWildcard {} -> pure (checkedOnly pat)
    PLit tc match lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern has no primitive type name"
      | otherwise -> do
          maybeLiteralTy <- plainLiteralPatternType lit
          case maybeLiteralTy of
            -- A given equality can make the scrutinee type different from
            -- the literal type, as @Element t@ and @Char@ are with the given
            -- @Element t ~ Char@. The cast lets the desugarer match the
            -- scrutinee at the literal type, as for a constructor pattern.
            Just literalTy -> do
              eqCt <- wantedEq sp scrutTy literalTy
              let castPattern = castPatternScrutinee (PendingTcCastAnnotation literalTy (ctEvVar eqCt) CastToRight) (PLit tc match (checkedLiteral literalTy lit))
              pure (checkedOnly castPattern) {pcWantedCts = [eqCt]}
            Nothing -> pure (checkedOnly (PLit tc match (checkedLiteral scrutTy lit)))
    PNegLit _ _ lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern has no primitive type name"
      | otherwise -> pure (checkedOnly pat)
    PAs tc name inner -> do
      let innerSpan = patternOwnSpan inner <|> sp
      innerCheck <- checkPatternWithoutResultType innerSpan inner scrutTy
      pure innerCheck {pcBindings = (name, scrutTy) : pcBindings innerCheck, pcPatterns = [PAs tc name (checkedPattern innerCheck)]}
    PStrict tc inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PStrict tc (checkedPattern innerCheck)]}
    PTypeSig _ inner tyAnn -> checkTypeSigPattern sp inner tyAnn scrutTy
    PIrrefutable tc inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PIrrefutable tc (checkedPattern innerCheck)]}
    PCon _ name _typeArgs subPats ->
      checkConPattern sp pat name subPats scrutTy
    PInfix _ lhs op rhs ->
      checkConPattern sp pat op [lhs, rhs] scrutTy
    PRecord _ name fields wildcard -> do
      when wildcard $
        abortTc ("record wildcard patterns are not supported at " <> show (patternOwnSpan pat <|> sp))
      head' <- lookupRecordHead name
      subPats <- orderRecordFields (patternOwnSpan pat <|> sp) head' fields (\_ _ -> pure (PWildcard emptyPatternTc))
      checkConPattern sp (PCon emptyPatternTc name [] subPats) name subPats scrutTy
    PList _ _ items -> checkListPattern sp items scrutTy
    PView tc viewExpr inner -> do
      let viewSpan = viewExprSpan viewExpr <|> sp
      (viewExpr', viewTy, viewCts) <- inferExprAt viewSpan viewExpr
      innerTy <- freshMetaTv
      eqCt <- wantedEq viewSpan viewTy (TcFunTy scrutTy innerTy)
      -- GHC unifies the type of the view function with the scrutinee type
      -- before it checks the inner pattern. Then a constructor pattern
      -- reads its universal arguments from the known result type. Thus, a
      -- GADT constructor gives its equalities in terms of these types, and
      -- not in terms of meta variables that the branch cannot solve. If
      -- the equality is not solved here, it stays a wanted of the pattern.
      before <- lift get
      result <- solveEquality eqCt
      solved <-
        case result of
          EqSolved -> pure True
          _ -> False <$ lift (put before)
      innerCheck <- checkPattern sp inner innerTy
      pure
        innerCheck
          { pcWantedCts = [eqCt | not solved] <> viewCts <> pcWantedCts innerCheck,
            pcPatterns = [PView tc viewExpr' (checkedPattern innerCheck)]
          }
    PTuple _ flavor items -> checkTuplePattern sp flavor items scrutTy
    -- A prefix tuple constructor, @(,) a b@, checks like the @(a, b)@ form.
    -- The type arguments follow 'PCon', which ignores them.
    PBuiltinCon _ (BuiltinTuple flavor arity) _typeArgs items
      | length items == arity ->
          checkTuplePattern sp flavor items scrutTy
    PUnboxedSum tc alternative arity inner -> do
      types <- mapM (const freshMetaTv) [1 .. arity]
      sumType <- unboxedSumType types
      equality <- wantedEq sp scrutTy sumType
      case drop alternative types of
        innerType : _ -> do
          checked <- checkPattern sp inner innerType
          pure
            checked
              { pcWantedCts = equality : pcWantedCts checked,
                pcPatterns = [PUnboxedSum tc alternative arity (checkedPattern checked)]
              }
        [] -> abortTc "invalid unboxed sum alternative"
    _ -> pure (checkedOnly pat)

-- | A pattern signature, @(ptr :: Ptr Word32)@. The signature is elaborated
-- in the enclosing scoped type variables and the sub-pattern is checked
-- against it, so the binders the sub-pattern introduces get the type the
-- signature gives them. A wanted equality ties the signature to the
-- scrutinee.
--
-- A type variable of the signature that is not in scope is bound here, as
-- in GHC: it stands for the type the signature matches, and it scopes over
-- the patterns that follow and over the body. With @replace (needle ::
-- Arr a) = ...@ under a signature without @forall@, the @a@ of a
-- where-bound signature is this @a@, and the equality with the argument
-- type makes it the @a@ of the outer signature.
--
-- The equality is solved at the pattern if it can be, as GHC unifies a
-- pattern signature at the pattern. Otherwise a GADT match in the body or
-- in the inner pattern can solve a meta variable from its own givens, and
-- the equality then fails outside that match. In sbv, the lambda pattern
-- @(SBVsCons as a :: SBVs (as :> a))@ is an example.
checkTypeSigPattern :: Maybe SourceSpan -> Pattern -> Type -> TcType -> TcM PatternCheck
checkTypeSigPattern sp inner tyAnn scrutTy = do
  scoped <- getScopedTyVars
  boundTyVars <- mapM bindPatternTyVar (filter (`Map.notMember` scoped) (freeTypeVars tyAnn))
  withPatternTyVars boundTyVars $ do
    scoped' <- getScopedTyVars
    -- The signature has the kind of the scrutinee. An unlifted field, as
    -- the @Proxy# s@ of @P (_ :: Proxy# t) x@, is not of kind 'Type'.
    scrutKind <- tcTypeKind =<< zonkType scrutTy
    sigTy <- checkSurfaceType scoped' tyAnn scrutKind
    eqCt <- wantedEq sp scrutTy sigTy
    before <- lift get
    result <- solveEquality eqCt
    solved <-
      case result of
        EqSolved -> pure True
        _ -> False <$ lift (put before)
    innerCheck <- checkPattern sp inner sigTy
    pure
      innerCheck
        { pcWantedCts = [eqCt | not solved] <> pcWantedCts innerCheck,
          pcTyVars = boundTyVars <> pcTyVars innerCheck,
          pcPatterns = [PTypeSig emptyPatternTc (checkedPattern innerCheck) tyAnn]
        }

-- | A fresh type variable for a pattern signature to bind, and the meta
-- variable it stands for.
bindPatternTyVar :: Text -> TcM PatternTyVar
bindPatternTyVar name = do
  kind <- freshKindMeta
  ty <- freshMetaTvOfKind kind
  unique <- freshUnique
  pure PatternTyVar {ptvTyVar = mkTyVarId name unique kind, ptvType = ty}

-- | Bring the type variables that pattern signatures bound into scope.
withPatternTyVars :: [PatternTyVar] -> TcM a -> TcM a
withPatternTyVars [] action = action
withPatternTyVars tyVars action =
  withScopedTyVars scope (withTyVarTypes bound action)
  where
    scope = Map.fromList [(tvName tyVar, (tyVar, tvKind tyVar)) | PatternTyVar tyVar _ <- tyVars]
    bound = Map.fromList [(tvUnique tyVar, ty) | PatternTyVar tyVar ty <- tyVars]

checkTuplePattern :: Maybe SourceSpan -> TupleFlavor -> [Pattern] -> TcType -> TcM PatternCheck
checkTuplePattern sp flavor items scrutTy = do
  let arity = length items
  wired <- wiredTupleTyCon flavor arity
  -- The wiring gives the full identity of the tuple type constructor.
  -- A bare name lookup can find a different constructor with the same name.
  tupleTyCon <- registeredWiredTyCon wired
  -- A constructor pattern of a GADT reads the type indices from its
  -- scrutinee type. With a fresh meta variable, an index is a new meta
  -- variable that nothing solves. Thus, when an item contains a
  -- constructor pattern, a known tuple type gives its element types to
  -- all items. The tuple equality is then reflexive. If another item got
  -- a fresh meta variable, the tuple equality could need a given of the
  -- constructor pattern, but the cast of the tuple pattern is outside the
  -- scope of that given. A tuple without a constructor pattern keeps
  -- fresh meta variables.
  knownScrutTy <- zonkType scrutTy
  elemTys <- case knownScrutTy of
    TcTyCon scrutTyCon arguments
      | scrutTyCon == tupleTyCon,
        length arguments == arity,
        any containsConstructorPattern items ->
          pure arguments
    _ -> mapM (const freshMetaTv) items
  let tupleTy = TcTyCon tupleTyCon elemTys
  eqCt <- wantedEq sp scrutTy tupleTy
  itemChecks <- checkPatterns sp (zip items elemTys)
  let tuplePattern = annotatePendingPatternAt sp (pendingAnnotation tupleTy [] [] []) (PTuple emptyPatternTc flavor (pcPatterns itemChecks))
      castPattern = castPatternScrutinee (PendingTcCastAnnotation tupleTy (ctEvVar eqCt) CastToRight) tuplePattern
  pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks, pcPatterns = [castPattern]}

-- | Whether a pattern contains a constructor pattern.
containsConstructorPattern :: Pattern -> Bool
containsConstructorPattern pat =
  case pat of
    PCon {} -> True
    PBuiltinCon {} -> True
    PInfix {} -> True
    PRecord {} -> True
    PAnn _ inner -> containsConstructorPattern inner
    PParen _ inner -> containsConstructorPattern inner
    PAs _ _ inner -> containsConstructorPattern inner
    PStrict _ inner -> containsConstructorPattern inner
    PIrrefutable _ inner -> containsConstructorPattern inner
    PView _ _ inner -> containsConstructorPattern inner
    PTypeSig _ inner _ -> containsConstructorPattern inner
    PUnboxedSum _ _ _ inner -> containsConstructorPattern inner
    PTuple _ _ inner -> any containsConstructorPattern inner
    PList _ _ inner -> any containsConstructorPattern inner
    _ -> False

checkedOnly :: Pattern -> PatternCheck
checkedOnly pat = mempty {pcPatterns = [pat]}

checkedLiteral :: TcType -> Literal -> Literal
checkedLiteral ty = LitTc (InfoType (pendingAnnotation ty [] [] []))

checkListPattern :: Maybe SourceSpan -> [Pattern] -> TcType -> TcM PatternCheck
checkListPattern sp items scrutTy =
  case items of
    [] -> do
      (nilCon, scheme) <- listConstructorScheme tcWiringNilDataCon
      (nilTy, _typeArgs, predicates, skolems) <- instantiateConstructorPattern scrutTy scheme
      scrutCt <- wantedEq sp scrutTy nilTy
      predicateGivens <- mapM (constructorGiven sp (tyConName nilCon)) predicates
      pure
        mempty
          { pcWantedCts = [scrutCt],
            pcGivenCts = predicateGivens,
            pcSkolems = skolems,
            pcPatterns = [PList emptyPatternTc Nothing []]
          }
    item : rest -> do
      (consCon, scheme) <- listConstructorScheme tcWiringConsDataCon
      (consTy, _typeArgs, predicates, skolems) <- instantiateConstructorPattern scrutTy scheme
      (argumentTypes, resultTy) <- splitConTy 2 consTy
      case argumentTypes of
        [itemTy, tailTy] -> do
          scrutCt <- wantedEq sp scrutTy resultTy
          itemCheck <- checkPattern sp item itemTy
          tailCheck <- withEarlierPatternBindings (pcBindings itemCheck) (checkListPattern sp rest tailTy)
          predicateGivens <- mapM (constructorGiven sp (tyConName consCon)) predicates
          let nestedCheck = itemCheck <> tailCheck
              checkedTailItems = case checkedPattern tailCheck of
                PAnn _ (PList _ _ patterns) -> patterns
                PList _ _ patterns -> patterns
                _ -> rest
              checkedItems = checkedPattern itemCheck : checkedTailItems
          pure
            nestedCheck
              { pcWantedCts = [scrutCt] <> pcWantedCts nestedCheck,
                pcGivenCts = predicateGivens <> pcGivenCts nestedCheck,
                pcSkolems = skolems <> pcSkolems nestedCheck,
                pcPatterns = [PList emptyPatternTc Nothing checkedItems]
              }
        _ -> abortTc "the wired list cons constructor has an invalid arity"

-- | The identity and the checked scheme of one wired list constructor.
listConstructorScheme :: (TcWiring -> TyCon) -> TcM (TyCon, TypeScheme)
listConstructorScheme select = do
  wired <- wiredTyConIdentity select
  maybeBinder <- lookupWiredTerm wired
  let name = tyConName wired
  case maybeBinder of
    Just (TcIdBinder scheme _) -> pure (wired, scheme)
    Just TcMonoIdBinder {} -> abortTc ("the wired list constructor is monomorphic: " <> T.unpack name)
    Nothing -> abortTc ("the wired list constructor is missing: " <> T.unpack name)

checkedPattern :: PatternCheck -> Pattern
checkedPattern check =
  case pcPatterns check of
    [pat] -> pat
    _ -> error "checkedPattern: expected exactly one checked pattern"

-- | The type of a plain character or string literal pattern.
--
-- A string pattern without OverloadedStrings has the type [Char]. The scrutinee
-- must get this type also when the function has no signature.
plainLiteralPatternType :: Literal -> TcM (Maybe TcType)
plainLiteralPatternType literal =
  case peelLiteralAnn literal of
    LitChar {} -> Just <$> charType
    LitString {} -> Just <$> stringType
    _ -> pure Nothing

-- | The check of a pattern whose match uses names from the resolver.
--
-- An overloaded literal pattern, an n+k pattern, and a list pattern under
-- OverloadedLists use syntax terms. A primitive literal pattern uses its
-- primitive type.
literalPatternCheck :: Maybe SourceSpan -> Pattern -> TcType -> Maybe (TcM PatternCheck)
literalPatternCheck sp pat scrutTy =
  case peelPatternAnn pat of
    PNPlusK _ match name _ -> Just (checkNPlusKPattern sp pat match name scrutTy)
    PList _ (Just match) items -> Just (checkOverloadedListPattern sp pat match items scrutTy)
    PLit _ match _ -> literalCheck False match
    PNegLit _ match _ -> literalCheck True match
    _ -> Nothing
  where
    literalCheck isNegative match =
      case match of
        LiteralMatchFixed -> Nothing
        LiteralMatchPrimitive typeName -> Just (checkPrimitiveLiteralPattern sp pat typeName scrutTy)
        LiteralMatchOverloaded overloaded -> Just (checkOverloadedLiteralPattern sp pat overloaded isNegative scrutTy)

isPrimitiveLiteral :: Literal -> Bool
isPrimitiveLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ numericType _ -> numericType /= TInteger
    LitFloat _ floatType _ -> floatType /= TFractional
    LitCharHash {} -> True
    LitStringHash {} -> True
    _ -> False

-- | Check a primitive literal pattern against the scrutinee type.
--
-- The resolver gives the pattern the name of the primitive type of the
-- literal. The pattern type is that primitive type, so the scrutinee must
-- equal it.
checkPrimitiveLiteralPattern :: Maybe SourceSpan -> Pattern -> Name -> TcType -> TcM PatternCheck
checkPrimitiveLiteralPattern sp pat typeName scrutTy = do
  resolution <- requiredMatchResolution typeName
  maybeInfo <- lookupResolvedTypeSyntax resolution
  info <-
    maybe
      (abortTc ("resolved primitive literal type missing from type environment: " <> show (resolutionTarget resolution)))
      pure
      maybeInfo
  let literalTy = TcTyCon (tciTyCon info) []
  eqCt <- wantedEq sp scrutTy literalTy
  pure (checkedOnly (checkedLiteralPattern scrutTy pat)) {pcWantedCts = [eqCt]}

-- | The type of a type name that the resolver gives a pattern, such as the
-- Integer type of an overloaded integer pattern.
--
-- The result is 'Nothing' when the resolver gives no type name.
matchTypeName :: Maybe Name -> TcM (Maybe TcType)
matchTypeName maybeName =
  case maybeName >>= nameResolution >>= resolvedOf of
    Just resolution -> fmap (\info -> TcTyCon (tciTyCon info) []) <$> lookupResolvedTypeSyntax resolution
    Nothing -> pure Nothing

-- | Attach the checked type to the literal inside a literal pattern.
checkedLiteralPattern :: TcType -> Pattern -> Pattern
checkedLiteralPattern ty pat =
  case pat of
    PAnn ann inner -> PAnn ann (checkedLiteralPattern ty inner)
    PParen tc inner -> PParen tc (checkedLiteralPattern ty inner)
    PStrict tc inner -> PStrict tc (checkedLiteralPattern ty inner)
    PIrrefutable tc inner -> PIrrefutable tc (checkedLiteralPattern ty inner)
    PLit tc match lit -> PLit tc match (checkedLiteral ty lit)
    PNegLit tc match lit -> PNegLit tc match (checkedLiteral ty lit)
    _ -> pat

-- | Check a literal pattern that a class method converts and equality tests.
--
-- The resolver can give the type of the literal before the conversion. The
-- argument type of the conversion method must then equal it.
checkOverloadedLiteralPattern :: Maybe SourceSpan -> Pattern -> OverloadedLiteralMatch -> Bool -> TcType -> TcM PatternCheck
checkOverloadedLiteralPattern sp pat match isNegative scrutTy = do
  literalTy <- matchTypeName (overloadedLiteralType match)
  let conversion = overloadedLiteralConversion match
  (conversion', conversionCts) <-
    checkPatternMethodWithExpected sp conversion $ \case
      TcFunTy argumentTy _ -> pure (scrutTy, TcFunTy (fromMaybe argumentTy literalTy) scrutTy)
      _ -> abortTc (T.unpack (nameText conversion) <> " does not have a function type")
  negateCheck <-
    case overloadedLiteralNegate match of
      Just negation | isNegative -> Just <$> checkPatternMethod sp negation scrutTy (TcFunTy scrutTy scrutTy)
      Just _ -> abortTc "a literal pattern that is not negated has a negate name"
      Nothing
        | isNegative -> abortTc "a negated literal pattern has no negate name"
        | otherwise -> pure Nothing
  (equality', eqCts) <-
    checkPatternMethodWithExpected sp (overloadedLiteralEquality match) $ \case
      TcFunTy _ (TcFunTy _ boolTy) ->
        let expectedTy = TcFunTy scrutTy (TcFunTy scrutTy boolTy)
         in pure (expectedTy, expectedTy)
      _ -> abortTc "== does not have a binary function type"
  let match' =
        match
          { overloadedLiteralConversion = conversion',
            overloadedLiteralNegate = fst <$> negateCheck,
            overloadedLiteralEquality = equality'
          }
      pat' = mapPatternBase (setLiteralMatch (LiteralMatchOverloaded match')) pat
      negateCts = maybe [] snd negateCheck
  pure
    PatternCheck
      { pcBindings = [],
        pcWantedCts = conversionCts <> negateCts <> eqCts,
        pcGivenCts = [],
        pcSkolems = [],
        pcTyVars = [],
        pcPatterns = [pat']
      }
  where
    setLiteralMatch match' base =
      case base of
        PLit tc _ literal -> PLit tc match' literal
        PNegLit tc _ literal -> PNegLit tc match' literal
        _ -> base

-- | Check a list pattern under OverloadedLists. As in GHC, the pattern
-- @[p1, ..., pn]@ applies @toList@ to the scrutinee and matches the list
-- pattern against the result. The @toList@ name gets the type from the
-- scrutinee type to the type of the list.
checkOverloadedListPattern :: Maybe SourceSpan -> Pattern -> ListMatch -> [Pattern] -> TcType -> TcM PatternCheck
checkOverloadedListPattern sp pat match items scrutTy = do
  listTy <- freshMetaTv
  let methodTy = TcFunTy scrutTy listTy
  (toList', methodCts) <- checkPatternMethod sp (listMatchToList match) methodTy methodTy
  listCheck <- checkListPattern sp items listTy
  let checkedItems = case peelPatternAnn (checkedPattern listCheck) of
        PList _ _ patterns -> patterns
        _ -> items
      pat' = mapPatternBase (\base -> PList (patternTc base) (Just match {listMatchToList = toList'}) checkedItems) pat
  pure listCheck {pcWantedCts = methodCts <> pcWantedCts listCheck, pcPatterns = [pat']}

-- | Check an n+k pattern.
--
-- The Haskell 98 Report gives the match of @n+k@ against @v@ as the test
-- @v >= fromInteger k@, and then @n@ binds to @v - fromInteger k@. Thus the
-- three methods have the scrutinee type, and @n@ has the scrutinee type.
-- GHC also wants an @Integral@ instance. The built-in scope has no
-- @Integral@ class, so this check does not want it.
checkNPlusKPattern :: Maybe SourceSpan -> Pattern -> NPlusKMatch -> UnqualifiedName -> TcType -> TcM PatternCheck
checkNPlusKPattern sp pat match name scrutTy = do
  integerTy <- matchTypeName (nPlusKType match)
  (conversion', conversionCts) <-
    checkPatternMethodWithExpected sp (nPlusKFromInteger match) $ \case
      TcFunTy argumentTy _ -> pure (scrutTy, TcFunTy (fromMaybe argumentTy integerTy) scrutTy)
      _ -> abortTc "fromInteger does not have a function type"
  (comparison', compCts) <-
    checkPatternMethodWithExpected sp (nPlusKGreaterEqual match) $ \case
      TcFunTy _ (TcFunTy _ boolTy) ->
        let expectedTy = TcFunTy scrutTy (TcFunTy scrutTy boolTy)
         in pure (expectedTy, expectedTy)
      _ -> abortTc ">= does not have a binary function type"
  let minusTy = TcFunTy scrutTy (TcFunTy scrutTy scrutTy)
  (minus', minusCts) <- checkPatternMethod sp (nPlusKMinus match) minusTy minusTy
  let match' = match {nPlusKFromInteger = conversion', nPlusKGreaterEqual = comparison', nPlusKMinus = minus'}
      pat' = mapPatternBase (setMatch match') pat
  pure
    PatternCheck
      { pcBindings = [(name, scrutTy)],
        pcWantedCts = conversionCts <> compCts <> minusCts,
        pcGivenCts = [],
        pcSkolems = [],
        pcTyVars = [],
        pcPatterns = [pat']
      }
  where
    setMatch match' base =
      case base of
        PNPlusK tc _ binder literal -> PNPlusK tc match' binder literal
        _ -> base

-- | Change the node below the annotation wrappers of a pattern.
mapPatternBase :: (Pattern -> Pattern) -> Pattern -> Pattern
mapPatternBase f pat =
  case pat of
    PAnn ann inner -> PAnn ann (mapPatternBase f inner)
    _ -> f pat

-- | Check a syntax term that the match of a pattern uses. The result is
-- the name with its checked type.
checkPatternMethod :: Maybe SourceSpan -> Name -> TcType -> TcType -> TcM (Name, [Ct])
checkPatternMethod sp name annotationTy expectedTy =
  checkPatternMethodWithExpected sp name (const (pure (annotationTy, expectedTy)))

checkPatternMethodWithExpected :: Maybe SourceSpan -> Name -> (TcType -> TcM (TcType, TcType)) -> TcM (Name, [Ct])
checkPatternMethodWithExpected sp name expectedTypes = do
  let displayName = nameText name
  resolution <- requiredMatchResolution name
  (actualTy, typeArgs, methodCts) <- inferResolvedPatternMethod sp displayName resolution
  (annotationTy, expectedTy) <- expectedTypes actualTy
  methodEq <- wantedMethodEq sp displayName actualTy expectedTy
  let pending = pendingAnnotation annotationTy typeArgs (map ctEvVar methodCts) []
  pure (name {nameTc = nameTc name <> [InfoType pending]}, methodCts <> [methodEq])

wantedMethodEq :: Maybe SourceSpan -> Text -> TcType -> TcType -> TcM Ct
wantedMethodEq sp method actual expected = do
  ev <- freshEvVar
  pure $
    mkWantedEqCt
      TypeTrace
        { typeTraceType = actual,
          typeTraceRole = ActualType,
          typeTraceOrigin = ConstraintTypeOrigin (OccurrenceOf method)
        }
      TypeTrace
        { typeTraceType = expected,
          typeTraceRole = ExpectedType,
          typeTraceOrigin = ConstraintTypeOrigin (LitOrigin sp)
        }
      ev
      (LitOrigin sp)
      sp

inferResolvedPatternMethod :: Maybe SourceSpan -> Text -> Resolved -> TcM (TcType, [TcType], [Ct])
inferResolvedPatternMethod sp displayName resolution = do
  mBinder <- lookupResolvedTerm displayName (resolutionTarget resolution)
  case mBinder of
    Just (TcIdBinder scheme _) -> do
      inst <- instantiateWithArgs scheme
      cts <- mapM (predToCt sp displayName) (instPreds inst)
      pure (instType inst, instTypeArgs inst, cts)
    Just (TcMonoIdBinder ty) ->
      pure (ty, [], [])
    Nothing ->
      abortTc ("resolved " <> T.unpack displayName <> " missing from type environment: " <> show (resolutionTarget resolution))

predToCt :: Maybe SourceSpan -> Text -> Pred -> TcM Ct
predToCt sp name pred' = do
  ev <- freshEvVar
  pure (mkWantedCt pred' ev (OccurrenceOf name) sp)

-- | The resolution of a name that the resolver gives the match of a pattern.
requiredMatchResolution :: Name -> TcM Resolved
requiredMatchResolution name =
  case nameResolution name >>= resolvedOf of
    Just resolution -> pure resolution
    Nothing -> do
      let message = "unresolved name in the match of a pattern: " <> T.unpack (nameText name)
      emitError Nothing (OtherError message)
      abortTc message

annotatePatternBindings :: [(UnqualifiedName, TcType)] -> Pattern -> Pattern
annotatePatternBindings bindings = mapPatternBinderNames (annotateBinderName bindings)

-- | Replace the pending annotation of the named binders of a pattern. A
-- top-level pattern binding uses this to record, on each binder, the type
-- variables its selector abstracts and the type arguments that instantiate
-- the shared right-hand side.
reannotatePatternBinders :: [(Text, PendingTcAnnotation)] -> Pattern -> Pattern
reannotatePatternBinders pendings = mapPatternBinderNames replaceBinderAnnotation
  where
    replaceBinderAnnotation name =
      case lookup (unqualifiedNameText name) pendings of
        Nothing -> name
        Just pending ->
          name
            { unqualifiedNameTc =
                [item | item <- unqualifiedNameTc name, not (infoIsPending item)] <> [InfoType pending]
            }

-- | Apply a function to every named binder of a pattern.
mapPatternBinderNames :: (UnqualifiedName -> UnqualifiedName) -> Pattern -> Pattern
mapPatternBinderNames annotateBinder =
  go
  where
    go pat =
      case pat of
        PAnn ann inner -> PAnn ann (go inner)
        PVar tc name -> PVar tc (annotateBinder name)
        PNPlusK tc match name literal -> PNPlusK tc match (annotateBinder name) literal
        PParen tc inner -> PParen tc (go inner)
        PAs tc name inner -> PAs tc (annotateBinder name) (go inner)
        PStrict tc inner -> PStrict tc (go inner)
        PIrrefutable tc inner -> PIrrefutable tc (go inner)
        PList tc match items -> PList tc match (map go items)
        PTuple tc flavor items -> PTuple tc flavor (map go items)
        PUnboxedSum tc alt arity inner -> PUnboxedSum tc alt arity (go inner)
        PInfix tc lhs op rhs -> PInfix tc (go lhs) op (go rhs)
        PView tc expr inner -> PView tc expr (go inner)
        PCon tc name typeArgs subPats -> PCon tc name typeArgs (map go subPats)
        PRecord tc name fields wildcard -> PRecord tc name (map annotateRecordField fields) wildcard
        PTypeSig tc inner type' -> PTypeSig tc (go inner) type'
        PSplice tc expr -> PSplice tc expr
        _ -> pat

    annotateRecordField :: RecordField Pattern -> RecordField Pattern
    annotateRecordField field =
      field {recordFieldValue = go (recordFieldValue field)}

annotateBinderName :: [(UnqualifiedName, TcType)] -> UnqualifiedName -> UnqualifiedName
annotateBinderName bindings name =
  case lookup name bindings of
    Nothing -> name
    Just ty
      | any infoIsPending (unqualifiedNameTc name) -> name
      | otherwise -> name {unqualifiedNameTc = unqualifiedNameTc name <> [InfoType (pendingAnnotation ty [] [] [])]}

checkConPattern :: Maybe SourceSpan -> Pattern -> Name -> [Pattern] -> TcType -> TcM PatternCheck
checkConPattern sp originalPat conSyntax subPats scrutTy = do
  let conName = patternNameText conSyntax
  target <- resolvedTermTarget conSyntax
  mBinder <- lookupResolvedTerm conName target
  mPatSyn <- lookupPatSynTarget target
  case mBinder of
    Just (TcIdBinder _ _)
      | Just info <- mPatSyn ->
          checkPatSynPattern sp originalPat conName info (psiScheme info) subPats scrutTy
    Just (TcIdBinder scheme _) -> do
      (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPattern scrutTy scheme
      (argTys, conResTy) <- splitConTy (length subPats) conTy
      scrutCt <- wantedEq sp scrutTy conResTy
      let Scheme inferred _ _ _ = scheme
      (boundTyVars, typeArgCts) <- checkConTypeArguments sp conName (patternTypeArguments originalPat) (drop (length inferred) typeArgs)
      subCheck <- withPatternTyVars boundTyVars (checkPatterns sp (zip subPats argTys))
      predicateGivens <- mapM (constructorGiven sp conName) predicates
      -- The annotation carries the constructor's result type, not the
      -- scrutinee's: the desugarer reads the type arguments of a newtype
      -- coercion from it, and a scrutinee type may be an unreduced type
      -- family application (@MutableGen (STGen g) (ST s)@) whose arguments
      -- are not the newtype's.
      let rebuiltPattern = replaceConstructorSubpatterns originalPat (pcPatterns subCheck)
          annotatedPattern
            | null predicateGivens && null skolems =
                annotatePendingPatternAt (patternOwnSpan originalPat <|> sp) (pendingAnnotation conResTy [] [] []) rebuiltPattern
            | otherwise =
                annotatePendingPatternAt
                  (patternOwnSpan originalPat <|> sp)
                  ( (pendingAnnotation conTy typeArgs (map ctEvVar predicateGivens) [])
                      { pendingTcAnnTypeBinders = skolems
                      }
                  )
                  rebuiltPattern
      pure
        subCheck
          { pcWantedCts = [scrutCt] <> typeArgCts <> pcWantedCts subCheck,
            pcGivenCts = predicateGivens <> pcGivenCts subCheck,
            pcSkolems = skolems <> pcSkolems subCheck,
            pcTyVars = boundTyVars <> pcTyVars subCheck,
            pcPatterns = [castPatternScrutinee (PendingTcCastAnnotation conResTy (ctEvVar scrutCt) CastToRight) annotatedPattern]
          }
    Just other ->
      abortTc ("resolved constructor is not an identifier binder: " <> show conName <> " resolved as " <> show target <> " with binder " <> show other)
    Nothing ->
      abortTc ("resolved constructor missing from type environment: " <> show conName <> " resolved as " <> show target)

-- | Check a pattern synonym use. The required predicates are wanted at
-- the use. The provided predicates are given to the branch. The annotation
-- records the type arguments, the required evidence and then the provided
-- evidence, and the existential skolems. The desugarer calls the matcher
-- with them.
checkPatSynPattern :: Maybe SourceSpan -> Pattern -> Text -> PatSynInfo -> TypeScheme -> [Pattern] -> TcType -> TcM PatternCheck
checkPatSynPattern sp originalPat conName info scheme subPats scrutTy = do
  when (length subPats /= psiArity info) $
    emitError sp (OtherError ("pattern synonym " <> T.unpack conName <> " takes " <> show (psiArity info) <> " arguments, but the pattern gives " <> show (length subPats)))
  -- A variable of the required context is universal, as a variable of the
  -- result type is, also when the result type does not mention it.
  let ForAll _ schemePredicates _ = scheme
      requiredVariables = foldMap predTyVars (take (length (psiReqTheta info)) schemePredicates)
  (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPatternWith requiredVariables scrutTy scheme
  let (requiredPreds, providedPreds) = splitAt (length (psiReqTheta info)) predicates
  (argTys, conResTy) <- splitConTy (length subPats) conTy
  scrutCt <- wantedEq sp scrutTy conResTy
  subCheck <- checkPatterns sp (zip subPats argTys)
  requiredCts <- mapM (predToCt sp conName) requiredPreds
  providedGivens <- mapM (constructorGiven sp conName) providedPreds
  let rebuiltPattern = replaceConstructorSubpatterns originalPat (pcPatterns subCheck)
      annotatedPattern =
        annotatePendingPatternAt
          (patternOwnSpan originalPat <|> sp)
          ( (pendingAnnotation conTy typeArgs (map ctEvVar requiredCts <> map ctEvVar providedGivens) [])
              { pendingTcAnnTypeBinders = skolems
              }
          )
          rebuiltPattern
  pure
    subCheck
      { pcWantedCts = [scrutCt] <> requiredCts <> pcWantedCts subCheck,
        pcGivenCts = providedGivens <> pcGivenCts subCheck,
        pcSkolems = skolems <> pcSkolems subCheck,
        pcPatterns = [annotatedPattern]
      }

constructorGiven :: Maybe SourceSpan -> Text -> Pred -> TcM Ct
constructorGiven sp constructorName predicate = do
  evidence <- freshEvVar
  bindEvidence evidence (EvGiven predicate)
  let origin = OccurrenceOf constructorName
  pure
    Ct
      { ctPred = predicate,
        ctFlavor = Given,
        ctEvVar = evidence,
        ctOrigin = origin,
        ctProvenance = FromCtOrigin origin,
        ctLoc = sp,
        ctBranchGivens = [],
        ctWrittenPred = predicate
      }

instantiateConstructorPattern :: TcType -> TypeScheme -> TcM (TcType, [TcType], [Pred], [TyVarId])
instantiateConstructorPattern = instantiateConstructorPatternWith Set.empty

-- | Instantiate a constructor pattern. The set holds more universal
-- variables than the result type gives.
instantiateConstructorPatternWith :: Set.Set Unique -> TcType -> TypeScheme -> TcM (TcType, [TcType], [Pred], [TyVarId])
instantiateConstructorPatternWith extraUniversals scrutTy (ForAll tyVars predicates body) = do
  matched <- matchConstructorResult (Set.fromList (map tvUnique tyVars)) (constructorResultType body) =<< zonkType scrutTy
  let resultTyVars = constructorResultTyVars body <> extraUniversals
      isUniversal tyVar = tvUnique tyVar `Set.member` resultTyVars
  (substitution, skolems) <- foldM (instantiateTyVar matched isUniversal) (Map.empty, []) tyVars
  let instantiateType = applySubst substitution
      typeArgs = map (instantiateType . TcTyVar) tyVars
  pure
    ( instantiateType body,
      typeArgs,
      map (applySubstPred substitution) predicates,
      skolems
    )
  where
    instantiateTyVar matched isUniversal (substitution, skolems) tyVar = do
      let kind = applySubst substitution (tvKind tyVar)
          extend ty extra = (Map.insert (tvUnique tyVar) ty substitution, skolems <> extra)
      case Map.lookup (tvUnique tyVar) matched of
        Just ty -> pure (extend ty [])
        Nothing
          | isUniversal tyVar -> do
              meta <- freshMetaTvOfKind kind
              pure (extend meta [])
          | otherwise -> do
              skolem <- setTyVarKind kind <$> freshSkolemTv (tvName tyVar)
              pure (extend (TcTyVar skolem) [skolem])

constructorResultTyVars :: TcType -> Set.Set Unique
constructorResultTyVars = typeTyVars . constructorResultType

constructorResultType :: TcType -> TcType
constructorResultType (TcFunTy _ result) = constructorResultType result
constructorResultType result = result

-- | Reuse indices that the scrutinee determines through nominal structure.
-- Repeated indices retain the first match. The result constraint checks the rest.
-- A family application cannot determine its arguments.
matchConstructorResult :: Set.Set Unique -> TcType -> TcType -> TcM (Map.Map Unique TcType)
matchConstructorResult variables result scrutinee =
  case result of
    TcTyVar variable
      | tvUnique variable `Set.member` variables ->
          do
            kinds <-
              if Set.null (typeTyVars (tvKind variable) `Set.intersection` variables)
                then pure Map.empty
                else tcTypeKind scrutinee >>= matchConstructorResult variables (tvKind variable)
            pure (Map.insert (tvUnique variable) scrutinee kinds)
    _ -> do
      children <- decomposeNominalEquality result scrutinee
      case children of
        Nothing -> pure Map.empty
        Just pairs -> Map.unions <$> mapM (uncurry (matchConstructorResult variables)) pairs

typeTyVars :: TcType -> Set.Set Unique
typeTyVars ty =
  case ty of
    TcTyVar tyVar -> Set.insert (tvUnique tyVar) (typeTyVars (tvKind tyVar))
    TcMetaTv {} -> Set.empty
    TcArrowTy -> Set.empty
    TcTyLit {} -> Set.empty
    TcKindedTyCon _ kindArguments -> Set.unions (map typeTyVars kindArguments)
    TcTyConApp _ kindArguments arguments -> Set.unions (map typeTyVars (kindArguments <> arguments))
    TcFunTy argument result -> typeTyVars argument <> typeTyVars result
    TcForAllTy tyVar body -> Set.delete (tvUnique tyVar) (typeTyVars body)
    TcQualTy predicates body -> Set.unions (typeTyVars body : map predTyVars predicates)
    TcAppTy function argument -> typeTyVars function <> typeTyVars argument

predTyVars :: Pred -> Set.Set Unique
predTyVars predicate =
  case predicate of
    ClassPred _ arguments -> Set.unions (map typeTyVars arguments)
    EqPred left right -> typeTyVars left <> typeTyVars right
    IParamPred _ payload -> typeTyVars payload
    IrredPred constraint -> typeTyVars constraint
    QuantifiedPred variables antecedents consequent ->
      foldr
        (Set.delete . tvUnique)
        (Set.unions (predTyVars consequent : map predTyVars antecedents))
        variables

replaceConstructorSubpatterns :: Pattern -> [Pattern] -> Pattern
replaceConstructorSubpatterns pat subPats =
  case pat of
    PCon tc name typeArgs _ -> PCon tc name typeArgs subPats
    PInfix tc _ op _ ->
      case subPats of
        [lhs, rhs] -> PInfix tc lhs op rhs
        _ -> pat
    _ -> pat

splitConTy :: Int -> TcType -> TcM ([TcType], TcType)
splitConTy 0 ty = pure ([], ty)
splitConTy n (TcFunTy arg rest) = do
  (args, result) <- splitConTy (n - 1) rest
  pure (arg : args, result)
splitConTy n result = do
  missingArgs <- mapM (const freshMetaTv) [1 .. n]
  pure (missingArgs, result)

-- | The explicit type arguments of a constructor pattern, as the
-- @\@"name"@ of @Forall \@"name" x@.
patternTypeArguments :: Pattern -> [Type]
patternTypeArguments pat =
  case pat of
    PCon _ _ typeArguments _ -> typeArguments
    _ -> []

-- | Check the explicit type arguments of a constructor pattern against the
-- specified type variables of the constructor, in order. Each argument is
-- an equality with the type that instantiates its variable. As in a pattern
-- signature, a type variable that is not in scope is bound by the pattern,
-- and the equality is then solved at the pattern.
checkConTypeArguments :: Maybe SourceSpan -> Text -> [Type] -> [TcType] -> TcM ([PatternTyVar], [Ct])
checkConTypeArguments _ _ [] _ = pure ([], [])
checkConTypeArguments sp conName typeArguments instantiated
  | length typeArguments > length instantiated = do
      emitError sp (OtherError ("the constructor pattern " <> T.unpack conName <> " has more type arguments than specified type variables"))
      pure ([], [])
  | otherwise = do
      scoped <- getScopedTyVars
      let unbound = List.nub (concatMap (filter (`Map.notMember` scoped) . freeTypeVars) typeArguments)
      boundTyVars <- mapM bindPatternTyVar unbound
      withPatternTyVars boundTyVars $ do
        scoped' <- getScopedTyVars
        equalities <- mapM (checkTypeArgument scoped') (zip typeArguments instantiated)
        let solvedAt eqCt
              | null boundTyVars = pure False
              | otherwise = do
                  result <- solveEquality eqCt
                  pure (case result of EqSolved -> True; _ -> False)
        solved <- mapM solvedAt equalities
        pure (boundTyVars, [eqCt | (eqCt, False) <- zip equalities solved])
  where
    checkTypeArgument scoped (typeArgument, instantiatedTy) = do
      kind <- tcTypeKind =<< zonkType instantiatedTy
      argumentTy <- checkSurfaceType scoped typeArgument kind
      wantedEq sp instantiatedTy argumentTy

wantedEq :: Maybe SourceSpan -> TcType -> TcType -> TcM Ct
wantedEq sp left right = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred left right) ev (AppOrigin sp) sp)

withPatternBindings :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withPatternBindings [] action = action
withPatternBindings ((name, ty) : rest) action =
  extendResolvedTermEnv name (TcMonoIdBinder ty) (withPatternBindings rest action)

-- | Run the body of a match in the scope of its checked patterns: their
-- term binders, the type variables their signatures bound, and the givens
-- of their constructors. A nested match solves its branch inside this
-- scope, so it can use these givens too.
withPatternScope :: PatternCheck -> TcM a -> TcM a
withPatternScope patCheck action =
  withGivenPredicates (map ctPred (pcGivenCts patCheck)) $
    withPatternTyVars (pcTyVars patCheck) (withPatternBindings (pcBindings patCheck) action)

-- | Whether a pattern can fail to match. A variable, a wildcard, a lazy
-- pattern, and a tuple of such patterns cannot fail. A constructor pattern
-- cannot fail when its type has one constructor and its fields cannot
-- fail. A pattern synonym, a list, a literal, and an unboxed sum can fail.
-- A @do@ bind with a pattern that can fail calls @fail@ in its default
-- alternative.
patternCanFail :: Pattern -> TcM Bool
patternCanFail pat =
  case pat of
    PAnn _ inner -> patternCanFail inner
    PParen _ inner -> patternCanFail inner
    PAs _ _ inner -> patternCanFail inner
    PStrict _ inner -> patternCanFail inner
    PTypeSig _ inner _ -> patternCanFail inner
    PView _ _ inner -> patternCanFail inner
    PVar {} -> pure False
    PWildcard _ -> pure False
    PIrrefutable {} -> pure False
    PTypeBinder {} -> pure False
    PTypeSyntax {} -> pure False
    PTuple _ _ items -> anyCanFail items
    PBuiltinCon _ (BuiltinTuple _ _) _ items -> anyCanFail items
    PCon _ name _ subPats -> constructorCanFail name subPats
    PInfix _ lhs op rhs -> constructorCanFail op [lhs, rhs]
    PRecord _ name fields _ -> constructorCanFail name (map recordFieldValue fields)
    _ -> pure True
  where
    anyCanFail [] = pure False
    anyCanFail (item : items) = do
      itemCanFail <- patternCanFail item
      if itemCanFail then pure True else anyCanFail items

    constructorCanFail conSyntax subPats = do
      target <- resolvedTermTarget conSyntax
      mBinder <- lookupResolvedTerm (patternNameText conSyntax) target
      mPatSyn <- lookupPatSynTarget target
      case mBinder of
        Just (TcIdBinder (Scheme _ _ _ conTy) _)
          | Nothing <- mPatSyn,
            TcTyCon tyCon _ <- functionResultType conTy -> do
              mDataType <- lookupDataType tyCon
              case mDataType of
                Just info -> siblingsCanFail (dtiConstructors info) subPats
                Nothing -> do
                  -- A data instance constructor belongs to its data family
                  -- instance, whose constructors are its siblings.
                  familyInstances <- getDataFamilyInstances
                  let conName = patternNameText conSyntax
                      siblings =
                        [ dfiiConstructors familyInstance
                        | familyInstance <- familyInstances,
                          TcTyCon familyTyCon _ <- [dfiiFamilyType familyInstance],
                          familyTyCon == tyCon,
                          conName `elem` map dciName (dfiiConstructors familyInstance)
                        ]
                  case siblings of
                    [constructors] -> siblingsCanFail constructors subPats
                    _ -> pure True
        _ -> pure True

    siblingsCanFail constructors subPats =
      case constructors of
        [_] -> anyCanFail subPats
        _ -> pure True

    functionResultType ty =
      case ty of
        TcFunTy _ result -> functionResultType result
        _ -> ty

patternNameText :: Name -> Text
patternNameText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name

-- | Split the checked type of function equations into one type for each
-- argument pattern and a result type.
--
-- The checked type can show fewer arrows than the equations have patterns.
-- Its result is then equal to an arrow type, but it is not an arrow. An
-- example is an associated family application: with
-- @type D Unit = Unit -> Unit@, the method @f :: a -> D a@ can have the
-- equation @f u x = x@. The other arguments get fresh types, and a wanted
-- equality relates their arrow type to the result that the checked type
-- shows. The first equation gets the pending cast for the FC desugarer.
splitMatchType :: TcType -> [Match] -> TcM ([TcType], TcType, [Match], [Ct])
splitMatchType ty matches =
  case matches of
    first : rest
      | missing > 0 -> do
          hidden <- mapM (const freshMetaTv) [1 .. missing]
          result <- freshMetaTv
          evidence <- freshEvVar
          let arrow = foldr TcFunTy result hidden
              location = listToMaybe (mapMaybe annotationSpan (matchAnns first))
              wanted = mkWantedCt (EqPred arrow shownResult) evidence (AppOrigin location) location
              item = InfoMatchArityCast (PendingTcMatchArityCast arrow evidence)
          pure (shown <> hidden, result, first {matchTc = item : matchTc first} : rest, [wanted])
    _ -> pure (shown, shownResult, matches, [])
  where
    arity =
      case matches of
        first : _ -> length (matchPats first)
        [] -> 0
    (shown, shownResult) = peel arity ty
    missing = arity - length shown
    peel count (TcFunTy argument rest)
      | count > 0 =
          let (arguments, result) = peel (count - 1) rest
           in (argument : arguments, result)
    peel _ other = ([], other)
