{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Shared type-checking support for term patterns.
module Aihc.Tc.Generate.Pattern
  ( PatternCheck (..),
    PatternTyVar (..),
    annotatePatternBindings,
    reannotatePatternBinders,
    checkPattern,
    checkPatterns,
    checkFunctionPatterns,
    checkedPattern,
    patternBinderNames,
    withPatternBindings,
    withPatternScope,
  )
where

import Aihc.Parser.Syntax
  ( Annotation,
    BuiltinCon (..),
    Expr (..),
    FloatType (..),
    Literal (..),
    Name (..),
    NumericType (..),
    Pattern (..),
    RecordField (..),
    SourceSpan,
    TupleFlavor (..),
    Type,
    UnqualifiedName (..),
    fromAnnotation,
    mkAnnotation,
    nameText,
    peelLiteralAnn,
    peelPatternAnn,
  )
import Aihc.Resolve (Identifier (..), ResolutionAnnotation (..), ResolutionNamespace (..), ResolvedName (..))
import Aihc.Tc.Annotations (PendingTcAnnotation (..), TcAnnotation, pendingAnnotation)
import Aihc.Tc.Constraint
import Aihc.Tc.Env (PatSynInfo (..), TyConInfo (..))
import Aihc.Tc.Error (TcErrorKind (..))
import Aihc.Tc.Evidence (EvTerm (..))
import {-# SOURCE #-} Aihc.Tc.Generate.Expr (inferExprAt)
import Aihc.Tc.Generate.Record (lookupRecordHead, orderRecordFields)
import Aihc.Tc.Instantiate (Instantiation (..), instantiateWithArgs)
import Aihc.Tc.Kind (checkSurfaceType, freeTypeVars, freshKindMeta, runtimeRepOrLifted, tcTypeKind, unboxedSumType)
import Aihc.Tc.Monad
import Aihc.Tc.Solve.Decompose (decomposeNominalEquality)
import Aihc.Tc.Types
import Aihc.Tc.Zonk (zonkType)
import Control.Applicative ((<|>))
import Control.Monad (foldM, when)
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
    PVar name -> [name]
    PTypeBinder _ -> []
    PTypeSyntax _ _ -> []
    PWildcard -> []
    PLit _ -> []
    PQuasiQuote _ _ -> []
    PTuple _ items -> concatMap patternBinderNames items
    PUnboxedSum _ _ inner -> patternBinderNames inner
    PList items -> concatMap patternBinderNames items
    PCon _ _ pats -> concatMap patternBinderNames pats
    PBuiltinCon _ _ pats -> concatMap patternBinderNames pats
    PInfix lhs _ rhs -> patternBinderNames lhs <> patternBinderNames rhs
    PView _ inner -> patternBinderNames inner
    PAs name inner -> name : patternBinderNames inner
    PStrict inner -> patternBinderNames inner
    PIrrefutable inner -> patternBinderNames inner
    PNegLit _ -> []
    PParen inner -> patternBinderNames inner
    PRecord _ fields _ -> concatMap (patternBinderNames . recordFieldValue) fields
    PTypeSig inner _ -> patternBinderNames inner
    PSplice _ -> []

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
    PVar name -> unqualifiedNameText name
    PParen inner -> functionArgumentName inner
    PAs name _ -> unqualifiedNameText name
    PStrict inner -> functionArgumentName inner
    PIrrefutable inner -> functionArgumentName inner
    PTypeSig inner _ -> functionArgumentName inner
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

localBinderKey :: UnqualifiedName -> Maybe TcTermKey
localBinderKey name =
  case mapMaybe (fromAnnotation @ResolutionAnnotation) (unqualifiedNameAnns name) of
    resolution : _ | ResolvedLocal unique _ <- resolutionTarget resolution -> Just (TcTermLocal unique)
    _ -> Nothing

checkPattern :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPattern sp pat scrutTy = do
  check <- case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing -> checkPatternCore sp pat scrutTy
  pure check {pcPatterns = map (checkedPatternType sp scrutTy) (pcPatterns check)}

checkPatternWithoutResultType :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternWithoutResultType sp pat scrutTy =
  case literalPatternCheck sp pat scrutTy of
    Just literalCheck -> literalCheck
    Nothing ->
      case pat of
        PAnn ann inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
        PParen inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PParen (checkedPattern innerCheck)]}
        PStrict inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PStrict (checkedPattern innerCheck)]}
        PIrrefutable inner -> do
          innerCheck <- checkPatternWithoutResultType sp inner scrutTy
          pure innerCheck {pcPatterns = [PIrrefutable (checkedPattern innerCheck)]}
        _ -> checkPatternCore sp pat scrutTy

checkedPatternType :: Maybe SourceSpan -> TcType -> Pattern -> Pattern
checkedPatternType sp ty pat
  | patternUsesBinderAnnotation pat = pat
  | not (patternNeedsCheckedType pat) = pat
  | patternHasPendingType pat = pat
  | otherwise = annotatePendingPatternAt (patternOwnSpan pat <|> sp) (pendingAnnotation ty [] [] []) pat

annotatePendingPatternAt :: Maybe SourceSpan -> PendingTcAnnotation -> Pattern -> Pattern
annotatePendingPatternAt Nothing pending = PAnn (mkAnnotation pending)
annotatePendingPatternAt (Just sp) pending = PAnn (mkAnnotation sp) . PAnn (mkAnnotation pending)

patternUsesBinderAnnotation :: Pattern -> Bool
patternUsesBinderAnnotation pat =
  case pat of
    PAnn _ inner -> patternUsesBinderAnnotation inner
    PParen inner -> patternUsesBinderAnnotation inner
    PVar {} -> True
    PAs {} -> True
    PStrict inner -> patternUsesBinderAnnotation inner
    PIrrefutable inner -> patternUsesBinderAnnotation inner
    PTypeSig inner _ -> patternUsesBinderAnnotation inner
    _ -> False

patternNeedsCheckedType :: Pattern -> Bool
patternNeedsCheckedType pat =
  case pat of
    PAnn _ inner -> patternNeedsCheckedType inner
    PParen inner -> patternNeedsCheckedType inner
    PLit {} -> False
    PNegLit {} -> False
    PStrict inner -> patternNeedsCheckedType inner
    PIrrefutable inner -> patternNeedsCheckedType inner
    PTypeSig inner _ -> patternNeedsCheckedType inner
    _ -> True

patternHasPendingType :: Pattern -> Bool
patternHasPendingType pat =
  case pat of
    PAnn ann inner -> annotationHasType ann || patternHasPendingType inner
    PParen inner -> patternHasPendingType inner
    PLit literal -> literalHasPendingType literal
    PStrict inner -> patternHasPendingType inner
    PIrrefutable inner -> patternHasPendingType inner
    PTypeSig inner _ -> patternHasPendingType inner
    _ -> False

literalHasPendingType :: Literal -> Bool
literalHasPendingType literal =
  case literal of
    LitAnn ann inner -> annotationHasType ann || literalHasPendingType inner
    _ -> False

annotationIsPending :: Annotation -> Bool
annotationIsPending ann =
  case fromAnnotation ann :: Maybe PendingTcAnnotation of
    Just _ -> True
    Nothing -> False

annotationHasType :: Annotation -> Bool
annotationHasType ann =
  annotationIsPending ann
    || case fromAnnotation ann :: Maybe TcAnnotation of
      Just _ -> True
      Nothing -> False

sourceSpanFromAnnotations :: [Annotation] -> Maybe SourceSpan
sourceSpanFromAnnotations = listToMaybe . mapMaybe (fromAnnotation @SourceSpan)

patternOwnSpan :: Pattern -> Maybe SourceSpan
patternOwnSpan pat =
  case pat of
    PAnn ann inner -> fromAnnotation @SourceSpan ann <|> patternOwnSpan inner
    PVar name -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PParen inner -> patternOwnSpan inner
    PAs name _ -> sourceSpanFromAnnotations (unqualifiedNameAnns name)
    PStrict inner -> patternOwnSpan inner
    PIrrefutable inner -> patternOwnSpan inner
    PCon name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PInfix _ name _ -> sourceSpanFromAnnotations (nameAnns name)
    PRecord name _ _ -> sourceSpanFromAnnotations (nameAnns name)
    PTypeSig inner _ -> patternOwnSpan inner
    PView expr inner -> viewExprSpan expr <|> patternOwnSpan inner
    _ -> Nothing

-- | The span of a view pattern function. The parser gives spans to names
-- and to annotated expressions only.
viewExprSpan :: Expr -> Maybe SourceSpan
viewExprSpan expr =
  case expr of
    EAnn ann inner -> fromAnnotation @SourceSpan ann <|> viewExprSpan inner
    EVar name -> sourceSpanFromAnnotations (nameAnns name)
    EParen inner -> viewExprSpan inner
    EPragma _ inner -> viewExprSpan inner
    EApp function _ -> viewExprSpan function
    _ -> Nothing

checkPatternCore :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPatternCore sp pat scrutTy =
  case pat of
    PVar name ->
      pure (checkedOnly pat) {pcBindings = [(name, scrutTy)]}
    PAnn ann inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PAnn ann (checkedPattern innerCheck)]}
    PParen inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PParen (checkedPattern innerCheck)]}
    PWildcard {} -> pure (checkedOnly pat)
    PLit lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern is missing its resolver type annotation"
      | otherwise -> do
          maybeLiteralTy <- charLiteralPatternType lit
          case maybeLiteralTy of
            Just literalTy -> do
              eqCt <- wantedEq sp scrutTy literalTy
              pure (checkedOnly (PLit (checkedLiteral scrutTy lit))) {pcWantedCts = [eqCt]}
            Nothing -> pure (checkedOnly (PLit (checkedLiteral scrutTy lit)))
    PNegLit lit
      | isPrimitiveLiteral lit ->
          abortTc "primitive literal pattern is missing its resolver type annotation"
      | otherwise -> pure (checkedOnly pat)
    PAs name inner -> do
      let innerSpan = patternOwnSpan inner <|> sp
      innerCheck <- checkPatternWithoutResultType innerSpan inner scrutTy
      pure innerCheck {pcBindings = (name, scrutTy) : pcBindings innerCheck, pcPatterns = [PAs name (checkedPattern innerCheck)]}
    PStrict inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PStrict (checkedPattern innerCheck)]}
    PTypeSig inner tyAnn -> checkTypeSigPattern sp inner tyAnn scrutTy
    PIrrefutable inner -> do
      innerCheck <- checkPattern sp inner scrutTy
      pure innerCheck {pcPatterns = [PIrrefutable (checkedPattern innerCheck)]}
    PCon name _typeArgs subPats ->
      checkConPattern sp pat name subPats scrutTy
    PInfix lhs op rhs ->
      checkConPattern sp pat op [lhs, rhs] scrutTy
    PRecord name fields wildcard -> do
      when wildcard $
        abortTc ("record wildcard patterns are not supported at " <> show (patternOwnSpan pat <|> sp))
      head' <- lookupRecordHead name
      subPats <- orderRecordFields (patternOwnSpan pat <|> sp) head' fields (\_ -> pure PWildcard)
      checkConPattern sp (PCon name [] subPats) name subPats scrutTy
    PList items -> checkListPattern sp items scrutTy
    PView viewExpr inner -> do
      let viewSpan = viewExprSpan viewExpr <|> sp
      (viewExpr', viewTy, viewCts) <- inferExprAt viewSpan viewExpr
      innerTy <- freshMetaTv
      eqCt <- wantedEq viewSpan viewTy (TcFunTy scrutTy innerTy)
      innerCheck <- checkPattern sp inner innerTy
      pure
        innerCheck
          { pcWantedCts = eqCt : viewCts <> pcWantedCts innerCheck,
            pcPatterns = [PView viewExpr' (checkedPattern innerCheck)]
          }
    PTuple flavor items -> checkTuplePattern sp flavor items scrutTy
    -- A prefix tuple constructor, @(,) a b@, checks like the @(a, b)@ form.
    -- The type arguments follow 'PCon', which ignores them.
    PBuiltinCon (BuiltinTuple flavor arity) _typeArgs items
      | length items == arity ->
          checkTuplePattern sp flavor items scrutTy
    PUnboxedSum alternative arity inner -> do
      types <- mapM (const freshMetaTv) [1 .. arity]
      sumType <- unboxedSumType types
      equality <- wantedEq sp scrutTy sumType
      case drop alternative types of
        innerType : _ -> do
          checked <- checkPattern sp inner innerType
          pure
            checked
              { pcWantedCts = equality : pcWantedCts checked,
                pcPatterns = [PUnboxedSum alternative arity (checkedPattern checked)]
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
checkTypeSigPattern :: Maybe SourceSpan -> Pattern -> Type -> TcType -> TcM PatternCheck
checkTypeSigPattern sp inner tyAnn scrutTy = do
  kinds <- getKinds
  scoped <- getScopedTyVars
  boundTyVars <- mapM bindPatternTyVar (filter (`Map.notMember` scoped) (freeTypeVars tyAnn))
  withPatternTyVars boundTyVars $ do
    scoped' <- getScopedTyVars
    sigTy <- checkSurfaceType scoped' tyAnn (typeKind kinds)
    eqCt <- wantedEq sp scrutTy sigTy
    innerCheck <- checkPattern sp inner sigTy
    pure
      innerCheck
        { pcWantedCts = eqCt : pcWantedCts innerCheck,
          pcTyVars = boundTyVars <> pcTyVars innerCheck,
          pcPatterns = [PTypeSig (checkedPattern innerCheck) tyAnn]
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
  kinds <- getKinds
  elemTys <- mapM (const freshMetaTv) items
  let arity = length items
  wired <- wiredTupleTyCon flavor arity
  elementKinds <- mapM tcTypeKind elemTys
  let fallbackKind =
        case flavor of
          Boxed -> foldr KFun (typeKind kinds) elementKinds
          Unboxed -> foldr KFun (mkTYPEKind kinds (tupleRep kinds (map (runtimeRepOrLifted kinds) elementKinds))) elementKinds
  -- The wiring gives the full identity of the tuple type constructor.
  -- A bare name lookup can find a different constructor with the same name.
  tupleTyCon <- mkWiredTyCon wired fallbackKind
  let tupleTy = TcTyCon tupleTyCon elemTys
  eqCt <- wantedEq sp scrutTy tupleTy
  itemChecks <- checkPatterns sp (zip items elemTys)
  pure itemChecks {pcWantedCts = eqCt : pcWantedCts itemChecks, pcPatterns = [PTuple flavor (pcPatterns itemChecks)]}

checkedOnly :: Pattern -> PatternCheck
checkedOnly pat = mempty {pcPatterns = [pat]}

checkedLiteral :: TcType -> Literal -> Literal
checkedLiteral ty = LitAnn (mkAnnotation (pendingAnnotation ty [] [] []))

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
            pcPatterns = [PList []]
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
                PAnn _ (PList patterns) -> patterns
                PList patterns -> patterns
                _ -> rest
              checkedItems = checkedPattern itemCheck : checkedTailItems
          pure
            nestedCheck
              { pcWantedCts = [scrutCt] <> pcWantedCts nestedCheck,
                pcGivenCts = predicateGivens <> pcGivenCts nestedCheck,
                pcSkolems = skolems <> pcSkolems nestedCheck,
                pcPatterns = [PList checkedItems]
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

charLiteralPatternType :: Literal -> TcM (Maybe TcType)
charLiteralPatternType literal =
  case peelLiteralAnn literal of
    LitChar {} -> Just <$> charType
    _ -> pure Nothing

-- | The check of a literal pattern that needs its resolver annotations.
--
-- An overloaded integer pattern uses the resolved syntax terms.
-- A string pattern has them only under OverloadedStrings.
-- A primitive literal pattern uses the resolved primitive type.
literalPatternCheck :: Maybe SourceSpan -> Pattern -> TcType -> Maybe (TcM PatternCheck)
literalPatternCheck sp pat scrutTy =
  case patternLiteral pat of
    Just (isNegative, lit)
      | isOverloadedIntegerLiteral lit -> Just (checkOverloadedIntegerPattern sp pat isNegative scrutTy)
      | isOverloadedFractionalLiteral lit -> Just (checkOverloadedLiteralPattern sp pat "fromRational" Nothing isNegative scrutTy)
      | isStringLiteral lit,
        hasPatternSyntaxTerm "fromString" pat ->
          Just (checkOverloadedLiteralPattern sp pat "fromString" Nothing isNegative scrutTy)
      | isPrimitiveLiteral lit -> Just (checkPrimitiveLiteralPattern sp pat scrutTy)
    _ -> Nothing

-- | The literal of a literal pattern, with a flag for a negated literal.
patternLiteral :: Pattern -> Maybe (Bool, Literal)
patternLiteral pat =
  case peelPatternAnn pat of
    PLit lit -> Just (False, lit)
    PNegLit lit -> Just (True, lit)
    _ -> Nothing

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
-- The resolver annotates the pattern with the primitive type of the literal.
-- The pattern type is that primitive type, so the scrutinee must equal it.
checkPrimitiveLiteralPattern :: Maybe SourceSpan -> Pattern -> TcType -> TcM PatternCheck
checkPrimitiveLiteralPattern sp pat scrutTy = do
  resolution <- requiredPrimitiveLiteralResolution pat
  maybeInfo <- lookupResolvedTypeSyntax resolution
  info <-
    maybe
      (abortTc ("resolved primitive literal type missing from type environment: " <> show (resolutionTarget resolution)))
      pure
      maybeInfo
  let literalTy = TcTyCon (tciTyCon info) []
  eqCt <- wantedEq sp scrutTy literalTy
  pure (checkedOnly (checkedLiteralPattern scrutTy pat)) {pcWantedCts = [eqCt]}

-- | The Integer type that the resolver gives an overloaded integer pattern.
--
-- The result is 'Nothing' when the built-in scope does not give the type.
resolvedIntegerPatternType :: Pattern -> TcM (Maybe TcType)
resolvedIntegerPatternType pat =
  case [resolution | resolution <- patternResolutions pat, resolutionNamespace resolution == ResolutionNamespaceType] of
    resolution : _ -> fmap (\info -> TcTyCon (tciTyCon info) []) <$> lookupResolvedTypeSyntax resolution
    [] -> pure Nothing

requiredPrimitiveLiteralResolution :: Pattern -> TcM ResolutionAnnotation
requiredPrimitiveLiteralResolution pat =
  case [resolution | resolution <- patternResolutions pat, resolutionNamespace resolution == ResolutionNamespaceType] of
    resolution : _ -> pure resolution
    [] -> abortTc "primitive literal pattern is missing its resolver type annotation"

-- | Attach the checked type to the literal inside a literal pattern.
checkedLiteralPattern :: TcType -> Pattern -> Pattern
checkedLiteralPattern ty pat =
  case pat of
    PAnn ann inner -> PAnn ann (checkedLiteralPattern ty inner)
    PParen inner -> PParen (checkedLiteralPattern ty inner)
    PStrict inner -> PStrict (checkedLiteralPattern ty inner)
    PIrrefutable inner -> PIrrefutable (checkedLiteralPattern ty inner)
    PLit lit -> PLit (checkedLiteral ty lit)
    PNegLit lit -> PNegLit (checkedLiteral ty lit)
    _ -> pat

isOverloadedIntegerLiteral :: Literal -> Bool
isOverloadedIntegerLiteral lit =
  case peelLiteralAnn lit of
    LitInt _ TInteger _ -> True
    _ -> False

isOverloadedFractionalLiteral :: Literal -> Bool
isOverloadedFractionalLiteral lit =
  case peelLiteralAnn lit of
    LitFloat _ TFractional _ -> True
    _ -> False

isStringLiteral :: Literal -> Bool
isStringLiteral lit =
  case peelLiteralAnn lit of
    LitString {} -> True
    _ -> False

-- | Whether the resolver gave the pattern the named syntax term.
--
-- A string pattern gets fromString only under OverloadedStrings.
hasPatternSyntaxTerm :: Text -> Pattern -> Bool
hasPatternSyntaxTerm name pat =
  any isTerm (patternResolutions pat)
  where
    isTerm resolution =
      resolutionNamespace resolution == ResolutionNamespaceTerm
        && resolutionIdentifier resolution == IdentifierNamed name

checkOverloadedIntegerPattern :: Maybe SourceSpan -> Pattern -> Bool -> TcType -> TcM PatternCheck
checkOverloadedIntegerPattern sp pat isNegative scrutTy = do
  integerTy <- resolvedIntegerPatternType pat
  checkOverloadedLiteralPattern sp pat "fromInteger" integerTy isNegative scrutTy

-- | Check a literal pattern that a class method converts and equality tests.
--
-- @literalTy@ is the resolved type of the literal, when the resolver gives
-- it. The argument type of the conversion method must then equal it.
checkOverloadedLiteralPattern :: Maybe SourceSpan -> Pattern -> Text -> Maybe TcType -> Bool -> TcType -> TcM PatternCheck
checkOverloadedLiteralPattern sp pat conversion literalTy isNegative scrutTy = do
  (conversionPending, conversionCts) <-
    checkPatternMethodWithExpected sp pat conversion $ \case
      TcFunTy argumentTy _ -> pure (scrutTy, TcFunTy (fromMaybe argumentTy literalTy) scrutTy)
      _ -> abortTc (T.unpack conversion <> " does not have a function type")
  negateCheck <-
    if isNegative
      then Just <$> checkPatternMethod sp pat "negate" scrutTy (TcFunTy scrutTy scrutTy)
      else pure Nothing
  (eqPending, eqCts) <-
    checkPatternMethodWithExpected sp pat "==" $ \case
      TcFunTy _ (TcFunTy _ boolTy) ->
        let expectedTy = TcFunTy scrutTy (TcFunTy scrutTy boolTy)
         in pure (expectedTy, expectedTy)
      _ -> abortTc "== does not have a binary function type"
  let methodAnnotations =
        [(conversion, conversionPending)]
          <> maybe [] (\(pending, _) -> [("negate", pending)]) negateCheck
          <> [("==", eqPending)]
      pat' = foldr (uncurry attachPendingPatternAnnotation) pat methodAnnotations
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

checkPatternMethod :: Maybe SourceSpan -> Pattern -> Text -> TcType -> TcType -> TcM (PendingTcAnnotation, [Ct])
checkPatternMethod sp pat name annotationTy expectedTy =
  checkPatternMethodWithExpected sp pat name (const (pure (annotationTy, expectedTy)))

checkPatternMethodWithExpected :: Maybe SourceSpan -> Pattern -> Text -> (TcType -> TcM (TcType, TcType)) -> TcM (PendingTcAnnotation, [Ct])
checkPatternMethodWithExpected sp pat name expectedTypes = do
  resolution <- requiredPatternResolution name pat
  (actualTy, typeArgs, methodCts) <- inferResolvedPatternMethod sp name resolution
  (annotationTy, expectedTy) <- expectedTypes actualTy
  methodEq <- wantedMethodEq sp name actualTy expectedTy
  pure
    ( pendingAnnotation
        annotationTy
        typeArgs
        (map ctEvVar methodCts)
        [],
      methodCts <> [methodEq]
    )

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

inferResolvedPatternMethod :: Maybe SourceSpan -> Text -> ResolutionAnnotation -> TcM (TcType, [TcType], [Ct])
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

requiredPatternResolution :: Text -> Pattern -> TcM ResolutionAnnotation
requiredPatternResolution name pat =
  case [resolution | resolution <- patternResolutions pat, resolutionIdentifier resolution == IdentifierNamed name, resolutionNamespace resolution == ResolutionNamespaceTerm] of
    resolution : _ -> pure resolution
    [] -> do
      emitError Nothing (OtherError ("missing resolver annotation for overloaded pattern method " <> T.unpack name))
      abortTc ("missing resolver annotation for overloaded pattern method " <> T.unpack name)

patternResolutions :: Pattern -> [ResolutionAnnotation]
patternResolutions pat =
  case pat of
    PAnn ann inner -> mapMaybe fromAnnotation [ann] <> patternResolutions inner
    PParen inner -> patternResolutions inner
    PStrict inner -> patternResolutions inner
    PIrrefutable inner -> patternResolutions inner
    PAs _ inner -> patternResolutions inner
    PTypeSig inner _ -> patternResolutions inner
    _ -> []

attachPendingPatternAnnotation :: Text -> PendingTcAnnotation -> Pattern -> Pattern
attachPendingPatternAnnotation target pending pat =
  case pat of
    PAnn ann inner ->
      case fromAnnotation ann of
        Just resolution
          | resolutionIdentifier resolution == IdentifierNamed target,
            resolutionNamespace resolution == ResolutionNamespaceTerm ->
              PAnn (mkAnnotation pending) (PAnn ann inner)
        _ -> PAnn ann (attachPendingPatternAnnotation target pending inner)
    PParen inner -> PParen (attachPendingPatternAnnotation target pending inner)
    PStrict inner -> PStrict (attachPendingPatternAnnotation target pending inner)
    PIrrefutable inner -> PIrrefutable (attachPendingPatternAnnotation target pending inner)
    PAs name inner -> PAs name (attachPendingPatternAnnotation target pending inner)
    PTypeSig inner ty -> PTypeSig (attachPendingPatternAnnotation target pending inner) ty
    _ -> pat

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
            { unqualifiedNameAnns =
                [ann | ann <- unqualifiedNameAnns name, not (annotationIsPending ann)] <> [mkAnnotation pending]
            }

-- | Apply a function to every named binder of a pattern.
mapPatternBinderNames :: (UnqualifiedName -> UnqualifiedName) -> Pattern -> Pattern
mapPatternBinderNames annotateBinder =
  go
  where
    go pat =
      case pat of
        PAnn ann inner -> PAnn ann (go inner)
        PVar name -> PVar (annotateBinder name)
        PParen inner -> PParen (go inner)
        PAs name inner -> PAs (annotateBinder name) (go inner)
        PStrict inner -> PStrict (go inner)
        PIrrefutable inner -> PIrrefutable (go inner)
        PList items -> PList (map go items)
        PTuple flavor items -> PTuple flavor (map go items)
        PUnboxedSum alt arity inner -> PUnboxedSum alt arity (go inner)
        PInfix lhs op rhs -> PInfix (go lhs) op (go rhs)
        PView expr inner -> PView expr (go inner)
        PCon name typeArgs subPats -> PCon name typeArgs (map go subPats)
        PRecord name fields wildcard -> PRecord name (map annotateRecordField fields) wildcard
        PTypeSig inner type' -> PTypeSig (go inner) type'
        PSplice expr -> PSplice expr
        _ -> pat

    annotateRecordField :: RecordField Pattern -> RecordField Pattern
    annotateRecordField field =
      field {recordFieldValue = go (recordFieldValue field)}

annotateBinderName :: [(UnqualifiedName, TcType)] -> UnqualifiedName -> UnqualifiedName
annotateBinderName bindings name =
  case lookup name bindings of
    Nothing -> name
    Just ty
      | any annotationIsPending (unqualifiedNameAnns name) -> name
      | otherwise -> name {unqualifiedNameAnns = unqualifiedNameAnns name <> [mkAnnotation (pendingAnnotation ty [] [] [])]}

checkConPattern :: Maybe SourceSpan -> Pattern -> Name -> [Pattern] -> TcType -> TcM PatternCheck
checkConPattern sp originalPat conSyntax subPats scrutTy = do
  let conName = patternNameText conSyntax
  target <- resolvedTermTarget conSyntax
  mBinder <- lookupResolvedTerm conName target
  mPatSyn <- lookupPatSynTarget target
  case mBinder of
    Just (TcIdBinder scheme _)
      | Just info <- mPatSyn ->
          checkPatSynPattern sp originalPat conName info scheme subPats scrutTy
    Just (TcIdBinder scheme _) -> do
      (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPattern scrutTy scheme
      (argTys, conResTy) <- splitConTy (length subPats) conTy
      scrutCt <- wantedEq sp scrutTy conResTy
      subCheck <- checkPatterns sp (zip subPats argTys)
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
          { pcWantedCts = [scrutCt] <> pcWantedCts subCheck,
            pcGivenCts = predicateGivens <> pcGivenCts subCheck,
            pcSkolems = skolems <> pcSkolems subCheck,
            pcPatterns = [annotatedPattern]
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
  (conTy, typeArgs, predicates, skolems) <- instantiateConstructorPattern scrutTy scheme
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
        ctLoc = sp
      }

instantiateConstructorPattern :: TcType -> TypeScheme -> TcM (TcType, [TcType], [Pred], [TyVarId])
instantiateConstructorPattern scrutTy (ForAll tyVars predicates body) = do
  matched <- matchConstructorResult (Set.fromList (map tvUnique tyVars)) (constructorResultType body) =<< zonkType scrutTy
  let resultTyVars = constructorResultTyVars body
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
    TcTyCon _ arguments -> Set.unions (map typeTyVars arguments)
    TcKindedTyCon _ kindArguments -> Set.unions (map typeTyVars kindArguments)
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
    PCon name typeArgs _ -> PCon name typeArgs subPats
    PInfix _ op _ ->
      case subPats of
        [lhs, rhs] -> PInfix lhs op rhs
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

wantedEq :: Maybe SourceSpan -> TcType -> TcType -> TcM Ct
wantedEq sp left right = do
  ev <- freshEvVar
  pure (mkWantedCt (EqPred left right) ev (AppOrigin sp) sp)

withPatternBindings :: [(UnqualifiedName, TcType)] -> TcM a -> TcM a
withPatternBindings [] action = action
withPatternBindings ((name, ty) : rest) action =
  extendResolvedTermEnv name (TcMonoIdBinder ty) (withPatternBindings rest action)

-- | Run the body of a match in the scope of its checked patterns: their
-- term binders and the type variables their signatures bound.
withPatternScope :: PatternCheck -> TcM a -> TcM a
withPatternScope patCheck action =
  withPatternTyVars (pcTyVars patCheck) (withPatternBindings (pcBindings patCheck) action)

patternNameText :: Name -> Text
patternNameText name =
  case nameQualifier name of
    Nothing -> nameText name
    Just qualifier -> qualifier <> "." <> nameText name
