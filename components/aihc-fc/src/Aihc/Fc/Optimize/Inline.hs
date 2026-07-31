-- | Binding-aware candidate generation for equality saturation.
--
-- System FC is lazy, so ordinary substitution is not a valid general-purpose
-- inliner: it can duplicate work or turn a shared thunk into several thunks.
-- This module performs call-by-need beta reduction. Non-trivial arguments used
-- more than once remain let-bound, and unlifted arguments remain case-bound so
-- their evaluation point is not moved.
module Aihc.Fc.Optimize.Inline
  ( inlineCandidatesProgram,
  )
where

import Aihc.Fc.Optimize.EGraph (selectSmallest)
import Aihc.Fc.Syntax
import Aihc.Tc.Evidence (Coercion (..), EvVar (..))
import Aihc.Tc.Types
  ( Pred (..),
    TcType (..),
    TyCon (tyConName),
    TyVarId (..),
    Unique (..),
    isUnliftedType,
    setTyVarKind,
    tvKind,
  )
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as Text

data InlineEnv = InlineEnv
  { inlineFunctions :: !(Map Text FcExpr),
    staticValues :: !(Map Text FcExpr),
    constructorArities :: !(Map Text Int)
  }

type VarKey = (Unique, Text)

type TyVarKey = (Unique, Text)

type TypeSubstitution = Map TyVarKey TcType

data CallArgument
  = TypeArgument !TcType
  | TermArgument !FcExpr

-- | Generate a binding-safe alternative, add it to a per-expression e-graph,
-- and retain the lowest lowered-size representation.
inlineCandidatesProgram :: FcProgram -> FcProgram
inlineCandidatesProgram program@(FcProgram topBinds) =
  FcProgram (evalState (traverse optimizeTopBind topBinds) (nextUnique program))
  where
    environment = buildInlineEnv program

    optimizeTopBind topBind =
      case topBind of
        FcTopBind bind -> FcTopBind <$> optimizeBind environment bind
        _ -> pure topBind

optimizeBind :: InlineEnv -> FcBind -> State Int FcBind
optimizeBind environment bind =
  case bind of
    FcNonRec binder rhs -> FcNonRec binder <$> optimizeExpr environment rhs
    FcRec bindings ->
      FcRec <$> traverse (\(binder, rhs) -> (binder,) <$> optimizeExpr environment rhs) bindings

optimizeExpr :: InlineEnv -> FcExpr -> State Int FcExpr
optimizeExpr environment original = do
  candidate <- normalizeExpr environment 48 original
  pure
    ( if candidate == original
        then original
        else selectSmallest callCosts original [candidate]
    )
  where
    -- A direct call carries the generated body behind it. Charging a small
    -- multiple of that body lets extraction recognize when unfolding exposes
    -- enough beta/case reduction to be smaller than the opaque call path.
    callCosts = Map.map ((8 *) . expressionSize) (inlineFunctions environment)

buildInlineEnv :: FcProgram -> InlineEnv
buildInlineEnv (FcProgram topBinds) =
  InlineEnv
    { inlineFunctions =
        Map.filterWithKey
          (\name rhs -> name `Set.member` acyclic && isDirectFunction rhs && expressionSize rhs <= inlineThreshold)
          values,
      staticValues = values,
      constructorArities = constructors
    }
  where
    values =
      Map.fromList
        [ (varName binder, rhs)
        | FcTopBind (FcNonRec binder rhs) <- topBinds
        ]
    topLevelVariables = Map.keysSet values
    dependencyNodes =
      [ ( (key, rhs),
          key,
          Set.toList (freeVariableKeys rhs `Set.intersection` topLevelVariables)
        )
      | (key, rhs) <- Map.toList values
      ]
    acyclic =
      Set.fromList
        [ key
        | AcyclicSCC (key, _) <- stronglyConnComp dependencyNodes
        ]
    constructors =
      Map.fromList
        [ (constructor, length fields)
        | FcData _ _ declarations <- topBinds,
          (constructor, fields) <- declarations
        ]

-- Large functions are left opaque in this first version. The important
-- library combinators and dictionary selectors are comfortably below this
-- limit, while the limit prevents accidental whole-program code explosion.
inlineThreshold :: Int
inlineThreshold = 80

isDirectFunction :: FcExpr -> Bool
isDirectFunction expression =
  case expression of
    FcTyLam _ body -> isDirectFunction body
    FcLam {} -> True
    FcCast body _ -> isDirectFunction body
    _ -> False

expressionSize :: FcExpr -> Int
expressionSize expression =
  1 + case expression of
    FcVar {} -> 0
    FcLit {} -> 0
    FcApp function argument -> expressionSize function + expressionSize argument
    FcTyApp function _ -> expressionSize function
    FcLam _ body -> expressionSize body
    FcTyLam _ body -> expressionSize body
    FcLet (FcNonRec _ rhs) body -> expressionSize rhs + expressionSize body
    FcLet (FcRec bindings) body -> sum (map (expressionSize . snd) bindings) + expressionSize body
    FcCase scrutinee _ alternatives -> expressionSize scrutinee + sum (map (expressionSize . altRhs) alternatives)
    FcCast body _ -> expressionSize body
    FcCallForeign _ arguments -> sum (map expressionSize arguments)

normalizeExpr :: InlineEnv -> Int -> FcExpr -> State Int FcExpr
normalizeExpr _ fuel expression | fuel <= 0 = pure expression
normalizeExpr environment fuel expression = do
  children <- normalizeChildren environment (fuel - 1) expression
  proposed <- rewriteRoot environment children
  let typePreserving = preservesExpressionType children proposed
      rewritten
        | typePreserving = proposed
        | otherwise = children
  if rewritten == children
    then pure children
    else normalizeExpr environment (fuel - 1) rewritten

-- Every rewrite is an equality only at one result type. Rejecting a candidate
-- locally is safer than allowing a malformed unboxed value to escape into a
-- lifted context and relying on a later lint pass to find it.
preservesExpressionType :: FcExpr -> FcExpr -> Bool
preservesExpressionType before after =
  case (expressionType before, expressionType after) of
    (Just beforeType, Just afterType) ->
      sameTypeShape beforeType afterType
        || (returnsIo beforeType && returnsIo afterType)
    _ -> True

-- IO's linked dictionary currently contains method annotations whose rigid
-- identities do not agree with the selector's annotations. Both forms erase
-- to lifted values, and the concrete result remains IO, so permit that narrow
-- discrepancy to expose the state-transformer implementation of bind.
returnsIo :: TcType -> Bool
returnsIo ty =
  case ty of
    TcFunTy _ result -> returnsIo result
    TcForAllTy _ body -> returnsIo body
    TcQualTy _ body -> returnsIo body
    TcTyCon tyCon _ -> tyConName tyCon == Text.pack "IO"
    TcAppTy function _ -> returnsIoHead function
    _ -> False
  where
    returnsIoHead function =
      case function of
        TcTyCon tyCon _ -> tyConName tyCon == Text.pack "IO"
        TcAppTy inner _ -> returnsIoHead inner
        _ -> False

-- Linked units currently allocate rigid variables independently, so their
-- identities cannot be compared directly here. Shape equality distinguishes
-- concrete lifted and unlifted types while accepting alpha-renamed
-- polymorphic library definitions.
sameTypeShape :: TcType -> TcType -> Bool
sameTypeShape left right =
  case (left, right) of
    (TcTyVar leftVar, TcTyVar rightVar) -> tvKind leftVar == tvKind rightVar
    (TcMetaTv leftUnique, TcMetaTv rightUnique) -> leftUnique == rightUnique
    (TcTyCon leftCon leftArguments, TcTyCon rightCon rightArguments) ->
      leftCon == rightCon
        && length leftArguments == length rightArguments
        && and (zipWith sameTypeShape leftArguments rightArguments)
    (TcFunTy leftArgument leftResult, TcFunTy rightArgument rightResult) ->
      sameTypeShape leftArgument rightArgument && sameTypeShape leftResult rightResult
    (TcForAllTy leftVar leftBody, TcForAllTy rightVar rightBody) ->
      tvKind leftVar == tvKind rightVar && sameTypeShape leftBody rightBody
    (TcQualTy leftPredicates leftBody, TcQualTy rightPredicates rightBody) ->
      length leftPredicates == length rightPredicates
        && and (zipWith samePredicateShape leftPredicates rightPredicates)
        && sameTypeShape leftBody rightBody
    (TcAppTy (TcTyCon leftCon leftArguments) leftArgument, TcTyCon rightCon rightArguments) ->
      sameTypeShape
        (TcTyCon leftCon (leftArguments <> [leftArgument]))
        (TcTyCon rightCon rightArguments)
    (TcTyCon leftCon leftArguments, TcAppTy (TcTyCon rightCon rightArguments) rightArgument) ->
      sameTypeShape
        (TcTyCon leftCon leftArguments)
        (TcTyCon rightCon (rightArguments <> [rightArgument]))
    (TcAppTy leftFunction leftArgument, TcAppTy rightFunction rightArgument) ->
      sameTypeShape leftFunction rightFunction && sameTypeShape leftArgument rightArgument
    _ -> False

samePredicateShape :: Pred -> Pred -> Bool
samePredicateShape left right =
  case (left, right) of
    (ClassPred leftClass leftArguments, ClassPred rightClass rightArguments) ->
      leftClass == rightClass
        && length leftArguments == length rightArguments
        && and (zipWith sameTypeShape leftArguments rightArguments)
    (EqPred leftFirst leftSecond, EqPred rightFirst rightSecond) ->
      sameTypeShape leftFirst rightFirst && sameTypeShape leftSecond rightSecond
    _ -> False

expressionType :: FcExpr -> Maybe TcType
expressionType expression =
  case expression of
    FcVar var -> Just (varType var)
    FcLit literal -> literalType literal
    FcApp function _ -> do
      functionType <- expressionType function
      case functionType of
        TcFunTy _ resultType -> Just resultType
        _ -> Nothing
    FcTyApp function argument -> do
      functionType <- expressionType function
      case functionType of
        TcForAllTy tyVar body ->
          Just (substituteType (Map.singleton (tyVarKey tyVar) argument) body)
        _ -> Nothing
    FcLam binder body -> TcFunTy (varType binder) <$> expressionType body
    FcTyLam tyVar body -> TcForAllTy tyVar <$> expressionType body
    FcLet _ body -> expressionType body
    FcCase _ _ alternatives ->
      case alternatives of
        alternative : _ -> expressionType (altRhs alternative)
        [] -> Nothing
    FcCast body (Refl _) -> expressionType body
    FcCast {} -> Nothing
    FcCallForeign foreignCall _ ->
      Just (fcForeignCallResultType (fcForeignCallSignature foreignCall))

normalizeChildren :: InlineEnv -> Int -> FcExpr -> State Int FcExpr
normalizeChildren environment fuel expression =
  case expression of
    FcVar {} -> pure expression
    FcLit {} -> pure expression
    FcApp function argument ->
      FcApp <$> normalizeExpr environment fuel function <*> normalizeExpr environment fuel argument
    FcTyApp function ty -> FcTyApp <$> normalizeExpr environment fuel function <*> pure ty
    FcLam binder body -> FcLam binder <$> normalizeExpr environment fuel body
    FcTyLam tyVar body -> FcTyLam tyVar <$> normalizeExpr environment fuel body
    FcLet bind body -> FcLet <$> normalizeBind environment fuel bind <*> normalizeExpr environment fuel body
    FcCase scrutinee binder alternatives ->
      FcCase
        <$> normalizeExpr environment fuel scrutinee
        <*> pure binder
        <*> traverse normalizeAlternative alternatives
    FcCast body coercion -> FcCast <$> normalizeExpr environment fuel body <*> pure coercion
    FcCallForeign foreignCall arguments ->
      FcCallForeign foreignCall <$> traverse (normalizeExpr environment fuel) arguments
  where
    normalizeAlternative alternative =
      (\rhs -> alternative {altRhs = rhs})
        <$> normalizeExpr environment fuel (altRhs alternative)

normalizeBind :: InlineEnv -> Int -> FcBind -> State Int FcBind
normalizeBind environment fuel bind =
  case bind of
    FcNonRec binder rhs -> FcNonRec binder <$> normalizeExpr environment fuel rhs
    FcRec bindings ->
      FcRec <$> traverse (\(binder, rhs) -> (binder,) <$> normalizeExpr environment fuel rhs) bindings

rewriteRoot :: InlineEnv -> FcExpr -> State Int FcExpr
rewriteRoot environment expression =
  case expression of
    FcTyApp (FcTyLam tyVar body) argument ->
      pure (substituteTypes (Map.singleton (tyVarKey tyVar) argument) body)
    FcApp (FcLam binder body) argument ->
      pure (bindArgument binder argument body)
    FcCase scrutinee binder alternatives
      | Just reduced <- reduceKnownCase environment scrutinee binder alternatives -> pure reduced
    FcCast body (Refl _) -> pure body
    _
      | Just (function, arguments) <- collectCall expression,
        Just rhs <- Map.lookup (varName function) (inlineFunctions environment) -> do
          freshRhs <- freshenExpression rhs
          pure (fromMaybe expression (applyCallArguments freshRhs arguments))
    _ -> pure expression

isTrivial :: FcExpr -> Bool
isTrivial expression =
  case expression of
    FcVar {} -> True
    FcLit {} -> True
    FcTyApp function _ -> isTrivial function
    FcCast body (Refl _) -> isTrivial body
    _ -> False

bindArgument :: Var -> FcExpr -> FcExpr -> FcExpr
bindArgument binder argument body
  | isUnliftedType (varType binder) =
      FcCase argument binder [FcAlt DefaultAlt [] body]
  | uses == 0 = body
  | uses == 1 || isTrivial argument = substituteTerm binder argument body
  | otherwise = FcLet (FcNonRec binder argument) body
  where
    uses = occurrenceCount binder body

applyCallArguments :: FcExpr -> [CallArgument] -> Maybe FcExpr
applyCallArguments = go False
  where
    go consumed expression arguments =
      case (expression, arguments) of
        (FcTyLam tyVar body, TypeArgument ty : rest) ->
          go True (substituteTypes (Map.singleton (tyVarKey tyVar) ty) body) rest
        (FcLam binder body, TermArgument argument : rest) ->
          go True (bindArgument binder argument body) rest
        (_, [])
          | consumed && not (isBinder expression) -> Just expression
          | otherwise -> Nothing
        _
          | consumed -> Just (rebuildCall expression arguments)
          | otherwise -> Nothing

    isBinder FcTyLam {} = True
    isBinder FcLam {} = True
    isBinder _ = False

collectCall :: FcExpr -> Maybe (Var, [CallArgument])
collectCall = go []
  where
    go arguments expression =
      case expression of
        FcApp function argument -> go (TermArgument argument : arguments) function
        FcTyApp function ty -> go (TypeArgument ty : arguments) function
        FcVar function
          | not (null arguments) -> Just (function, arguments)
        _ -> Nothing

rebuildCall :: FcExpr -> [CallArgument] -> FcExpr
rebuildCall = List.foldl' apply
  where
    apply function argument =
      case argument of
        TypeArgument ty -> FcTyApp function ty
        TermArgument value -> FcApp function value

reduceKnownCase :: InlineEnv -> FcExpr -> Var -> [FcAlt] -> Maybe FcExpr
reduceKnownCase environment scrutinee binder alternatives =
  case knownCaseValue environment Set.empty scrutinee of
    Just (KnownConstructor constructor fields) -> do
      (isExact, alternative) <- findAlternative (DataAlt constructor) alternatives
      if isExact
        then
          if length fields == length (altBinders alternative)
            then
              let withFields =
                    foldr
                      (\(fieldBinder, field) body -> bindArgument fieldBinder field body)
                      (altRhs alternative)
                      (zip (altBinders alternative) fields)
               in Just (bindArgument binder scrutinee withFields)
            else Nothing
        else Just (bindDefaultAlternative binder scrutinee alternative)
    Just (KnownLiteral literal) -> do
      (isExact, alternative) <- findAlternative (LitAlt literal) alternatives
      if isExact
        then Just (bindArgument binder scrutinee (altRhs alternative))
        else Just (bindDefaultAlternative binder scrutinee alternative)
    Nothing -> Nothing

-- DEFAULT binders denote the complete scrutinee, not the fields of a known
-- constructor. Alias them through the case binder so the scrutinee remains a
-- single call-by-need computation even when several binders are referenced.
bindDefaultAlternative :: Var -> FcExpr -> FcAlt -> FcExpr
bindDefaultAlternative binder scrutinee alternative =
  bindArgument binder scrutinee body
  where
    body =
      foldr
        (\defaultBinder -> substituteTerm defaultBinder (FcVar binder))
        (altRhs alternative)
        (altBinders alternative)

data KnownCaseValue
  = KnownConstructor !Text ![FcExpr]
  | KnownLiteral !Literal

knownCaseValue :: InlineEnv -> Set Text -> FcExpr -> Maybe KnownCaseValue
knownCaseValue environment visited expression =
  case expression of
    FcLit literal -> Just (KnownLiteral literal)
    FcVar var
      | varName var `Set.notMember` visited,
        Just rhs <- Map.lookup (varName var) (staticValues environment) ->
          knownCaseValue environment (Set.insert (varName var) visited) rhs
    _ -> do
      (constructor, arguments) <- collectConstructor expression
      arity <- Map.lookup (varName constructor) (constructorArities environment)
      let fields = [field | TermArgument field <- arguments]
      if length fields == arity
        then Just (KnownConstructor (varName constructor) fields)
        else Nothing

collectConstructor :: FcExpr -> Maybe (Var, [CallArgument])
collectConstructor = go []
  where
    go arguments expression =
      case expression of
        FcApp function argument -> go (TermArgument argument : arguments) function
        FcTyApp function ty -> go (TypeArgument ty : arguments) function
        FcVar constructor -> Just (constructor, arguments)
        _ -> Nothing

findAlternative :: FcAltCon -> [FcAlt] -> Maybe (Bool, FcAlt)
findAlternative constructor alternatives =
  case List.find ((== constructor) . altCon) alternatives of
    Just alternative -> Just (True, alternative)
    Nothing -> (False,) <$> List.find ((== DefaultAlt) . altCon) alternatives

substituteTerm :: Var -> FcExpr -> FcExpr -> FcExpr
substituteTerm target replacement = go
  where
    go expression =
      case expression of
        FcVar var
          | sameVar var target -> replacement
          | otherwise -> expression
        FcLit {} -> expression
        FcApp function argument -> FcApp (go function) (go argument)
        FcTyApp function ty -> FcTyApp (go function) ty
        FcLam binder body
          | sameVar binder target -> expression
          | otherwise -> FcLam binder (go body)
        FcTyLam tyVar body -> FcTyLam tyVar (go body)
        FcLet (FcNonRec binder rhs) body ->
          FcLet
            (FcNonRec binder (go rhs))
            (if sameVar binder target then body else go body)
        FcLet (FcRec bindings) body
          | any (sameVar target . fst) bindings -> expression
          | otherwise -> FcLet (FcRec [(binder, go rhs) | (binder, rhs) <- bindings]) (go body)
        FcCase scrutinee binder alternatives ->
          FcCase
            (go scrutinee)
            binder
            [ if sameVar binder target || any (sameVar target) (altBinders alternative)
                then alternative
                else alternative {altRhs = go (altRhs alternative)}
            | alternative <- alternatives
            ]
        FcCast body coercion -> FcCast (go body) coercion
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map go arguments)

occurrenceCount :: Var -> FcExpr -> Int
occurrenceCount target = go
  where
    go expression =
      case expression of
        FcVar var -> fromEnum (sameVar var target)
        FcLit {} -> 0
        FcApp function argument -> go function + go argument
        FcTyApp function _ -> go function
        FcLam binder body
          | sameVar binder target -> 0
          | otherwise -> go body
        FcTyLam _ body -> go body
        FcLet (FcNonRec binder rhs) body ->
          go rhs + if sameVar binder target then 0 else go body
        FcLet (FcRec bindings) body
          | any (sameVar target . fst) bindings -> 0
          | otherwise -> sum (map (go . snd) bindings) + go body
        FcCase scrutinee binder alternatives ->
          go scrutinee
            + sum
              [ if sameVar binder target || any (sameVar target) (altBinders alternative)
                  then 0
                  else go (altRhs alternative)
              | alternative <- alternatives
              ]
        FcCast body _ -> go body
        FcCallForeign _ arguments -> sum (map go arguments)

freeVariableKeys :: FcExpr -> Set Text
freeVariableKeys expression =
  case expression of
    FcVar var -> Set.singleton (varName var)
    FcLit {} -> Set.empty
    FcApp function argument -> freeVariableKeys function <> freeVariableKeys argument
    FcTyApp function _ -> freeVariableKeys function
    FcLam binder body -> Set.delete (varName binder) (freeVariableKeys body)
    FcTyLam _ body -> freeVariableKeys body
    FcLet (FcNonRec binder rhs) body ->
      freeVariableKeys rhs <> Set.delete (varName binder) (freeVariableKeys body)
    FcLet (FcRec bindings) body ->
      let binders = Set.fromList (map (varName . fst) bindings)
       in (Set.unions (map (freeVariableKeys . snd) bindings) <> freeVariableKeys body) Set.\\ binders
    FcCase scrutinee binder alternatives ->
      freeVariableKeys scrutinee
        <> Set.unions
          [ freeVariableKeys (altRhs alternative)
              Set.\\ Set.fromList (map varName (binder : altBinders alternative))
          | alternative <- alternatives
          ]
    FcCast body _ -> freeVariableKeys body
    FcCallForeign _ arguments -> Set.unions (map freeVariableKeys arguments)

varKey :: Var -> VarKey
varKey var = (varUnique var, varName var)

sameVar :: Var -> Var -> Bool
sameVar left right = varKey left == varKey right

substituteTypes :: TypeSubstitution -> FcExpr -> FcExpr
substituteTypes = go
  where
    go current expression =
      case expression of
        FcVar var -> FcVar (substituteVarType current var)
        FcLit {} -> expression
        FcApp function argument -> FcApp (go current function) (go current argument)
        FcTyApp function ty -> FcTyApp (go current function) (substituteType current ty)
        FcLam binder body -> FcLam (substituteVarType current binder) (go current body)
        FcTyLam tyVar body -> FcTyLam tyVar (go (Map.delete (tyVarKey tyVar) current) body)
        FcLet bind body -> FcLet (goBind current bind) (go current body)
        FcCase scrutinee binder alternatives ->
          FcCase
            (go current scrutinee)
            (substituteVarType current binder)
            [ alternative
                { altBinders = map (substituteVarType current) (altBinders alternative),
                  altRhs = go current (altRhs alternative)
                }
            | alternative <- alternatives
            ]
        FcCast body coercion -> FcCast (go current body) (substituteCoercion current coercion)
        FcCallForeign foreignCall arguments -> FcCallForeign foreignCall (map (go current) arguments)

    goBind current bind =
      case bind of
        FcNonRec binder rhs -> FcNonRec (substituteVarType current binder) (go current rhs)
        FcRec bindings ->
          FcRec
            [ (substituteVarType current binder, go current rhs)
            | (binder, rhs) <- bindings
            ]

substituteVarType :: TypeSubstitution -> Var -> Var
substituteVarType substitutions var = var {varType = substituteType substitutions (varType var)}

substituteCoercion :: TypeSubstitution -> Coercion -> Coercion
substituteCoercion substitutions coercion =
  case coercion of
    CoVar {} -> coercion
    Refl ty -> Refl (substituteType substitutions ty)
    Sym inner -> Sym (substituteCoercion substitutions inner)
    Trans left right -> Trans (substituteCoercion substitutions left) (substituteCoercion substitutions right)
    TyConAppCo tyCon arguments -> TyConAppCo tyCon (map (substituteCoercion substitutions) arguments)
    AxiomInstCo name arguments -> AxiomInstCo name (map (substituteType substitutions) arguments)

substituteType :: TypeSubstitution -> TcType -> TcType
substituteType substitutions ty =
  case ty of
    TcTyVar tyVar -> Map.findWithDefault ty (tyVarKey tyVar) substitutions
    TcMetaTv {} -> ty
    TcTyCon tyCon arguments -> TcTyCon tyCon (map (substituteType substitutions) arguments)
    TcFunTy argument result -> TcFunTy (substituteType substitutions argument) (substituteType substitutions result)
    TcForAllTy tyVar body ->
      TcForAllTy tyVar (substituteType (Map.delete (tyVarKey tyVar) substitutions) body)
    TcQualTy predicates body ->
      TcQualTy (map substitutePredicate predicates) (substituteType substitutions body)
    TcAppTy function argument -> TcAppTy (substituteType substitutions function) (substituteType substitutions argument)
  where
    substitutePredicate predicate =
      case predicate of
        ClassPred className arguments -> ClassPred className (map (substituteType substitutions) arguments)
        EqPred left right -> EqPred (substituteType substitutions left) (substituteType substitutions right)

freshenExpression :: FcExpr -> State Int FcExpr
freshenExpression = freshen Map.empty Map.empty
  where
    freshen termRenames typeRenames expression =
      case expression of
        FcVar var -> pure (FcVar (renameVar termRenames typeRenames var))
        FcLit {} -> pure expression
        FcApp function argument ->
          FcApp <$> freshen termRenames typeRenames function <*> freshen termRenames typeRenames argument
        FcTyApp function ty ->
          FcTyApp <$> freshen termRenames typeRenames function <*> pure (renameType typeRenames ty)
        FcLam binder body -> do
          freshBinder <- freshVar typeRenames binder
          FcLam freshBinder <$> freshen (Map.insert (varKey binder) freshBinder termRenames) typeRenames body
        FcTyLam tyVar body -> do
          freshTyVar <- freshTypeVariable tyVar
          let innerTypes = Map.insert (tyVarKey tyVar) freshTyVar typeRenames
          FcTyLam freshTyVar <$> freshen termRenames innerTypes body
        FcLet (FcNonRec binder rhs) body -> do
          freshRhs <- freshen termRenames typeRenames rhs
          freshBinder <- freshVar typeRenames binder
          freshBody <- freshen (Map.insert (varKey binder) freshBinder termRenames) typeRenames body
          pure (FcLet (FcNonRec freshBinder freshRhs) freshBody)
        FcLet (FcRec bindings) body -> do
          freshBinders <- traverse (freshVar typeRenames . fst) bindings
          let innerTerms = Map.union (Map.fromList (zip (map (varKey . fst) bindings) freshBinders)) termRenames
          freshRhss <- traverse (freshen innerTerms typeRenames . snd) bindings
          freshBody <- freshen innerTerms typeRenames body
          pure (FcLet (FcRec (zip freshBinders freshRhss)) freshBody)
        FcCase scrutinee binder alternatives -> do
          freshScrutinee <- freshen termRenames typeRenames scrutinee
          freshBinder <- freshVar typeRenames binder
          freshAlternatives <- traverse (freshAlternative (Map.insert (varKey binder) freshBinder termRenames) typeRenames) alternatives
          pure (FcCase freshScrutinee freshBinder freshAlternatives)
        FcCast body coercion ->
          FcCast <$> freshen termRenames typeRenames body <*> pure (renameCoercion typeRenames coercion)
        FcCallForeign foreignCall arguments ->
          FcCallForeign foreignCall <$> traverse (freshen termRenames typeRenames) arguments

    freshAlternative termRenames typeRenames alternative = do
      freshBinders <- traverse (freshVar typeRenames) (altBinders alternative)
      let innerTerms = Map.union (Map.fromList (zip (map varKey (altBinders alternative)) freshBinders)) termRenames
      freshRhs <- freshen innerTerms typeRenames (altRhs alternative)
      pure alternative {altBinders = freshBinders, altRhs = freshRhs}

renameVar :: Map VarKey Var -> Map TyVarKey TyVarId -> Var -> Var
renameVar termRenames typeRenames var =
  fromMaybe
    var {varType = renameType typeRenames (varType var)}
    (Map.lookup (varKey var) termRenames)

renameType :: Map TyVarKey TyVarId -> TcType -> TcType
renameType renames = substituteType (TcTyVar <$> renames)

renameCoercion :: Map TyVarKey TyVarId -> Coercion -> Coercion
renameCoercion renames = substituteCoercion (TcTyVar <$> renames)

freshVar :: Map TyVarKey TyVarId -> Var -> State Int Var
freshVar typeRenames var = do
  unique <- freshUnique
  let freshName = varName var <> Text.pack "$inl" <> Text.pack (show (uniqueInt unique))
  pure
    var
      { varName = freshName,
        varUnique = unique,
        varType = renameType typeRenames (varType var)
      }

freshTypeVariable :: TyVarId -> State Int TyVarId
freshTypeVariable tyVar =
  setTyVarKind (tvKind tyVar) . TyVarId (tvName tyVar) <$> freshUnique

freshUnique :: State Int Unique
freshUnique = do
  next <- get
  put (next + 1)
  pure (Unique next)

nextUnique :: FcProgram -> Int
nextUnique program =
  case programUniques program of
    [] -> 1
    uniques -> maximum uniques + 1

programUniques :: FcProgram -> [Int]
programUniques (FcProgram topBinds) = concatMap topBindUniques topBinds

topBindUniques :: FcTopBind -> [Int]
topBindUniques topBind =
  case topBind of
    FcData _ tyVars constructors -> concatMap tyVarUniques tyVars <> concatMap (concatMap typeUniques . snd) constructors
    FcNewtype declaration ->
      concatMap tyVarUniques (fcNewtypeTyVars declaration)
        <> typeUniques (fcNewtypeRepresentation declaration)
        <> typeUniques (fcNewtypeResult declaration)
    FcPrimitive var _ -> varUniques var
    FcForeignImport {} -> []
    FcTopBind bind -> bindUniques bind

bindUniques :: FcBind -> [Int]
bindUniques bind =
  case bind of
    FcNonRec binder rhs -> varUniques binder <> expressionUniques rhs
    FcRec bindings -> concatMap (\(binder, rhs) -> varUniques binder <> expressionUniques rhs) bindings

expressionUniques :: FcExpr -> [Int]
expressionUniques expression =
  case expression of
    FcVar var -> varUniques var
    FcLit {} -> []
    FcApp function argument -> expressionUniques function <> expressionUniques argument
    FcTyApp function ty -> expressionUniques function <> typeUniques ty
    FcLam binder body -> varUniques binder <> expressionUniques body
    FcTyLam tyVar body -> tyVarUniques tyVar <> expressionUniques body
    FcLet bind body -> bindUniques bind <> expressionUniques body
    FcCase scrutinee binder alternatives ->
      expressionUniques scrutinee
        <> varUniques binder
        <> concatMap (\alternative -> concatMap varUniques (altBinders alternative) <> expressionUniques (altRhs alternative)) alternatives
    FcCast body coercion -> expressionUniques body <> coercionUniques coercion
    FcCallForeign _ arguments -> concatMap expressionUniques arguments

varUniques :: Var -> [Int]
varUniques var = uniqueInt (varUnique var) : typeUniques (varType var)

tyVarUniques :: TyVarId -> [Int]
tyVarUniques = pure . uniqueInt . tvUnique

typeUniques :: TcType -> [Int]
typeUniques ty =
  case ty of
    TcTyVar tyVar -> tyVarUniques tyVar
    TcMetaTv unique -> [uniqueInt unique]
    TcTyCon _ arguments -> concatMap typeUniques arguments
    TcFunTy argument result -> typeUniques argument <> typeUniques result
    TcForAllTy tyVar body -> tyVarUniques tyVar <> typeUniques body
    TcQualTy predicates body -> concatMap predicateUniques predicates <> typeUniques body
    TcAppTy function argument -> typeUniques function <> typeUniques argument

predicateUniques :: Pred -> [Int]
predicateUniques predicate =
  case predicate of
    ClassPred _ arguments -> concatMap typeUniques arguments
    EqPred left right -> typeUniques left <> typeUniques right

coercionUniques :: Coercion -> [Int]
coercionUniques coercion =
  case coercion of
    CoVar (EvVar unique) -> [uniqueInt unique]
    Refl ty -> typeUniques ty
    Sym inner -> coercionUniques inner
    Trans left right -> coercionUniques left <> coercionUniques right
    TyConAppCo _ arguments -> concatMap coercionUniques arguments
    AxiomInstCo _ arguments -> concatMap typeUniques arguments

uniqueInt :: Unique -> Int
uniqueInt (Unique value) = value

tyVarKey :: TyVarId -> TyVarKey
tyVarKey tyVar = (tvUnique tyVar, tvName tyVar)
