-- | Rewrite rules: which rules a pass may fire, and the matcher that
-- decides whether one fires at an application.
--
-- A rule fires at an application whose head names the head of the rule's
-- left-hand side. The matcher is first-order and syntactic, as GHC's is:
-- it looks for the one substitution of the rule's type binders and value
-- binders that makes the left-hand side equal to the application, modulo
-- the names of binders that both sides bind in the same place. Nothing is
-- beta-reduced or eta-expanded on the way. The application may give the
-- head more arguments than the left-hand side names; the surplus applies
-- to the rewritten result.
--
-- A binder of the rule never matches an expression that mentions a
-- variable the application binds inside the part being matched, because
-- that variable would escape its scope in the right-hand side.
module Aihc.Fc.Rules
  ( RuleTable,
    ruleTable,
    ruleActiveIn,
    ruleHeadName,
    RuleMatch (..),
    matchRule,
  )
where

import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.TypeOf (TypeEnv, typesEqual)
import Control.Monad (foldM, guard)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set

-- | The rules a pass may fire, by the head of their left-hand sides.
type RuleTable = Map Name [RuleDecl]

-- | The rules of a program that are active in a phase, by head.
ruleTable :: Int -> [Decl] -> RuleTable
ruleTable phase decls =
  Map.fromListWith
    (flip (<>))
    [ (headName, [rule])
    | DeclRule rule <- decls,
      ruleActiveIn phase (ruleActivation rule),
      Just headName <- [ruleHeadName rule]
    ]

-- | Whether a rule fires in a phase. The phases count down as GHC's do:
-- @[n]@ is active in phase @n@ and the phases after it, @[~n]@ in the
-- phases before it.
ruleActiveIn :: Int -> RuleActivation -> Bool
ruleActiveIn phase activation =
  case activation of
    AlwaysActive -> True
    ActiveAfter start -> phase <= start
    ActiveBefore end -> phase > end
    NeverActive -> False

-- | The top-level value at the head of a rule's left-hand side.
ruleHeadName :: RuleDecl -> Maybe Name
ruleHeadName rule =
  case fst (collectSpine (ruleLhs rule)) of
    ExVar name | OriginTop {} <- nameOrigin name -> Just name
    _ -> Nothing

-- | What firing a rule at an application gives: the substitution of the
-- rule's type binders and value binders, and the arguments the
-- application had beyond the ones the left-hand side names.
data RuleMatch = RuleMatch
  { matchTypes :: !(Map Name Type),
    matchValues :: !(Map Name Expr),
    matchSurplus :: ![Either Type Expr]
  }

-- | Match a rule against an application of its head to arguments.
matchRule :: TypeEnv -> RuleDecl -> [Either Type Expr] -> Maybe RuleMatch
matchRule env rule args = do
  let (_, templates) = collectSpine (ruleLhs rule)
  guard (length args >= length templates)
  let (matched, surplus) = splitAt (length templates) args
      matcher =
        Matcher
          { mEnv = env,
            mTypeVars = Set.fromList (map binderName (ruleTypeBinders rule)),
            mValueVars = Set.fromList (map binderName (ruleBinders rule))
          }
  subst <- foldM (matchArg matcher emptyScope) emptySubst (zip templates matched)
  -- Every binder of the rule must be determined, or the right-hand side
  -- would keep a free variable.
  guard (all (`Map.member` substTypes subst) (mTypeVars matcher))
  guard (all (`Map.member` substValues subst) (mValueVars matcher))
  pure (RuleMatch (substTypes subst) (substValues subst) surplus)

data Matcher = Matcher
  { mEnv :: !TypeEnv,
    mTypeVars :: !(Set Name),
    mValueVars :: !(Set Name)
  }

-- | The binders both sides bind in the same place, template name to
-- application name, and the names the application binds inside the part
-- being matched.
data Scope = Scope
  { scopeRenaming :: !(Map Name Name),
    scopeInner :: !(Set Name)
  }

emptyScope :: Scope
emptyScope = Scope Map.empty Set.empty

data Subst = Subst
  { substTypes :: !(Map Name Type),
    substValues :: !(Map Name Expr)
  }

emptySubst :: Subst
emptySubst = Subst Map.empty Map.empty

matchArg :: Matcher -> Scope -> Subst -> (Either Type Expr, Either Type Expr) -> Maybe Subst
matchArg matcher scope subst pair =
  case pair of
    (Left template, Left target) -> matchType matcher scope subst template target
    (Right template, Right target) -> matchExpr matcher scope subst template target
    _ -> Nothing

matchExpr :: Matcher -> Scope -> Subst -> Expr -> Expr -> Maybe Subst
matchExpr matcher scope subst template target
  -- The desugarer eta-expands a binder of the rule that is passed at a
  -- polymorphic or a function type, so the template reads @Λb. g @b@ or
  -- @λx. g x@ where the source said @g@. The binder stands for the whole
  -- argument, so the template is matched eta-reduced.
  | Just reduced <- etaReduced template = matchExpr matcher scope subst reduced target
matchExpr matcher scope subst template target =
  case (template, target) of
    (ExVar name, _)
      | name `Set.member` mValueVars matcher ->
          case Map.lookup name (substValues subst) of
            Just bound -> do
              guard (bound == target)
              pure subst
            Nothing -> do
              guard (Set.null (Set.intersection (scopeInner scope) (exprNames target)))
              pure subst {substValues = Map.insert name target (substValues subst)}
    (ExVar name, ExVar targetName) -> do
      guard (renamed scope name == targetName)
      pure subst
    (ExLit literal, ExLit targetLiteral) -> do
      guard (literal == targetLiteral)
      pure subst
    (ExCoercion proof, ExCoercion targetProof) -> do
      guard (proof == targetProof)
      pure subst
    (ExApp function argument, ExApp targetFunction targetArgument) -> do
      subst' <- matchExpr matcher scope subst function targetFunction
      matchExpr matcher scope subst' argument targetArgument
    (ExTyApp function ty, ExTyApp targetFunction targetTy) -> do
      subst' <- matchExpr matcher scope subst function targetFunction
      matchType matcher scope subst' ty targetTy
    (ExLam binder body, ExLam targetBinder targetBody) -> do
      (scope', subst') <- matchBinder matcher scope subst binder targetBinder
      matchExpr matcher scope' subst' body targetBody
    (ExTyLam binder body, ExTyLam targetBinder targetBody) -> do
      (scope', subst') <- matchBinder matcher scope subst binder targetBinder
      matchExpr matcher scope' subst' body targetBody
    (ExLet bind body, ExLet targetBind targetBody) -> do
      subst' <- matchExpr matcher scope subst (bindRhs bind) (bindRhs targetBind)
      (scope', subst'') <- matchBinder matcher scope subst' (bindBinder bind) (bindBinder targetBind)
      matchExpr matcher scope' subst'' body targetBody
    (ExCast body proof, ExCast targetBody targetProof) -> do
      guard (proof == targetProof)
      matchExpr matcher scope subst body targetBody
    (ExCase scrutinee binder resultType alternatives, ExCase targetScrutinee targetBinder targetResultType targetAlternatives) -> do
      guard (length alternatives == length targetAlternatives)
      subst' <- matchExpr matcher scope subst scrutinee targetScrutinee
      subst'' <- matchType matcher scope subst' resultType targetResultType
      (scope', subst''') <- matchBinder matcher scope subst'' binder targetBinder
      foldM (matchAlternative matcher scope') subst''' (zip alternatives targetAlternatives)
    (ExForeignCall call types arguments, ExForeignCall targetCall targetTypes targetArguments) -> do
      guard (foreignCallName call == foreignCallName targetCall)
      guard (length types == length targetTypes && length arguments == length targetArguments)
      subst' <- foldM (\current (ty, targetTy) -> matchType matcher scope current ty targetTy) subst (zip types targetTypes)
      foldM (\current (argument, targetArgument) -> matchExpr matcher scope current argument targetArgument) subst' (zip arguments targetArguments)
    _ -> Nothing

matchAlternative :: Matcher -> Scope -> Subst -> (Alt, Alt) -> Maybe Subst
matchAlternative matcher scope subst (alternative, target) = do
  guard (altCon alternative == altCon target)
  guard (length (altTypeBinders alternative) == length (altTypeBinders target))
  guard (length (altBinders alternative) == length (altBinders target))
  (scope', subst') <- matchBinders matcher scope subst (altTypeBinders alternative) (altTypeBinders target)
  (scope'', subst'') <- matchBinders matcher scope' subst' (altBinders alternative) (altBinders target)
  matchExpr matcher scope'' subst'' (altRhs alternative) (altRhs target)

matchBinders :: Matcher -> Scope -> Subst -> [Binder] -> [Binder] -> Maybe (Scope, Subst)
matchBinders matcher scope subst binders targets =
  foldM (\(currentScope, currentSubst) (binder, target) -> matchBinder matcher currentScope currentSubst binder target) (scope, subst) (zip binders targets)

-- | Bind a binder of the template to the binder the application has in
-- its place. Their types must match, and from here on the template's name
-- stands for the application's.
matchBinder :: Matcher -> Scope -> Subst -> Binder -> Binder -> Maybe (Scope, Subst)
matchBinder matcher scope subst binder target = do
  subst' <- matchType matcher scope subst (binderType binder) (binderType target)
  let scope' =
        Scope
          { scopeRenaming = Map.insert (binderName binder) (binderName target) (scopeRenaming scope),
            scopeInner = Set.insert (binderName target) (scopeInner scope)
          }
  pure (scope', subst')

matchType :: Matcher -> Scope -> Subst -> Type -> Type -> Maybe Subst
matchType matcher scope subst template target =
  case template of
    TyVar name
      | name `Set.member` mTypeVars matcher ->
          case Map.lookup name (substTypes subst) of
            Just bound -> do
              guard (typesEqual (mEnv matcher) bound target)
              pure subst
            Nothing -> pure subst {substTypes = Map.insert name target (substTypes subst)}
    _ ->
      case (template, target) of
        (TyVar name, TyVar targetName) -> do
          guard (renamed scope name == targetName)
          pure subst
        (TyApp function argument, TyApp targetFunction targetArgument) -> do
          subst' <- matchType matcher scope subst function targetFunction
          matchType matcher scope subst' argument targetArgument
        (TyFun rep1 rep2 argument result, TyFun targetRep1 targetRep2 targetArgument targetResult) -> do
          subst' <- foldM (\current (ty, targetTy) -> matchType matcher scope current ty targetTy) subst [(rep1, targetRep1), (rep2, targetRep2)]
          subst'' <- matchType matcher scope subst' argument targetArgument
          matchType matcher scope subst'' result targetResult
        (TyEq left right, TyEq targetLeft targetRight) -> do
          subst' <- matchType matcher scope subst left targetLeft
          matchType matcher scope subst' right targetRight
        (TyForAll binder body, TyForAll targetBinder targetBody) -> do
          (scope', subst') <- matchBinder matcher scope subst binder targetBinder
          matchType matcher scope' subst' body targetBody
        _
          -- A template without binders of the rule is a type of its own,
          -- which the application must give up to synonyms.
          | Set.null (Set.intersection (mTypeVars matcher) (typeNames template)),
            Map.null (scopeRenaming scope) ->
              do
                guard (typesEqual (mEnv matcher) template target)
                pure subst
          | otherwise -> do
              guard (template == target)
              pure subst

-- | The function an eta-expansion applies, when the template is one:
-- @Λb. f @b@ or @λx. f x@ with the binder free in neither @f@ nor its type.
etaReduced :: Expr -> Maybe Expr
etaReduced template =
  case template of
    ExTyLam binder (ExTyApp function (TyVar name))
      | name == binderName binder,
        not (Set.member name (exprNames function)) ->
          Just function
    ExLam binder (ExApp function (ExVar name))
      | name == binderName binder,
        not (Set.member name (exprNames function)) ->
          Just function
    _ -> Nothing

renamed :: Scope -> Name -> Name
renamed scope name = Map.findWithDefault name name (scopeRenaming scope)

collectSpine :: Expr -> (Expr, [Either Type Expr])
collectSpine = go []
  where
    go args expr =
      case expr of
        ExApp function argument -> go (Right argument : args) function
        ExTyApp function ty -> go (Left ty : args) function
        _ -> (expr, args)

-- | Every name an expression mentions, bound or free, so that a binder
-- of the rule never takes an expression that names what the application
-- binds around it.
exprNames :: Expr -> Set Name
exprNames expr =
  case expr of
    ExVar name -> Set.singleton name
    ExLit {} -> Set.empty
    ExCoercion {} -> Set.empty
    ExApp function argument -> exprNames function <> exprNames argument
    ExTyApp function ty -> exprNames function <> typeNames ty
    ExLam binder body -> typeNames (binderType binder) <> exprNames body
    ExTyLam binder body -> typeNames (binderType binder) <> exprNames body
    ExLet bind body -> exprNames (bindRhs bind) <> exprNames body
    ExRec binds body -> foldMap (exprNames . bindRhs) binds <> exprNames body
    ExCase scrutinee _ resultType alternatives ->
      exprNames scrutinee <> typeNames resultType <> foldMap (exprNames . altRhs) alternatives
    ExCast body _ -> exprNames body
    ExForeignCall _ types arguments -> foldMap typeNames types <> foldMap exprNames arguments

typeNames :: Type -> Set Name
typeNames ty =
  case ty of
    TyVar name -> Set.singleton name
    TyCon name -> Set.singleton name
    TyApp function argument -> typeNames function <> typeNames argument
    TyFun rep1 rep2 argument result -> foldMap typeNames [rep1, rep2, argument, result]
    TyForAll binder body -> typeNames (binderType binder) <> typeNames body
    TyEq left right -> typeNames left <> typeNames right
    TyLit {} -> Set.empty
