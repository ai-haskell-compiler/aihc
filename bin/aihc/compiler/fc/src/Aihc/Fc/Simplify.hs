{-# LANGUAGE OverloadedStrings #-}

-- | The System FC simplifier: the local rewrites of one expression.
--
-- The simplifier walks a body once and applies the rewrites that need no
-- copy of a callee, or that the inliner asks for with a copy in hand: a
-- lambda applied to an argument, a let in the head of an application, the
-- case of a known constructor or literal, a case on a comparison with a
-- literal, a strict pure primitive call bound twice, a case whose
-- default alternative is a case on the same value, a case of a case with
-- join points, a cast against its symmetry, a case that only evaluates a
-- value that is already evaluated, and a lazy constructor application of
-- a primitive call that is safe to run early.
--
-- A copy of a candidate at a use site is one of its rewrites. The
-- 'Simpl' environment carries the candidates and the site policy that
-- the inliner gives it; 'simplifyProgram', the standalone pass, gives it
-- none. The decision about which values are candidates, and the walk
-- over the call graph, belong to "Aihc.Fc.Inline".
--
-- Tidied programs reuse local names across sibling scopes, so every copy
-- that moves into another scope gets binders with uniques above the
-- program's largest, and the pass ends with 'tidyProgram'.
module Aihc.Fc.Simplify
  ( -- * The standalone pass
    SimplifyReport (..),
    simplifyProgram,

    -- * The simplifier
    Simpl (..),
    SimplState (..),
    initialSimplState,
    SimplM,
    Candidate (..),
    CandidateSites (..),
    simplifyExpr,

    -- * Views of expressions
    isConstructorName,
    isKnownConstructor,
    isCheapValue,
    isInlinable,
    isTrivial,
    functionArity,
    collectSpine,
    castedSpine,
    exprValueNames,
    maxLocalUnique,
    safePrimitiveCall,
    freshenExprFrom,

    -- * Substitution
    substExpr,
    substTypeExpr,

    -- * Call arity
    callArityAnalysis,
    topCallArities,
    ruleValueNames,
  )
where

import Aihc.Fc.Fold (foldForeignCall)
import Aihc.Fc.Imports (declReferences, pruneImports)
import Aihc.Fc.Name
import Aihc.Fc.Normalize (normalizeCaseAlternatives)
import Aihc.Fc.Rules (RuleMatch (..), RuleTable, matchRule, ruleTable)
import Aihc.Fc.Size (exprSize, isLiftedBinder, isLiftedType, isStrictBinder, programSize)
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), coercionEndpoints, extendBinder, lookupHeaderType, reduceType, repOf, substType, substTypes, typeEnvFromProgram, typeHead, viewForAll, viewFun)
import Aihc.Fc.Wired (primPackageFromScopes)
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, guard, mapAndUnzipM)
import Control.Monad.Trans.State.Strict (State, get, gets, modify', runState, state)
import Data.Bifunctor (first)
import Data.Either (lefts, rights)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, isJust, listToMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- * The standalone pass

-- | What the standalone simplifying pass did.
data SimplifyReport = SimplifyReport
  { simplifySizeBefore :: !Int,
    simplifySizeAfter :: !Int,
    simplifyRulesFired :: !Int
  }
  deriving (Eq, Show)

-- | Walk every value body of a program once with the local rewrites and
-- no candidate to copy. A case on a top-level value that is a known
-- constructor application still selects its alternative.
--
-- This is the pass that runs after the eta expansion that follows the
-- inliner, which wraps a value in a lambda that applies the old body to
-- the new parameter, under the casts of a newtype it unfolded.
simplifyProgram :: Int -> Program -> (Program, SimplifyReport)
simplifyProgram phase program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, SimplifyReport size0 size0 0)
    Just primPackage ->
      let env = typeEnvFromProgram primPackage program
          bodies = Map.fromList [(valName declaration, valBody declaration) | DeclVal declaration <- programDecls program]
          arities = Map.map functionArity bodies
          simpl =
            Simpl
              { spEnv = env,
                spInline = Map.empty,
                spKnown = Map.filter (isKnownConstructor arities) bodies,
                spArity = arities,
                spLocals = Map.empty,
                spExcluded = Map.empty,
                spCse = Map.empty,
                spEvaluated = Set.empty,
                spDone = Map.empty,
                spSiteLimit = 0,
                spRequestedSiteLimit = 0,
                spReducingSiteLimit = 0,
                spDiscount = 0,
                spRules = ruleTable phase (programDecls program),
                spCredit = 0,
                spInside = False,
                spCredits = Map.empty,
                spSpeculative = False
              }
          escaping = Set.fromList [valName declaration | DeclVal declaration <- programDecls program, valVis declaration == Pub] <> ruleValueNames (programDecls program)
          callArities = topCallArities escaping (Map.elems bodies)
          simplifyDecl decl =
            case decl of
              DeclVal declaration ->
                let credit = Map.findWithDefault 0 (valName declaration) callArities
                 in (\body -> DeclVal declaration {valBody = body}) <$> simplifyExpr (bodyEnv simpl credit (valBody declaration)) (valBody declaration)
              _ -> pure decl
          (decls, final) = runState (mapM simplifyDecl (programDecls program)) (initialSimplState (maxLocalUnique program + 1) 0)
          result = tidyProgram (pruneImports program {programDecls = decls})
       in (result, SimplifyReport size0 (programSize result) (ssRulesFired final))
  where
    size0 = programSize program

-- * Views of expressions

-- in the desugarer's output and in a parsed program alike.
isConstructorName :: Name -> Bool
isConstructorName name = nameSort name == SortDataConstructor

isKnownConstructor :: Map Name Int -> Expr -> Bool
isKnownConstructor arities expr =
  case expr of
    ExTyLam _ body -> isKnownConstructor arities body
    ExLam _ body -> isKnownConstructor arities body
    ExCast body _ -> isKnownConstructor arities body
    _ ->
      case collectSpine expr of
        (ExVar name, args)
          | isConstructorName name -> all (either (const True) (isCheapValue arities)) args
        _ -> False

-- | A body that can be inlined: a function, which is inlined at a call
-- that gives it an argument, or a trivial value. A constructor
-- application is not inlined as a value: a case on it selects a field
-- through 'knownConstructor' instead, and a copy at any other site only
-- allocates what the shared value already holds.
isInlinable :: Expr -> Bool
isInlinable body = functionArity body > 0 || isTrivial body

-- | An expression that does no work when it is evaluated: a literal, a
-- variable, a lambda, or a constructor or partial application of cheap
-- arguments.
isCheapValue :: Map Name Int -> Expr -> Bool
isCheapValue arities expr =
  case expr of
    ExLit {} -> True
    ExVar {} -> True
    ExCoercion {} -> True
    ExLam {} -> True
    ExTyLam _ body -> isCheapValue arities body
    ExCast body _ -> isCheapValue arities body
    ExTyApp body _ -> isCheapValue arities body
    ExApp {} ->
      case collectSpine expr of
        (ExVar name, args)
          | isConstructorName name -> cheapArgs args
          | Just arity <- Map.lookup name arities -> length [() | Right _ <- args] < arity && cheapArgs args
        _ -> False
    _ -> False
  where
    cheapArgs = all (either (const True) (isCheapValue arities))

functionArity :: Expr -> Int
functionArity expr =
  case expr of
    ExLam _ body -> 1 + functionArity body
    ExTyLam _ body -> functionArity body
    _ -> 0

-- * Simplifier

data Candidate = Candidate
  { candidateBody :: !Expr,
    -- | How the sites of the candidate are decided.
    candidateSites :: !CandidateSites,
    -- | Whether the pragma of the value asks for its copies, whatever
    -- its size. A reducing site of such a value is decided by the
    -- reducing site limit.
    candidateRequested :: !Bool
  }

-- | How the sites of a candidate are decided.
data CandidateSites
  = -- | Take every site, whatever its growth. The site charges the
    -- allowance with its growth.
    SitesUnconditional
  | -- | Take a site within the requested site limit without a charge to
    -- the allowance: the pragma of the value asks for each copy. Decide
    -- a larger site as a measured one.
    SitesRequested
  | -- | Take a site when its growth fits the site limit and the
    -- allowance.
    SitesMeasured
  deriving (Eq, Show)

data Simpl = Simpl
  { spEnv :: !TypeEnv,
    spInline :: !(Map Name Candidate),
    spKnown :: !(Map Name Expr),
    spArity :: !(Map Name Int),
    -- | Local bindings whose right-hand side is a known constructor
    -- application.
    spLocals :: !(Map Name Expr),
    -- | Alternatives that a variable cannot select in this scope.
    spExcluded :: !(Map Expr (Set AltCon)),
    -- | Strict bindings in scope whose right-hand side is a pure
    -- primitive call, keyed by that call. A later binding of the same call
    -- names the earlier binder instead: the earlier binding is evaluated
    -- on every path that reaches the later one.
    spCse :: !(Map Expr Name),
    -- | Local binders that hold a value in weak-head normal form: case
    -- binders, variable scrutinees inside their alternatives, binders of
    -- strict fields, and let binders of values.
    spEvaluated :: !(Set Name),
    -- | Local binders whose one use takes a right-hand side that is
    -- already simplified. A use is replaced by a fresh copy of that
    -- right-hand side, and the copy is not simplified again: it can hold
    -- sites that were decided already, and each walk over it would decide
    -- them again.
    spDone :: !(Map Name Expr),
    spSiteLimit :: !Int,
    -- | The largest growth a requested site may cause without a charge
    -- to the allowance.
    spRequestedSiteLimit :: !Int,
    -- | The largest growth a strong reducing site of a requested value
    -- may cause without a charge to the allowance, with the copies inside
    -- it.
    spReducingSiteLimit :: !Int,
    -- | The discount one function argument of a call site takes off the
    -- growth of inlining it.
    spDiscount :: !Int,
    -- | The rewrite rules that may fire, by the head of their left-hand
    -- side.
    spRules :: !RuleTable,
    -- | How many value arguments the expression under simplification
    -- receives from every use, when it is a right-hand side in leading
    -- position: the call arity of its binding, less the lambdas passed.
    -- Zero anywhere else. See 'useArities'.
    spCredit :: !Int,
    -- | Whether a lambda of the binding has been passed. A lambda with a
    -- credit is entered at most once per call only inside the first
    -- lambda: the work before the first lambda belongs to the closure of
    -- the binding, which every call shares.
    spInside :: !Bool,
    -- | The credit of each local binder of the body under
    -- simplification, by 'callArityAnalysis' of the body as it was when
    -- its simplification began. A binder a copy brings in later is
    -- absent, and gets no credit until the next walk.
    spCredits :: !(Map Name Int),
    -- | Whether the walk simplifies a trial copy that 'pushIntoCase' may
    -- throw away. A trial copy pushes no context of its own. See
    -- 'letOfCase'.
    spSpeculative :: !Bool
  }

data SimplState = SimplState
  { ssSupply :: !Int,
    -- | The growth the remaining sites may still cause.
    ssAllowance :: !Int,
    ssInlined :: !Int,
    -- | The growth the walk took without a charge to the allowance. The
    -- limit of the value grows by it, so that a later round does not
    -- charge it either.
    ssExempt :: !Int,
    ssRulesFired :: !Int,
    -- | How many copies of its right-hand side each moved binder has
    -- taken so far, over every walk of the body. See 'mkLet'.
    ssCopied :: !(Map Name Int),
    -- | How many more rules may fire in this walk. Rules are not checked
    -- for termination, so a bound keeps a looping pair of rules finite.
    ssRuleFuel :: !Int
  }

-- | The state of one walk, with the unique supply and the growth allowance.
initialSimplState :: Int -> Int -> SimplState
initialSimplState supply allowance =
  SimplState
    { ssSupply = supply,
      ssAllowance = allowance,
      ssInlined = 0,
      ssExempt = 0,
      ssRulesFired = 0,
      ssCopied = Map.empty,
      ssRuleFuel = ruleFuel
    }

-- | How many rules may fire in one walk over one body.
ruleFuel :: Int
ruleFuel = 1000

type SimplM = State SimplState

type Arg = Either Type Expr

simplifyExpr :: Simpl -> Expr -> SimplM Expr
simplifyExpr env expr =
  case expr of
    ExVar {} -> simplifyApp env expr []
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp {}
      | Just pushed <- pushHeadCasts expr -> simplifyExpr env pushed
      | otherwise -> uncurry (simplifyApp env) (collectSpine expr)
    ExTyApp {} -> uncurry (simplifyApp env) (collectSpine expr)
    ExLam binder body -> do
      body' <- simplifyExpr (markUnlifted [binder] (passLambda env)) body
      ExLam binder <$> readFieldsInside env body'
    ExTyLam binder body -> ExTyLam binder <$> simplifyExpr (extendTypeBinder env binder) body
    ExLet bind body -> do
      let binder = bindBinder bind
      rhs <- simplifyExpr (rhsEnv env (binderName binder)) (bindRhs bind)
      let continue env' rhs'
            | isTrivial rhs' = simplifyExpr env' (substExpr (Map.singleton (binderName binder) rhs') body)
            | otherwise = do
                body' <- simplifyExpr (bindingEnv env' binder rhs') body
                mkLet env' (Bind binder rhs') body'
      -- A strict let evaluates its right-hand side before its body, so the
      -- chain of the right-hand side can move out of the let as it moves
      -- out of a scrutinee. See 'floatChain'. The body is then simplified
      -- once, where the chain knows its scrutinees. The cases of the chain
      -- take the type of the body, so the body must show it.
      if isStrictBinder (spEnv env) binder && isChain rhs
        then case tailType env body of
          Just resultType -> do
            chain <- freshenExpr rhs
            floatChain env resultType chain continue
          Nothing -> continue env rhs
        else continue env rhs
    ExRec binds body -> do
      binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> simplifyExpr (rhsEnv env (binderName (bindBinder bind))) (bindRhs bind)) binds
      simplifyExpr env body >>= sinkGroup binds' ExRec
    ExCase scrutinee binder resultType alternatives
      | (ExVar name, args) <- collectSpine (fromMaybe scrutinee (pushHeadCasts scrutinee)),
        Just candidate <- Map.lookup name (spInline env),
        takesArgument env candidate args ->
          inlineScrutinee (noOneShot env) name candidate args binder resultType alternatives
      | otherwise -> do
          scrutinee' <- simplifyExpr (noOneShot env) scrutinee
          if isChain scrutinee'
            then do
              chain <- freshenExpr scrutinee'
              floatChain env resultType chain (\env' tailExpr -> simplifyCase env' tailExpr binder resultType alternatives)
            else simplifyCase env scrutinee' binder resultType alternatives
    ExCast body coercion -> do
      body' <- simplifyExpr env body
      mkCast body' coercion
    ExForeignCall call types arguments -> do
      arguments' <- mapM (simplifyExpr (noOneShot env)) arguments
      floated <- floatPrimitiveArgument env call types arguments'
      case floated of
        Just result -> pure result
        Nothing -> do
          let call' = fromMaybe (ExForeignCall call types arguments') (foldForeignCall (spEnv env) call types arguments')
          pure (maybe call' ExVar (Map.lookup call' (spCse env)))

-- | Read the fields of a known constructor inside a lambda that uses both
-- the constructor and its fields: @λx. f b v@ where @b@ is @K v@ is
-- @λx. case b of K v' -> f b v'@. The closure of the lambda then holds
-- @b@ only, and does not hold each field beside it. The case selects the
-- fields of a value, so it reads them and evaluates nothing.
--
-- Such a lambda comes from a case of a known constructor inside it,
-- which put the field where the code read it from @b@. A chain of
-- continuation closures, such as the reads of the @Get@ monad, each hold
-- every earlier value, so each value held twice doubles every closure.
--
-- The case needs the result type of the body, which the body must show.
readFieldsInside :: Simpl -> Expr -> SimplM Expr
readFieldsInside env body0 = foldM readFields body0 (Map.toList (spLocals env))
  where
    readFields body (name, application)
      | Set.member name used,
        (ExVar con, args) <- collectSpine application,
        isConstructorName con,
        Just fields <- mapM fieldVariable (rights args),
        any (`Set.member` used) fields,
        name `notElem` fields,
        Just (fieldTypes, conResult) <- constructorFields (lefts args) con,
        length fieldTypes == length fields,
        isLiftedType (spEnv env) conResult,
        Just resultType <- tailType env body = do
          fresh <- mapM freshLocal fields
          binder <- freshLocal name
          let renamed = substExpr (Map.fromList (zip fields (map ExVar fresh))) body
              binders = zipWith Binder fresh fieldTypes
          pure (ExCase (ExVar name) (Binder binder conResult) resultType [Alt (AltData con) [] binders renamed])
      | otherwise = pure body
      where
        used = exprValueNames body
    fieldVariable argument =
      case argument of
        ExVar var -> Just var
        _ -> Nothing
    -- The field types and the result type of a constructor at its type
    -- arguments. A constructor with an existential type takes more type
    -- arguments than its result type has, and gives nothing: the
    -- alternative would have to bind the existential types.
    constructorFields types con = do
      conType <- lookupHeaderType (spEnv env) con
      instantiated <- foldM instantiate conType types
      (fieldTypes, result) <- split instantiated
      let (_, resultArgs) = typeSpine (reduceType (spEnv env) result)
      if length resultArgs == length types then Just (fieldTypes, result) else Nothing
    instantiate ty argument = do
      (binder, inner) <- viewForAll (spEnv env) ty
      Just (substType (binderName binder) argument inner)
    split ty =
      case viewFun (spEnv env) ty of
        Just (_, _, argument, result) -> do
          (arguments, final) <- split result
          Just (argument : arguments, final)
        Nothing
          | Just _ <- viewForAll (spEnv env) ty -> Nothing
          | otherwise -> Just ([], ty)

-- | Whether an expression starts a chain: a let, or a case of one
-- alternative. See 'floatChain'.
isChain :: Expr -> Bool
isChain expr =
  case expr of
    ExLet {} -> True
    ExCase _ _ _ [_] -> True
    _ -> False

-- | Move the chain of a simplified scrutinee out of its case:
-- @case (let x = a in case s of K y -> b) of alts@ is
-- @let x = a in case s of K y -> case b of alts@. A strict let in the
-- chain also gives up the chain of its right-hand side:
-- @let v = (case s of K y -> b) in c@ is @case s of K y -> let v = b in c@.
-- The scrutinee is evaluated before the alternatives, so each part of the
-- chain runs at the same point in both forms, and no part is copied.
--
-- The tail of the chain is continued in the environment of the chain.
-- There, a case of the chain gives the fields of its scrutinee to a
-- later case on the same scrutinee, and a tail that is a constructor
-- selects an alternative. The case of a primitive argument that
-- 'floatPrimitiveArgument' moves out ends up here, in the scrutinee of
-- the case or in the strict let around the next read.
--
-- The parts of the chain are simplified already and stay as they are.
-- The caller gives the chain fresh binders, so that they capture no
-- name of the alternatives.
floatChain :: Simpl -> Type -> Expr -> (Simpl -> Expr -> SimplM Expr) -> SimplM Expr
floatChain env resultType expr continue =
  case expr of
    ExLet bind inner
      | isStrictBinder (spEnv env) binder,
        isChain (bindRhs bind) ->
          floatChain env resultType (bindRhs bind) $ \env' rhs ->
            ExLet bind {bindRhs = rhs} <$> floatChain (bindingEnv env' binder rhs) resultType inner continue
      | otherwise ->
          ExLet bind <$> floatChain (bindingEnv env binder (bindRhs bind)) resultType inner continue
      where
        binder = bindBinder bind
    ExCase scrutinee binder _ [alternative] -> do
      rhs <- floatChain (alternativeEnv env scrutinee binder [alternative] alternative) resultType (altRhs alternative) continue
      pure (mkCase (spEnv env) scrutinee binder resultType [alternative {altRhs = rhs}])
    _ -> continue env expr

-- | The type of an expression, when its tail shows it: a case gives its
-- result type, and a constructor, a top-level value or a primitive call
-- gives its declared type at its arguments. A local variable or a
-- literal gives nothing.
tailType :: Simpl -> Expr -> Maybe Type
tailType env expr =
  case expr of
    ExCase _ _ resultType _ -> Just resultType
    ExLet _ body -> tailType env body
    ExRec _ body -> tailType env body
    ExForeignCall call types _ -> snd <$> primitiveSignature (spEnv env) call types
    _ ->
      case collectSpine expr of
        (ExVar name, args) -> do
          headType <- lookupHeaderType (spEnv env) name
          foldM applyArgument headType args
        _ -> Nothing
  where
    applyArgument ty argument =
      case argument of
        Left argumentType -> do
          (binder, body) <- viewForAll (spEnv env) ty
          Just (substType (binderName binder) argumentType body)
        Right _ -> do
          (_, _, _, result) <- viewFun (spEnv env) ty
          Just result

-- | Simplify a case whose scrutinee is simplified and whose alternatives
-- are not. A scrutinee that compares a value with a literal turns into a
-- case on the value first, and a scrutinee that is a known constructor or
-- literal selects its alternative.
--
-- The alternatives are simplified once, where they end up: inside the
-- inner case when the scrutinee is a case, or in place otherwise.
simplifyCase :: Simpl -> Expr -> Binder -> Type -> [Alt] -> SimplM Expr
simplifyCase env scrutinee binder resultType originalAlternatives
  -- A case that only evaluates an evaluated value does nothing. The case
  -- binder is the scrutinee.
  | [Alt AltDefault [] [] rhs] <- alternatives,
    isEvaluated env scrutinee =
      simplifyExpr env (substExpr (Map.singleton (binderName binder) scrutinee) rhs)
  | Just rewritten <- literalEqualityCase (spEnv env) scrutinee binder resultType alternatives = simplifyExpr env rewritten
  | otherwise = do
      reduced <- caseOfKnown env scrutinee binder alternatives
      case reduced of
        Just result -> simplifyExpr env result
        Nothing -> do
          pushed <- caseOfCaseRaw env scrutinee binder resultType alternatives
          accepted <- case pushed of
            Just pushed' -> acceptGrowth env (pushedGrowth env pushed')
            Nothing -> pure False
          case pushed of
            Just pushed' | accepted -> expandJoins env (pushedJoins pushed') (pushedSmall pushed')
            _ -> do
              alternatives' <- mapM (simplifyAlt env scrutinee binder alternatives) alternatives
              pure (mkCase (spEnv env) scrutinee binder resultType alternatives')
  where
    alternatives =
      normalizeCaseAlternatives
        (spEnv env)
        binder
        [ alternative
        | alternative <- originalAlternatives,
          altCon alternative `Set.notMember` Map.findWithDefault Set.empty scrutinee (spExcluded env)
        ]

-- | Inline a candidate whose call is the scrutinee of a case, and decide
-- the site on the case as a whole. The case of the inlined call takes
-- the alternatives into the body of the callee, where a tail that is a
-- known constructor selects one of them. What the site costs is the
-- difference between that and the case of the call, and a callee whose
-- every tail is a known constructor earns the discount of a call that
-- the case resolves: the case, the boxed result, and the call all go.
--
-- The alternatives are not simplified before the decision, and their
-- size is not measured: a rejected site must not pay for them, and they
-- are as large as the rest of the function.
inlineScrutinee :: Simpl -> Name -> Candidate -> [Arg] -> Binder -> Type -> [Alt] -> SimplM Expr
inlineScrutinee env name candidate args binder resultType originalAlternatives = do
  let alternatives = normalizeCaseAlternatives (spEnv env) binder originalAlternatives
  args' <- mapM (either (pure . Left) (fmap Right . simplifyExpr env)) args
  before <- get
  inlined <- inlineCandidate env name candidate args'
  paid <- gets (nestedPaid before)
  let original = rebuildSpine (ExVar name) args'
      discount =
        callDiscount env (candidateBody candidate) args'
          + (if tailsAreKnown env inlined then spDiscount env else 0)
          + paid
      callGrowth = exprSize (spEnv env) inlined - exprSize (spEnv env) original - discount
      fallback = do
        restoreSite before
        alternatives' <- mapM (simplifyAlt env original binder alternatives) alternatives
        pure (mkCase (spEnv env) original binder resultType alternatives')
      decide growth result = do
        accepted <- acceptSite env candidate (siteReduction env (candidateBody candidate) args') paid growth
        if accepted
          then result
          else fallback
  reduced <- caseOfKnown env inlined binder alternatives
  case reduced of
    -- One alternative replaces the case and the call: a saving whatever
    -- the sizes are.
    Just result -> decide (-1) (simplifyExpr env result)
    Nothing -> do
      pushed <- caseOfCaseRaw env inlined binder resultType alternatives
      case pushed of
        Just pushed' -> decide (pushedGrowth env pushed' + callGrowth) (expandJoins env (pushedJoins pushed') (pushedSmall pushed'))
        -- The alternatives use the remaining allowance. Simplify them only
        -- after this site reserves its growth.
        Nothing -> decide callGrowth $ do
          alternatives' <- mapM (simplifyAlt env inlined binder alternatives) alternatives
          pure (mkCase (spEnv env) inlined binder resultType alternatives')

-- | The allowance the sites inside a copy took, from the state before
-- the copy. The site around them takes it off its growth: that growth
-- is in its result, and it must not be charged twice.
nestedPaid :: SimplState -> SimplState -> Int
nestedPaid before after = ssAllowance before - ssAllowance after

-- | Decide a site whose growth is measured, and record it when it is
-- taken.
--
-- The growth of a site is what its result adds over the call it
-- replaces, less the discounts of the call and what the sites inside
-- the copy have paid.
--
-- An unconditional site is taken whatever its growth, but it still
-- charges the allowance with what it grew: the size metric counts a
-- case once for each path of its scrutinee, so a copy can be larger
-- than the value it replaces, and the sites after it must see the
-- allowance that is left.
--
-- A requested site within the requested site limit is taken and does
-- not charge the allowance: the pragma asks for the copy, and a copy of
-- a few nodes must not starve the other sites of the value. The sites
-- inside the copy still charge the allowance. A requested site that
-- grows more is measured like any other: its growth counts the copies
-- inside it that went free, so a chain of requested copies stops where
-- it grows past the limit.
--
-- A reducing site that is not unconditional is taken without a charge
-- to the allowance when the copy, with the copies inside it, grows at
-- most by the site limit, or by the reducing site limit for a strong
-- reduction of a requested value. The allowance that the copies inside it took is given back: the
-- limit bounds them as part of the site. See 'Reduction'.
--
-- The growth that a site takes without a charge is recorded, and the
-- limit of the value grows by it, so a later round does not charge it.
acceptSite :: Simpl -> Candidate -> Reduction -> Int -> Int -> SimplM Bool
acceptSite env candidate reduction paid growth
  | reduction /= NoReduction,
    candidateSites candidate /= SitesUnconditional,
    growth + paid <= reducingLimit = do
      modify' (\st -> st {ssAllowance = ssAllowance st + paid, ssInlined = ssInlined st + 1, ssExempt = ssExempt st + max 0 (growth + paid)})
      pure True
  | otherwise =
      case candidateSites candidate of
        SitesUnconditional -> do
          modify' (\st -> st {ssAllowance = ssAllowance st - growth, ssInlined = ssInlined st + 1})
          pure True
        SitesRequested
          | growth <= spRequestedSiteLimit env -> do
              modify' (\st -> st {ssInlined = ssInlined st + 1, ssExempt = ssExempt st + max 0 growth})
              pure True
          | otherwise -> measured
        SitesMeasured -> measured
  where
    reducingLimit
      | reduction == StrongReduction && candidateRequested candidate = spReducingSiteLimit env
      | otherwise = spSiteLimit env
    measured = do
      accepted <- acceptGrowth env growth
      if accepted
        then do
          modify' (\st -> st {ssInlined = ssInlined st + 1})
          pure True
        else pure False

-- | How a call reduces its callee. A call reduces its callee when it gives
-- a known constructor to a parameter that the callee scrutinises. The
-- case on the parameter in the copy then selects its alternative, and when
-- the callee returns a constructor, the case of the next call on the
-- result selects its alternative too. A chain of such calls becomes
-- straight-line code with no call, no case, and no constructor in between.
data Reduction
  = NoReduction
  | -- | The argument is a local variable that holds a known constructor,
    -- such as the case binder of an alternative. The copy saves the case
    -- and no allocation.
    WeakReduction
  | -- | The argument is a constructor that the copy removes, a
    -- constructor application or an expression whose every tail is one,
    -- or a variable that names a known top-level value, such as a
    -- dictionary.
    StrongReduction
  deriving (Eq, Ord, Show)

-- | The strongest reduction that a call gives its callee. See 'Reduction'.
siteReduction :: Simpl -> Expr -> [Arg] -> Reduction
siteReduction env body args =
  List.foldl'
    max
    NoReduction
    [ knownArgument argument
    | (binder, argument) <- valueArguments body args,
      scrutinised (binderName binder) body
    ]
  where
    knownArgument argument =
      case fst (peelCasts argument) of
        ExVar name
          | Map.member name (spKnown env) -> StrongReduction
          | Map.member name (spLocals env) || maybe False (not . Set.null) (Map.lookup argument (spExcluded env)) -> WeakReduction
          | otherwise -> NoReduction
        core
          | tailsAreKnown env core -> StrongReduction
          | otherwise -> NoReduction
    scrutinised name expr =
      case expr of
        ExCase scrutinee _ _ alternatives ->
          isVariable name scrutinee
            || scrutinised name scrutinee
            || any (scrutinised name . altRhs) alternatives
        ExLam _ inner -> scrutinised name inner
        ExTyLam _ inner -> scrutinised name inner
        ExLet bind inner -> scrutinised name (bindRhs bind) || scrutinised name inner
        ExRec binds inner -> any (scrutinised name . bindRhs) binds || scrutinised name inner
        ExApp function argument -> scrutinised name function || scrutinised name argument
        ExTyApp function _ -> scrutinised name function
        ExCast inner _ -> scrutinised name inner
        ExForeignCall _ _ arguments -> any (scrutinised name) arguments
        _ -> False
    isVariable name expr =
      case fst (peelCasts expr) of
        ExVar var -> var == name
        _ -> False

-- | Forget the sites and the allowance a rejected copy took: its result
-- is discarded, so nothing inside it happened. The supply stays, so that
-- no name of the discarded copy is handed out again.
restoreSite :: SimplState -> SimplM ()
restoreSite before =
  modify' (\st -> st {ssAllowance = ssAllowance before, ssInlined = ssInlined before, ssExempt = ssExempt before})

-- | Whether every tail of an expression is a known constructor
-- application or a literal, so that a case on the expression resolves
-- in each of them.
tailsAreKnown :: Simpl -> Expr -> Bool
tailsAreKnown env expr =
  case expr of
    ExCase _ _ _ alternatives -> all (tailsAreKnown env . altRhs) alternatives
    ExLet _ body -> tailsAreKnown env body
    ExRec _ body -> tailsAreKnown env body
    ExCast body _ -> tailsAreKnown env body
    ExTyLam _ body -> tailsAreKnown env body
    ExLit {} -> True
    _ -> isKnownConstructor (spArity env) expr

-- | Whether a call gives a candidate enough arguments to inline.
--
-- A call that gives every parameter reduces to the body. A call that
-- gives fewer reduces to the lambdas that are left, with the arguments
-- bound outside them, so no work moves under a lambda that a partial
-- application shared. What the copy shows is the function value the call
-- built: @(.) f g@ becomes @λx. f (g x)@, and a caller whose result that
-- was is then a function of one more argument to the arity analysis,
-- where before it returned a partial application.
--
-- Such a call is taken only when one of its arguments is interesting: not
-- a variable, or a variable that names a known function. That is GHC's
-- rule for an unsaturated call, and it is what keeps an instance method
-- out of its own dictionary. The method helper is applied to the
-- dictionary parameters alone, @$fEqPair$c== @a $d@, and copying its body
-- into the constructor would make every case on the dictionary reduce to
-- that body, whatever its size. The size rule still decides a site that
-- this rule admits.
takesArgument :: Simpl -> Candidate -> [Arg] -> Bool
takesArgument env candidate args =
  count >= functionArity (candidateBody candidate)
    || (count > 0 && any interesting valueArgs)
  where
    valueArgs = rights args
    count = length valueArgs
    interesting argument =
      case argument of
        ExVar name -> Map.member name (spArity env)
        ExTyApp body _ -> interesting body
        ExCast body _ -> interesting body
        _ -> True

-- | A fresh copy of a candidate applied to simplified arguments. The
-- candidate is not inlined into its own copy.
inlineCandidate :: Simpl -> Name -> Candidate -> [Arg] -> SimplM Expr
inlineCandidate env name candidate args = do
  copy <- freshenExpr (candidateBody candidate)
  betaReduce env {spInline = Map.delete name (spInline env)} copy args

-- | The environment of the body of a binding. A known constructor
-- application is recorded for the case of a known constructor, and a
-- strict pure primitive call for the reuse of its binder.
bindingEnv :: Simpl -> Binder -> Expr -> Simpl
bindingEnv env binder rhs
  | isKnownConstructor (spArity env) rhs = evaluatedEnv {spLocals = Map.insert (binderName binder) rhs (spLocals env)}
  | isStrictBinder (spEnv env) binder,
    isPurePrimitiveCall (spEnv env) rhs || isStateToken rhs,
    Map.notMember rhs (spCse env) =
      evaluatedEnv {spCse = Map.insert rhs (binderName binder) (spCse env)}
  | otherwise = evaluatedEnv
  where
    evaluatedEnv
      | isValue env rhs = markEvaluated [binderName binder] env
      | otherwise = markUnlifted [binder] env

-- | Record that the binders of an unlifted type hold values: such a value
-- is never a thunk. A case with one default alternative on such a binder
-- then only names it, as on any evaluated variable.
markUnlifted :: [Binder] -> Simpl -> Simpl
markUnlifted binders env =
  markEvaluated [binderName binder | binder <- binders, not (isLiftedBinder (spEnv env) binder)] env

-- | Record that binders hold values in weak-head normal form.
markEvaluated :: [Name] -> Simpl -> Simpl
markEvaluated names env = env {spEvaluated = List.foldl' (flip Set.insert) (spEvaluated env) names}

-- | Whether an expression is a variable, under casts, that holds a value
-- in weak-head normal form.
isEvaluated :: Simpl -> Expr -> Bool
isEvaluated env expr =
  case fst (peelCasts expr) of
    ExVar name -> Set.member name (spEvaluated env)
    _ -> False

-- | Whether an expression is in weak-head normal form: a literal, a
-- lambda, a constructor application, or an evaluated variable.
isValue :: Simpl -> Expr -> Bool
isValue env expr =
  case expr of
    ExLit {} -> True
    ExCoercion {} -> True
    ExLam {} -> True
    ExVar name -> isConstructorName name || Set.member name (spEvaluated env)
    ExTyLam _ body -> isValue env body
    ExTyApp body _ -> isValue env body
    ExCast body _ -> isValue env body
    ExLet _ body -> isValue env body
    ExRec _ body -> isValue env body
    ExApp {} ->
      case collectSpine expr of
        (ExVar name, _) -> isConstructorName name
        _ -> False
    _ -> False

-- | Simplify an alternative. Inside a constructor alternative, the case
-- binder and a scrutinee variable are known to be that constructor
-- applied to the alternative binders.
simplifyAlt :: Simpl -> Expr -> Binder -> [Alt] -> Alt -> SimplM Alt
simplifyAlt env scrutinee binder alternatives alternative = do
  let body =
        case (scrutinee, altCon alternative) of
          (ExVar name, AltDefault) ->
            substExpr (Map.singleton name (ExVar (binderName binder))) (altRhs alternative)
          _ -> altRhs alternative
  rhs <- simplifyExpr (alternativeEnv env scrutinee binder alternatives alternative) body
  pure alternative {altRhs = rhs}

-- | The environment inside an alternative: its type binders are in scope,
-- and in a constructor alternative the case binder is that constructor
-- applied to the alternative binders. A scrutinee that is a variable
-- under casts is the same application under the symmetric casts, so a
-- later case on that variable, cast the same way, selects its fields.
alternativeEnv :: Simpl -> Expr -> Binder -> [Alt] -> Alt -> Simpl
alternativeEnv env scrutinee binder alternatives alternative =
  markUnlifted (altBinders alternative) . markEvaluated (binderName binder : maybe [] pure scrutineeName <> strictBinders) $ case known of
    Just application ->
      typeEnv
        { spLocals =
            Map.insert (binderName binder) application
              . maybe id (\name -> Map.insert name (List.foldl' (\body co -> ExCast body (coSym co)) application (reverse scrutineeCasts))) scrutineeName
              $ spLocals typeEnv
        }
    Nothing -> typeEnv
  where
    typeEnv = (List.foldl' extendTypeBinder env (altTypeBinders alternative)) {spExcluded = exclusions}
    exclusions
      | AltDefault <- altCon alternative,
        Just _ <- scrutineeName =
          let excluded =
                Map.findWithDefault Set.empty scrutinee (spExcluded env)
                  <> Set.fromList [altCon alt | alt <- alternatives, altCon alt /= AltDefault]
           in Map.insert
                (ExVar (binderName binder))
                excluded
                (Map.insert scrutinee excluded (spExcluded env))
      | otherwise = spExcluded env
    known =
      case altCon alternative of
        AltData con -> constructorApplication (spEnv env) con (binderType binder) alternative
        _ -> Nothing
    (scrutineeCore, scrutineeCasts) = peelCasts scrutinee
    scrutineeName =
      case scrutineeCore of
        ExVar name -> Just name
        _ -> Nothing
    -- The binders of the strict fields of the constructor.
    strictBinders =
      case altCon alternative of
        AltData con ->
          let strict = Map.findWithDefault [] con (teConStrictFields (spEnv env))
           in [binderName field | (position, field) <- zip [0 ..] (altBinders alternative), position `elem` strict]
        _ -> []

-- | Strip the casts on an expression. The coercions come innermost
-- first, in the order the casts apply.
peelCasts :: Expr -> (Expr, [Coercion])
peelCasts expr =
  case expr of
    ExCast body coercion -> let (core, casts) = peelCasts body in (core, casts <> [coercion])
    _ -> (expr, [])

-- | The constructor application that an alternative matches: the
-- constructor at the type of the scrutinee, applied to the type binders
-- and the field binders of the alternative. Each type argument of the
-- constructor is either fixed by the scrutinee type or bound by the
-- alternative, or the application is unknown.
constructorApplication :: TypeEnv -> Name -> Type -> Alt -> Maybe Expr
constructorApplication env con scrutineeType alternative = do
  conType <- lookupHeaderType env con
  let (foralls, result) = splitForAlls env conType
      (_, resultArgs) = typeSpine (reduceType env result)
      (_, scrutineeArgs) = typeSpine (reduceType env scrutineeType)
  if length resultArgs /= length scrutineeArgs then Nothing else Just ()
  let fixed = Map.fromList [(name, ty) | (TyVar name, ty) <- zip resultArgs scrutineeArgs]
  typeArgs <- fill foralls (altTypeBinders alternative) fixed
  Just (rebuildSpine (ExVar con) (map Left typeArgs <> map (Right . ExVar . binderName) (altBinders alternative)))
  where
    fill foralls existentials fixed =
      case foralls of
        [] -> if null existentials then Just [] else Nothing
        binder : rest
          | Just ty <- Map.lookup (binderName binder) fixed -> (ty :) <$> fill rest existentials fixed
          | existential : more <- existentials -> (TyVar (binderName existential) :) <$> fill rest more fixed
          | otherwise -> Nothing

extendTypeBinder :: Simpl -> Binder -> Simpl
extendTypeBinder env binder = env {spEnv = extendBinder (spEnv env) binder}

-- | Simplify an application spine. The head and the arguments are
-- simplified first. A head that names a candidate is replaced by a copy of
-- its body when the size rule accepts the reduced result.
-- | The environment under a lambda: the result receives one argument
-- fewer, and the first lambda of the binding has been passed.
passLambda :: Simpl -> Simpl
passLambda env = env {spCredit = max 0 (spCredit env - 1), spInside = True}

-- | The environment of a part that is not in leading position, such as
-- the head or an argument of an application: no lambda in it is known to
-- be entered once.
noOneShot :: Simpl -> Simpl
noOneShot env = env {spCredit = 0, spInside = False}

-- | The environment of the right-hand side of a local binder: its
-- credit from the analysis of the body, when it has one.
rhsEnv :: Simpl -> Name -> Simpl
rhsEnv env name = env {spCredit = Map.findWithDefault 0 name (spCredits env), spInside = False}

-- | The environment of a top-level body with the given credit: the
-- credits of its local binders come from one analysis of the body.
bodyEnv :: Simpl -> Int -> Expr -> Simpl
bodyEnv env credit body = env {spCredit = credit, spInside = False, spCredits = snd (callArityAnalysis credit False body)}

-- | Simplify an application. The head and the arguments are not in
-- leading position, but what the application becomes is: a copy of the
-- callee that lands here keeps the credit of the position.
simplifyApp :: Simpl -> Expr -> [Arg] -> SimplM Expr
simplifyApp env headExpr args =
  case headExpr of
    -- A binder whose right-hand side is already simplified takes a copy
    -- of it as it is. Only arguments that the use adds give the copy new
    -- facts, so only then is its application rebuilt, with the arguments
    -- of the copy and the new ones together.
    ExVar name
      | Just done <- Map.lookup name (spDone env) -> do
          modify' (\st -> st {ssCopied = Map.insertWith (+) name 1 (ssCopied st)})
          copy <- freshenExpr done
          if null args
            then pure copy
            else do
              args' <- simplifyArgs (noOneShot env) args
              let (doneHead, doneArgs) = collectSpine copy
              rebuildApp env doneHead (doneArgs ++ args')
    _ -> do
      headExpr' <- case headExpr of
        ExVar {} -> pure headExpr
        _ -> simplifyExpr (noOneShot env) headExpr
      args' <- simplifyArgs (noOneShot env) args
      rebuildApp env headExpr' args'

simplifyArgs :: Simpl -> [Arg] -> SimplM [Arg]
simplifyArgs env = mapM (either (pure . Left) (fmap Right . simplifyExpr env))

-- | Rebuild an application whose head and arguments are simplified.
rebuildApp :: Simpl -> Expr -> [Arg] -> SimplM Expr
rebuildApp env headExpr' args' = do
  fired <- fireRule env headExpr' args'
  case headExpr' of
    _ | Just rewritten <- fired -> simplifyExpr env rewritten
    ExVar name
      | Just candidate <- Map.lookup name (spInline env),
        takesArgument env candidate args' -> do
          let original = rebuildSpine headExpr' args'
          before <- get
          result <- inlineCandidate env name candidate args'
          paid <- gets (nestedPaid before)
          let discount = callDiscount env (candidateBody candidate) args' + paid
              growth = exprSize (spEnv env) result - exprSize (spEnv env) original - discount
          accepted <- acceptSite env candidate (siteReduction env (candidateBody candidate) args') paid growth
          if accepted
            then pure result
            else do
              restoreSite before
              pure original
    ExLam {} | not (null args') -> betaReduce env headExpr' args'
    ExTyLam {} | not (null args') -> betaReduce env headExpr' args'
    -- A let in the head of an application is a let around the
    -- application: the binding is evaluated as often as before, and the
    -- arguments move under it once each. Rebuilding the let with 'mkLet'
    -- gives the binding a fresh chance to move to its use, because the
    -- application may have taken it out of a lambda.
    ExLet bind body
      | not (null args'),
        all (either (const True) (unused (binderName (bindBinder bind)))) args' -> do
          inner <- simplifyApp env body args'
          mkLet env bind inner
    ExCase scrutinee binder resultType alternatives
      | not (null args'),
        all (either (const True) (copiable env)) args',
        Just resultType' <- appliedType (spEnv env) resultType args' -> do
          -- The arguments are trivial, so a copy in each alternative costs
          -- no work. The alternative binders are distinct from every name
          -- in scope, so the copies capture nothing.
          alternatives' <- mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> simplifyApp (alternativeEnv env scrutinee binder alternatives alternative) (altRhs alternative) args') alternatives
          pure (ExCase scrutinee binder resultType' alternatives')
    ExRec binds body
      | not (null args'),
        all (either (const True) (\argument -> all ((`unused` argument) . binderName . bindBinder) binds)) args' -> do
          inner <- simplifyApp env body args'
          pure (ExRec binds inner)
    -- A cast on a case, a let or a recursive group in the head of an
    -- application moves into the branches, and the application follows
    -- it there. The lowered code erases the cast, and a call in a branch
    -- then gives the arguments of the application in one call: an @IO@
    -- loop whose recursive call is a case alternative gets the state
    -- token this way.
    ExCast inner coercion
      | not (null args'),
        Just pushed <- castIntoBranches (spEnv env) inner coercion -> do
          pushed' <- pushed
          rebuildApp env pushed' args'
    _ -> do
      speculated <- speculateArguments env headExpr' args'
      case speculated of
        Just result -> pure result
        Nothing -> bindApplication env headExpr' args'

-- | A cast of a case, a let or a recursive group as the same expression
-- with the cast on each branch. The result type of the case becomes the
-- right endpoint of the coercion. A cast under the cast composes with it.
castIntoBranches :: TypeEnv -> Expr -> Coercion -> Maybe (SimplM Expr)
castIntoBranches env inner coercion =
  case inner of
    ExCase scrutinee binder _ alternatives -> do
      (_, right) <- coercionEndpoints env coercion
      pure (ExCase scrutinee binder right <$> mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> mkCast (altRhs alternative) coercion) alternatives)
    ExLet bind body -> Just (ExLet bind <$> mkCast body coercion)
    ExRec binds body -> Just (ExRec binds <$> mkCast body coercion)
    ExCast deeper outer -> castIntoBranches env deeper (CoTrans outer coercion)
    _ -> Nothing

-- | Whether an argument can stand in several alternatives at no cost: a
-- trivial expression that is not a binder whose one use takes a copy of
-- its right-hand side, because each occurrence of such a binder becomes
-- a copy of that right-hand side.
copiable :: Simpl -> Expr -> Bool
copiable env argument =
  isTrivial argument && not (any (`Map.member` spDone env) (Set.toList (exprValueNames argument)))

-- | An application whose head is not copied, with the safe primitive calls
-- in its lazy constructor arguments bound by strict lets first.
bindApplication :: Simpl -> Expr -> [Arg] -> SimplM Expr
bindApplication env headExpr args = do
  bound <- mapM (either (pure . (,) [] . Left) (fmap (fmap Right) . bindLazyPrimitives env . primitiveConstructor env)) args
  pure (foldr ExLet (rebuildSpine headExpr (map snd bound)) (concatMap fst bound))

-- | Move out of the arguments of an application each case that cannot
-- fail and does not evaluate anything: a case on an evaluated variable
-- with one alternative, whose constructor is the only constructor of its
-- type. In a lazy argument, such a case is a thunk that only takes a value
-- apart. Around the application, it runs once, and the argument that is
-- left is often a constructor with a primitive call, which
-- 'bindLazyPrimitives' then binds by a strict let.
--
-- The case gets the type of the application as its result type, so the
-- head must be a constructor or a top-level value whose type is known.
-- The case moves into a larger scope, so its binders are fresh.
speculateArguments :: Simpl -> Expr -> [Arg] -> SimplM (Maybe Expr)
speculateArguments env headExpr args
  | ExVar name <- headExpr,
    any (either (const False) speculable) args,
    Just headType <- lookupHeaderType (spEnv env) name,
    Just resultType <- appliedType (spEnv env) headType args = do
      (wrappers, args') <- mapAndUnzipM (speculate resultType) args
      inner <- bindApplication env headExpr args'
      pure (Just (foldr ($) inner (concat wrappers)))
  | otherwise = pure Nothing
  where
    speculable argument =
      case argument of
        ExCase scrutinee binder _ [Alt (AltData con) [] _ _] ->
          isEvaluated env scrutinee && onlyConstructor (binderType binder) == Just con
        _ -> False
    onlyConstructor ty = do
      tyCon <- typeHead (reduceType (spEnv env) ty)
      [con] <- Map.lookup tyCon (teDataCons (spEnv env))
      pure con
    speculate resultType argument
      | Right value <- argument,
        speculable value = do
          fresh <- freshenExpr value
          pure $ case fresh of
            ExCase scrutinee binder _ [alternative] ->
              ([\inner -> ExCase scrutinee binder resultType [alternative {altRhs = inner}]], Right (altRhs alternative))
            _ -> ([], argument)
      | otherwise = pure ([], argument)

-- | Fire the first active rule that matches an application, if any. The
-- right-hand side is copied with fresh binders, instantiated by the
-- match, and applied to the arguments the left-hand side did not name.
-- Rules are tried before the head is inlined, as in GHC, so that a rule
-- written for a function sees its calls.
--
-- The inliner binds the arguments of a copy to lets, so an argument that
-- a rule wants to see as an application often arrives under lets:
-- @foldr k z (let x = e in build g)@ does not match @foldr k z (build g)@.
-- When no rule matches the arguments as they are, the lazy lets at the
-- front of each value argument come off, and the rules are tried again.
-- A rule that then matches fires, and the lets go around the result, as
-- in GHC's Note [Matching lets]. The lets get fresh binders first, so
-- that they capture no name of another argument. A lazy let only
-- allocates, so the move changes no evaluation. A strict let stays.
fireRule :: Simpl -> Expr -> [Arg] -> SimplM (Maybe Expr)
fireRule env headExpr args =
  case headExpr of
    ExVar name
      | Just rules <- Map.lookup name (spRules env) -> do
          fuel <- gets ssRuleFuel
          let firstMatch current = listToMaybe [(rule, match) | fuel > 0, rule <- rules, Just match <- [matchRule (spEnv env) rule current]]
          case firstMatch args of
            Just (rule, match) -> Just <$> fire rule match
            Nothing
              | fuel > 0,
                any (either (const False) (not . null . fst . frontLets)) args -> do
                  peeled <- traverse peelArgument args
                  let floated = concatMap fst peeled
                  case firstMatch (map snd peeled) of
                    Just (rule, match) -> Just . (\result -> foldr ExLet result floated) <$> fire rule match
                    Nothing -> pure Nothing
              | otherwise -> pure Nothing
    _ -> pure Nothing
  where
    fire rule match = do
      rhs <- freshenExpr (ruleRhs rule)
      modify' (\st -> st {ssRulesFired = ssRulesFired st + 1, ssRuleFuel = ssRuleFuel st - 1})
      let instantiated = substExpr (matchValues match) (substTypeExpr (matchTypes match) rhs)
      pure (rebuildSpine instantiated (matchSurplus match))
    frontLets expr =
      case expr of
        ExLet bind body
          | isLiftedBinder (spEnv env) (bindBinder bind) ->
              let (binds, inner) = frontLets body in (bind : binds, inner)
        _ -> ([], expr)
    peelArgument arg =
      case arg of
        Right expr
          | (binds@(_ : _), inner) <- frontLets expr -> do
              (binds', inner') <- freshenLets binds inner
              pure (binds', Right inner')
        _ -> pure ([], arg)

-- | The discount a call site takes off the growth of inlining, one for
-- each value argument that names a function and that the callee applies.
--
-- Inlining such an argument turns the unknown call in the body of the
-- callee into a direct call of the value the argument names, and drops
-- the closure the unknown call needed. Neither saving is a node of the
-- result, so the size alone never accepts a wrapper whose whole purpose
-- is to call its argument: @bindIO@, @thenIO@, @withForeignPtr@ and the
-- rest stay at a small positive growth for ever.
--
-- An argument that the callee scrutinises earns no discount here. The
-- case of a known constructor reduces while the copy is simplified, so
-- that saving is already a smaller result.
callDiscount :: Simpl -> Expr -> [Arg] -> Int
callDiscount env body args =
  spDiscount env
    * length
      [ ()
      | (binder, argument) <- valueArguments body args,
        valueArity (spArity env) argument > 0,
        saturatedCalls (binderName binder) 1 body > 0
      ]

-- | Pair the value binders of a lambda chain with the value arguments a
-- call gives them.
valueArguments :: Expr -> [Arg] -> [(Binder, Expr)]
valueArguments body args =
  case (body, args) of
    (ExTyLam _ inner, Left _ : rest) -> valueArguments inner rest
    (ExLam binder inner, Right argument : rest) -> (binder, argument) : valueArguments inner rest
    _ -> []

-- | The type of a value of the given type applied to the arguments.
appliedType :: TypeEnv -> Type -> [Arg] -> Maybe Type
appliedType env ty args =
  case args of
    [] -> Just ty
    Left argument : rest -> do
      (binder, body) <- viewForAll env ty
      appliedType env (substType (binderName binder) argument body) rest
    Right _ : rest -> do
      (_, _, _, result) <- viewFun env ty
      appliedType env result rest

-- | Apply a lambda chain to simplified arguments. A trivial argument
-- replaces its parameter. Another argument is bound by a let, which the
-- let rule then simplifies.
betaReduce :: Simpl -> Expr -> [Arg] -> SimplM Expr
betaReduce env expr args =
  case (expr, args) of
    (ExTyLam binder body, Left ty : rest) ->
      betaReduce env (substTypeExpr (Map.singleton (binderName binder) ty) body) rest
    (ExLam binder body, Right argument : rest)
      | isTrivial argument -> betaReduce env (substExpr (Map.singleton (binderName binder) argument) body) rest
      | otherwise -> do
          body' <- betaReduce (bindingEnv env binder argument) body rest
          mkLet env (Bind binder argument) body'
    _ -> simplifyExpr env (rebuildSpine expr args)

-- | Build a cast on a simplified body.
--
-- A reflexive coercion casts nothing. A cast of a cast by the symmetric
-- coercion is the body: the two coercions compose to a reflexive one. A
-- cast of a let is a cast of its body, which brings the two casts of a
-- newtype wrapper together once the binding of the wrapper stands between
-- them.
mkCast :: Expr -> Coercion -> SimplM Expr
mkCast body coercion =
  case coercion of
    CoRefl _ -> pure body
    _ ->
      case body of
        ExCast inner innerCoercion
          | cancels innerCoercion coercion -> pure inner
        ExLet bind inner -> ExLet bind <$> mkCast inner coercion
        _ -> pure (ExCast body coercion)
  where
    cancels = cancelsCoercion

-- | Whether a cast by one coercion undoes a cast by the other.
cancelsCoercion :: Coercion -> Coercion -> Bool
cancelsCoercion left right = left == CoSym right || right == CoSym left

-- | Whether the name occurs nowhere in the expression.
unused :: Name -> Expr -> Bool
unused name expr =
  case occurrences name expr of
    Occurrences count _ -> count == 0

-- | How many more value arguments a value takes before it does work.
--
-- A lambda takes the arguments it binds. A partial application of a known
-- function takes the arguments it still lacks, and its arguments must be
-- trivial, because moving the application under a lambda would otherwise
-- build its thunks once for each call.
--
-- A cast looks through to a partial application but not to a lambda. A
-- saturated call of a known function allocates nothing whatever casts
-- stand between the two, while a lambda that a cast keeps from its
-- arguments still needs its closure.
valueArity :: Map Name Int -> Expr -> Int
valueArity arities expr =
  case expr of
    ExLam _ body -> 1 + valueArity arities body
    ExTyLam _ body -> valueArity arities body
    _ ->
      case castedSpine expr of
        (ExVar name, args)
          | not (isConstructorName name),
            Just arity <- Map.lookup name arities,
            given <- length [() | Right _ <- args],
            given < arity,
            all (either (const True) isTrivial) args ->
              arity - given
        _ -> 0

-- | How many more value arguments the right-hand side of a binding takes
-- before it does work, for the move of the binding to its one call.
--
-- This is 'valueArity', but it also looks through a cast to a lambda.
-- The move is permitted only when the call casts the name back, see
-- 'castedBackUses'. Then 'mkCast' cancels the two casts at the call, and
-- the lambda lands on its arguments.
movableArity :: Map Name Int -> Expr -> (Int, Maybe Coercion)
movableArity arities expr =
  case expr of
    ExCast inner coercion
      | arity <- valueArity arities inner,
        arity > 0 ->
          (arity, Just coercion)
    _ -> (valueArity arities expr, Nothing)

-- | How many occurrences of the name stand directly under a cast that
-- undoes a cast by the coercion.
castedBackUses :: Name -> Coercion -> Expr -> Int
castedBackUses name coercion = go
  where
    go expr =
      case expr of
        ExCast (ExVar var) outer
          | var == name,
            cancelsCoercion coercion outer ->
              1
        ExVar {} -> 0
        ExLit {} -> 0
        ExCoercion {} -> 0
        ExApp function argument -> go function + go argument
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) + go body
        ExRec binds body -> sum (map (go . bindRhs) binds) + go body
        ExCase scrutinee _ _ alternatives -> go scrutinee + sum (map (go . altRhs) alternatives)
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> sum (map go arguments)

-- | The lifted lets around a right-hand side whose innermost body is a
-- value that 'movableArity' accepts.
--
-- @let x = let c = e in \y -> b@ becomes @let c = e in let x = \y -> b@.
-- The binding of @c@ is lazy, so it allocates a thunk in both forms, and
-- @x@ becomes a function instead of a thunk that returns one. A strict
-- binding does work when it is evaluated, so it does not float out.
floatValueLets :: Simpl -> Expr -> Maybe ([Bind], Expr)
floatValueLets env expr =
  case peelLiftedLets expr of
    (binds@(_ : _), inner)
      | fst (movableArity (spArity env) inner) > 0 -> Just (binds, inner)
    _ -> Nothing
  where
    peelLiftedLets e =
      case e of
        ExLet bind body
          | isLiftedBinder (spEnv env) (bindBinder bind) ->
              let (binds, inner) = peelLiftedLets body in (bind : binds, inner)
        _ -> ([], e)

-- | How many occurrences of the name stand in the head of an application
-- that gives it at least the given number of value arguments.
saturatedCalls :: Name -> Int -> Expr -> Int
saturatedCalls name arity = go
  where
    go expr =
      case castedSpine expr of
        (ExVar var, args)
          | var == name,
            length [() | Right _ <- args] >= arity ->
              1 + sum (map argument args)
        (function, args) -> bare function + sum (map argument args)
    argument = either (const 0) go
    -- 'castedSpine' leaves no application, type application or cast in
    -- the head of the spine.
    bare expr =
      case expr of
        ExVar {} -> 0
        ExLit {} -> 0
        ExCoercion {} -> 0
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) + go body
        ExRec binds body -> sum (map (go . bindRhs) binds) + go body
        ExCase scrutinee _ _ alternatives -> go scrutinee + sum (map (go . altRhs) alternatives)
        ExForeignCall _ _ arguments -> sum (map go arguments)
        ExApp {} -> 0
        ExTyApp {} -> 0
        ExCast {} -> 0

-- | Build a let from a simplified right-hand side and a simplified body.
-- A lifted binding with no use is dropped. A strict binding with no use
-- is dropped when its right-hand side is a cheap value, because the
-- evaluation of that value does no work and cannot fail. A lifted
-- binding with one use outside a lambda, and a lifted function whose one
-- use is a call with a value argument, move to their use. A binding whose body is
-- only its binder becomes its right-hand side.
mkLet :: Simpl -> Bind -> Expr -> SimplM Expr
mkLet env bind body
  | isTrivial rhs = simplifyExpr env (substExpr (Map.singleton name rhs) body)
  -- A let whose body is its own binder is its right-hand side. This is
  -- also correct for a strict binding, because the right-hand side is
  -- evaluated at the same point in both forms.
  | ExVar var <- body, var == name = pure rhs
  | Occurrences 0 _ <- uses, lifted || isCheapValue (spArity env) rhs = pure body
  -- The right-hand side is simplified already, so it moves as it is: the
  -- walk simplifies only the body around the use. A chain of calls whose
  -- sites the policy rejects, @f (g (f (g x)))@, otherwise has each
  -- argument simplified again in the copy of each call around it, and
  -- every inner site is decided again at every level. A use that the
  -- walk does not reach keeps the binding.
  --
  -- The walk can copy the one occurrence: beta reduction puts a trivial
  -- argument at every use of its parameter, and a pushed case puts its
  -- arguments in every alternative. Each copy of the occurrence would
  -- take a copy of the right-hand side. The copies are counted, and a
  -- walk that made more than one is done again with the binding kept.
  --
  -- Only the copies of this walk count. The body was simplified before,
  -- and a binding that it keeps is moved again in each walk around it,
  -- so the total from earlier walks is no measure of this one. A count
  -- from them made every such move look like two copies, and each kept
  -- binding then doubled the walks of the bindings inside it: a @do@
  -- block of twenty statements took 2^20 walks.
  | lifted,
    Occurrences 1 False <- uses = do
      before <- get
      let earlier = Map.findWithDefault 0 name (ssCopied before)
      body' <- simplifyExpr env {spDone = Map.insert name rhs (spDone env)} body
      copies <- gets (subtract earlier . Map.findWithDefault 0 name . ssCopied)
      if copies <= 1
        then pure (if unused name body' then body' else ExLet bind body')
        else do
          modify' (const before)
          body'' <- simplifyExpr env body
          pure (ExLet bind body'')
  -- A value whose one use is a call also moves to its use, even from
  -- under a lambda. A lambda that lands on its arguments and a partial
  -- application that its use completes both allocate nothing where they
  -- land, and the call runs the body exactly where it ran it before. A
  -- call that gives fewer arguments than the arity is a partial
  -- application at the use. That application allocates a closure for
  -- each run of the lambda around it, and the reduced value allocates
  -- one closure in its place. The one use is the call, because the whole
  -- body holds one occurrence and the call accounts for it.
  --
  -- A lambda under a cast moves only when the call casts it back, so
  -- that the two casts cancel and the lambda lands on its arguments.
  | lifted,
    Occurrences 1 True <- uses,
    (arity, cast) <- movableArity (spArity env) rhs,
    arity > 0,
    saturatedCalls name 1 body == 1,
    maybe True (\coercion -> castedBackUses name coercion body == 1) cast = do
      copy <- freshenExpr rhs
      simplifyExpr env (substExpr (Map.singleton name copy) body)
  -- A shared case can reduce at each use under a case on the same variable.
  -- Keep the original binding if the copies exceed the growth allowance.
  | lifted,
    not (spSpeculative env),
    ExCase scrutinee _ _ _ <- rhs,
    ExVar {} <- scrutinee,
    ExCase outer _ _ _ <- body,
    scrutinee == outer,
    separateUses body = do
      before <- get
      copy <- freshenExpr rhs
      result <- simplifyExpr env {spSpeculative = True} (substExpr (Map.singleton name copy) body)
      accepted <- acceptGrowth env (exprSize (spEnv env) result - exprSize (spEnv env) (ExLet bind body))
      if accepted
        then pure result
        else do
          restoreSite before
          sinkGroup [bind] (flip (foldr ExLet)) body
  -- Lifted lets around a value float out of the right-hand side. The
  -- binding is then a value, which gets a new chance to move to its use.
  --
  -- The floated binders were beside the body, and a tidied program keeps
  -- only the binders that nest apart: a binder of the body can have the
  -- name of a floated one. Once the floated lets scope over the body, and
  -- once the value moves under such a binder, that name is captured. The
  -- floated lets are copied with fresh binders first, so that the body
  -- captures nothing.
  | lifted,
    Just (floated, value) <- floatValueLets env rhs = do
      (floated', value') <- freshenLets floated value
      inner <- mkLet env (Bind binder value') body
      pure (foldr ExLet inner floated')
  -- A lazy constructor application runs its safe primitive calls first,
  -- so that lowering stores the value instead of a thunk.
  | lifted,
    hasLazyPrimitive (spEnv env) rhs = do
      (binds, rhs') <- bindLazyPrimitives env rhs
      pure (foldr ExLet (ExLet (Bind binder rhs') body) binds)
  -- A constructor of trivial arguments only allocates, so it moves to its
  -- uses: past the lets after it, and into the alternatives of a case
  -- that use it, when an alternative does not use it. Each path still
  -- allocates it at most once, and a path that does not use it allocates
  -- nothing. A worker builds its unboxed parameters again this way, and
  -- often only one branch needs the box.
  | lifted,
    isConstructorOfTrivials rhs =
      pure (fst (sinkGroupWith EveryAlternative (exprValueNames rhs) [bind] (ExLet bind) body))
  -- Any other lifted binding moves into the one alternative that uses
  -- it. A path that does not use it then allocates nothing for it.
  | lifted = sinkGroup [bind] (flip (foldr ExLet)) body
  -- A strict binding whose one use is the scrutinee of the case that
  -- follows it is that case on the right-hand side: the case evaluates it
  -- first either way. Only a comparison with a literal gains from the
  -- move, because a case on the comparison is a case on the compared
  -- value.
  | Occurrences 1 False <- uses,
    ExCase (ExVar scrutinee) caseBinder resultType alternatives <- body,
    scrutinee == name,
    Just rewritten <- literalEqualityCase (spEnv env) rhs caseBinder resultType alternatives =
      pure rewritten
  | otherwise = letOfCase env bind body
  where
    rhs = bindRhs bind
    binder = bindBinder bind
    name = binderName binder
    lifted = isLiftedBinder (spEnv env) binder
    separateUses expr
      | unused name expr = True
      | Occurrences 1 False <- occurrences name expr = True
      | ExCase scrutinee _ _ alternatives <- expr =
          unused name scrutinee && all (separateUses . altRhs) alternatives
      | otherwise = False
    uses = occurrencesUnder (spCredit env) (spInside env) name body

-- | A saturated constructor application whose arguments are trivial.
isConstructorOfTrivials :: Expr -> Bool
isConstructorOfTrivials expr =
  case collectSpine expr of
    (ExVar name, args@(_ : _)) -> isConstructorName name && all (either (const True) isTrivial) args
    _ -> False

-- | Which alternatives of a case a moved group may enter.
data SinkMode
  = -- | Each alternative that uses the group gets a copy, when a path
    -- through the case does not use it. Only a constructor of trivial
    -- arguments moves this way: a copy of it is small, and each path
    -- still allocates it at most once.
    EveryAlternative
  | -- | The group enters a case only when exactly one alternative uses
    -- it. The move copies nothing, so it suits a function or a thunk of
    -- any size.
    OneAlternative
  deriving (Eq)

-- | Move a let or a recursive group into the one alternative of a case
-- that uses it, as 'sinkGroupWith' describes in the 'OneAlternative'
-- mode. The group stays where it was when it enters no case, since a
-- move past lets alone gains nothing.
--
-- The right-hand sides get fresh binders before the move. A loop body
-- often binds the same names as the code around it, and a tidied program
-- has no binder that hides another. The move would put such a binder
-- under its namesake, and the walks that follow, such as the common
-- subexpression map, then see the wrong value for the name. The capture
-- check with every name of the right-hand sides then passes, because
-- only their free names can be the same as a binder they move under.
--
-- A local loop is often used in one branch of its function only. The
-- copy loop of @snappy-hs@ defines two loops above its guards, and its
-- most frequent branch, a short copy, uses neither. Above the guards,
-- each call allocated a closure for each loop.
sinkGroup :: [Bind] -> ([Bind] -> Expr -> Expr) -> Expr -> SimplM Expr
sinkGroup binds wrap body
  | snd (sinkGroupWith OneAlternative (foldMap (exprFreeNames . bindRhs) binds) binds (wrap binds) body) = do
      fresh <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> freshenExpr (bindRhs bind)) binds
      pure (fst (sinkGroupWith OneAlternative (foldMap (exprValueNames . bindRhs) fresh) fresh (wrap fresh) body))
  | otherwise = pure (wrap binds body)

-- | Move a let or a recursive group to its uses, as 'mkLet' describes.
-- The group passes a let or a recursive group whose right-hand sides do
-- not use it, and enters the alternatives of a case that use it, as the
-- mode permits, when the scrutinee does not use it. It stops at anything
-- else, and where a binder would hide one of its own binders or one of
-- the given names of its right-hand sides. A body that does not use the
-- group drops it. The move never enters a lambda, so the group is
-- evaluated at most as often as before.
--
-- The result tells whether the group entered a case or went away.
sinkGroupWith :: SinkMode -> Set Name -> [Bind] -> (Expr -> Expr) -> Expr -> (Expr, Bool)
sinkGroupWith mode rhsNames binds wrap = go
  where
    names = Set.fromList (map (binderName . bindBinder) binds)
    uses expr = not (Set.disjoint names (exprValueNames expr))
    safeBinder binder = Set.notMember (binderName binder) names && Set.notMember (binderName binder) rhsNames
    -- The moved expression, and whether the group entered a case or
    -- went away.
    go expr
      | not (uses expr) = (expr, True)
      | otherwise =
          case expr of
            ExLet inner rest
              | safeBinder (bindBinder inner),
                not (uses (bindRhs inner)) ->
                  ExLet inner `first` go rest
            ExRec inners rest
              | all (safeBinder . bindBinder) inners,
                not (any (uses . bindRhs) inners) ->
                  ExRec inners `first` go rest
            ExCase scrutinee binder ty alternatives
              | movable expr ->
                  (ExCase scrutinee binder ty [alternative {altRhs = fst (go (altRhs alternative))} | alternative <- alternatives], True)
            _ -> (wrap expr, False)
    -- Whether the group can enter a case.
    movable expr =
      case expr of
        ExCase scrutinee binder _ alternatives ->
          safeBinder binder
            && not (uses scrutinee)
            && all (all safeBinder . altBinders) alternatives
            && case mode of
              EveryAlternative -> any (avoids . altRhs) alternatives
              OneAlternative -> length (filter (uses . altRhs) alternatives) == 1
        _ -> False
    -- Whether a path through the expression does not use the group.
    avoids expr
      | not (uses expr) = True
      | otherwise =
          case expr of
            ExLet inner rest
              | safeBinder (bindBinder inner),
                not (uses (bindRhs inner)) ->
                  avoids rest
            ExRec inners rest
              | all (safeBinder . bindBinder) inners,
                not (any (uses . bindRhs) inners) ->
                  avoids rest
            _ -> movable expr

-- | Accept a growth of the program: always when nothing grows, and in
-- budget mode while the allowance and the site limit permit it.
acceptGrowth :: Simpl -> Int -> SimplM Bool
acceptGrowth env growth
  | growth <= 0 = pure True
  | otherwise = do
      allowance <- gets ssAllowance
      if growth <= allowance && growth <= spSiteLimit env
        then do
          modify' (\st -> st {ssAllowance = ssAllowance st - growth})
          pure True
        else pure False

-- | A case of a case with the outer alternatives moved into the inner
-- one, before the result is simplified or accepted.
--
-- The outer alternatives are not copied into the inner alternatives as
-- they are. Each one first becomes a join point, a name for its
-- right-hand side abstracted over its binders, so that the context that
-- is copied is a case whose alternatives are calls. A copy at a known
-- inner tail then selects a call, at the cost of nothing. The size of
-- the result is measured on this form, and the join points are put back
-- in their uses only when the result is taken. The right-hand sides are
-- then simplified once, in the position they end up in, instead of once
-- per inner tail.
data Pushed = Pushed
  { -- | The inner case with the small context in its alternatives.
    pushedSmall :: !Expr,
    -- | The case of the small context on the original scrutinee.
    pushedFallback :: !Expr,
    -- | The join points, each abstracted over the binders of its
    -- alternative.
    pushedJoins :: !(Map Name Expr)
  }

-- | The growth of taking a pushed case: the small forms compared, plus
-- the copies of a join point that the inner tails use more than once,
-- minus one that they do not use at all.
pushedGrowth :: Simpl -> Pushed -> Int
pushedGrowth env pushed =
  expandedSize env (pushedSmall pushed)
    - expandedSize env (pushedFallback pushed)
    + sum
      [ (uses - 1) * expandedSize env rhs
      | (name, rhs) <- Map.toList (pushedJoins pushed),
        Occurrences uses _ <- [occurrences name (pushedSmall pushed)],
        uses /= 1
      ]

-- | The size of an expression with what it becomes: a binder whose one
-- use takes a copy of its right-hand side is that right-hand side at
-- each occurrence. A copy that counts such a binder as one node would
-- otherwise look free and bring the whole right-hand side with it.
expandedSize :: Simpl -> Expr -> Int
expandedSize env expr =
  exprSize (spEnv env) expr
    + sum
      [ count * exprSize (spEnv env) rhs
      | (name, rhs) <- Map.toList (spDone env),
        Occurrences count _ <- [occurrences name expr],
        count > 0
      ]

-- | Put the join points of a pushed case in their uses, and simplify
-- each right-hand side there, once. The walk follows the tails of the
-- pushed expression, where the calls are, and touches nothing else: the
-- rest was simplified before the push.
expandJoins :: Simpl -> Map Name Expr -> Expr -> SimplM Expr
expandJoins env joins = go env
  where
    go env' expr =
      case expr of
        ExLet bind body
          | isTrivial (bindRhs bind) -> go env' (substExpr (Map.singleton (binderName (bindBinder bind)) (bindRhs bind)) body)
          | otherwise -> ExLet bind <$> go (bindingEnv env' (bindBinder bind) (bindRhs bind)) body
        ExRec binds body -> ExRec binds <$> go env' body
        ExCase scrutinee binder resultType alternatives -> do
          alternatives' <-
            mapM
              (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (alternativeEnv env' scrutinee binder alternatives alternative) (altRhs alternative))
              alternatives
          pure (mkCase (spEnv env') scrutinee binder resultType alternatives')
        _ ->
          case collectSpine expr of
            (ExVar name, _) | Map.member name joins -> simplifyExpr env' (substExpr joins expr)
            _ -> pure expr

-- | 'Nothing' when the scrutinee has no case in its tail.
caseOfCaseRaw :: Simpl -> Expr -> Binder -> Type -> [Alt] -> SimplM (Maybe Pushed)
caseOfCaseRaw env scrutinee binder resultType alternatives
  | not (hasCaseTail scrutinee) = pure Nothing
  | otherwise = do
      joined <- mapM joinPoint alternatives
      let joins = Map.fromList [(name, rhs) | (Just (name, rhs), _) <- joined]
          small = map snd joined
      pushed <- pushSmall env binder resultType small scrutinee
      pure (Just (Pushed pushed (ExCase scrutinee binder resultType small) joins))
  where
    hasCaseTail expr =
      case expr of
        ExCase {} -> True
        ExLet _ body -> hasCaseTail body
        ExRec _ body -> hasCaseTail body
        _ -> False
    -- The case binder is a parameter of every join point: each copy of
    -- the small case binds it under a fresh name, and the call passes
    -- that name.
    --
    -- The join point takes fresh parameters. Its body is simplified where
    -- the call lands, inside an alternative of the scrutinee, and a
    -- binder of that alternative may carry the same name as the case
    -- binder or an alternative binder: the two were siblings in the
    -- tidied program. The environment is keyed by name and assumes that
    -- no binder shadows another, so a parameter under the old name would
    -- take the value that the inner binder holds.
    joinPoint alternative
      | isTrivial (altRhs alternative) || not (null (altTypeBinders alternative)) = pure (Nothing, alternative)
      | otherwise = do
          name <- freshLocal (binderName binder)
          let binders = binder : altBinders alternative
              call = rebuildSpine (ExVar name) (map (Right . ExVar . binderName) binders)
          body <- freshenExpr (foldr ExLam (altRhs alternative) binders)
          pure (Just (name, body), alternative {altRhs = call})

-- | Push a small case, whose alternatives call join points, into the
-- tails of a simplified expression. A tail that is a case takes the
-- small case into its own alternatives, a let keeps the small case
-- under it, a known constructor or literal selects an alternative, and
-- any other tail is scrutinised by a copy of the small case. Nothing
-- that was simplified is simplified again.
pushSmall :: Simpl -> Binder -> Type -> [Alt] -> Expr -> SimplM Expr
pushSmall env binder resultType small = go env
  where
    go env' expr =
      case expr of
        ExLet bind body -> ExLet bind <$> go (bindingEnv env' (bindBinder bind) (bindRhs bind)) body
        ExRec binds body -> ExRec binds <$> go env' body
        ExCase scrutinee innerBinder _ alternatives -> do
          alternatives' <-
            mapM
              (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> go (alternativeEnv env' scrutinee innerBinder alternatives alternative) (altRhs alternative))
              alternatives
          pure (mkCase (spEnv env') scrutinee innerBinder resultType alternatives')
        _ -> do
          copy <- freshenExpr (ExCase expr binder resultType small)
          case copy of
            ExCase leaf binder' _ small' -> fromMaybe copy <$> caseOfKnown env' leaf binder' small'
            _ -> pure copy

-- | A local name that no binder in the program uses.
freshLocal :: Name -> SimplM Name
freshLocal name = state (\st -> (name {nameOrigin = OriginLocal (Unique (ssSupply st))}, st {ssSupply = ssSupply st + 1}))

-- | Move a strict let whose right-hand side is a case into the
-- alternatives of that case. The result type of the pushed case is the
-- result type of the body, when the body shows it.
--
-- The push simplifies a copy of the body in each alternative before the
-- size rule decides it. Inside such a trial copy, a strict let of a case
-- stays as it is. Otherwise each nested let would try its own push in
-- each copy of the one around it, and a chain of n such lets, the @do@
-- block of an @IO@ action, would be walked 2^n times. A let that a trial
-- copy keeps is pushed by a later walk, if the push still pays.
letOfCase :: Simpl -> Bind -> Expr -> SimplM Expr
letOfCase env bind body
  | spSpeculative env = pure fallback
  | otherwise =
      case syntacticResultType body of
        Just resultType -> pushIntoCase env (bindRhs bind) (\inner -> ExLet bind {bindRhs = inner} body) resultType fallback
        Nothing -> pure fallback
  where
    fallback = ExLet bind body

-- | The result type of an expression, when its syntax shows it.
syntacticResultType :: Expr -> Maybe Type
syntacticResultType expr =
  case expr of
    ExCase _ _ resultType _ -> Just resultType
    ExLet _ body -> syntacticResultType body
    ExRec _ body -> syntacticResultType body
    _ -> Nothing

-- | Push a context around a case into the alternatives of that case. The
-- context is copied into each alternative, where an inner result that is
-- a known constructor or literal selects the path through the context.
-- The result stands when the size rule accepts it. Let bindings around
-- the inner case move out first.
pushIntoCase :: Simpl -> Expr -> (Expr -> Expr) -> Type -> Expr -> SimplM Expr
pushIntoCase env scrutinee context resultType fallback = do
  pushed <- pushIntoCaseRaw env scrutinee context resultType
  case pushed of
    Nothing -> pure fallback
    Just result -> do
      let growth = exprSize (spEnv env) result - exprSize (spEnv env) fallback
      accepted <- acceptGrowth env growth
      pure (if accepted then result else fallback)

-- | 'pushIntoCase' before the size rule: 'Nothing' when the scrutinee is
-- not a case under its let bindings.
pushIntoCaseRaw :: Simpl -> Expr -> (Expr -> Expr) -> Type -> SimplM (Maybe Expr)
pushIntoCaseRaw env scrutinee context resultType =
  case core of
    ExCase inner innerBinder _ innerAlternatives -> do
      innerAlternatives' <- mapM (push inner innerBinder innerAlternatives) innerAlternatives
      pure (Just (foldr ExLet (mkCase (spEnv env) inner innerBinder resultType innerAlternatives') floated))
    _ -> pure Nothing
  where
    (floated, core) = peelLets scrutinee
    -- The floated bindings scope over the pushed copies, so the copies
    -- see them like the body of the let did.
    floatedEnv = List.foldl' (\acc bind -> bindingEnv acc (bindBinder bind) (bindRhs bind)) env floated
    push inner innerBinder innerAlternatives alternative = do
      copy <- freshenExpr (context (altRhs alternative))
      rhs <- simplifyExpr ((alternativeEnv floatedEnv inner innerBinder innerAlternatives alternative) {spSpeculative = True}) copy
      pure alternative {altRhs = rhs}
    peelLets expr =
      case expr of
        ExLet bind inner -> let (binds, deepest) = peelLets inner in (bind : binds, deepest)
        _ -> ([], expr)

-- | Select the alternative of a case whose scrutinee is a known
-- constructor application. The fields bind the alternative binders, and
-- the scrutinee binds the case binder when the alternative uses it.
caseOfKnown :: Simpl -> Expr -> Binder -> [Alt] -> SimplM (Maybe Expr)
caseOfKnown env scrutinee binder alternatives
  | ExLit literal <- scrutinee =
      pure $ do
        alternative <-
          List.find (matchesLiteral literal . altCon) alternatives
            <|> List.find ((== AltDefault) . altCon) alternatives
        Just (substExpr (Map.singleton (binderName binder) scrutinee) (altRhs alternative))
  | otherwise = caseOfKnownConstructor env scrutinee binder alternatives

caseOfKnownConstructor :: Simpl -> Expr -> Binder -> [Alt] -> SimplM (Maybe Expr)
caseOfKnownConstructor env scrutinee binder alternatives = do
  known <- knownConstructor env scrutinee
  pure $ do
    (binds, con, types, fields) <- known
    alternative <- List.find ((== AltData con) . altCon) alternatives <|> List.find ((== AltDefault) . altCon) alternatives
    let rhs = altRhs alternative
        caseBinderBind
          | Occurrences 0 _ <- occurrences (binderName binder) rhs = Just []
          | isLiftedBinder (spEnv env) binder = Just [Bind binder scrutinee]
          | otherwise = Nothing
    caseBinds <- caseBinderBind
    body <- case altCon alternative of
      AltDefault -> Just rhs
      _ -> do
        existentials <- existentialTypes (spEnv env) con types
        if length existentials /= length (altTypeBinders alternative) || length fields /= length (altBinders alternative)
          then Nothing
          else do
            let typeSubst = Map.fromList (zip (map binderName (altTypeBinders alternative)) existentials)
                -- The type of a field binder can name an existential type
                -- binder of the alternative, which the case no longer binds.
                fieldBinds = zipWith (\field -> Bind field {binderType = substTypes typeSubst (binderType field)}) (altBinders alternative) fields
            Just (foldr ExLet (substTypeExpr typeSubst rhs) fieldBinds)
    Just (foldr ExLet body (binds <> caseBinds))

-- | View a simplified expression as a constructor application. A variable
-- that a known value or a known local binds is unfolded first. The
-- bindings that the unfolding needs come back with the application.
--
-- The casts on the expression and the casts on the unfolded body form
-- one stack, which is reduced before any cast is pushed: a cast by a
-- coercion and a cast by its symmetry cancel, wherever a variable stood
-- between them. A newtype constant is a constructor application under
-- the symmetric axiom, and its use under the axiom is then the bare
-- application.
knownConstructor :: Simpl -> Expr -> SimplM (Maybe ([Bind], Name, [Type], [Expr]))
knownConstructor env expr = do
  known <-
    case collectSpine core of
      (ExVar name, args)
        | isConstructorName name -> pure (Just ([], name, lefts args, rights args, []))
        | Just body <- Map.lookup name (spLocals env) -> unfold body args
        | Just body <- Map.lookup name (spKnown env) -> unfold body args
      _ -> pure Nothing
  pure $ do
    (binds, con, types, fields, innerCasts) <- known
    foldM (flip (pushCast (spEnv env))) (binds, con, types, fields) (reduceCasts (innerCasts <> casts))
  where
    (core, casts) = peelCasts expr
    unfold body args = do
      copy <- freshenExpr body
      pure (peel [] copy args)
    peel binds body args =
      case (body, args) of
        (ExTyLam binder inner, Left ty : rest) -> peel binds (substTypeExpr (Map.singleton (binderName binder) ty) inner) rest
        (ExLam binder inner, Right argument : rest)
          | isTrivial argument -> peel binds (substExpr (Map.singleton (binderName binder) argument) inner) rest
          | otherwise -> peel (Bind binder argument : binds) inner rest
        (ExLam {}, []) -> Nothing
        (ExTyLam {}, []) -> Nothing
        (ExCast inner coercion, []) -> do
          (binds', con, types, fields, innerCasts) <- peel binds inner []
          Just (binds', con, types, fields, innerCasts <> [coercion])
        _ ->
          case collectSpine body of
            (ExVar con, conArgs)
              | isConstructorName con,
                null args ->
                  Just (reverse binds, con, lefts conArgs, rights conArgs, [])
            _ -> Nothing

-- | Reduce a stack of casts, innermost first: a reflexive coercion casts
-- nothing, and a coercion next to its symmetry cancels it.
reduceCasts :: [Coercion] -> [Coercion]
reduceCasts = reverse . List.foldl' step []
  where
    step stack coercion =
      case coercion of
        CoRefl _ -> stack
        _ ->
          case stack of
            top : rest | cancels top coercion -> rest
            _ -> coercion : stack
    cancels left right = left == CoSym right || right == CoSym left

-- | Push a cast on a constructor application into the application.
--
-- A coercion between two applications of one type constructor carries a
-- coercion for each of its arguments, so the same constructor stands at
-- the right-hand arguments once each field carries the coercion that the
-- argument coercions lift its type to. The rule needs the constructor to
-- be a plain one of that type constructor: a constructor with an
-- existential or a refined result type keeps its cast.
pushCast :: TypeEnv -> Coercion -> ([Bind], Name, [Type], [Expr]) -> Maybe ([Bind], Name, [Type], [Expr])
pushCast env coercion (binds, con, types, fields) = do
  (tyCon, argumentCoercions) <- case coercion of
    CoTyConApp name arguments -> Just (name, arguments)
    _ -> Nothing
  conType <- lookupHeaderType env con
  let (binders, fieldTypes, result) = splitConstructorType env conType
      (resultHead, resultArgs) = typeSpine (reduceType env result)
  guard (resultHead == TyCon tyCon)
  guard (resultArgs == map (TyVar . binderName) binders)
  guard (length binders == length types)
  guard (length binders == length argumentCoercions)
  guard (length fieldTypes == length fields)
  types' <- mapM (fmap snd . coercionEndpoints env) argumentCoercions
  let subst = Map.fromList (zip (map binderName binders) argumentCoercions)
  fieldCoercions <- mapM (liftCoercion env subst) fieldTypes
  Just (binds, con, types', zipWith cast fields fieldCoercions)
  where
    cast field fieldCoercion =
      case fieldCoercion of
        CoRefl _ -> field
        _ -> ExCast field fieldCoercion

-- | The coercion that a substitution of coercions for type variables
-- lifts a type to. A type that the substitution does not touch lifts to
-- reflexivity.
--
-- A shape that has no coercion form has no lifting, and neither has one
-- that would put a representational coercion where the form takes a
-- nominal one. An application is such a form, so a class whose fields
-- are not all function types keeps its cast until the lint reads the
-- roles of a type constructor instead of asking every argument of a
-- 'CoTyConApp' to be nominal.
liftCoercion :: TypeEnv -> Map Name Coercion -> Type -> Maybe Coercion
liftCoercion env subst ty
  | Set.disjoint (typeVariables ty) (Map.keysSet subst) = Just (CoRefl ty)
  | otherwise =
      case ty of
        TyVar name -> Map.lookup name subst
        TyApp function argument -> do
          function' <- liftCoercion env subst function
          argument' <- liftCoercion env subst argument
          if isNominalCoercion env function' && isNominalCoercion env argument'
            then Just (CoApp function' argument')
            else Nothing
        TyFun rep1 rep2 argument result
          | Set.disjoint (typeVariables rep1 <> typeVariables rep2) (Map.keysSet subst) ->
              CoFun <$> liftCoercion env subst argument <*> liftCoercion env subst result
        _ -> Nothing

-- | Whether a coercion proves a nominal equality: the forms that take a
-- nominal argument accept only such a coercion.
isNominalCoercion :: TypeEnv -> Coercion -> Bool
isNominalCoercion env coercion =
  case coercion of
    CoVar _ -> True
    CoRefl _ -> True
    CoSym inner -> isNominalCoercion env inner
    CoTrans left right -> isNominalCoercion env left && isNominalCoercion env right
    CoApp function argument -> isNominalCoercion env function && isNominalCoercion env argument
    CoNth _ inner -> isNominalCoercion env inner
    CoFun domain range -> isNominalCoercion env domain && isNominalCoercion env range
    CoForAll _ body -> isNominalCoercion env body
    CoTyConApp _ arguments -> all (isNominalCoercion env) arguments
    CoAxiom name _ ->
      case Map.lookup name (teAxioms env) of
        Just declaration -> axiomRole declaration == Nominal
        Nothing -> False

-- | The universal binders, the field types, and the result type of the
-- header type of a constructor.
splitConstructorType :: TypeEnv -> Type -> ([Binder], [Type], Type)
splitConstructorType env ty =
  case ty of
    TyForAll binder body ->
      let (binders, fields, result) = splitConstructorType env body
       in (binder : binders, fields, result)
    TyFun _ _ argument body ->
      let (binders, fields, result) = splitConstructorType env body
       in (binders, argument : fields, result)
    _ ->
      let reduced = reduceType env ty
       in if reduced == ty then ([], [], ty) else splitConstructorType env reduced

-- | The head of a type application and its arguments, outermost last.
typeSpine :: Type -> (Type, [Type])
typeSpine ty =
  case ty of
    TyApp function argument -> let (headType, args) = typeSpine function in (headType, args <> [argument])
    _ -> (ty, [])

-- | The types of the existential binders of a constructor application: the
-- type arguments that the result type of the constructor does not fix.
existentialTypes :: TypeEnv -> Name -> [Type] -> Maybe [Type]
existentialTypes env con types = do
  conType <- lookupHeaderType env con
  let (foralls, result) = splitForAlls env conType
      free = typeVariables result
  if length foralls /= length types
    then Nothing
    else Just [ty | (binder, ty) <- zip foralls types, binderName binder `Set.notMember` free]

splitForAlls :: TypeEnv -> Type -> ([Binder], Type)
splitForAlls env ty =
  case ty of
    TyForAll binder body ->
      let (binders, result) = splitForAlls env body
       in (binder : binders, result)
    TyFun _ _ _ body -> splitForAlls env body
    _ ->
      let reduced = reduceType env ty
       in if reduced == ty then ([], ty) else splitForAlls env reduced

typeVariables :: Type -> Set Name
typeVariables ty =
  case ty of
    TyVar name -> Set.singleton name
    TyCon {} -> Set.empty
    TyLit {} -> Set.empty
    TyApp function argument -> typeVariables function <> typeVariables argument
    TyFun r1 r2 argument result -> Set.unions (map typeVariables [r1, r2, argument, result])
    TyForAll binder body -> Set.delete (binderName binder) (typeVariables body) <> typeVariables (binderType binder)
    TyEq left right -> typeVariables left <> typeVariables right

-- | An expression that costs nothing to copy.
isTrivial :: Expr -> Bool
isTrivial expr =
  case expr of
    ExVar {} -> True
    ExLit {} -> True
    ExCoercion {} -> True
    ExTyApp body _ -> isTrivial body
    -- A type abstraction runs no code: an instance method that names a
    -- function at its own types, @Λa Λb. bindIO @a @b@, is an alias of
    -- that function.
    ExTyLam _ body -> isTrivial body
    ExCast body _ -> isTrivial body
    _ -> False

collectSpine :: Expr -> (Expr, [Arg])
collectSpine = go []
  where
    go args expr =
      case expr of
        ExApp function argument -> go (Right argument : args) function
        ExTyApp function ty -> go (Left ty : args) function
        _ -> (expr, args)

-- | Collect an application spine through the casts on its head. A cast
-- is erased in the lowered code, so it neither hides a call nor stands
-- between a function and the arguments a call gives it.
castedSpine :: Expr -> (Expr, [Arg])
castedSpine = go []
  where
    go args expr =
      case expr of
        ExApp function argument -> go (Right argument : args) function
        ExTyApp function ty -> go (Left ty : args) function
        ExCast body _ -> go args body
        _ -> (expr, args)

-- | Push a cast on the head of an application spine into the arguments
-- the spine gives it:
--
-- @(f ▷ fun-co g h) x@ becomes @(f (x ▷ sym g)) ▷ h@.
--
-- The two are the same program, because a cast is erased in the lowered
-- code. What the rewrite changes is what the call site sees: a method of
-- a newtype-derived instance reaches its use under one cast for each
-- newtype between the two types, and every such use hides a plain call
-- of a small known function behind a head that is not a variable.
-- 'simplifyApp' inlines only a variable head, so without this rewrite
-- none of those calls is ever a candidate.
--
-- Only a value argument moves. A coercion between two quantified types
-- has no form in 'Coercion', so a type application keeps its cast.
--
-- 'Nothing' means no cast moved, so the caller does not walk the spine
-- again.
pushHeadCasts :: Expr -> Maybe Expr
pushHeadCasts expr =
  case collectSpine expr of
    (ExCast body coercion, args) -> push body coercion args
    _ -> Nothing
  where
    -- Move one value argument under the cast, and then as many more as
    -- the coercion that is left allows.
    push body coercion args =
      case (funCoercion coercion, args) of
        (Just (argCo, resultCo), Right argument : rest) ->
          let applied = ExApp body (mkCoercionCast argument (coSym argCo))
           in Just (fromMaybe (rebuildSpine (mkCoercionCast applied resultCo) rest) (push applied resultCo rest))
        _ -> Nothing

-- | View a coercion between two function types as the coercion of its
-- argument and the coercion of its result. A symmetric coercion of a
-- function coercion is the two symmetric coercions.
funCoercion :: Coercion -> Maybe (Coercion, Coercion)
funCoercion coercion =
  case coercion of
    CoFun argCo resultCo -> Just (argCo, resultCo)
    CoSym (CoFun argCo resultCo) -> Just (coSym argCo, coSym resultCo)
    _ -> Nothing

-- | The symmetric coercion. Two symmetries cancel, and reflexivity is its
-- own symmetry.
coSym :: Coercion -> Coercion
coSym coercion =
  case coercion of
    CoSym inner -> inner
    CoRefl ty -> CoRefl ty
    _ -> CoSym coercion

-- | Cast an unsimplified expression. A reflexive coercion casts nothing.
mkCoercionCast :: Expr -> Coercion -> Expr
mkCoercionCast body coercion =
  case coercion of
    CoRefl _ -> body
    _ -> ExCast body coercion

rebuildSpine :: Expr -> [Arg] -> Expr
rebuildSpine = List.foldl' apply
  where
    apply function arg =
      case arg of
        Left ty -> ExTyApp function ty
        Right argument -> ExApp function argument

-- * Cases on primitive values

-- | Build a case from simplified parts. A default alternative that is a
-- case on the same scrutinee merges into the outer case, when that
-- scrutinee is a variable or a pure primitive call of trivial arguments:
-- evaluating it again gives the value the outer case tested, so the
-- inner alternatives continue the outer ones. An inner alternative that
-- the outer case already covers cannot be reached and is dropped.
mkCase :: TypeEnv -> Expr -> Binder -> Type -> [Alt] -> Expr
mkCase env scrutinee binder resultType originalAlternatives =
  case List.partition ((== AltDefault) . altCon) alternatives of
    -- A case that returns its own binder is its scrutinee: both are
    -- undefined when the scrutinee is, and both are its value otherwise. A
    -- call in the scrutinee then stays a tail call.
    ([Alt AltDefault [] [] (ExVar returned)], [])
      | returned == binderName binder,
        resultType == binderType binder ->
          scrutinee
    ([defaultAlt], others)
      | ExCase inner innerBinder _ innerAlternatives <- altRhs defaultAlt,
        inner == scrutinee,
        isTrivial scrutinee || isPurePrimitiveCall env scrutinee ->
          let renamed = substExpr (Map.singleton (binderName innerBinder) (ExVar (binderName binder)))
              covered = Set.fromList (map altCon others)
              continued =
                [ alternative {altRhs = renamed (altRhs alternative)}
                | alternative <- innerAlternatives,
                  altCon alternative `Set.notMember` covered
                ]
              (innerDefaults, innerOthers) = List.partition ((== AltDefault) . altCon) continued
           in ExCase scrutinee binder resultType (normalizeCaseAlternatives env binder (others <> innerOthers <> innerDefaults))
    _ -> ExCase scrutinee binder resultType alternatives
  where
    alternatives = normalizeCaseAlternatives env binder originalAlternatives

-- | Rewrite a case on a comparison of a value with a literal into a case
-- on the value: @case x ==# 3# of { 1# -> a; _ -> b }@ is
-- @case x of { 3# -> a; _ -> b }@. The case binder of the comparison
-- stands for the literal each alternative selects. The value is then the
-- scrutinee of a case that a later case on the same value merges into.
literalEqualityCase :: TypeEnv -> Expr -> Binder -> Type -> [Alt] -> Maybe Expr
literalEqualityCase env scrutinee binder resultType alternatives = do
  (call, arguments) <- case scrutinee of
    ExForeignCall call [] arguments | foreignCallConvention call == Prim -> Just (call, arguments)
    _ -> Nothing
  negated <- List.lookup (nameText (foreignCallName call)) literalEqualities
  (argumentTypes, resultType') <- foreignSignature env (foreignCallType call)
  resultRep <- reduceType env <$> repOf env resultType'
  (compared, comparedType, literal) <-
    case (arguments, argumentTypes) of
      ([ExLit literal, other], [_, ty]) -> Just (other, ty, literal)
      ([other, ExLit literal], [ty, _]) -> Just (other, ty, literal)
      _ -> Nothing
  let select value = do
        alternative <-
          List.find (matchesLiteral (LitInt resultRep value) . altCon) alternatives
            <|> List.find ((== AltDefault) . altCon) alternatives
        Just (substExpr (Map.singleton (binderName binder) (ExLit (LitInt resultRep value))) (altRhs alternative))
  hit <- select (if negated then 0 else 1)
  miss <- select (if negated then 1 else 0)
  Just
    ( mkCase
        env
        compared
        (Binder (binderName binder) comparedType)
        resultType
        [Alt (AltLit literal) [] [] hit, Alt AltDefault [] [] miss]
    )

-- | The primitive comparisons of a value with a literal, and whether each
-- gives one when the two differ.
literalEqualities :: [(Text, Bool)]
literalEqualities =
  [ ("==#", False),
    ("/=#", True),
    ("eqChar#", False),
    ("neChar#", True),
    ("eqWord#", False),
    ("neWord#", True)
  ]

matchesLiteral :: Literal -> AltCon -> Bool
matchesLiteral literal con =
  case (con, literal) of
    (AltLit (LitInt _ left), LitInt _ right) -> left == right
    (AltLit (LitChar _ left), LitChar _ right) -> left == right
    (AltLit (LitAddr _ left), LitAddr _ right) -> left == right
    _ -> False

-- | Move a let, or a case of one alternative, out of an argument of a
-- primitive call: @f# a (case s of K x -> e)@ is
-- @case s of K x -> f# a e@. The call evaluates an unlifted argument
-- before it runs, so the let or the case runs at the same point in both
-- forms when every argument before it is trivial. The move repeats on
-- the lets and cases inside the moved one.
--
-- In the alternative, a later case on the same scrutinee selects its
-- fields. The read of a word from a byte string, such as @word32be@,
-- reads each byte in an argument of a primitive call. After the move, it
-- evaluates the string once instead of once for each byte.
--
-- A case of more alternatives stays where it is, because the call would
-- be copied into each alternative. The moved expression gets fresh
-- binders, so that they capture no name of the other arguments.
floatPrimitiveArgument :: Simpl -> ForeignCall -> [Type] -> [Expr] -> SimplM (Maybe Expr)
floatPrimitiveArgument env call types arguments
  | foreignCallConvention call /= Prim = pure Nothing
  | otherwise =
      case (primitiveSignature (spEnv env) call types, span isTrivial arguments) of
        (Just (argumentTypes, resultType), (before, argument : after))
          | length argumentTypes == length arguments,
            isUnliftedArgument (argumentTypes !! length before),
            movable argument -> do
              argument' <- freshenExpr argument
              pure (Just (float resultType before after argument'))
        _ -> pure Nothing
  where
    movable expr =
      case expr of
        ExCase _ _ _ [_] -> True
        ExLet {} -> True
        _ -> False
    float resultType before after expr =
      case expr of
        ExCase scrutinee binder _ [alternative] ->
          ExCase scrutinee binder resultType [alternative {altRhs = float resultType before after (altRhs alternative)}]
        ExLet bind body -> ExLet bind (float resultType before after body)
        _ -> ExForeignCall call types (before <> (expr : after))
    -- An argument whose representation is known and is not lifted. A
    -- representation variable can stand for a lifted one.
    isUnliftedArgument ty =
      case reduceType (spEnv env) <$> repOf (spEnv env) ty of
        Just (TyVar _) -> False
        Just _ -> not (isLiftedType (spEnv env) ty)
        Nothing -> False

-- | The argument types and the result type of a primitive call at its
-- type arguments.
primitiveSignature :: TypeEnv -> ForeignCall -> [Type] -> Maybe ([Type], Type)
primitiveSignature env call types = foldM instantiate (foreignCallType call) types >>= foreignSignature env
  where
    instantiate ty argument = do
      (binder, body) <- viewForAll env ty
      Just (substType (binderName binder) argument body)

-- | The argument types and the result type of a foreign type without
-- binders.
foreignSignature :: TypeEnv -> Type -> Maybe ([Type], Type)
foreignSignature env ty =
  case viewFun env ty of
    Just (_, _, argument, result) -> do
      (arguments, final) <- foreignSignature env result
      Just (argument : arguments, final)
    Nothing
      | Just _ <- viewForAll env ty -> Nothing
      | otherwise -> Just ([], ty)

-- | A primitive call that a binder may stand for at every later use: its
-- arguments are trivial, so it reads only values, and its type mentions
-- no state token, so it neither performs an effect nor depends on one.
isPurePrimitiveCall :: TypeEnv -> Expr -> Bool
isPurePrimitiveCall env expr =
  case expr of
    ExForeignCall call types arguments ->
      foreignCallConvention call == Prim
        && null types
        && all isTrivial arguments
        && case foreignSignature env (foreignCallType call) of
          Just (argumentTypes, resultType) -> not (any (mentionsState env) (resultType : argumentTypes))
          Nothing -> False
    _ -> False

-- | The call that makes the state token. Every call gives the same token
-- of no width, so a binder of one call stands for every later call.
isStateToken :: Expr -> Bool
isStateToken expr =
  case expr of
    ExForeignCall call [] [] -> foreignCallConvention call == Prim && nameText (foreignCallName call) == "realWorld#"
    _ -> False

-- | Whether a type mentions a state token or a mutable or address type.
mentionsState :: TypeEnv -> Type -> Bool
mentionsState env ty =
  case typeSpine (reduceType env ty) of
    (TyCon name, args) -> nameText name `elem` ["State#", "MutVar#", "MVar#", "TVar#", "MutableArray#", "MutableByteArray#", "SmallMutableArray#", "MutableArrayArray#", "Weak#", "StablePtr#", "StableName#", "ThreadId#", "BCO", "Addr#"] || any (mentionsState env) args
    (_, args) -> any (mentionsState env) args

-- * Primitive calls in lazy constructor applications

-- | Bind the safe primitive calls in a constructor application to strict
-- lets outside it. The application is in a lazy position, and lowering
-- makes a thunk of a constructor application that has an unlifted operand
-- that is not a value. With the calls bound, it stores the value. The
-- walk goes into the lifted constructor applications among the arguments,
-- which are lazy too, and into nothing else, so every free variable of a
-- call is in scope outside the application.
--
-- A safe call cannot fail and does little work, so it can run when the
-- application is made instead of when it is evaluated.
bindLazyPrimitives :: Simpl -> Expr -> SimplM ([Bind], Expr)
bindLazyPrimitives env expr
  | hasLazyPrimitive (spEnv env) expr,
    (ExVar con, args) <- collectSpine expr = do
      bound <- mapM argument args
      pure (concatMap fst bound, rebuildSpine (ExVar con) (map snd bound))
  | otherwise = pure ([], expr)
  where
    argument arg =
      case arg of
        Left ty -> pure ([], Left ty)
        Right value
          | Just ty <- safePrimitiveCall (spEnv env) value -> do
              name <- freshLocal (Name "argument" SortValue (OriginLocal (Unique 0)))
              pure ([Bind (Binder name ty) value], Right (ExVar name))
          | otherwise -> fmap Right <$> bindLazyPrimitives env value

-- | An argument that is a case that only binds the value of a safe
-- primitive call for a constructor, as the constructor with the call as
-- its argument. A copy of the wrapper of a function with a constructed
-- result gives such a case, @case x +# y of r -> I# r@. In an argument,
-- the case is a thunk, where @I# (x +# y)@ is a constructor whose
-- primitive call 'bindLazyPrimitives' binds in front of the application.
-- A safe primitive cannot fail, so the call can move into the
-- constructor.
primitiveConstructor :: Simpl -> Expr -> Expr
primitiveConstructor env expr =
  case expr of
    ExCase scrutinee binder _ [Alt AltDefault [] [] rhs]
      | isStrictBinder (spEnv env) binder,
        isJust (safePrimitiveCall (spEnv env) scrutinee),
        (ExVar con, args) <- collectSpine rhs,
        isConstructorName con,
        [()] <- [() | Right (ExVar argument) <- args, argument == binderName binder],
        Occurrences 1 False <- occurrences (binderName binder) rhs ->
          substExpr (Map.singleton (binderName binder) scrutinee) rhs
    _ -> expr

-- | Whether a constructor application has a safe primitive call among its
-- arguments or among the arguments of its constructor arguments.
hasLazyPrimitive :: TypeEnv -> Expr -> Bool
hasLazyPrimitive env expr =
  case collectSpine expr of
    (ExVar con, args)
      | isConstructorName con ->
          any (\value -> isJust (safePrimitiveCall env value) || hasLazyPrimitive env value) (rights args)
    _ -> False

-- | The unlifted result type of a primitive call that is safe to run
-- early: a call of an arithmetic, comparison, bit or conversion primitive
-- on trivial arguments or on such calls. It has no effect, it reads no
-- memory, and it cannot fail. A division can fail, so it is not safe.
safePrimitiveCall :: TypeEnv -> Expr -> Maybe Type
safePrimitiveCall env expr =
  case expr of
    ExForeignCall call [] arguments
      | foreignCallConvention call == Prim,
        isSafePrimitive (nameText (foreignCallName call)),
        all (\argument -> isTrivial argument || isJust (safePrimitiveCall env argument)) arguments,
        Just (argumentTypes, resultType) <- foreignSignature env (foreignCallType call),
        not (any (mentionsState env) (resultType : argumentTypes)),
        -- One primitive value: not a tuple, and not a heap object.
        Just (TyCon _) <- reduceType env <$> repOf env resultType,
        not (isLiftedType env resultType) ->
          Just resultType
    _ -> Nothing

-- | The primitives that cannot fail and read no memory.
isSafePrimitive :: Text -> Bool
isSafePrimitive name =
  not (any (`T.isInfixOf` name) ["quot", "rem", "div", "mod", "index", "read", "write", "Addr", "Array"])
    && ( T.all (`elem` ("+-*=/<>#" :: String)) name
           || any (`T.isPrefixOf` name) safePrefixes
           || "To" `T.isInfixOf` name
       )
  where
    safePrefixes =
      [ "plus",
        "minus",
        "times",
        "negate",
        "eq",
        "ne",
        "lt",
        "le",
        "gt",
        "ge",
        "and",
        "or",
        "xor",
        "not",
        "narrow",
        "unchecked",
        "int2",
        "word2",
        "float2",
        "double2",
        "chr#",
        "ord#"
      ]

-- * Occurrences

-- | How often a name occurs, and whether an occurrence sits under a lambda
-- or inside a recursive binding.
data Occurrences = Occurrences !Int !Bool

instance Semigroup Occurrences where
  Occurrences count1 repeated1 <> Occurrences count2 repeated2 =
    Occurrences (count1 + count2) (repeated1 || repeated2)

instance Monoid Occurrences where
  mempty = Occurrences 0 False

occurrences :: Name -> Expr -> Occurrences
occurrences = occurrencesUnder 0 False

-- | The occurrences of a name in an expression whose result receives the
-- given number of arguments from every use, where the first lambda of
-- its binding has been passed when the flag says so. A leading lambda
-- inside the first one that still has a credit is entered at most once
-- per call, so a use under it is not repeated. The leading lambdas are
-- the ones on the path through type lambdas, casts, the body of a let or
-- a recursive group, and the alternatives of a case.
occurrencesUnder :: Int -> Bool -> Name -> Expr -> Occurrences
occurrencesUnder credit0 inside0 name = go credit0 inside0
  where
    go credit inside expr =
      case expr of
        ExVar var
          | var == name -> Occurrences 1 False
          | otherwise -> mempty
        ExLit {} -> mempty
        ExApp function argument -> go 0 False function <> go 0 False argument
        ExTyApp function _ -> go 0 False function
        ExLam _ body
          | inside && credit > 0 -> go (credit - 1) True body
          | otherwise -> repeated (go (max 0 (credit - 1)) True body)
        ExTyLam _ body -> go credit inside body
        ExLet bind body -> go 0 False (bindRhs bind) <> go credit inside body
        ExRec binds body -> repeated (foldMap (go 0 False . bindRhs) binds) <> go credit inside body
        ExCase scrutinee _ _ alternatives -> go 0 False scrutinee <> foldMap (go credit inside . altRhs) alternatives
        ExCast body coercion -> go credit inside body <> coercionUses coercion
        ExCoercion coercion -> coercionUses coercion
        ExForeignCall _ _ arguments -> foldMap (go 0 False) arguments
    coercionUses coercion =
      Occurrences (length (filter (== name) (coercionVariables coercion))) False
    repeated (Occurrences count _) = Occurrences count (count > 0)

-- * Call arity

-- | How an expression uses a name: the fewest value arguments any use
-- gives it, and its occurrences.
data Use = Use !Int !Occurrences

instance Semigroup Use where
  Use args1 occurrences1 <> Use args2 occurrences2 = Use (min args1 args2) (occurrences1 <> occurrences2)

-- | The call arity of every name an expression uses, and the credit of
-- every binder the expression binds, when the result of the expression
-- itself receives the given number of arguments from every use, and the
-- first lambda of its binding has been passed when the flag says so.
-- One walk over the expression gives both.
--
-- The call arity of a name is the fewest value arguments that any use
-- gives it. A use that is not a call, such as the name passed as an
-- argument or returned, shares its closures and gives zero. A closure
-- built by one of the first that many lambdas of a name, after its
-- first lambda, is a partial application that no use shares, so that
-- lambda is entered at most once per call. The arity lets a binding
-- with one use under such a lambda move there, and lets a cast in the
-- head of an application reach the calls under it. This is Breitner's
-- call arity.
--
-- The credit of a let-bound function is the arity of its uses, which
-- its tails pass on to the calls in them: @let y = λz. go x in y a@
-- calls @go@ with two arguments. A let-bound thunk gets the credit only
-- when it is used once and not under a repeated lambda, because its
-- value is a closure that every use shares. The functions of a
-- recursive group get their credits by a fixpoint from the uses in the
-- body and in the group, from the body down; a thunk in a group gets
-- none.
callArityAnalysis :: Int -> Bool -> Expr -> (Map Name Use, Map Name Int)
callArityAnalysis = go
  where
    go credit inside expr =
      case expr of
        ExVar name -> (Map.singleton name (Use credit (Occurrences 1 False)), Map.empty)
        ExLit {} -> (Map.empty, Map.empty)
        ExCoercion coercion -> (coercionUses coercion, Map.empty)
        ExApp function argument -> go (credit + 1) inside function `both` go 0 False argument
        ExTyApp function _ -> go credit inside function
        ExLam _ body
          | inside && credit > 0 -> go (credit - 1) True body
          | otherwise -> repeated (go (max 0 (credit - 1)) True body)
        ExTyLam _ body -> go credit inside body
        ExLet bind body ->
          let name = binderName (bindBinder bind)
              (inBody, bodyCredits) = go credit inside body
              rhsCredit = letCredit (isFunctionRhs (bindRhs bind)) (Map.lookup name inBody)
              (inRhs, rhsCredits) = go rhsCredit False (bindRhs bind)
           in (Map.unionWith (<>) (Map.delete name inBody) inRhs, Map.insert name rhsCredit (Map.union bodyCredits rhsCredits))
        ExRec binds body ->
          let names = map (binderName . bindBinder) binds
              (inBody, bodyCredits) = go credit inside body
              functions = Set.fromList [name | (name, bind) <- zip names binds, isFunctionRhs (bindRhs bind)]
              initial = Map.fromSet (\name -> maybe 0 useArgs (Map.lookup name inBody)) functions
              (credits, analysed) = settle initial
              inGroup = List.foldl' (\acc (inRhs, _) -> Map.unionWith (<>) acc inRhs) Map.empty analysed
              groupCredits = Map.unions (map snd analysed)
           in ( Map.withoutKeys (Map.unionWith (<>) inBody (fst (repeated (inGroup, Map.empty)))) (Set.fromList names),
                Map.unions [credits, bodyCredits, groupCredits]
              )
          where
            -- The group lowers each credit until the uses in the group
            -- agree with it. A credit only goes down, so this ends.
            settle current =
              let analysed = [go (Map.findWithDefault 0 (binderName (bindBinder bind)) current) False (bindRhs bind) | bind <- binds]
                  next = Map.mapWithKey (\name assumed -> minimum (assumed : [useArgs use | (inRhs, _) <- analysed, Just use <- [Map.lookup name inRhs]])) current
               in if next == current then (current, analysed) else settle next
        ExCase scrutinee _ _ alternatives -> List.foldl' both (go 0 False scrutinee) (map (go credit inside . altRhs) alternatives)
        ExCast body coercion -> go credit inside body `both` (coercionUses coercion, Map.empty)
        ExForeignCall _ _ arguments -> List.foldl' both (Map.empty, Map.empty) (map (go 0 False) arguments)
    both (uses1, credits1) (uses2, credits2) = (Map.unionWith (<>) uses1 uses2, Map.union credits1 credits2)
    repeated (uses, credits) = (Map.map (\(Use args (Occurrences count _)) -> Use args (Occurrences count (count > 0))) uses, credits)
    coercionUses coercion = Map.fromListWith (<>) [(name, Use 0 (Occurrences 1 False)) | name <- coercionVariables coercion]
    useArgs (Use args _) = args
    letCredit function use =
      case use of
        Nothing -> 0
        Just (Use args uses)
          | function -> args
          | Occurrences 1 False <- uses -> args
          | otherwise -> 0

-- | Whether a right-hand side is a function: a lambda under type
-- lambdas and casts, so that each call runs it afresh.
isFunctionRhs :: Expr -> Bool
isFunctionRhs expr =
  case expr of
    ExLam {} -> True
    ExTyLam _ body -> isFunctionRhs body
    ExCast body _ -> isFunctionRhs body
    _ -> False

-- | The call arity of every top-level value in the bodies, except the
-- escaping ones, which another scope can use in any way: the exported
-- values of a module and the values a rewrite rule names.
topCallArities :: Set Name -> [Expr] -> Map Name Int
topCallArities escaping bodies =
  Map.filterWithKey (\name _ -> isTop name) (Map.map (\(Use args _) -> args) (List.foldl' (Map.unionWith (<>)) Map.empty [fst (callArityAnalysis 0 False body) | body <- bodies])) `Map.withoutKeys` escaping
  where
    isTop candidate =
      case nameOrigin candidate of
        OriginTop {} -> True
        OriginLocal {} -> False

-- | The values that the rewrite rules of a program name.
ruleValueNames :: [Decl] -> Set Name
ruleValueNames decls = Set.unions [exprValueNames (ruleLhs rule) <> exprValueNames (ruleRhs rule) | DeclRule rule <- decls]

coercionVariables :: Coercion -> [Name]
coercionVariables coercion =
  case coercion of
    CoVar name -> [name]
    CoRefl {} -> []
    CoSym inner -> coercionVariables inner
    CoTrans left right -> coercionVariables left <> coercionVariables right
    CoApp left right -> coercionVariables left <> coercionVariables right
    CoFun left right -> coercionVariables left <> coercionVariables right
    CoForAll _ body -> coercionVariables body
    CoNth _ inner -> coercionVariables inner
    CoTyConApp _ inners -> concatMap coercionVariables inners
    CoAxiom {} -> []

-- | The value-class names an expression uses.
exprValueNames :: Expr -> Set Name
exprValueNames = go
  where
    go expr =
      case expr of
        ExVar name -> Set.singleton name
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (go . bindRhs) binds <> go body
        ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> foldMap go arguments

-- | The value names that occur free in an expression.
exprFreeNames :: Expr -> Set Name
exprFreeNames = go
  where
    go expr =
      case expr of
        ExVar name -> Set.singleton name
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam binder body -> Set.delete (binderName binder) (go body)
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) <> Set.delete (binderName (bindBinder bind)) (go body)
        ExRec binds body -> (foldMap (go . bindRhs) binds <> go body) `Set.difference` Set.fromList (map (binderName . bindBinder) binds)
        ExCase scrutinee binder _ alternatives -> go scrutinee <> Set.delete (binderName binder) (foldMap alternative alternatives)
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> foldMap go arguments
    alternative alt = go (altRhs alt) `Set.difference` Set.fromList (map binderName (altBinders alt))

-- * Substitution

-- | Replace names by expressions. The binders of the target are distinct
-- from every free name of a replacement, so no occurrence is captured.
substExpr :: Map Name Expr -> Expr -> Expr
substExpr subst = go
  where
    go expr =
      case expr of
        ExVar name -> Map.findWithDefault expr name subst
        ExLit {} -> expr
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) ty
        ExLam binder body -> ExLam binder (go body)
        ExTyLam binder body -> ExTyLam binder (go body)
        ExLet bind body -> ExLet bind {bindRhs = go (bindRhs bind)} (go body)
        ExRec binds body -> ExRec [bind {bindRhs = go (bindRhs bind)} | bind <- binds] (go body)
        ExCase scrutinee binder resultType alternatives ->
          ExCase (go scrutinee) binder resultType [alternative {altRhs = go (altRhs alternative)} | alternative <- alternatives]
        ExCast body coercion -> ExCast (go body) (substCoercion coercion)
        ExCoercion coercion -> ExCoercion (substCoercion coercion)
        ExForeignCall call types arguments -> ExForeignCall call types (map go arguments)
    substCoercion coercion =
      case coercion of
        CoVar name ->
          case Map.lookup name subst of
            Just (ExCoercion replacement) -> replacement
            Just (ExVar replacement) -> CoVar replacement
            _ -> coercion
        CoRefl {} -> coercion
        CoSym inner -> CoSym (substCoercion inner)
        CoTrans left right -> CoTrans (substCoercion left) (substCoercion right)
        CoApp left right -> CoApp (substCoercion left) (substCoercion right)
        CoFun left right -> CoFun (substCoercion left) (substCoercion right)
        CoForAll binder body -> CoForAll binder (substCoercion body)
        CoNth index inner -> CoNth index (substCoercion inner)
        CoTyConApp name inners -> CoTyConApp name (map substCoercion inners)
        CoAxiom {} -> coercion

-- | Replace type variables in every type of an expression.
substTypeExpr :: Map Name Type -> Expr -> Expr
substTypeExpr subst = go
  where
    onType = substTypes subst
    onBinder binder = binder {binderType = onType (binderType binder)}
    go expr =
      case expr of
        ExVar {} -> expr
        ExLit literal -> ExLit (onLiteral literal)
        ExApp function argument -> ExApp (go function) (go argument)
        ExTyApp function ty -> ExTyApp (go function) (onType ty)
        ExLam binder body -> ExLam (onBinder binder) (go body)
        ExTyLam binder body -> ExTyLam (onBinder binder) (go body)
        ExLet bind body -> ExLet (onBind bind) (go body)
        ExRec binds body -> ExRec (map onBind binds) (go body)
        ExCase scrutinee binder resultType alternatives ->
          ExCase (go scrutinee) (onBinder binder) (onType resultType) (map onAlt alternatives)
        ExCast body coercion -> ExCast (go body) (onCoercion coercion)
        ExCoercion coercion -> ExCoercion (onCoercion coercion)
        ExForeignCall call types arguments -> ExForeignCall call (map onType types) (map go arguments)
    onBind bind = Bind (onBinder (bindBinder bind)) (go (bindRhs bind))
    onAlt alternative =
      alternative
        { altCon = onAltCon (altCon alternative),
          altTypeBinders = map onBinder (altTypeBinders alternative),
          altBinders = map onBinder (altBinders alternative),
          altRhs = go (altRhs alternative)
        }
    onAltCon con =
      case con of
        AltLit literal -> AltLit (onLiteral literal)
        _ -> con
    onLiteral literal =
      case literal of
        LitInt ty value -> LitInt (onType ty) value
        LitChar ty value -> LitChar (onType ty) value
        LitAddr ty value -> LitAddr (onType ty) value
    onCoercion = substCoercionTypes subst

-- | Replace type variables in every type of a coercion. A quantifier of the
-- coercion hides the substitution of its own variable.
substCoercionTypes :: Map Name Type -> Coercion -> Coercion
substCoercionTypes subst coercion =
  case coercion of
    CoVar {} -> coercion
    CoRefl ty -> CoRefl (onType ty)
    CoSym inner -> CoSym (again inner)
    CoTrans left right -> CoTrans (again left) (again right)
    CoApp left right -> CoApp (again left) (again right)
    CoFun left right -> CoFun (again left) (again right)
    CoForAll binder body ->
      CoForAll binder {binderType = onType (binderType binder)} (substCoercionTypes (Map.delete (binderName binder) subst) body)
    CoNth index inner -> CoNth index (again inner)
    CoTyConApp name inners -> CoTyConApp name (map again inners)
    CoAxiom name types -> CoAxiom name (map onType types)
  where
    onType = substTypes subst
    again = substCoercionTypes subst

-- * Fresh names

-- | Give the binders of a let chain and of its body names that no other
-- binder of the program has, and give the chain back in its two parts.
freshenLets :: [Bind] -> Expr -> SimplM ([Bind], Expr)
freshenLets binds inner = peel (length binds) <$> freshenExpr (foldr ExLet inner binds)
  where
    peel :: Int -> Expr -> ([Bind], Expr)
    peel count expr =
      case expr of
        ExLet bind body
          | count > 0 ->
              let (rest, deepest) = peel (count - 1) body
               in (bind : rest, deepest)
        _ -> ([], expr)

-- | A copy of an expression with fresh binders, from the given unique
-- on, and the next free unique.
freshenExprFrom :: Int -> Expr -> (Expr, Int)
freshenExprFrom supply expr = runState (renameExpr Map.empty expr) supply

-- | Give every binder of an expression a name that no other binder of the
-- program has. The copy can then go into any scope without a clash.
freshenExpr :: Expr -> SimplM Expr
freshenExpr expr = state (\st -> let (result, supply) = runState (renameExpr Map.empty expr) (ssSupply st) in (result, st {ssSupply = supply}))

type FreshM = State Int

freshName :: Name -> FreshM Name
freshName name = state (\supply -> (name {nameOrigin = OriginLocal (Unique supply)}, supply + 1))

renameBinder :: Map Name Name -> Binder -> FreshM (Binder, Map Name Name)
renameBinder renaming binder = do
  ty <- renameType renaming (binderType binder)
  name <- freshName (binderName binder)
  pure (Binder name ty, Map.insert (binderName binder) name renaming)

renameBinders :: Map Name Name -> [Binder] -> FreshM ([Binder], Map Name Name)
renameBinders renaming =
  foldM
    (\(done, current) binder -> (\(binder', next) -> (done <> [binder'], next)) <$> renameBinder current binder)
    ([], renaming)

renameUse :: Map Name Name -> Name -> Name
renameUse renaming name = Map.findWithDefault name name renaming

renameType :: Map Name Name -> Type -> FreshM Type
renameType renaming ty =
  case ty of
    TyVar name -> pure (TyVar (renameUse renaming name))
    TyCon {} -> pure ty
    TyLit {} -> pure ty
    TyApp function argument -> TyApp <$> renameType renaming function <*> renameType renaming argument
    TyFun r1 r2 argument result ->
      TyFun <$> renameType renaming r1 <*> renameType renaming r2 <*> renameType renaming argument <*> renameType renaming result
    TyForAll binder body -> do
      (binder', bodyRenaming) <- renameBinder renaming binder
      TyForAll binder' <$> renameType bodyRenaming body
    TyEq left right -> TyEq <$> renameType renaming left <*> renameType renaming right

renameCoercion :: Map Name Name -> Coercion -> FreshM Coercion
renameCoercion renaming coercion =
  case coercion of
    CoVar name -> pure (CoVar (renameUse renaming name))
    CoRefl ty -> CoRefl <$> renameType renaming ty
    CoSym inner -> CoSym <$> renameCoercion renaming inner
    CoTrans left right -> CoTrans <$> renameCoercion renaming left <*> renameCoercion renaming right
    CoApp left right -> CoApp <$> renameCoercion renaming left <*> renameCoercion renaming right
    CoFun left right -> CoFun <$> renameCoercion renaming left <*> renameCoercion renaming right
    CoForAll binder body -> do
      (binder', bodyRenaming) <- renameBinder renaming binder
      CoForAll binder' <$> renameCoercion bodyRenaming body
    CoNth index inner -> CoNth index <$> renameCoercion renaming inner
    CoTyConApp name inners -> CoTyConApp name <$> mapM (renameCoercion renaming) inners
    CoAxiom name types -> CoAxiom name <$> mapM (renameType renaming) types

renameLiteral :: Map Name Name -> Literal -> FreshM Literal
renameLiteral renaming literal =
  case literal of
    LitInt ty value -> (`LitInt` value) <$> renameType renaming ty
    LitChar ty value -> (`LitChar` value) <$> renameType renaming ty
    LitAddr ty value -> (`LitAddr` value) <$> renameType renaming ty

renameExpr :: Map Name Name -> Expr -> FreshM Expr
renameExpr renaming expr =
  case expr of
    ExVar name -> pure (ExVar (renameUse renaming name))
    ExLit literal -> ExLit <$> renameLiteral renaming literal
    ExApp function argument -> ExApp <$> renameExpr renaming function <*> renameExpr renaming argument
    ExTyApp function ty -> ExTyApp <$> renameExpr renaming function <*> renameType renaming ty
    ExLam binder body -> do
      (binder', bodyRenaming) <- renameBinder renaming binder
      ExLam binder' <$> renameExpr bodyRenaming body
    ExTyLam binder body -> do
      (binder', bodyRenaming) <- renameBinder renaming binder
      ExTyLam binder' <$> renameExpr bodyRenaming body
    ExLet bind body -> do
      rhs <- renameExpr renaming (bindRhs bind)
      (binder', bodyRenaming) <- renameBinder renaming (bindBinder bind)
      ExLet (Bind binder' rhs) <$> renameExpr bodyRenaming body
    ExRec binds body -> do
      (binders, groupRenaming) <- renameBinders renaming (map bindBinder binds)
      rhss <- mapM (renameExpr groupRenaming . bindRhs) binds
      ExRec (zipWith Bind binders rhss) <$> renameExpr groupRenaming body
    ExCase scrutinee binder resultType alternatives -> do
      scrutinee' <- renameExpr renaming scrutinee
      (binder', caseRenaming) <- renameBinder renaming binder
      resultType' <- renameType renaming resultType
      ExCase scrutinee' binder' resultType' <$> mapM (renameAlt caseRenaming) alternatives
    ExCast body coercion -> ExCast <$> renameExpr renaming body <*> renameCoercion renaming coercion
    ExCoercion coercion -> ExCoercion <$> renameCoercion renaming coercion
    ExForeignCall call types arguments -> do
      -- The foreign type is closed. Its binders take fresh names so that
      -- no binder of the declaration repeats.
      foreignType <- renameType Map.empty (foreignCallType call)
      ExForeignCall call {foreignCallType = foreignType} <$> mapM (renameType renaming) types <*> mapM (renameExpr renaming) arguments

renameAlt :: Map Name Name -> Alt -> FreshM Alt
renameAlt renaming alternative = do
  con <- case altCon alternative of
    AltLit literal -> AltLit <$> renameLiteral renaming literal
    other -> pure other
  (typeBinders, typeRenaming) <- renameBinders renaming (altTypeBinders alternative)
  (binders, rhsRenaming) <- renameBinders typeRenaming (altBinders alternative)
  rhs <- renameExpr rhsRenaming (altRhs alternative)
  pure (Alt con typeBinders binders rhs)

-- | The largest local unique of the program.
maxLocalUnique :: Program -> Int
maxLocalUnique program = maximum (0 : mapMaybe localValue (Set.toList names))
  where
    names = foldMap declReferences (programDecls program) <> foldMap declBinderNames (programDecls program)
    localValue name =
      case nameOrigin name of
        OriginLocal (Unique unique) -> Just unique
        OriginTop {} -> Nothing

declBinderNames :: Decl -> Set Name
declBinderNames decl =
  case decl of
    DeclVal declaration -> exprBinderNames (valBody declaration) <> typeBinderNames (valType declaration)
    DeclRule declaration ->
      Set.fromList (map binderName (ruleTypeBinders declaration <> ruleBinders declaration))
        <> foldMap (typeBinderNames . binderType) (ruleTypeBinders declaration <> ruleBinders declaration)
        <> typeBinderNames (ruleType declaration)
        <> exprBinderNames (ruleLhs declaration)
        <> exprBinderNames (ruleRhs declaration)
    DeclType declaration -> Set.fromList (map binderName (typeBinders declaration)) <> foldMap (typeBinderNames . conType) (typeCons declaration)
    DeclSynonym declaration -> Set.fromList (map binderName (synBinders declaration)) <> typeBinderNames (synBody declaration)
    DeclAxiom declaration -> Set.fromList (map binderName (axiomBinders declaration))

typeBinderNames :: Type -> Set Name
typeBinderNames ty =
  case ty of
    TyVar {} -> Set.empty
    TyCon {} -> Set.empty
    TyLit {} -> Set.empty
    TyApp function argument -> typeBinderNames function <> typeBinderNames argument
    TyFun r1 r2 argument result -> foldMap typeBinderNames [r1, r2, argument, result]
    TyForAll binder body -> Set.insert (binderName binder) (typeBinderNames (binderType binder) <> typeBinderNames body)
    TyEq left right -> typeBinderNames left <> typeBinderNames right

exprBinderNames :: Expr -> Set Name
exprBinderNames = go
  where
    binderNames binder = Set.insert (binderName binder) (typeBinderNames (binderType binder))
    go expr =
      case expr of
        ExVar {} -> Set.empty
        ExLit {} -> Set.empty
        ExCoercion {} -> Set.empty
        ExApp function argument -> go function <> go argument
        ExTyApp function ty -> go function <> typeBinderNames ty
        ExLam binder body -> binderNames binder <> go body
        ExTyLam binder body -> binderNames binder <> go body
        ExLet bind body -> binderNames (bindBinder bind) <> go (bindRhs bind) <> go body
        ExRec binds body -> foldMap (\bind -> binderNames (bindBinder bind) <> go (bindRhs bind)) binds <> go body
        ExCase scrutinee binder resultType alternatives ->
          go scrutinee <> binderNames binder <> typeBinderNames resultType <> foldMap altNames alternatives
        ExCast body _ -> go body
        ExForeignCall call types arguments -> typeBinderNames (foreignCallType call) <> foldMap typeBinderNames types <> foldMap go arguments
    altNames alternative = foldMap binderNames (altTypeBinders alternative <> altBinders alternative) <> go (altRhs alternative)
