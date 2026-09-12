{-# LANGUAGE OverloadedStrings #-}

-- | Inline the value declarations of one System FC program.
--
-- The inliner follows the non-recursive inliner of MLton. It walks the
-- value declarations from the leaves of the call graph to the roots. At
-- each use of a non-recursive value it puts a copy of the body in place,
-- reduces the copy, and keeps the result when the size rule of the mode
-- accepts it. A value that nothing uses after this is dropped when the
-- program does not need to keep it.
--
-- Two modes exist. 'InlineShrink' accepts only a use site that does not
-- make the program larger. 'InlineBudget' also accepts a site that makes
-- the program larger while the program size stays under the limit.
--
-- Before the walk, the method bodies of each dictionary get their own
-- top-level helper. A dictionary is then a small constructor application,
-- and a class method applied to a known dictionary reduces to a direct
-- call of the helper.
module Aihc.Fc.Inline
  ( InlineMode (..),
    InlineConfig (..),
    InlineReport (..),
    inlineProgram,
    programSize,
    exprSize,
  )
where

import Aihc.Fc.Imports (declReferences)
import Aihc.Fc.Name
import Aihc.Fc.Syntax
import Aihc.Fc.Tidy (tidyProgram)
import Aihc.Fc.TypeOf (TypeEnv (..), extendBinder, lookupHeaderType, reduceType, repOf, substType, substTypes, typeEnvFromProgram, viewForAll, viewFun)
import Aihc.Fc.Wired (liftedRepName, primPackageFromScopes)
import Aihc.Resolve (PackageId (..))
import Aihc.Tc.Types (Unique (..))
import Control.Applicative ((<|>))
import Control.Monad (foldM, mapAndUnzipM)
import Control.Monad.Trans.State.Strict (State, gets, modify', runState, state)
import Data.Either (lefts, rights)
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text qualified as T

-- | How the inliner decides at a use site.
data InlineMode
  = -- | Accept a use site only when the program does not grow.
    InlineShrink
  | -- | Accept a use site that makes the program grow while the program
    -- size stays at or under the given limit.
    InlineBudget !Int
  deriving (Eq, Show)

data InlineConfig = InlineConfig
  { inlineMode :: !InlineMode,
    -- | The values the program must keep. 'Nothing' keeps every public
    -- value. A root that the program does not declare has no effect.
    inlineRoots :: !(Maybe [Name]),
    -- | The largest growth one use site may cause in budget mode.
    inlineSiteLimit :: !Int,
    -- | The largest number of walks over the program.
    inlineRounds :: !Int
  }
  deriving (Eq, Show)

-- | What one run of the inliner did.
data InlineReport = InlineReport
  { reportSizeBefore :: !Int,
    reportSizeAfter :: !Int,
    reportInlinedSites :: !Int,
    reportDroppedValues :: !Int,
    reportHelpers :: !Int
  }
  deriving (Eq, Show)

-- | The size of a program: the sum of the sizes of its value bodies, plus
-- one for each value.
programSize :: Program -> Int
programSize program =
  sum [1 + exprSize env (valBody declaration) | DeclVal declaration <- programDecls program]
  where
    env = typeEnvFromProgram (fromMaybe (PackageId "aihc-prim") (primPackageFromScopes (programScopes program))) program

-- | The number of nodes of an expression that reach the lowered code. A
-- type, a coercion, and a type application have no size.
--
-- The GRIN CPS conversion copies the continuation of a case in bind
-- position into each of its alternatives. A case whose scrutinee has
-- several tail leaves therefore counts its alternatives once for each
-- leaf, and the body of a strict let counts once for each leaf of its
-- right-hand side. The size then follows the lowered code, and a case of
-- a case is not free. The environment gives the kinds of the type
-- variables in scope, which decide whether a let is strict.
exprSize :: TypeEnv -> Expr -> Int
exprSize env expr =
  case expr of
    ExVar {} -> 1
    ExLit {} -> 1
    ExCoercion {} -> 1
    ExApp function argument -> 1 + exprSize env function + exprSize env argument
    ExTyApp function _ -> exprSize env function
    ExLam _ body -> 1 + exprSize env body
    ExTyLam binder body -> exprSize (extendBinder env binder) body
    ExLet bind body
      | isStrictBinder env (bindBinder bind) -> 1 + exprSize env (bindRhs bind) + tailLeaves env (bindRhs bind) * exprSize env body
      | otherwise -> 1 + exprSize env (bindRhs bind) + exprSize env body
    ExRec binds body -> 1 + sum (map (exprSize env . bindRhs) binds) + exprSize env body
    ExCase scrutinee _ _ alternatives ->
      exprSize env scrutinee + tailLeaves env scrutinee * sum [1 + altSize alternative | alternative <- alternatives]
    ExCast body _ -> exprSize env body
    ExForeignCall _ _ arguments -> 1 + sum (map (exprSize env) arguments)
  where
    altSize alternative = exprSize (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative)

-- | The number of paths through the tail of an expression: one for a
-- value or a call, the sum over the alternatives of a case, and the
-- product of the right-hand side and the body of a strict let.
tailLeaves :: TypeEnv -> Expr -> Int
tailLeaves env expr =
  case expr of
    ExCase _ _ _ alternatives -> max 1 (sum [tailLeaves (List.foldl' extendBinder env (altTypeBinders alternative)) (altRhs alternative) | alternative <- alternatives])
    ExLet bind body
      | isStrictBinder env (bindBinder bind) -> tailLeaves env (bindRhs bind) * tailLeaves env body
      | otherwise -> tailLeaves env body
    ExRec _ body -> tailLeaves env body
    ExCast body _ -> tailLeaves env body
    ExTyLam binder body -> tailLeaves (extendBinder env binder) body
    _ -> 1

-- | A binder that a let evaluates before its body: one whose type is not
-- lifted.
isStrictBinder :: TypeEnv -> Binder -> Bool
isStrictBinder env = not . isLiftedBinder env

-- | Inline the values of a program under the given configuration.
inlineProgram :: InlineConfig -> Program -> (Program, InlineReport)
inlineProgram config program =
  case primPackageFromScopes (programScopes program) of
    Nothing -> (program, InlineReport size0 size0 0 0 0)
    Just primPackage ->
      let env = typeEnvFromProgram primPackage program
          constructors = programConstructors program
          supply0 = maxLocalUnique program + 1
          (lifted, helperCount) = liftDictionaryMethods env constructors (programDecls program)
          state0 = initialInliner config env constructors lifted supply0
          final = runRounds config (inlineRounds config) state0
          decls = rebuildDecls lifted final
          result =
            tidyProgram
              ( pruneImports
                  program {programDecls = decls}
              )
          report =
            InlineReport
              { reportSizeBefore = size0,
                reportSizeAfter = programSize result,
                reportInlinedSites = inSites final,
                reportDroppedValues = length lifted - length decls,
                reportHelpers = helperCount
              }
       in (result, report)
  where
    size0 = programSize program

-- | The data constructors a program knows: those of its type declarations,
-- and those its imports mark. A parsed program marks no constructor in a
-- use, so a use is checked against this set.
programConstructors :: Program -> Set Name
programConstructors program =
  Set.fromList
    ( [conName constructor | DeclType declaration <- programDecls program, constructor <- typeCons declaration]
        <> [name | name <- Map.keys (importHeaders (programImports program)), nameSort name == SortDataConstructor]
    )

-- * Driver

data Inliner = Inliner
  { inEnv :: !TypeEnv,
    inConstructors :: !(Set Name),
    inDecls :: !(Map Name ValDecl),
    inBodies :: !(Map Name Expr),
    -- | The values each body references.
    inRefs :: !(Map Name (Set Name)),
    inTotal :: !Int,
    inSupply :: !Int,
    inSites :: !Int,
    inRoots :: !(Set Name)
  }

initialInliner :: InlineConfig -> TypeEnv -> Set Name -> [Decl] -> Int -> Inliner
initialInliner config env constructors decls supply =
  Inliner
    { inEnv = env,
      inConstructors = constructors,
      inDecls = declarations,
      inBodies = bodies,
      inRefs = Map.map (valueReferences declarations) bodies,
      inTotal = sum [1 + exprSize env body | body <- Map.elems bodies],
      inSupply = supply,
      inSites = 0,
      inRoots = roots
    }
  where
    declarations = Map.fromList [(valName declaration, declaration) | DeclVal declaration <- decls]
    bodies = Map.map valBody declarations
    roots =
      case inlineRoots config of
        Nothing -> Map.keysSet (Map.filter ((== Pub) . valVis) declarations)
        Just names -> Set.fromList names

valueReferences :: Map Name ValDecl -> Expr -> Set Name
valueReferences declarations body =
  Set.filter (`Map.member` declarations) (exprValueNames body)

rebuildDecls :: [Decl] -> Inliner -> [Decl]
rebuildDecls decls final = mapMaybe rebuild decls
  where
    rebuild decl =
      case decl of
        DeclVal declaration ->
          case Map.lookup (valName declaration) (inBodies final) of
            Nothing -> Nothing
            Just body -> Just (DeclVal declaration {valBody = body})
        _ -> Just decl

runRounds :: InlineConfig -> Int -> Inliner -> Inliner
runRounds config rounds st
  | rounds <= 0 = st
  | otherwise =
      let before = inSites st
          st' = dropUnused (inlineRound config st)
       in if inSites st' == before then st' else runRounds config (rounds - 1) st'

-- | Walk the values from the leaves of the call graph to its roots, and
-- inline into each body the candidates that it references.
inlineRound :: InlineConfig -> Inliner -> Inliner
inlineRound config st0 = List.foldl' step st0 (stronglyConnComp graph)
  where
    graph = [(name, name, Set.toList references) | (name, references) <- Map.toList (inRefs st0)]
    counts = occurrenceCounts (Map.elems (inBodies st0))
    known = knownValues st0
    recursive = Set.fromList (concat [names | CyclicSCC names <- stronglyConnComp graph])
    step st scc =
      case scc of
        AcyclicSCC name -> simplifyValue config counts known recursive st name
        CyclicSCC names -> List.foldl' (simplifyValue config counts known recursive) st names

-- | Simplify one body with the candidates it references.
simplifyValue :: InlineConfig -> Map Name Int -> Map Name Expr -> Set Name -> Inliner -> Name -> Inliner
simplifyValue config counts known recursive st name =
  case Map.lookup name (inBodies st) of
    Nothing -> st
    Just body ->
      let references = Map.findWithDefault Set.empty name (inRefs st)
          candidates =
            Map.fromList
              [ (callee, Candidate calleeBody (unconditional callee calleeBody))
              | callee <- Set.toList references,
                callee /= name,
                callee `Set.notMember` recursive,
                Just calleeBody <- [Map.lookup callee (inBodies st)],
                isInlinable calleeBody
              ]
       in if Map.null candidates && Map.null known
            then st
            else
              let simpl =
                    Simpl
                      { spEnv = inEnv st,
                        spConstructors = inConstructors st,
                        spInline = candidates,
                        spKnown = known,
                        spArity = arities,
                        spLocals = Map.empty,
                        spSiteLimit = inlineSiteLimit config
                      }
                  allowance =
                    case inlineMode config of
                      InlineShrink -> 0
                      InlineBudget limit -> max 0 (limit - inTotal st)
                  (body', simplState) =
                    runState (simplifyExpr simpl body) (SimplState (inSupply st) allowance 0)
                  oldSize = exprSize (inEnv st) body
                  newSize = exprSize (inEnv st) body'
               in st
                    { inBodies = Map.insert name body' (inBodies st),
                      inRefs = Map.insert name (valueReferences (inDecls st) body') (inRefs st),
                      inTotal = inTotal st + newSize - oldSize,
                      inSupply = ssSupply simplState,
                      inSites = inSites st + ssInlined simplState
                    }
  where
    arities = Map.map functionArity (inBodies st)
    -- A removable value that is inlined at every use goes away. When the
    -- copies together are no larger than the value, every site takes it.
    unconditional callee calleeBody =
      removable callee
        && let size = exprSize (inEnv st) calleeBody
               uses = Map.findWithDefault 0 callee counts
            in uses * (size - 1) - (size + 1) <= 0
    removable callee = callee `Set.notMember` inRoots st

-- | Drop every value that no root reaches.
dropUnused :: Inliner -> Inliner
dropUnused st =
  st
    { inBodies = Map.restrictKeys (inBodies st) reachable,
      inRefs = Map.restrictKeys (inRefs st) reachable,
      inTotal = sum [1 + exprSize (inEnv st) body | body <- Map.elems (Map.restrictKeys (inBodies st) reachable)]
    }
  where
    reachable = close Set.empty (Set.toList (Set.filter (`Map.member` inBodies st) (inRoots st)))
    close visited pending =
      case pending of
        [] -> visited
        name : rest
          | Set.member name visited -> close visited rest
          | otherwise ->
              close (Set.insert name visited) (Set.toList (Map.findWithDefault Set.empty name (inRefs st)) <> rest)

-- | How often each value occurs in the bodies.
occurrenceCounts :: [Expr] -> Map Name Int
occurrenceCounts = List.foldl' (\counts body -> Map.unionWith (+) counts (countTopUses body)) Map.empty

countTopUses :: Expr -> Map Name Int
countTopUses = go
  where
    go expr =
      case expr of
        ExVar name
          | isTop name -> Map.singleton name 1
          | otherwise -> Map.empty
        ExLit {} -> Map.empty
        ExCoercion {} -> Map.empty
        ExApp function argument -> Map.unionWith (+) (go function) (go argument)
        ExTyApp function _ -> go function
        ExLam _ body -> go body
        ExTyLam _ body -> go body
        ExLet bind body -> Map.unionWith (+) (go (bindRhs bind)) (go body)
        ExRec binds body -> List.foldl' (Map.unionWith (+)) (go body) (map (go . bindRhs) binds)
        ExCase scrutinee _ _ alternatives -> List.foldl' (Map.unionWith (+)) (go scrutinee) (map (go . altRhs) alternatives)
        ExCast body _ -> go body
        ExForeignCall _ _ arguments -> List.foldl' (Map.unionWith (+)) Map.empty (map go arguments)
    isTop name =
      case nameOrigin name of
        OriginTop {} -> True
        OriginLocal {} -> False

-- | The values whose body is a cheap constructor application under
-- lambdas. A case on such a value selects a field without the case.
knownValues :: Inliner -> Map Name Expr
knownValues st = Map.filter (isKnownConstructor (inConstructors st) arities) (inBodies st)
  where
    arities = Map.map functionArity (inBodies st)

isKnownConstructor :: Set Name -> Map Name Int -> Expr -> Bool
isKnownConstructor constructors arities expr =
  case expr of
    ExTyLam _ body -> isKnownConstructor constructors arities body
    ExLam _ body -> isKnownConstructor constructors arities body
    _ ->
      case collectSpine expr of
        (ExVar name, args)
          | Set.member name constructors -> all (either (const True) (isCheapValue constructors arities)) args
        _ -> False

-- | A body that can be inlined: a function, which is inlined at a call
-- that gives every parameter, or a trivial value. A constructor
-- application is not inlined as a value: a case on it selects a field
-- through 'knownConstructor' instead, and a copy at any other site only
-- allocates what the shared value already holds.
isInlinable :: Expr -> Bool
isInlinable body = functionArity body > 0 || isTrivial body

-- | An expression that does no work when it is evaluated: a literal, a
-- variable, a lambda, or a constructor or partial application of cheap
-- arguments.
isCheapValue :: Set Name -> Map Name Int -> Expr -> Bool
isCheapValue constructors arities expr =
  case expr of
    ExLit {} -> True
    ExVar {} -> True
    ExCoercion {} -> True
    ExLam {} -> True
    ExTyLam _ body -> isCheapValue constructors arities body
    ExCast body _ -> isCheapValue constructors arities body
    ExTyApp body _ -> isCheapValue constructors arities body
    ExApp {} ->
      case collectSpine expr of
        (ExVar name, args)
          | Set.member name constructors -> cheapArgs args
          | Just arity <- Map.lookup name arities -> length [() | Right _ <- args] < arity && cheapArgs args
        _ -> False
    _ -> False
  where
    cheapArgs = all (either (const True) (isCheapValue constructors arities))

functionArity :: Expr -> Int
functionArity expr =
  case expr of
    ExLam _ body -> 1 + functionArity body
    ExTyLam _ body -> functionArity body
    _ -> 0

-- * Simplifier

data Candidate = Candidate
  { candidateBody :: !Expr,
    -- | Take every site, whatever its growth.
    candidateUnconditional :: !Bool
  }

data Simpl = Simpl
  { spEnv :: !TypeEnv,
    spConstructors :: !(Set Name),
    spInline :: !(Map Name Candidate),
    spKnown :: !(Map Name Expr),
    spArity :: !(Map Name Int),
    -- | Local bindings whose right-hand side is a known constructor
    -- application.
    spLocals :: !(Map Name Expr),
    spSiteLimit :: !Int
  }

data SimplState = SimplState
  { ssSupply :: !Int,
    -- | The growth the remaining sites may still cause.
    ssAllowance :: !Int,
    ssInlined :: !Int
  }

type SimplM = State SimplState

type Arg = Either Type Expr

simplifyExpr :: Simpl -> Expr -> SimplM Expr
simplifyExpr env expr =
  case expr of
    ExVar {} -> simplifyApp env expr []
    ExLit {} -> pure expr
    ExCoercion {} -> pure expr
    ExApp {} -> uncurry (simplifyApp env) (collectSpine expr)
    ExTyApp {} -> uncurry (simplifyApp env) (collectSpine expr)
    ExLam binder body -> ExLam binder <$> simplifyExpr env body
    ExTyLam binder body -> ExTyLam binder <$> simplifyExpr (extendTypeBinder env binder) body
    ExLet bind body -> do
      rhs <- simplifyExpr env (bindRhs bind)
      let binder = bindBinder bind
      if isTrivial rhs
        then simplifyExpr env (substExpr (Map.singleton (binderName binder) rhs) body)
        else do
          let bodyEnv
                | isKnownConstructor (spConstructors env) (spArity env) rhs = env {spLocals = Map.insert (binderName binder) rhs (spLocals env)}
                | otherwise = env
          body' <- simplifyExpr bodyEnv body
          mkLet env (Bind binder rhs) body'
    ExRec binds body -> do
      binds' <- mapM (\bind -> (\rhs -> bind {bindRhs = rhs}) <$> simplifyExpr env (bindRhs bind)) binds
      ExRec binds' <$> simplifyExpr env body
    ExCase scrutinee binder resultType alternatives -> do
      scrutinee' <- simplifyExpr env scrutinee
      reduced <- caseOfKnown env scrutinee' binder alternatives
      case reduced of
        Just result -> simplifyExpr env result
        Nothing -> do
          alternatives' <- mapM (simplifyAlt env scrutinee' binder) alternatives
          caseOfCase env scrutinee' binder resultType alternatives'
    ExCast body coercion -> (`ExCast` coercion) <$> simplifyExpr env body
    ExForeignCall call types arguments -> ExForeignCall call types <$> mapM (simplifyExpr env) arguments

-- | Simplify an alternative. Inside a constructor alternative, the case
-- binder and a scrutinee variable are known to be that constructor
-- applied to the alternative binders.
simplifyAlt :: Simpl -> Expr -> Binder -> Alt -> SimplM Alt
simplifyAlt env scrutinee binder alternative = do
  let typeEnv = List.foldl' extendTypeBinder env (altTypeBinders alternative)
      known =
        case altCon alternative of
          AltData con -> constructorApplication (spEnv env) con (binderType binder) alternative
          _ -> Nothing
      scrutineeName =
        case scrutinee of
          ExVar name -> [name]
          _ -> []
      altEnv =
        case known of
          Just application ->
            typeEnv {spLocals = List.foldl' (\locals name -> Map.insert name application locals) (spLocals typeEnv) (binderName binder : scrutineeName)}
          Nothing -> typeEnv
  rhs <- simplifyExpr altEnv (altRhs alternative)
  pure alternative {altRhs = rhs}

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
    typeSpine ty =
      case ty of
        TyApp function argument -> let (headType, args) = typeSpine function in (headType, args <> [argument])
        _ -> (ty, [])

extendTypeBinder :: Simpl -> Binder -> Simpl
extendTypeBinder env binder = env {spEnv = extendBinder (spEnv env) binder}

-- | Simplify an application spine. The head and the arguments are
-- simplified first. A head that names a candidate is replaced by a copy of
-- its body when the size rule accepts the reduced result.
simplifyApp :: Simpl -> Expr -> [Arg] -> SimplM Expr
simplifyApp env headExpr args = do
  headExpr' <- case headExpr of
    ExVar {} -> pure headExpr
    _ -> simplifyExpr env headExpr
  args' <- mapM (either (pure . Left) (fmap Right . simplifyExpr env)) args
  case headExpr' of
    ExVar name
      | Just candidate <- Map.lookup name (spInline env),
        length [() | Right _ <- args'] >= functionArity (candidateBody candidate) -> do
          let original = rebuildSpine headExpr' args'
              inner = env {spInline = Map.delete name (spInline env)}
          copy <- freshenExpr (candidateBody candidate)
          result <- betaReduce inner copy args'
          let growth = exprSize (spEnv env) result - exprSize (spEnv env) original
          accepted <- if candidateUnconditional candidate then pure True else acceptGrowth env growth
          if accepted
            then do
              modify' (\st -> st {ssInlined = ssInlined st + 1})
              pure result
            else pure original
    ExLam {} | not (null args') -> betaReduce env headExpr' args'
    ExTyLam {} | not (null args') -> betaReduce env headExpr' args'
    ExCase scrutinee binder resultType alternatives
      | not (null args'),
        all (either (const True) isTrivial) args',
        Just resultType' <- appliedType (spEnv env) resultType args' -> do
          -- The arguments are trivial, so a copy in each alternative costs
          -- no work. The alternative binders are distinct from every name
          -- in scope, so the copies capture nothing.
          alternatives' <- mapM (\alternative -> (\rhs -> alternative {altRhs = rhs}) <$> simplifyApp env (altRhs alternative) args') alternatives
          pure (ExCase scrutinee binder resultType' alternatives')
    _ -> pure (rebuildSpine headExpr' args')

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
          let bodyEnv
                | isKnownConstructor (spConstructors env) (spArity env) argument = env {spLocals = Map.insert (binderName binder) argument (spLocals env)}
                | otherwise = env
          body' <- betaReduce bodyEnv body rest
          mkLet env (Bind binder argument) body'
    _ -> simplifyExpr env (rebuildSpine expr args)

-- | Build a let from a simplified right-hand side and a simplified body.
-- A lifted binding with no use is dropped. A lifted binding with one use
-- outside a lambda moves to its use.
mkLet :: Simpl -> Bind -> Expr -> SimplM Expr
mkLet env bind body
  | isTrivial rhs = simplifyExpr env (substExpr (Map.singleton name rhs) body)
  | lifted, Occurrences 0 _ <- uses = pure body
  | lifted,
    Occurrences 1 False <- uses = do
      copy <- freshenExpr rhs
      simplifyExpr env (substExpr (Map.singleton name copy) body)
  | lifted = pure (ExLet bind body)
  | otherwise = letOfCase env bind body
  where
    rhs = bindRhs bind
    binder = bindBinder bind
    name = binderName binder
    lifted = isLiftedBinder (spEnv env) binder
    uses = occurrences name body

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

-- | Move a case whose scrutinee is a case into the alternatives of the
-- inner case.
caseOfCase :: Simpl -> Expr -> Binder -> Type -> [Alt] -> SimplM Expr
caseOfCase env scrutinee binder resultType alternatives =
  pushIntoCase env scrutinee (\inner -> ExCase inner binder resultType alternatives) resultType fallback
  where
    fallback = ExCase scrutinee binder resultType alternatives

-- | Move a strict let whose right-hand side is a case into the
-- alternatives of that case. The result type of the pushed case is the
-- result type of the body, when the body shows it.
letOfCase :: Simpl -> Bind -> Expr -> SimplM Expr
letOfCase env bind body =
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
pushIntoCase env scrutinee context resultType fallback =
  case core of
    ExCase inner innerBinder _ innerAlternatives -> do
      innerAlternatives' <- mapM push innerAlternatives
      let result = foldr ExLet (ExCase inner innerBinder resultType innerAlternatives') floated
          growth = exprSize (spEnv env) result - exprSize (spEnv env) fallback
      accepted <- acceptGrowth env growth
      pure (if accepted then result else fallback)
    _ -> pure fallback
  where
    (floated, core) = peelLets scrutinee
    push alternative = do
      copy <- freshenExpr (context (altRhs alternative))
      rhs <- simplifyExpr (List.foldl' extendTypeBinder env (altTypeBinders alternative)) copy
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
  where
    matchesLiteral literal con =
      case (con, literal) of
        (AltLit (LitInt _ left), LitInt _ right) -> left == right
        (AltLit (LitChar _ left), LitChar _ right) -> left == right
        (AltLit (LitAddr _ left), LitAddr _ right) -> left == right
        _ -> False

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
                fieldBinds = zipWith Bind (altBinders alternative) fields
            Just (foldr ExLet (substTypeExpr typeSubst rhs) fieldBinds)
    Just (foldr ExLet body (binds <> caseBinds))

-- | View a simplified expression as a constructor application. A variable
-- that a known value or a known local binds is unfolded first. The
-- bindings that the unfolding needs come back with the application.
knownConstructor :: Simpl -> Expr -> SimplM (Maybe ([Bind], Name, [Type], [Expr]))
knownConstructor env expr =
  case collectSpine expr of
    (ExVar name, args)
      | Set.member name (spConstructors env) -> pure (Just ([], name, lefts args, rights args))
      | Just body <- Map.lookup name (spLocals env) -> unfold body args
      | Just body <- Map.lookup name (spKnown env) -> unfold body args
    _ -> pure Nothing
  where
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
        _ ->
          case collectSpine body of
            (ExVar con, conArgs)
              | Set.member con (spConstructors env),
                null args ->
                  Just (reverse binds, con, lefts conArgs, rights conArgs)
            _ -> Nothing

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
    TyApp function argument -> typeVariables function <> typeVariables argument
    TyFun r1 r2 argument result -> Set.unions (map typeVariables [r1, r2, argument, result])
    TyForAll binder body -> Set.delete (binderName binder) (typeVariables body) <> typeVariables (binderType binder)
    TyEq left right -> typeVariables left <> typeVariables right

isLiftedBinder :: TypeEnv -> Binder -> Bool
isLiftedBinder env binder =
  case reduceType env <$> repOf env (binderType binder) of
    Just (TyCon name) -> nameText name == "LiftedRep"
    Just (TyApp (TyCon boxed) (TyCon levity)) ->
      nameText boxed == "BoxedRep" && nameText levity == "Lifted"
    _ -> False

-- | An expression that costs nothing to copy.
isTrivial :: Expr -> Bool
isTrivial expr =
  case expr of
    ExVar {} -> True
    ExLit {} -> True
    ExCoercion {} -> True
    ExTyApp body _ -> isTrivial body
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

rebuildSpine :: Expr -> [Arg] -> Expr
rebuildSpine = List.foldl' apply
  where
    apply function arg =
      case arg of
        Left ty -> ExTyApp function ty
        Right argument -> ExApp function argument

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
occurrences name = go
  where
    go expr =
      case expr of
        ExVar var
          | var == name -> Occurrences 1 False
          | otherwise -> mempty
        ExLit {} -> mempty
        ExApp function argument -> go function <> go argument
        ExTyApp function _ -> go function
        ExLam _ body -> repeated (go body)
        ExTyLam _ body -> go body
        ExLet bind body -> go (bindRhs bind) <> go body
        ExRec binds body -> repeated (foldMap (go . bindRhs) binds) <> go body
        ExCase scrutinee _ _ alternatives -> go scrutinee <> foldMap (go . altRhs) alternatives
        ExCast body coercion -> go body <> coercionUses coercion
        ExCoercion coercion -> coercionUses coercion
        ExForeignCall _ _ arguments -> foldMap go arguments
    coercionUses coercion =
      Occurrences (length (filter (== name) (coercionVariables coercion))) False
    repeated (Occurrences count _) = Occurrences count (count > 0)

coercionVariables :: Coercion -> [Name]
coercionVariables coercion =
  case coercion of
    CoVar name -> [name]
    CoRefl {} -> []
    CoSym inner -> coercionVariables inner
    CoTrans left right -> coercionVariables left <> coercionVariables right
    CoApp left right -> coercionVariables left <> coercionVariables right
    CoFun left right -> coercionVariables left <> coercionVariables right
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
    onCoercion coercion =
      case coercion of
        CoVar {} -> coercion
        CoRefl ty -> CoRefl (onType ty)
        CoSym inner -> CoSym (onCoercion inner)
        CoTrans left right -> CoTrans (onCoercion left) (onCoercion right)
        CoApp left right -> CoApp (onCoercion left) (onCoercion right)
        CoFun left right -> CoFun (onCoercion left) (onCoercion right)
        CoNth index inner -> CoNth index (onCoercion inner)
        CoTyConApp name inners -> CoTyConApp name (map onCoercion inners)
        CoAxiom name types -> CoAxiom name (map onType types)

-- * Fresh names

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
    DeclType declaration -> Set.fromList (map binderName (typeBinders declaration)) <> foldMap (typeBinderNames . conType) (typeCons declaration)
    DeclSynonym declaration -> Set.fromList (map binderName (synBinders declaration)) <> typeBinderNames (synBody declaration)
    DeclAxiom declaration -> Set.fromList (map binderName (axiomBinders declaration))

typeBinderNames :: Type -> Set Name
typeBinderNames ty =
  case ty of
    TyVar {} -> Set.empty
    TyCon {} -> Set.empty
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

-- * Dictionary methods

-- | Give each method body of a dictionary its own top-level helper. The
-- dictionary then applies the constructor to the helpers, so a method
-- selection on a known dictionary becomes a direct reference.
--
-- A dictionary is a value whose body is a chain of lambdas around an
-- application of a dictionary constructor. Each non-trivial argument of
-- the constructor becomes a helper that takes the lambda parameters of the
-- dictionary. Returns the declarations with the helpers, and the number of
-- helpers.
liftDictionaryMethods :: TypeEnv -> Set Name -> [Decl] -> ([Decl], Int)
liftDictionaryMethods env constructors decls = (concat lifted, sum (map (subtract 1 . length) lifted))
  where
    lifted = map liftDecl decls
    liftDecl decl =
      case decl of
        DeclVal declaration
          | Just (declaration', helpers) <- liftDictionary env constructors declaration -> DeclVal declaration' : map DeclVal helpers
        _ -> [decl]

liftDictionary :: TypeEnv -> Set Name -> ValDecl -> Maybe (ValDecl, [ValDecl])
liftDictionary env constructors declaration = do
  let (outer, inner) = splitLambdas (valBody declaration)
  (ExVar con, args) <- Just (collectSpine inner)
  if Set.member con constructors && "$Dict$" `T.isPrefixOf` nameText con && any needsHelper args
    then do
      conType <- lookupHeaderType env con
      let (foralls, fields) = splitConstructorType env conType
          typeArgs = lefts args
      if length foralls /= length typeArgs then Nothing else Just ()
      let fieldTypes = map (substTypes (Map.fromList (zip (map binderName foralls) typeArgs))) fields
      OriginTop package moduleName <- Just (nameOrigin (valName declaration))
      let helperName index =
            Name
              { nameText = nameText (valName declaration) <> "$m" <> T.pack (show (index :: Int)),
                nameSort = SortValue,
                nameOrigin = OriginTop package moduleName
              }
          outerEnv = List.foldl' extendBinder env (lefts outer)
      (args', helpers) <- mapAndUnzipM (liftField helperName outerEnv outer) (zip3 (fieldIndexes args) args (fieldTypesFor args fieldTypes))
      Just (declaration {valBody = wrapLambdas outer (rebuildSpine (ExVar con) args')}, concat helpers)
    else Nothing
  where
    needsHelper arg =
      case arg of
        Right argument -> not (isTrivial argument)
        Left _ -> False
    -- The helpers number the value arguments from zero.
    fieldIndexes args = snd (List.mapAccumL (\next arg -> either (const (next, next)) (const (next + 1, next)) arg) (0 :: Int) args)
    -- The field types follow the value arguments in order.
    fieldTypesFor = go
      where
        go (Left _ : rest) types = Nothing : go rest types
        go (Right _ : rest) (ty : types) = Just ty : go rest types
        go (Right _ : rest) [] = Nothing : go rest []
        go [] _ = []
    liftField helperName outerEnv outer (index, arg, fieldType) =
      case (arg, fieldType) of
        (Right argument, Just ty)
          | not (isTrivial argument) -> do
              helperType <- lambdaType outerEnv outer ty
              let name = helperName index
                  helper =
                    ValDecl
                      { valVis = Private,
                        valName = name,
                        valType = helperType,
                        valBody = wrapLambdas outer argument
                      }
                  reference = rebuildSpine (ExVar name) [either (Left . TyVar . binderName) (Right . ExVar . binderName) binder | binder <- outer]
              Just (Right reference, [helper])
        _ -> Just (arg, [])

-- | The lambda chain at the head of a body, and the body under it.
splitLambdas :: Expr -> ([Either Binder Binder], Expr)
splitLambdas expr =
  case expr of
    ExTyLam binder body -> let (binders, inner) = splitLambdas body in (Left binder : binders, inner)
    ExLam binder body -> let (binders, inner) = splitLambdas body in (Right binder : binders, inner)
    _ -> ([], expr)

wrapLambdas :: [Either Binder Binder] -> Expr -> Expr
wrapLambdas binders body = foldr wrap body binders
  where
    wrap binder inner = either (`ExTyLam` inner) (`ExLam` inner) binder

-- | The type of a lambda chain around a body of the given type.
lambdaType :: TypeEnv -> [Either Binder Binder] -> Type -> Maybe Type
lambdaType env binders result =
  case binders of
    [] -> Just result
    Left binder : rest -> TyForAll binder <$> lambdaType (extendBinder env binder) rest result
    Right binder : rest -> do
      inner <- lambdaType env rest result
      argumentRep <- repOf env (binderType binder)
      resultRep <- repOf env inner
      Just (TyFun (liftedRepSpelling argumentRep) (liftedRepSpelling resultRep) (binderType binder) inner)
  where
    -- The desugarer spells the lifted representation by its synonym, and
    -- the printer shows such an arrow as an arrow.
    liftedRepSpelling rep =
      case rep of
        TyApp (TyCon boxed) (TyCon levity)
          | nameText boxed == "BoxedRep" && nameText levity == "Lifted" -> TyCon (liftedRepName (tePrimPackage env))
        _ -> rep

splitConstructorType :: TypeEnv -> Type -> ([Binder], [Type])
splitConstructorType env ty =
  case ty of
    TyForAll binder body ->
      let (binders, fields) = splitConstructorType env body
       in (binder : binders, fields)
    TyFun _ _ argument body ->
      let (binders, fields) = splitConstructorType env body
       in (binders, argument : fields)
    _ ->
      let reduced = reduceType env ty
       in if reduced == ty then ([], []) else splitConstructorType env reduced

-- * Imports

-- | Keep only the imports that the declarations reach.
pruneImports :: Program -> Program
pruneImports program = program {programImports = imports'}
  where
    imports = programImports program
    direct = foldMap declReferences (programDecls program)
    used = close direct
    close current =
      let next = current <> importReferences current
       in if Set.size next == Set.size current then current else close next
    importReferences current =
      Set.unions
        [ typeReferences ty
        | (name, ty) <- Map.toList (importHeaders imports) <> Map.toList (importSynonyms imports) <> Map.toList (importBinders imports),
          Set.member name current
        ]
        <> Set.unions
          [ axiomReferences axiom
          | (name, axiom) <- Map.toList (importAxioms imports),
            keepAxiom current name axiom
          ]
    axiomReferences axiom =
      foldMap (typeReferences . binderType) (axiomBinders axiom) <> typeReferences (axiomLeft axiom) <> typeReferences (axiomRight axiom)
    -- An equation of a family that the program uses stays with the family.
    keepAxiom current name axiom =
      Set.member name current
        || (axiomRole axiom == Nominal && not (Set.disjoint current (typeReferences (axiomLeft axiom))))
    imports' =
      Imports
        { importHeaders = Map.filterWithKey (\name _ -> Set.member name used) (importHeaders imports),
          importSynonyms = Map.filterWithKey (\name _ -> Set.member name used) (importSynonyms imports),
          importAxioms = Map.filterWithKey (keepAxiom used) (importAxioms imports),
          importBinders = Map.filterWithKey (\name _ -> Set.member name used) (importBinders imports)
        }
    typeReferences ty =
      case ty of
        TyVar {} -> Set.empty
        TyCon name -> Set.singleton name
        TyApp function argument -> typeReferences function <> typeReferences argument
        TyFun r1 r2 argument result -> foldMap typeReferences [r1, r2, argument, result]
        TyForAll binder body -> typeReferences (binderType binder) <> typeReferences body
        TyEq left right -> typeReferences left <> typeReferences right
