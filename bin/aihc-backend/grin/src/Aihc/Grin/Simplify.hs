-- | Simplify strict GRIN by what a function body statically knows about the
-- heap objects it names.
--
-- Lowering names every heap object it allocates, and a global holds one
-- static node. A node with a constructor tag or a closure tag is already in
-- weak-head normal form and is never updated, so a pointer to one is a
-- value the code can look through. Only a thunk is opaque: once entered it
-- is overwritten with its result, so nothing is known about it until it is
-- evaluated.
--
-- Each rule applies where its operand is such a value:
--
-- * @eval v@ is @v@ itself when @v@ points at a constructor or a closure,
--   or is a variable an earlier @eval@ or a case bound.
--
-- * @apply f a ...@ enters the code of a closure directly when the groups
--   supply all its remaining arguments, and otherwise builds the closure of
--   the remaining arguments in place. A partially applied constructor grows
--   the same way. An application that supplies more groups than the closure
--   takes stays as it is.
--
-- * A direct @call@ of a function whose body is one @store@, one
--   @constant@, one @primitive-call@, or one tail @call@ on its parameters
--   is that operation, with the arguments in place of the parameters. The
--   call site already has the operands in hand, so the operation is not
--   larger than the call, and a @store@ that it exposes is a known node for
--   the rules that follow. An operand that is a global or a literal can
--   cost more code at each call site than in the callee, so such a body
--   stays a call. A chain of such calls unfolds until it gets to a function
--   that is already in the chain.
--
-- * @case v of ...@ takes the alternative the node selects. The binders of
--   the alternative are bound to the fields of the node.
--
-- * A @store@ or a @constant@ that nothing after it uses disappears. This
--   removes the closures the application rule replaced. A bind whose body
--   only returns what it bound is that value expression.
--
-- * A @store-rec@ group is split into its strongly connected components, in
--   dependency order. A binding that is not part of a cycle becomes a plain
--   @store@, so that the rules above see its node.
--
-- The pass keeps the bind spine as it finds it and leaves copy binds behind;
-- the ANF normalizer that follows it folds them.
module Aihc.Grin.Simplify
  ( simplifyGrinProgram,
  )
where

import Aihc.Grin.Analysis (freeExprVars, freeNodeVars, freeValueVars)
import Aihc.Grin.Syntax
import Control.Applicative ((<|>))
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data Env = Env
  { -- | The parameter count and the result of every function of the
    -- program.
    envFunctions :: !(Map FunctionName (Int, GrinResultRep)),
    -- | The functions whose body is one operation, with their parameters
    -- and their result.
    envUnfoldings :: !(Map FunctionName Unfolding),
    -- | The layout of every constructor of the program.
    envConstructors :: !(Map Text [[GrinRep]]),
    -- | The static node behind every global-table reference that has one:
    -- the globals of the program, and the shared object of each nullary
    -- constructor.
    envStatics :: !(Map Text GrinNode),
    -- | The node each local pointer was stored with.
    envKnown :: !(Map GrinVar GrinNode),
    -- | The local pointers that are in weak-head normal form without a
    -- known node: the results of an @eval@, and the binders of a case.
    envEvaluated :: !(Set GrinVar)
  }

-- | Simplify every function body of a program.
simplifyGrinProgram :: GrinProgram -> GrinProgram
simplifyGrinProgram program =
  program {grinFunctions = map simplifyFunction (grinFunctions program)}
  where
    env =
      Env
        { envFunctions =
            Map.fromList
              [ (grinFunctionName function, (length (grinFunctionParameters function), grinFunctionResultRep function))
              | function <- grinFunctions program
              ],
          envUnfoldings =
            Map.fromList
              [ (grinFunctionName function, unfolding)
              | function <- grinFunctions program,
                Just unfolding <- [functionUnfolding function]
              ],
          envConstructors = Map.fromList [(grinConstructorName c, grinConstructorLayouts c) | c <- grinConstructors program],
          envStatics =
            Map.fromList
              ( [(grinConstructorName c, GrinNode (GrinConstructor (grinConstructorName c) 0) []) | c <- grinConstructors program, null (grinConstructorLayouts c)]
                  <> [(grinGlobalName global, grinGlobalNode global) | global <- grinGlobals program]
              ),
          envKnown = Map.empty,
          envEvaluated = Set.empty
        }
    simplifyFunction function =
      function {grinFunctionBody = fst (simplifyExpr env (grinFunctionBody function))}

-- | The node a value points at, when it is known.
knownNode :: Env -> GrinValue -> Maybe GrinNode
knownNode env value =
  case value of
    GrinVarValue var -> Map.lookup var (envKnown env)
    GrinGlobalValue name -> Map.lookup name (envStatics env)
    GrinLitValue {} -> Nothing

-- | Whether a value points at a node in weak-head normal form.
isEvaluated :: Env -> GrinValue -> Bool
isEvaluated env value =
  case knownNode env value of
    Just node -> isValueNode node
    Nothing ->
      case value of
        GrinVarValue var -> Set.member var (envEvaluated env)
        _ -> False

-- | A node that is never updated: anything but a thunk.
isValueNode :: GrinNode -> Bool
isValueNode node =
  case grinNodeTag node of
    GrinThunk {} -> False
    GrinConstructor {} -> True
    GrinClosure {} -> True

-- | Forget what is known about names a binder group rebinds.
forget :: [GrinVar] -> Env -> Env
forget vars env =
  env
    { envKnown = List.foldl' (flip Map.delete) (envKnown env) vars,
      envEvaluated = envEvaluated env `Set.difference` Set.fromList vars
    }

know :: GrinVar -> GrinNode -> Env -> Env
know var node env = env {envKnown = Map.insert var node (envKnown env)}

evaluated :: GrinVar -> Env -> Env
evaluated var env = env {envEvaluated = Set.insert var (envEvaluated env)}

-- | What a bind teaches about the names it binds.
bindResults :: Env -> [GrinVar] -> GrinExpr -> Env
bindResults env vars value =
  case (vars, value) of
    ([var], GrinStore node) -> know var node rebound
    ([var], GrinConstant [copied])
      | Just node <- knownNode rebound copied -> know var node rebound
      | isEvaluated rebound copied -> evaluated var rebound
    ([var], GrinEval {}) -> evaluated var rebound
    _ -> rebound
  where
    rebound = forget vars env

-- | An expression that allocates or names a value and does nothing else, so
-- that it can go when its result goes.
isPureExpression :: GrinExpr -> Bool
isPureExpression expression =
  case expression of
    GrinStore {} -> True
    GrinConstant {} -> True
    _ -> False

-- | Simplify an expression, and give the variables free in the result.
simplifyExpr :: Env -> GrinExpr -> (GrinExpr, Set GrinVar)
simplifyExpr env expression =
  case expression of
    GrinBind vars valueExpression body ->
      let (value', valueFree) = simplifyExpr env valueExpression
       in bindSimplified vars (value', valueFree) (simplifyExpr (bindResults env vars value') body)
    GrinStoreRec bindings body -> simplifyStoreRec env bindings body
    GrinStoreRecUnchecked bindings body ->
      let inner = List.foldl' (\current (var, node) -> know var node current) (forget (map fst bindings) env) bindings
          (body', bodyFree) = simplifyExpr inner body
       in ( GrinStoreRecUnchecked bindings body',
            (foldMap (freeNodeVars . snd) bindings <> bodyFree) `Set.difference` Set.fromList (map fst bindings)
          )
    GrinCall resultRep functionName arguments
      | Just inlined <- inlineCall env Set.empty resultRep functionName arguments ->
          -- The unfolding is final: simplifying it again would unfold a
          -- call that closes a cycle once more.
          (inlined, freeExprVars inlined)
    GrinEval _ _ value
      | isEvaluated env value -> (GrinConstant [value], freeValueVars value)
    GrinApply resultRep function arguments
      | Just node <- knownNode env function,
        Just applied <- applyKnown env resultRep node arguments ->
          simplifyExpr env applied
    GrinCase scrutinee binder alternatives
      | Just selected <- selectAlternative env scrutinee binder alternatives ->
          simplifyExpr env selected
      | otherwise ->
          let simplified = map (simplifyAlternative env scrutinee binder) alternatives
           in ( GrinCase scrutinee binder (map fst simplified),
                freeValueVars scrutinee <> foldMap snd simplified
              )
    _ -> (expression, freeExprVars expression)

-- | Rebuild a bind from its simplified value expression and body. A pure
-- value expression whose results the body does not use is dropped, and a
-- body that only returns those results is the value expression itself.
bindSimplified :: [GrinVar] -> (GrinExpr, Set GrinVar) -> (GrinExpr, Set GrinVar) -> (GrinExpr, Set GrinVar)
bindSimplified vars (value, valueFree) (body, bodyFree)
  | isPureExpression value && not (any (`Set.member` bodyFree) vars) = (body, bodyFree)
  | body == GrinConstant (map GrinVarValue vars) = (value, valueFree)
  | otherwise = (GrinBind vars value body, valueFree <> (bodyFree `Set.difference` Set.fromList vars))

-- | Simplify an alternative under what matching it establishes: the case
-- binder and a scrutinee variable are in weak-head normal form, and in a
-- constructor alternative they are that constructor applied to the
-- alternative binders.
simplifyAlternative :: Env -> GrinValue -> GrinVar -> GrinAlt -> (GrinAlt, Set GrinVar)
simplifyAlternative env scrutinee binder alternative =
  (alternative {grinAltRhs = rhs}, rhsFree `Set.difference` Set.fromList (binder : grinAltBinders alternative))
  where
    (rhs, rhsFree) = simplifyExpr altEnv (grinAltRhs alternative)
    matched =
      binder
        : case scrutinee of
          GrinVarValue var -> [var]
          _ -> []
    altEnv
      | grinValueRuntimeRep scrutinee /= liftedGrinRep = env
      | otherwise =
          case grinAltCon alternative of
            GrinDataAlt name ->
              let node = GrinNode (GrinConstructor name 0) (map GrinVarValue (grinAltBinders alternative))
               in List.foldl' (\current var -> know var node current) (forget matched env) matched
            _ -> List.foldl' (flip evaluated) (forget matched env) matched

-- | The alternative a known scrutinee selects, with its binders bound to
-- what they match. The binds are copies that the normalizer folds.
selectAlternative :: Env -> GrinValue -> GrinVar -> [GrinAlt] -> Maybe GrinExpr
selectAlternative env scrutinee binder alternatives =
  case knownNode env scrutinee of
    Just (GrinNode (GrinConstructor name 0) fields) -> do
      alternative <- List.find ((== GrinDataAlt name) . grinAltCon) alternatives <|> defaultAlternative
      fieldBinds <-
        case grinAltCon alternative of
          GrinDataAlt {}
            | length (grinAltBinders alternative) == length fields,
              and (zipWith sameRep (grinAltBinders alternative) fields) ->
                Just (zip (grinAltBinders alternative) fields)
          GrinDataAlt {} -> Nothing
          _ -> Just []
      pure (bindCopies ((binder, scrutinee) : fieldBinds) (grinAltRhs alternative))
    _ ->
      case scrutinee of
        GrinLitValue literal -> do
          alternative <- List.find ((== GrinLitAlt literal) . grinAltCon) alternatives <|> defaultAlternative
          pure (bindCopies [(binder, scrutinee)] (grinAltRhs alternative))
        _ -> Nothing
  where
    defaultAlternative = List.find ((== GrinDefaultAlt) . grinAltCon) alternatives
    sameRep var value = grinVarRuntimeRep var == grinValueRuntimeRep value

bindCopies :: [(GrinVar, GrinValue)] -> GrinExpr -> GrinExpr
bindCopies copies body =
  foldr (\(var, value) rest -> GrinBind [var] (GrinConstant [value]) rest) body copies

-- | Apply argument groups to a known node. The node must take at least as
-- many arguments as the groups supply.
applyKnown :: Env -> GrinResultRep -> GrinNode -> [[GrinValue]] -> Maybe GrinExpr
applyKnown env resultRep node groups =
  case grinNodeTag node of
    GrinClosure functionName layouts
      | length groups <= length layouts,
        groupReps == take (length groups) layouts ->
          case drop (length groups) layouts of
            []
              | Just (arity, declared) <- Map.lookup functionName (envFunctions env),
                arity == length fields,
                declared == ResultForwarded || declared == resultRep ->
                  Just (GrinCall resultRep functionName fields)
            remaining@(_ : _)
              | resultRep == liftedResultRep ->
                  Just (GrinStore (GrinNode (GrinClosure functionName remaining) fields))
            _ -> Nothing
    GrinConstructor name remaining
      | remaining >= length groups,
        resultRep == liftedResultRep,
        Just layouts <- Map.lookup name (envConstructors env),
        length layouts >= remaining,
        groupReps == take (length groups) (drop (length layouts - remaining) layouts) ->
          Just (GrinStore (GrinNode (GrinConstructor name (remaining - length groups)) fields))
    _ -> Nothing
  where
    groupReps = map (map grinValueRuntimeRep) groups
    fields = grinNodeFields node <> concat groups

-- | The parameters, the result, and the body of a function whose body is
-- one operation on its parameters.
data Unfolding = Unfolding ![GrinVar] !GrinResultRep !GrinExpr

functionUnfolding :: GrinFunction -> Maybe Unfolding
functionUnfolding function =
  case grinFunctionBody function of
    body@(GrinStore node) | onlyVariables (grinNodeFields node) -> placed body
    body@(GrinConstant values) | onlyVariables values -> placed body
    body@(GrinPrimitiveCall _ _ values) | onlyVariables values -> placed body
    body@(GrinCall _ _ values) | onlyVariables values -> Just (unfolding body)
    _ -> Nothing
  where
    onlyVariables = all isVariable
    isVariable value =
      case value of
        GrinVarValue {} -> True
        _ -> False
    unfolding = Unfolding (grinFunctionParameters function) (grinFunctionResultRep function)
    -- Only a function that places its result has one of these bodies.
    placed body
      | grinFunctionResultRep function == ResultForwarded = Nothing
      | otherwise = Just (unfolding body)

-- | The operation that a direct call of a function with an unfolding is.
-- The visited functions are the ones the chain already unfolded.
inlineCall :: Env -> Set FunctionName -> GrinResultRep -> FunctionName -> [GrinValue] -> Maybe GrinExpr
inlineCall env visited resultRep functionName arguments = do
  Unfolding parameters declared body <- Map.lookup functionName (envUnfoldings env)
  if Set.member functionName visited
    || length parameters /= length arguments
    || (declared /= ResultForwarded && declared /= resultRep)
    then Nothing
    else
      let substitution = Map.fromList (zip parameters arguments)
          substitute value =
            case value of
              GrinVarValue var -> Map.findWithDefault value var substitution
              _ -> value
          substituteNode (GrinNode tag fields) = GrinNode tag (map substitute fields)
       in case body of
            GrinStore node -> Just (GrinStore (substituteNode node))
            GrinConstant values -> Just (GrinConstant (map substitute values))
            GrinPrimitiveCall runtimeRep name values -> Just (GrinPrimitiveCall runtimeRep name (map substitute values))
            GrinCall calleeRep callee values ->
              -- A function that forwards its result forwards it to a
              -- callee that also forwards, so the callee serves the layout
              -- of this call site too.
              let siteRep = if declared == ResultForwarded then resultRep else calleeRep
                  arguments' = map substitute values
               in inlineCall env (Set.insert functionName visited) siteRep callee arguments'
                    <|> Just (GrinCall siteRep callee arguments')
            _ -> Nothing

-- | Split a recursive allocation group into its strongly connected
-- components and allocate each in turn, dependencies first.
simplifyStoreRec :: Env -> [(GrinVar, GrinNode)] -> GrinExpr -> (GrinExpr, Set GrinVar)
simplifyStoreRec env bindings body = allocate (forget (map fst bindings) env) components
  where
    groupVars = Set.fromList (map fst bindings)
    components =
      stronglyConnComp
        [ (binding, var, Set.toList (freeNodeVars node `Set.intersection` groupVars))
        | binding@(var, node) <- bindings
        ]
    allocate current remaining =
      case remaining of
        [] -> simplifyExpr current body
        AcyclicSCC (var, node) : rest ->
          bindSimplified [var] (GrinStore node, freeNodeVars node) (allocate (know var node current) rest)
        CyclicSCC group : rest ->
          let inner = List.foldl' (\accumulated (var, node) -> know var node accumulated) current group
              (rest', restFree) = allocate inner rest
              vars = map fst group
           in if any (`Set.member` restFree) vars
                then
                  ( GrinStoreRec group rest',
                    (foldMap (freeNodeVars . snd) group <> restFree) `Set.difference` Set.fromList vars
                  )
                else (rest', restFree)
