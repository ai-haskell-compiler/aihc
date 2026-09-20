-- | Structural validation for GRIN programs.
module Aihc.Grin.Lint
  ( GrinLintError (..),
    lintProgram,
    lintCpsProgram,
    lintGcProgram,
  )
where

import Aihc.Grin.Cps (CpsGrinProgram, cpsFunctionContinuations, cpsGrinProgram)
import Aihc.Grin.Gc (GcGrinProgram, gcFunctionContinuations, gcGrinProgram)
import Aihc.Grin.Syntax
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data GrinLintError
  = GrinLintDuplicateFunction !FunctionName
  | GrinLintDuplicateGlobal !Text
  | GrinLintUnboundVariable !GrinVar
  | GrinLintUnknownFunction !FunctionName
  | GrinLintUnknownPrimitive !Text
  | GrinLintFunctionArity !FunctionName !Int !Int
  | GrinLintSaturatedClosure !FunctionName
  | GrinLintContinuationParameter !FunctionName
  | GrinLintThunkResult !FunctionName !GrinResultRep
  | GrinLintRepresentationMismatch !String !GrinRep !GrinRep
  | GrinLintResultLayout !String ![GrinRep] ![GrinRep]
  | -- | A forwarded result reached a position that places a value: a bind,
    -- or the tail of a function that has a layout for its result.
    GrinLintForwardedResultPlaced !String
  | -- | A function with a forwarded result placed a value in tail position
    -- instead of handing its continuation to a callee.
    GrinLintForwardedFunctionPlaces ![GrinRep]
  | -- | A call expects a result the callee does not declare. A callee with
    -- a forwarded result serves every call site.
    GrinLintCallResult !FunctionName !GrinResultRep !GrinResultRep
  | GrinLintEvalNonLifted !GrinRep
  | GrinLintUpdateNonLifted !GrinRep
  | GrinLintForeignArity !Text !Int !Int
  | GrinLintUnknownForeignCall !Text
  | GrinLintForeignCallDescriptorMismatch !Text
  | GrinLintConstructorLayout !Text ![GrinRep] ![GrinRep]
  deriving (Eq, Show)

data LintEnv = LintEnv
  { lintFunctionArities :: !(Map FunctionName Int),
    lintFunctionNodeArities :: !(Map FunctionName Int),
    lintFunctionResults :: !(Map FunctionName GrinResultRep),
    lintPrimitiveArities :: !(Map Text Int),
    lintConstructorLayouts :: !(Map Text [[GrinRep]]),
    lintForeignCalls :: !(Map Text GrinForeignCall)
  }

-- | Validate direct GRIN. No function has a hidden continuation parameter.
lintProgram :: GrinProgram -> [GrinLintError]
lintProgram = lintProgramWith Map.empty

-- | Validate CPS-GRIN. The program metadata gives the hidden continuation
-- parameter of every computation entry.
lintCpsProgram :: CpsGrinProgram -> [GrinLintError]
lintCpsProgram cps = lintProgramWith (cpsFunctionContinuations cps) (cpsGrinProgram cps)

-- | Validate GC-GRIN. The GC phase keeps the CPS metadata unchanged.
lintGcProgram :: GcGrinProgram -> [GrinLintError]
lintGcProgram gc = lintProgramWith (gcFunctionContinuations gc) (gcGrinProgram gc)

-- | Validate one GRIN program. The first argument gives the hidden
-- continuation parameter of each computation entry. The CPS transformation
-- adds that parameter to the entry, but it does not add a field to the thunk
-- and closure nodes that name the entry. A node therefore supplies one value
-- less than the entry has parameters.
lintProgramWith :: Map FunctionName GrinVar -> GrinProgram -> [GrinLintError]
lintProgramWith continuations program =
  duplicateFunctionErrors
    <> duplicateGlobalErrors
    <> continuationParameterErrors
    <> concatMap (lintGlobal env) (grinGlobals program)
    <> concatMap (lintFunction env) (grinFunctions program)
  where
    functions = grinFunctions program
    globals = grinGlobals program
    functionNames = map grinFunctionName functions
    globalNames = map grinGlobalName globals
    duplicateFunctionErrors = map GrinLintDuplicateFunction (duplicates functionNames)
    duplicateGlobalErrors = map GrinLintDuplicateGlobal (duplicates globalNames)
    env =
      LintEnv
        { lintFunctionArities =
            Map.fromList
              [ (grinFunctionName function, length (grinFunctionParameters function))
              | function <- functions
              ],
          lintFunctionResults =
            Map.fromList
              [(grinFunctionName function, grinFunctionResultRep function) | function <- functions],
          lintFunctionNodeArities =
            Map.fromList
              [ (grinFunctionName function, semanticFunctionArity function)
              | function <- functions
              ],
          lintPrimitiveArities = Map.fromList [(grinVarName var, arity) | (var, arity) <- grinPrimitives program],
          lintConstructorLayouts = Map.fromList [(grinConstructorName c, grinConstructorLayouts c) | c <- grinConstructors program],
          lintForeignCalls = Map.fromList [(grinForeignCallName call, call) | call <- grinForeignCalls program]
        }
    -- The number of values that a thunk or closure node must supply.
    semanticFunctionArity function =
      length (grinFunctionParameters function) - hiddenParameterCount function
    hiddenParameterCount function
      | Map.member (grinFunctionName function) continuations = 1
      | otherwise = 0
    -- A recorded continuation must be the last parameter of its entry.
    -- Nothing else keeps the node arities and the calling convention in step.
    continuationParameterErrors =
      [ GrinLintContinuationParameter (grinFunctionName function)
      | function <- functions,
        Just continuation <- [Map.lookup (grinFunctionName function) continuations],
        lastParameter function /= Just continuation
      ]
    lastParameter function =
      case reverse (grinFunctionParameters function) of
        parameter : _ -> Just parameter
        [] -> Nothing

lintGlobal :: LintEnv -> GrinGlobal -> [GrinLintError]
lintGlobal env global = lintNode env Set.empty (grinGlobalNode global)

lintFunction :: LintEnv -> GrinFunction -> [GrinLintError]
lintFunction env function =
  resultErrors
    <> lintFunctionResult env (grinFunctionResultRep function) (grinFunctionBody function)
    <> lintExpr env bound (grinFunctionBody function)
  where
    bound = Set.fromList (grinFunctionParameters function)
    results = exprResults (grinFunctionBody function)
    resultErrors =
      case grinFunctionResultRep function of
        ResultRep runtimeRep ->
          let expected = runtimeRepComponents runtimeRep
           in [GrinLintResultLayout "function result" expected actual | Placed actual <- results, actual /= expected]
                <> [GrinLintForwardedResultPlaced "function result" | Forwarded <- results]
        ResultForwarded -> [GrinLintForwardedFunctionPlaces actual | Placed actual <- results]

lintFunctionResult :: LintEnv -> GrinResultRep -> GrinExpr -> [GrinLintError]
lintFunctionResult env resultRep expr =
  case expr of
    GrinBind _ _ body -> lintFunctionResult env resultRep body
    GrinStore (GrinNode (GrinClosure functionName []) _) ->
      [ GrinLintSaturatedClosure functionName
      | Map.lookup functionName (lintFunctionResults env) == Just resultRep
      ]
    GrinStore {} -> []
    GrinEnsureHeap {} -> []
    GrinStoreUnchecked (GrinNode (GrinClosure functionName []) _) ->
      [ GrinLintSaturatedClosure functionName
      | Map.lookup functionName (lintFunctionResults env) == Just resultRep
      ]
    GrinStoreUnchecked {} -> []
    GrinStoreRec _ body -> lintFunctionResult env resultRep body
    GrinStoreRecUnchecked _ body -> lintFunctionResult env resultRep body
    GrinIfWhnf _ ready slow -> lintFunctionResult env resultRep ready <> lintFunctionResult env resultRep slow
    GrinCase _ _ alternatives -> concatMap (lintFunctionResult env resultRep . grinAltRhs) alternatives
    _ -> []

lintExpr :: LintEnv -> Set GrinVar -> GrinExpr -> [GrinLintError]
lintExpr env bound expr =
  case expr of
    GrinConstant values -> concatMap (lintValue bound) values
    GrinBind vars valueExpr body ->
      bindRepresentationErrors vars valueExpr
        <> lintExpr env bound valueExpr
        <> lintExpr env (Set.fromList vars <> bound) body
    GrinStore node -> lintNode env bound node
    GrinEnsureHeap requiredWords roots -> lintValue bound requiredWords <> concatMap (lintValue bound) roots
    GrinStoreUnchecked node -> lintNode env bound node
    GrinStoreRec bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinStoreRecUnchecked bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinUpdate pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue bound pointer
        <> lintValue bound value
    GrinUpdateBlackhole pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue bound pointer
        <> lintValue bound value
    GrinEval _ value ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedGrinRep]
        <> lintValue bound value
    GrinCpsEval _ value continuation ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedGrinRep]
        <> lintValue bound value
        <> lintValue bound continuation
    GrinCall resultRep functionName arguments ->
      lintKnownCall env bound resultRep functionName arguments
    GrinPrimitiveCall _ name arguments ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue bound) arguments
    GrinCpsPrimitiveCall _ name arguments continuation ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue bound) arguments
        <> lintValue bound continuation
    GrinApply _ function arguments -> lintValue bound function <> concatMap (lintValue bound) arguments
    GrinCpsApply _ function arguments continuation ->
      lintValue bound function
        <> concatMap (lintValue bound) arguments
        <> lintValue bound continuation
    GrinContinue continuation values ->
      lintValue bound continuation <> concatMap (lintValue bound) values
    GrinCpsRaise exception continuation ->
      lintValue bound exception <> lintValue bound continuation
    GrinHalt values -> concatMap (lintValue bound) values
    GrinExit status ->
      [GrinLintRepresentationMismatch "exit status" (grinValueRuntimeRep status) IntRep | grinValueRuntimeRep status /= IntRep]
        <> lintValue bound status
    GrinIfWhnf value ready slow ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedGrinRep]
        <> lintValue bound value
        <> lintExpr env bound ready
        <> lintExpr env bound slow
    GrinCase scrutinee binder alternatives ->
      lintValue bound scrutinee
        <> concatMap (lintAlt env (Set.insert binder bound)) alternatives
    GrinThrow exception -> lintValue bound exception
    GrinCatch _ action handler state ->
      lintValue bound action
        <> lintValue bound handler
        <> concatMap (lintValue bound) state
    GrinForeignCallExpr foreignCall arguments ->
      let expectedReps = grinForeignOperandReps (grinForeignCallSignature foreignCall)
          actualReps = map grinValueRuntimeRep arguments
          descriptorErrors =
            case Map.lookup (grinForeignCallName foreignCall) (lintForeignCalls env) of
              Nothing -> [GrinLintUnknownForeignCall (grinForeignCallName foreignCall)]
              Just declared
                | declared /= foreignCall -> [GrinLintForeignCallDescriptorMismatch (grinForeignCallName foreignCall)]
                | otherwise -> []
       in descriptorErrors
            <> [ GrinLintForeignArity (grinForeignCallName foreignCall) (length expectedReps) (length actualReps)
               | length expectedReps /= length actualReps
               ]
            <> [ GrinLintRepresentationMismatch "foreign call argument" expected actual
               | (expected, actual) <- zip expectedReps actualReps,
                 expected /= actual
               ]
            <> concatMap (lintValue bound) arguments

lintKnownCall :: LintEnv -> Set GrinVar -> GrinResultRep -> FunctionName -> [GrinValue] -> [GrinLintError]
lintKnownCall env bound resultRep functionName arguments =
  functionErrors <> resultErrors <> concatMap (lintValue bound) arguments
  where
    functionErrors =
      case Map.lookup functionName (lintFunctionArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected /= length arguments -> [GrinLintFunctionArity functionName expected (length arguments)]
        Just _ -> []
    -- A callee that forwards its result serves a call site of any layout:
    -- the value goes to the continuation the call site reified. A callee
    -- with a layout must be called for that layout. After the CPS pass a
    -- call never returns, and its empty result says so.
    resultErrors =
      case Map.lookup functionName (lintFunctionResults env) of
        Just declared
          | declared /= ResultForwarded,
            declared /= resultRep,
            resultRep /= ResultRep cpsCallResultRep ->
              [GrinLintCallResult functionName declared resultRep]
        _ -> []

-- | The result of every call after the CPS pass, which hands the callee a
-- continuation and never returns.
cpsCallResultRep :: GrinRep
cpsCallResultRep = TupleRep []

bindRepresentationErrors :: [GrinVar] -> GrinExpr -> [GrinLintError]
bindRepresentationErrors vars valueExpr =
  [GrinLintResultLayout "bind" expected actual | Placed actual <- results, actual /= expected]
    <> [GrinLintForwardedResultPlaced "bind" | Forwarded <- results]
  where
    results = exprResults valueExpr
    expected = map grinVarRuntimeRep vars

lintAlt :: LintEnv -> Set GrinVar -> GrinAlt -> [GrinLintError]
lintAlt env bound alt =
  lintExpr env (Set.fromList (grinAltBinders alt) <> bound) (grinAltRhs alt)

lintValue :: Set GrinVar -> GrinValue -> [GrinLintError]
lintValue bound value =
  case value of
    GrinVarValue var
      | var `Set.member` bound -> []
      | otherwise -> [GrinLintUnboundVariable var]
    GrinGlobalValue _ -> []
    GrinLitValue _ -> []

lintNode :: LintEnv -> Set GrinVar -> GrinNode -> [GrinLintError]
lintNode env bound node =
  concatMap (lintValue bound) (grinNodeFields node)
    <> lintNodeFunction env node
    <> lintConstructorFields env node

lintConstructorFields :: LintEnv -> GrinNode -> [GrinLintError]
lintConstructorFields env node =
  case grinNodeTag node of
    GrinConstructor name remaining ->
      case Map.lookup name (lintConstructorLayouts env) of
        Just layouts
          | let suppliedCount = length layouts - remaining,
            suppliedCount < 0
              || actual /= concat (take suppliedCount layouts) ->
              [GrinLintConstructorLayout name (concat layouts) actual]
        _ -> []
    _ -> []
  where
    actual = map grinValueRuntimeRep (grinNodeFields node)

lintNodeFunction :: LintEnv -> GrinNode -> [GrinLintError]
lintNodeFunction env node =
  case grinNodeTag node of
    GrinThunk functionName -> checkFunctionArity functionName fieldCount <> checkThunkResult functionName
    GrinClosure functionName argumentLayouts -> checkClosureArity functionName argumentLayouts
    _ -> []
  where
    fieldCount = length (grinNodeFields node)
    checkFunctionArity functionName actual =
      case Map.lookup functionName (lintFunctionNodeArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected == actual -> []
          | otherwise -> [GrinLintFunctionArity functionName expected actual]
    checkClosureArity functionName argumentLayouts =
      checkFunctionArity functionName (fieldCount + length (concat argumentLayouts))
    -- A thunk's result is placed by the update, so it has a layout, and a
    -- lifted one.
    checkThunkResult functionName =
      case Map.lookup functionName (lintFunctionResults env) of
        Just (ResultRep runtimeRep)
          | isLiftedRuntimeRep runtimeRep -> []
        Just resultRep -> [GrinLintThunkResult functionName resultRep]
        Nothing -> []

duplicates :: (Ord a) => [a] -> [a]
duplicates = go Set.empty Set.empty
  where
    go _ repeated [] = Set.toAscList repeated
    go seen repeated (value : rest)
      | value `Set.member` seen = go seen (Set.insert value repeated) rest
      | otherwise = go (Set.insert value seen) repeated rest

-- | What one exit of an expression produces: values of a layout, or a
-- result a callee forwards to the continuation of the enclosing function.
data ExprResult
  = Placed ![GrinRep]
  | Forwarded

-- | Each returning case alternative contributes its own result.
-- Control transfers do not produce a result at this expression.
exprResults :: GrinExpr -> [ExprResult]
exprResults expr =
  case expr of
    GrinConstant values -> [Placed (map grinValueRuntimeRep values)]
    GrinBind _ _ body -> exprResults body
    GrinStore {} -> [Placed [liftedGrinRep]]
    GrinEnsureHeap _ roots -> [Placed (map grinValueRuntimeRep roots)]
    GrinStoreUnchecked {} -> [Placed [liftedGrinRep]]
    GrinStoreRec _ body -> exprResults body
    GrinStoreRecUnchecked _ body -> exprResults body
    GrinUpdate _ value -> [Placed [grinValueRuntimeRep value]]
    GrinUpdateBlackhole _ value -> [Placed [grinValueRuntimeRep value]]
    GrinEval runtimeRep _ -> [Placed (runtimeRepComponents runtimeRep)]
    GrinCpsEval {} -> []
    GrinCall resultRep _ _ ->
      case resultRepComponents resultRep of
        Nothing -> [Forwarded]
        Just [] -> []
        Just components -> [Placed components]
    GrinPrimitiveCall runtimeRep _ _ -> [Placed (runtimeRepComponents runtimeRep)]
    GrinCpsPrimitiveCall {} -> []
    GrinApply resultRep _ _ -> maybe [Forwarded] (pure . Placed) (resultRepComponents resultRep)
    GrinCpsApply {} -> []
    GrinContinue {} -> []
    GrinCpsRaise {} -> []
    GrinHalt {} -> []
    GrinExit {} -> []
    GrinIfWhnf _ ready slow -> exprResults ready <> exprResults slow
    GrinCase _ _ alternatives ->
      concatMap (exprResults . grinAltRhs) alternatives
    GrinThrow {} -> []
    GrinCatch runtimeRep _ _ _ -> [Placed (runtimeRepComponents runtimeRep)]
    GrinForeignCallExpr foreignCall _ ->
      [Placed (grinForeignCallResultReps (grinForeignCallSignature foreignCall))]
