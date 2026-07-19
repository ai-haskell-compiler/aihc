{-# LANGUAGE OverloadedStrings #-}

-- | Structural validation for GRIN programs.
module Aihc.Grin.Lint
  ( GrinLintError (..),
    lintProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (Levity (..), RuntimeRep (..), liftedRuntimeRep)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

data GrinLintError
  = GrinLintDuplicateFunction !FunctionName
  | GrinLintDuplicateWhnfGlobal !GrinVar
  | GrinLintDuplicateCaf !GrinVar
  | GrinLintUnboundVariable !GrinVar
  | GrinLintUnknownFunction !FunctionName
  | GrinLintUnknownPrimitive !Text
  | GrinLintFunctionArity !FunctionName !Int !Int
  | GrinLintSaturatedClosure !FunctionName
  | GrinLintThunkResult !FunctionName !RuntimeRep
  | GrinLintRepresentationMismatch !String !RuntimeRep !RuntimeRep
  | GrinLintResultLayout !String ![RuntimeRep] ![RuntimeRep]
  | GrinLintEvalNonLifted !RuntimeRep
  | GrinLintUpdateNonLifted !RuntimeRep
  | GrinLintForeignArity !Text !Int !Int
  | GrinLintUnknownForeignCall !Text
  | GrinLintForeignCallDescriptorMismatch !Text
  | GrinLintConstructorLayout !Text ![RuntimeRep] ![RuntimeRep]
  deriving (Eq, Show)

data LintEnv = LintEnv
  { lintFunctionArities :: !(Map FunctionName Int),
    lintFunctionNodeArities :: !(Map FunctionName Int),
    lintFunctionResults :: !(Map FunctionName RuntimeRep),
    lintPrimitiveArities :: !(Map Text Int),
    lintGlobalVars :: !(Set GrinVar),
    lintGlobalNames :: !(Set Text),
    lintConstructorLayouts :: !(Map Text [[RuntimeRep]]),
    lintForeignCalls :: !(Map Text GrinForeignCall)
  }

lintProgram :: GrinProgram -> [GrinLintError]
lintProgram program =
  duplicateFunctionErrors
    <> duplicateGlobalErrors
    <> duplicateCafErrors
    <> concatMap (lintWhnfGlobal env) (grinWhnfGlobals program)
    <> concatMap (lintCaf env) (grinCafs program)
    <> concatMap (lintFunction env) (grinFunctions program)
  where
    functions = grinFunctions program
    globals = grinWhnfGlobals program
    cafs = grinCafs program
    functionNames = map grinFunctionName functions
    globalVars = map fst globals
    cafVars = map fst cafs
    duplicateFunctionErrors = map GrinLintDuplicateFunction (duplicates functionNames)
    duplicateGlobalErrors = map GrinLintDuplicateWhnfGlobal (duplicates globalVars)
    duplicateCafErrors = map GrinLintDuplicateCaf (duplicates cafVars)
    env =
      LintEnv
        { lintFunctionArities =
            Map.fromList
              ( [ (grinFunctionName function, length (grinFunctionParameters function))
                | function <- functions
                ]
                  <> [ (grinCodeFunctionName info, length (concat (grinCodeParameterLayouts info)))
                     | info <- grinExternalFunctions program
                     ]
              ),
          lintFunctionResults =
            Map.fromList
              ( [(grinFunctionName function, grinFunctionResultRep function) | function <- functions]
                  <> [(grinCodeFunctionName info, grinCodeResultRep info) | info <- grinExternalFunctions program]
              ),
          lintFunctionNodeArities =
            Map.fromList
              ( [ (grinFunctionName function, semanticFunctionArity function)
                | function <- functions
                ]
                  <> [ (grinCodeFunctionName info, semanticExternalArity info)
                     | info <- grinExternalFunctions program
                     ]
              ),
          lintPrimitiveArities = Map.fromList [(grinVarName var, arity) | (var, arity) <- grinPrimitives program],
          lintGlobalVars = Set.fromList (globalVars <> cafVars),
          lintGlobalNames =
            Set.fromList
              ( [name | (name, layouts) <- builtinConstructors, null layouts]
                  <> [name | (name, fields) <- grinConstructors program, null fields]
                  <> grinExternalGlobals program
                  <> map grinVarName globalVars
                  <> map grinVarName cafVars
              ),
          lintConstructorLayouts = Map.fromList (grinConstructors program),
          lintForeignCalls = Map.fromList [(grinForeignCallName call, call) | call <- grinForeignCalls program]
        }
    semanticFunctionArity function =
      if "$cps$" `T.isPrefixOf` unFunctionName (grinFunctionName function)
        then length (grinFunctionParameters function)
        else case reverse (grinFunctionParameters function) of
          continuation : rest
            | grinVarName continuation == "$cps_return" -> length rest
          _ -> length (grinFunctionParameters function)
    semanticExternalArity info =
      case reverse (grinCodeParameterLayouts info) of
        [BoxedRep Lifted] : rest -> length (concat (reverse rest))
        _ -> length (concat (grinCodeParameterLayouts info))

lintWhnfGlobal :: LintEnv -> (GrinVar, GrinNode) -> [GrinLintError]
lintWhnfGlobal env (var, node) =
  [ GrinLintRepresentationMismatch "global" (grinVarRuntimeRep var) liftedRuntimeRep
  | grinVarRuntimeRep var /= liftedRuntimeRep
  ]
    <> lintNode env (lintGlobalVars env) node

lintCaf :: LintEnv -> (GrinVar, GrinNode) -> [GrinLintError]
lintCaf env (var, node) =
  [ GrinLintRepresentationMismatch "CAF" (grinVarRuntimeRep var) liftedRuntimeRep
  | grinVarRuntimeRep var /= liftedRuntimeRep
  ]
    <> lintNode env (lintGlobalVars env) node

lintFunction :: LintEnv -> GrinFunction -> [GrinLintError]
lintFunction env function =
  resultErrors
    <> lintFunctionResult env (grinFunctionResultRep function) (grinFunctionBody function)
    <> lintExpr env bound (grinFunctionBody function)
  where
    bound = Set.fromList (grinFunctionParameters function) <> lintGlobalVars env
    resultErrors =
      case exprRuntimeReps (grinFunctionBody function) of
        Just actual
          | actual /= expected ->
              [GrinLintResultLayout "function result" expected actual]
        _ -> []
    expected = runtimeRepComponents (grinFunctionResultRep function)

lintFunctionResult :: LintEnv -> RuntimeRep -> GrinExpr -> [GrinLintError]
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
    GrinCase _ _ alternatives -> concatMap (lintFunctionResult env resultRep . grinAltRhs) alternatives
    _ -> []

lintExpr :: LintEnv -> Set GrinVar -> GrinExpr -> [GrinLintError]
lintExpr env bound expr =
  case expr of
    GrinConstant values -> concatMap (lintValue env bound) values
    GrinBind vars valueExpr body ->
      bindRepresentationErrors vars valueExpr
        <> lintExpr env bound valueExpr
        <> lintExpr env (Set.fromList vars <> bound) body
    GrinStore node -> lintNode env bound node
    GrinEnsureHeap _ roots -> concatMap (lintValue env bound) roots
    GrinStoreUnchecked node -> lintNode env bound node
    GrinStoreRec bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinStoreRecUnchecked bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinFetch _ pointer -> lintValue env bound pointer
    GrinUpdate pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue env bound pointer
        <> lintValue env bound value
    GrinUpdateBlackhole pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue env bound pointer
        <> lintValue env bound value
    GrinEval _ value ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedRuntimeRep]
        <> lintValue env bound value
    GrinCpsEval _ value continuation updateContinuation ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedRuntimeRep]
        <> lintValue env bound value
        <> lintValue env bound continuation
        <> lintValue env bound updateContinuation
    GrinCall runtimeRep functionName arguments ->
      lintKnownCall env bound runtimeRep functionName arguments
    GrinPrimitiveCall _ name arguments ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue env bound) arguments
    GrinCpsPrimitiveCall _ name arguments continuation ->
      [GrinLintUnknownPrimitive name | name `Map.notMember` lintPrimitiveArities env]
        <> concatMap (lintValue env bound) arguments
        <> lintValue env bound continuation
    GrinApply _ function arguments -> lintValue env bound function <> concatMap (lintValue env bound) arguments
    GrinCpsApply _ function arguments continuation ->
      lintValue env bound function
        <> concatMap (lintValue env bound) arguments
        <> lintValue env bound continuation
    GrinContinue continuation values ->
      lintValue env bound continuation <> concatMap (lintValue env bound) values
    GrinHalt values -> concatMap (lintValue env bound) values
    GrinCase scrutinee binder alternatives ->
      lintValue env bound scrutinee
        <> concatMap (lintAlt env (Set.insert binder bound)) alternatives
    GrinThrow exception -> lintValue env bound exception
    GrinCatch _ action handler state ->
      lintValue env bound action
        <> lintValue env bound handler
        <> concatMap (lintValue env bound) state
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
            <> concatMap (lintValue env bound) arguments

lintKnownCall :: LintEnv -> Set GrinVar -> RuntimeRep -> FunctionName -> [GrinValue] -> [GrinLintError]
lintKnownCall env bound _runtimeRep functionName arguments =
  functionErrors <> concatMap (lintValue env bound) arguments
  where
    functionErrors =
      case Map.lookup functionName (lintFunctionArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected /= length arguments -> [GrinLintFunctionArity functionName expected (length arguments)]
        Just _ -> []

bindRepresentationErrors :: [GrinVar] -> GrinExpr -> [GrinLintError]
bindRepresentationErrors vars valueExpr =
  case exprRuntimeReps valueExpr of
    Just actual
      | actual /= expected ->
          [GrinLintResultLayout "bind" expected actual]
    _ -> []
  where
    expected = map grinVarRuntimeRep vars

lintAlt :: LintEnv -> Set GrinVar -> GrinAlt -> [GrinLintError]
lintAlt env bound alt =
  lintExpr env (Set.fromList (grinAltBinders alt) <> bound) (grinAltRhs alt)

lintValue :: LintEnv -> Set GrinVar -> GrinValue -> [GrinLintError]
lintValue env bound value =
  case value of
    GrinVarValue var
      | var `Set.member` bound -> []
      | grinVarName var `Set.member` lintGlobalNames env -> []
      | otherwise -> [GrinLintUnboundVariable var]
    GrinLitValue _ -> []

lintNode :: LintEnv -> Set GrinVar -> GrinNode -> [GrinLintError]
lintNode env bound node =
  concatMap (lintValue env bound) (grinNodeFields node)
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
    checkThunkResult functionName =
      case Map.lookup functionName (lintFunctionResults env) of
        Just runtimeRep
          | not (isLiftedRuntimeRep runtimeRep) -> [GrinLintThunkResult functionName runtimeRep]
        _ -> []

duplicates :: (Ord a) => [a] -> [a]
duplicates = go Set.empty Set.empty
  where
    go _ repeated [] = Set.toAscList repeated
    go seen repeated (value : rest)
      | value `Set.member` seen = go seen (Set.insert value repeated) rest
      | otherwise = go (Set.insert value seen) repeated rest

exprRuntimeReps :: GrinExpr -> Maybe [RuntimeRep]
exprRuntimeReps expr =
  case expr of
    GrinConstant values -> Just (map grinValueRuntimeRep values)
    GrinBind _ _ body -> exprRuntimeReps body
    GrinStore {} -> Just [liftedRuntimeRep]
    GrinEnsureHeap _ roots -> Just (map grinValueRuntimeRep roots)
    GrinStoreUnchecked {} -> Just [liftedRuntimeRep]
    GrinStoreRec _ body -> exprRuntimeReps body
    GrinStoreRecUnchecked _ body -> exprRuntimeReps body
    GrinFetch runtimeRep _ -> Just (runtimeRepComponents runtimeRep)
    GrinUpdate _ value -> Just [grinValueRuntimeRep value]
    GrinUpdateBlackhole _ value -> Just [grinValueRuntimeRep value]
    GrinEval runtimeRep _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsEval {} -> Nothing
    GrinCall runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinPrimitiveCall runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsPrimitiveCall {} -> Nothing
    GrinApply runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinCpsApply {} -> Nothing
    GrinContinue {} -> Nothing
    GrinHalt {} -> Nothing
    GrinCase _ _ alternatives ->
      case alternatives of
        first : _ -> exprRuntimeReps (grinAltRhs first)
        [] -> Nothing
    GrinThrow {} -> Nothing
    GrinCatch runtimeRep _ _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinForeignCallExpr foreignCall _ ->
      Just (grinForeignCallResultReps (grinForeignCallSignature foreignCall))
