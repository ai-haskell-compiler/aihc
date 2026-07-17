{-# LANGUAGE OverloadedStrings #-}

-- | Structural validation for GRIN programs.
module Aihc.Grin.Lint
  ( GrinLintError (..),
    lintProgram,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep, liftedRuntimeRep)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

data GrinLintError
  = GrinLintDuplicateFunction !FunctionName
  | GrinLintDuplicateWhnfGlobal !GrinVar
  | GrinLintDuplicateCaf !GrinVar
  | GrinLintUnboundVariable !GrinVar
  | GrinLintUnknownFunction !FunctionName
  | GrinLintFunctionArity !FunctionName !Int !Int
  | GrinLintThunkResult !FunctionName !RuntimeRep
  | GrinLintRepresentationMismatch !String !RuntimeRep !RuntimeRep
  | GrinLintResultLayout !String ![RuntimeRep] ![RuntimeRep]
  | GrinLintEvalNonLifted !RuntimeRep
  | GrinLintUpdateNonLifted !RuntimeRep
  | GrinLintForeignArity !Text !Int !Int
  | GrinLintUnknownForeignCall !Text
  | GrinLintForeignCallDescriptorMismatch !Text
  | GrinLintConstructorLayout !Text ![RuntimeRep] ![RuntimeRep]
  | GrinLintInvalidProjectIndex !Int
  | GrinLintProjectNonConstructor !GrinNodeTag
  | GrinLintProjectNonAtomic !RuntimeRep
  deriving (Eq, Show)

data LintEnv = LintEnv
  { lintFunctionArities :: !(Map FunctionName Int),
    lintFunctionResults :: !(Map FunctionName RuntimeRep),
    lintGlobalVars :: !(Set GrinVar),
    lintGlobalNames :: !(Set Text),
    lintConstructorLayouts :: !(Map Text [RuntimeRep]),
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
              [ (grinFunctionName function, length (grinFunctionParameters function))
              | function <- functions
              ],
          lintFunctionResults = Map.fromList [(grinFunctionName function, grinFunctionResultRep function) | function <- functions],
          lintGlobalVars = Set.fromList (globalVars <> cafVars),
          lintGlobalNames =
            Set.fromList
              ( map fst builtinConstructors
                  <> map fst (grinConstructors program)
                  <> map (grinVarName . fst) (grinPrimitives program)
                  <> map grinVarName globalVars
                  <> map grinVarName cafVars
              ),
          lintConstructorLayouts = Map.fromList (grinConstructors program),
          lintForeignCalls = Map.fromList [(grinForeignCallName call, call) | call <- grinForeignCalls program]
        }

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

lintExpr :: LintEnv -> Set GrinVar -> GrinExpr -> [GrinLintError]
lintExpr env bound expr =
  case expr of
    GrinReturn values -> concatMap (lintValue env bound) values
    GrinBind vars valueExpr body ->
      bindRepresentationErrors vars valueExpr
        <> lintExpr env bound valueExpr
        <> lintExpr env (Set.fromList vars <> bound) body
    GrinStore node -> lintNode env bound node
    GrinStoreRec bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinFetch _ pointer -> lintValue env bound pointer
    GrinUpdate pointer value ->
      [GrinLintUpdateNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, not (isLiftedRuntimeRep runtimeRep)]
        <> lintValue env bound pointer
        <> lintValue env bound value
    GrinEval _ value ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedRuntimeRep]
        <> lintValue env bound value
    GrinApply _ function arguments -> lintValue env bound function <> concatMap (lintValue env bound) arguments
    GrinCase scrutinee binder alternatives ->
      lintValue env bound scrutinee
        <> concatMap (lintAlt env (Set.insert binder bound)) alternatives
    GrinProject runtimeRep object index ->
      [GrinLintInvalidProjectIndex index | index < 0]
        <> [GrinLintProjectNonAtomic runtimeRep | runtimeRepComponents runtimeRep /= [runtimeRep]]
        <> lintProject runtimeRep object index
        <> lintValue env bound object
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
  where
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
    GrinNodeValue node -> lintNode env bound node

lintProject :: RuntimeRep -> GrinValue -> Int -> [GrinLintError]
lintProject expected object index =
  case object of
    GrinNodeValue node ->
      case grinNodeTag node of
        GrinConstructor _
          | index >= length fields -> [GrinLintInvalidProjectIndex index]
          | index >= 0,
            let actual = grinValueRuntimeRep (fields !! index),
            expected /= actual ->
              [GrinLintRepresentationMismatch "projection result" expected actual]
          | otherwise -> []
        tag -> [GrinLintProjectNonConstructor tag]
      where
        fields = grinNodeFields node
    _ -> []

lintNode :: LintEnv -> Set GrinVar -> GrinNode -> [GrinLintError]
lintNode env bound node =
  concatMap (lintValue env bound) (grinNodeFields node)
    <> lintNodeFunction env node
    <> lintConstructorFields env node

lintConstructorFields :: LintEnv -> GrinNode -> [GrinLintError]
lintConstructorFields env node =
  case grinNodeTag node of
    GrinConstructor name ->
      case Map.lookup name (lintConstructorLayouts env) of
        Just expected
          | expected /= actual -> [GrinLintConstructorLayout name expected actual]
        _ -> []
    _ -> []
  where
    actual = map grinValueRuntimeRep (grinNodeFields node)

lintNodeFunction :: LintEnv -> GrinNode -> [GrinLintError]
lintNodeFunction env node =
  case grinNodeTag node of
    GrinThunk functionName -> checkFunctionArity functionName fieldCount <> checkThunkResult functionName
    GrinClosure functionName argumentCount -> checkClosureArity functionName argumentCount
    _ -> []
  where
    fieldCount = length (grinNodeFields node)
    checkFunctionArity functionName actual =
      case Map.lookup functionName (lintFunctionArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected == actual -> []
          | otherwise -> [GrinLintFunctionArity functionName expected actual]
    checkClosureArity functionName argumentCount =
      checkFunctionArity functionName (fieldCount + argumentCount)
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
    GrinReturn values -> Just (map grinValueRuntimeRep values)
    GrinBind _ _ body -> exprRuntimeReps body
    GrinStore {} -> Just [liftedRuntimeRep]
    GrinStoreRec _ body -> exprRuntimeReps body
    GrinFetch runtimeRep _ -> Just (runtimeRepComponents runtimeRep)
    GrinUpdate _ value -> Just [grinValueRuntimeRep value]
    GrinEval runtimeRep _ -> Just (runtimeRepComponents runtimeRep)
    GrinApply runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinCase _ _ alternatives ->
      case alternatives of
        first : _ -> exprRuntimeReps (grinAltRhs first)
        [] -> Nothing
    GrinProject runtimeRep _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinThrow {} -> Nothing
    GrinCatch runtimeRep _ _ _ -> Just (runtimeRepComponents runtimeRep)
    GrinForeignCallExpr foreignCall _ ->
      Just (grinForeignCallResultReps (grinForeignCallSignature foreignCall))
