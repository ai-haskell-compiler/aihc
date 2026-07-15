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
  | GrinLintDuplicateCaf !GrinVar
  | GrinLintUnboundVariable !GrinVar
  | GrinLintUnknownFunction !FunctionName
  | GrinLintFunctionArity !FunctionName !Int !Int
  | GrinLintRepresentationMismatch !String !RuntimeRep !RuntimeRep
  | GrinLintEvalNonLifted !RuntimeRep
  | GrinLintConstructorLayout !Text ![RuntimeRep] ![RuntimeRep]
  deriving (Eq, Show)

data LintEnv = LintEnv
  { lintFunctionArities :: !(Map FunctionName Int),
    lintGlobalVars :: !(Set GrinVar),
    lintGlobalNames :: !(Set Text),
    lintConstructorLayouts :: !(Map Text [RuntimeRep])
  }

lintProgram :: GrinProgram -> [GrinLintError]
lintProgram program =
  duplicateFunctionErrors
    <> duplicateCafErrors
    <> concatMap (lintCaf env) (grinCafs program)
    <> concatMap (lintFunction env) (grinFunctions program)
  where
    functions = grinFunctions program
    cafs = grinCafs program
    functionNames = map grinFunctionName functions
    cafVars = map fst cafs
    duplicateFunctionErrors = map GrinLintDuplicateFunction (duplicates functionNames)
    duplicateCafErrors = map GrinLintDuplicateCaf (duplicates cafVars)
    env =
      LintEnv
        { lintFunctionArities =
            Map.fromList
              [ (grinFunctionName function, length (grinFunctionParameters function))
              | function <- functions
              ],
          lintGlobalVars = Set.fromList cafVars,
          lintGlobalNames =
            Set.fromList
              ( map fst builtinConstructors
                  <> map fst (grinConstructors program)
                  <> map (grinVarName . fst) (grinPrimitives program)
                  <> map grinForeignCallName (grinForeignCalls program)
                  <> map grinVarName cafVars
              ),
          lintConstructorLayouts = Map.fromList (grinConstructors program)
        }

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
      case exprRuntimeRep (grinFunctionBody function) of
        Just actual
          | actual /= grinFunctionResultRep function ->
              [GrinLintRepresentationMismatch "function result" (grinFunctionResultRep function) actual]
        _ -> []

lintExpr :: LintEnv -> Set GrinVar -> GrinExpr -> [GrinLintError]
lintExpr env bound expr =
  case expr of
    GrinReturn value -> lintValue env bound value
    GrinBind var valueExpr body ->
      bindRepresentationErrors var valueExpr
        <> lintExpr env bound valueExpr
        <> lintExpr env (Set.insert var bound) body
    GrinStore node -> lintNode env bound node
    GrinStoreRec bindings body ->
      let recursiveBound = Set.fromList (map fst bindings) <> bound
       in concatMap (lintNode env recursiveBound . snd) bindings
            <> lintExpr env recursiveBound body
    GrinFetch _ pointer -> lintValue env bound pointer
    GrinUpdate pointer value -> lintValue env bound pointer <> lintValue env bound value
    GrinEval _ value ->
      [GrinLintEvalNonLifted runtimeRep | let runtimeRep = grinValueRuntimeRep value, runtimeRep /= liftedRuntimeRep]
        <> lintValue env bound value
    GrinApply _ function argument -> lintValue env bound function <> lintValue env bound argument
    GrinCase scrutinee binder alternatives ->
      lintValue env bound scrutinee
        <> concatMap (lintAlt env (Set.insert binder bound)) alternatives
    GrinDictSelect _ dictionary _ -> lintValue env bound dictionary
    GrinThrow exception -> lintValue env bound exception
    GrinCatch _ action handler state ->
      lintValue env bound action
        <> lintValue env bound handler
        <> lintValue env bound state
  where
    bindRepresentationErrors var valueExpr =
      case exprRuntimeRep valueExpr of
        Just actual
          | actual /= grinVarRuntimeRep var ->
              [GrinLintRepresentationMismatch "bind" (grinVarRuntimeRep var) actual]
        _ -> []

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
    GrinThunk functionName -> checkFunctionArity functionName fieldCount
    GrinClosure functionName -> checkFunctionArity functionName (fieldCount + 1)
    _ -> []
  where
    fieldCount = length (grinNodeFields node)
    checkFunctionArity functionName actual =
      case Map.lookup functionName (lintFunctionArities env) of
        Nothing -> [GrinLintUnknownFunction functionName]
        Just expected
          | expected == actual -> []
          | otherwise -> [GrinLintFunctionArity functionName expected actual]

duplicates :: (Ord a) => [a] -> [a]
duplicates = go Set.empty Set.empty
  where
    go _ repeated [] = Set.toAscList repeated
    go seen repeated (value : rest)
      | value `Set.member` seen = go seen (Set.insert value repeated) rest
      | otherwise = go (Set.insert value seen) repeated rest

exprRuntimeRep :: GrinExpr -> Maybe RuntimeRep
exprRuntimeRep expr =
  case expr of
    GrinReturn value -> Just (grinValueRuntimeRep value)
    GrinBind _ _ body -> exprRuntimeRep body
    GrinStore {} -> Just liftedRuntimeRep
    GrinStoreRec _ body -> exprRuntimeRep body
    GrinFetch runtimeRep _ -> Just runtimeRep
    GrinUpdate _ value -> Just (grinValueRuntimeRep value)
    GrinEval runtimeRep _ -> Just runtimeRep
    GrinApply runtimeRep _ _ -> Just runtimeRep
    GrinCase _ _ alternatives ->
      case alternatives of
        first : _ -> exprRuntimeRep (grinAltRhs first)
        [] -> Nothing
    GrinDictSelect runtimeRep _ _ -> Just runtimeRep
    GrinThrow {} -> Nothing
    GrinCatch runtimeRep _ _ _ -> Just runtimeRep
