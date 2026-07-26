-- | Validate and query the GRIN facts required by native code generation.
module Aihc.Arm64.Codegen.Analysis
  ( findFunction,
    programNodes,
    programRuntimeReps,
    validatePrimitiveName,
    validateProgramPrimitives,
    validatePrimitiveNames,
    validateRuntimeRep,
  )
where

import Aihc.Arm64.Codegen.Types
import Aihc.Grin.Syntax
import Aihc.Native (supportedNativePrimitiveNames)
import Aihc.Tc.Types (RuntimeRep (..))
import Data.Text (Text)

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Arm64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Arm64Error ()
validatePrimitiveNames = mapM_ (validatePrimitiveName False)

validatePrimitiveName :: Bool -> Text -> Either Arm64Error ()
validatePrimitiveName allowUnsupported name
  | name `elem` supportedNativePrimitiveNames = Right ()
  | allowUnsupported = Right ()
  | otherwise = Left (Arm64UnsupportedPrimitive name)

validateRuntimeRep :: RuntimeRep -> Either Arm64Error ()
validateRuntimeRep runtimeRep =
  case runtimeRep of
    VecRep {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    TupleRep fieldReps -> mapM_ validateRuntimeRep fieldReps
    SumRep alternativeReps -> mapM_ validateRuntimeRep alternativeReps
    RuntimeRepVar {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    RuntimeRepMeta {} -> Left (Arm64UnsupportedRuntimeRep runtimeRep)
    _ -> Right ()

programRuntimeReps :: GrinProgram -> [RuntimeRep]
programRuntimeReps program =
  concatMap (concat . snd) (grinConstructors program)
    <> map (grinVarRuntimeRep . fst) (grinPrimitives program)
    <> concatMap globalRuntimeReps (grinWhnfGlobals program)
    <> concatMap cafRuntimeReps (grinCafs program)
    <> concatMap functionRuntimeReps (grinFunctions program)
  where
    globalRuntimeReps (var, node) = grinVarRuntimeRep var : nodeRuntimeReps node
    cafRuntimeReps (var, node) = grinVarRuntimeRep var : nodeRuntimeReps node
    functionRuntimeReps function =
      grinFunctionResultRep function
        : map grinVarRuntimeRep (grinFunctionParameters function)
          <> exprRuntimeReps (grinFunctionBody function)

programNodes :: GrinProgram -> [GrinNode]
programNodes program =
  map snd (grinWhnfGlobals program)
    <> map snd (grinCafs program)
    <> concatMap (exprNodes . grinFunctionBody) (grinFunctions program)

exprNodes :: GrinExpr -> [GrinNode]
exprNodes expression =
  case expression of
    GrinBind _ valueExpression body -> exprNodes valueExpression <> exprNodes body
    GrinStore node -> [node]
    GrinStoreUnchecked node -> [node]
    GrinStoreRec bindings body -> map snd bindings <> exprNodes body
    GrinStoreRecUnchecked bindings body -> map snd bindings <> exprNodes body
    GrinCase _ _ alternatives -> concatMap (exprNodes . grinAltRhs) alternatives
    GrinConstant {} -> []
    GrinEnsureHeap {} -> []
    GrinFetch {} -> []
    GrinUpdate {} -> []
    GrinUpdateBlackhole {} -> []
    GrinEval {} -> []
    GrinCpsEval {} -> []
    GrinCall {} -> []
    GrinPrimitiveCall {} -> []
    GrinCpsPrimitiveCall {} -> []
    GrinApply {} -> []
    GrinCpsApply {} -> []
    GrinContinue {} -> []
    GrinHalt {} -> []
    GrinThrow {} -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []

exprRuntimeReps :: GrinExpr -> [RuntimeRep]
exprRuntimeReps expression =
  case expression of
    GrinConstant values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinEnsureHeap _ roots -> concatMap valueRuntimeReps roots
    GrinStoreUnchecked node -> nodeRuntimeReps node
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    GrinStoreRecUnchecked bindings body ->
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    GrinFetch runtimeRep pointer -> runtimeRep : valueRuntimeReps pointer
    GrinUpdate pointer value -> valueRuntimeReps pointer <> valueRuntimeReps value
    GrinUpdateBlackhole pointer value -> valueRuntimeReps pointer <> valueRuntimeReps value
    GrinEval runtimeRep value -> runtimeRep : valueRuntimeReps value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      runtimeRep : concatMap valueRuntimeReps [value, continuation, updateContinuation]
    GrinCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinPrimitiveCall runtimeRep _ arguments -> runtimeRep : concatMap valueRuntimeReps arguments
    GrinCpsPrimitiveCall runtimeRep _ arguments continuation ->
      runtimeRep : concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinApply runtimeRep function arguments ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments <> valueRuntimeReps continuation
    GrinContinue continuation values -> valueRuntimeReps continuation <> concatMap valueRuntimeReps values
    GrinHalt values -> concatMap valueRuntimeReps values
    GrinCase scrutinee binder alternatives ->
      valueRuntimeReps scrutinee <> (grinVarRuntimeRep binder : concatMap altRuntimeReps alternatives)
    GrinThrow exception -> valueRuntimeReps exception
    GrinCatch runtimeRep action handler state -> runtimeRep : concatMap valueRuntimeReps (action : handler : state)
    GrinForeignCallExpr foreignCall arguments ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall) <> concatMap valueRuntimeReps arguments
  where
    altRuntimeReps alternative =
      map grinVarRuntimeRep (grinAltBinders alternative) <> exprRuntimeReps (grinAltRhs alternative)

valueRuntimeReps :: GrinValue -> [RuntimeRep]
valueRuntimeReps value = [grinValueRuntimeRep value]

nodeRuntimeReps :: GrinNode -> [RuntimeRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)

findFunction :: FunctionName -> [GrinFunction] -> Maybe GrinFunction
findFunction name =
  foldr
    (\function rest -> if grinFunctionName function == name then Just function else rest)
    Nothing
