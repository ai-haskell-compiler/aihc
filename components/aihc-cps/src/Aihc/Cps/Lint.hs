-- | Structural and calling-convention validation for Loom programs.
module Aihc.Cps.Lint
  ( LoomLintError (..),
    lintProgram,
  )
where

import Aihc.Cps.Syntax
import Aihc.Grin.Syntax (grinValueRuntimeRep, grinVarRuntimeRep)
import Aihc.Tc.Types (RuntimeRep)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map

data LoomLintError
  = LoomLintDuplicateContinuation !LoomContVar
  | LoomLintUnboundContinuation !LoomContVar
  | LoomLintContinuationSignature !LoomContVar ![RuntimeRep] ![RuntimeRep]
  | LoomLintContinuationParameters !LoomContVar ![RuntimeRep] ![RuntimeRep]
  | LoomLintContinuationArguments !LoomContVar ![RuntimeRep] ![RuntimeRep]
  | LoomLintOperationContinuation !LoomContVar ![RuntimeRep] ![RuntimeRep]
  deriving (Eq, Show)

type ContEnv = Map LoomContVar [RuntimeRep]

lintProgram :: LoomProgram -> [LoomLintError]
lintProgram = concatMap lintFunction . loomFunctions

lintFunction :: LoomFunction -> [LoomLintError]
lintFunction function =
  lintTerm
    (Map.singleton returnContinuation (loomContRuntimeReps returnContinuation))
    (loomFunctionBody function)
  where
    returnContinuation = loomFunctionReturn function

lintTerm :: ContEnv -> LoomTerm -> [LoomLintError]
lintTerm env term =
  case term of
    LoomLetCont continuation body ->
      declarationErrors
        <> lintTerm env (loomContinuationBody continuation)
        <> lintTerm extendedEnv body
      where
        continuationName = loomContinuationName continuation
        parameterReps = map grinVarRuntimeRep (loomContinuationParameters continuation)
        signature = loomContRuntimeReps continuationName
        declarationErrors =
          [LoomLintDuplicateContinuation continuationName | Map.member continuationName env]
            <> [ LoomLintContinuationParameters continuationName signature parameterReps
               | signature /= parameterReps
               ]
        extendedEnv = Map.insert continuationName signature env
    LoomInvoke operation continuation ->
      lintContinuation env continuation
        <> case loomOperationResultReps operation of
          Just resultReps
            | resultReps /= loomContRuntimeReps continuation ->
                [LoomLintOperationContinuation continuation (loomContRuntimeReps continuation) resultReps]
          _ -> []
    LoomContinue continuation arguments ->
      lintContinuation env continuation
        <> [ LoomLintContinuationArguments continuation expected actual
           | expected /= actual
           ]
      where
        expected = loomContRuntimeReps continuation
        actual = map grinValueRuntimeRep arguments
    LoomCase _ _ alternatives -> concatMap (lintTerm env . loomAltBody) alternatives
    LoomStoreRec _ body -> lintTerm env body

lintContinuation :: ContEnv -> LoomContVar -> [LoomLintError]
lintContinuation env continuation =
  case Map.lookup continuation env of
    Nothing -> [LoomLintUnboundContinuation continuation]
    Just declaredSignature
      | declaredSignature /= actualSignature ->
          [LoomLintContinuationSignature continuation declaredSignature actualSignature]
      | otherwise -> []
  where
    actualSignature = loomContRuntimeReps continuation
