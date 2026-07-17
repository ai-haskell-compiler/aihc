{-# LANGUAGE OverloadedStrings #-}

-- | Convert strict GRIN sequencing into explicit Loom continuations.
module Aihc.Cps.Lower
  ( LoomLowerError (..),
    lowerProgram,
  )
where

import Aihc.Cps.Syntax
import Aihc.Grin.Syntax
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)

-- | A violated precondition at the GRIN-to-Loom boundary. Exception control
-- must be expressed using ordinary GRIN control flow before CPS conversion.
data LoomLowerError
  = LoomLowerUnexpectedThrow !FunctionName
  | LoomLowerUnexpectedCatch !FunctionName
  deriving (Eq, Show)

lowerProgram :: GrinProgram -> Either LoomLowerError LoomProgram
lowerProgram program = do
  functions <- traverse lowerFunction (grinFunctions program)
  pure
    LoomProgram
      { loomConstructors = grinConstructors program,
        loomPrimitives = grinPrimitives program,
        loomForeignCalls = grinForeignCalls program,
        loomWhnfGlobals = grinWhnfGlobals program,
        loomCafs = grinCafs program,
        loomFunctions = functions
      }

lowerFunction :: GrinFunction -> Either LoomLowerError LoomFunction
lowerFunction function = do
  body <- evalStateT (lowerExpr functionName returnContinuation (grinFunctionBody function)) 1
  pure
    LoomFunction
      { loomFunctionName = functionName,
        loomFunctionParameters = grinFunctionParameters function,
        loomFunctionReturn = returnContinuation,
        loomFunctionBody = body
      }
  where
    functionName = grinFunctionName function
    returnContinuation = LoomContVar "return" 0 (runtimeRepComponents (grinFunctionResultRep function))

type Lower = StateT Int (Either LoomLowerError)

lowerExpr :: FunctionName -> LoomContVar -> GrinExpr -> Lower LoomTerm
lowerExpr functionName successor expression =
  case expression of
    GrinReturn values -> pure (LoomContinue successor values)
    GrinBind parameters valueExpression body -> do
      continuationName <- freshContinuation parameters
      continuationBody <- lowerExpr functionName successor body
      valueTerm <- lowerExpr functionName continuationName valueExpression
      pure
        ( LoomLetCont
            LoomContinuation
              { loomContinuationName = continuationName,
                loomContinuationParameters = parameters,
                loomContinuationBody = continuationBody
              }
            valueTerm
        )
    GrinStore node -> pure (LoomInvoke (LoomStore node) successor)
    GrinStoreRec bindings body -> LoomStoreRec bindings <$> lowerExpr functionName successor body
    GrinFetch runtimeRep pointer -> pure (LoomInvoke (LoomFetch runtimeRep pointer) successor)
    GrinUpdate pointer value -> pure (LoomInvoke (LoomUpdate pointer value) successor)
    GrinEval runtimeRep value -> pure (LoomInvoke (LoomEval runtimeRep value) successor)
    GrinApply runtimeRep function arguments -> pure (LoomInvoke (LoomApply runtimeRep function arguments) successor)
    GrinCase scrutinee binder alternatives ->
      LoomCase scrutinee binder <$> mapM (lowerAlt functionName successor) alternatives
    GrinThrow {} -> lift (Left (LoomLowerUnexpectedThrow functionName))
    GrinCatch {} -> lift (Left (LoomLowerUnexpectedCatch functionName))
    GrinForeignCallExpr foreignCall arguments -> pure (LoomInvoke (LoomForeignCall foreignCall arguments) successor)

lowerAlt :: FunctionName -> LoomContVar -> GrinAlt -> Lower LoomAlt
lowerAlt functionName successor alternative =
  LoomAlt
    (grinAltCon alternative)
    (grinAltBinders alternative)
    <$> lowerExpr functionName successor (grinAltRhs alternative)

freshContinuation :: [GrinVar] -> Lower LoomContVar
freshContinuation parameters = do
  unique <- get
  put (unique + 1)
  pure (LoomContVar "k" unique (map grinVarRuntimeRep parameters))
