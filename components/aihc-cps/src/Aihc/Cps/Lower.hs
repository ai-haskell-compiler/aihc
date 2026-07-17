{-# LANGUAGE OverloadedStrings #-}

-- | Convert strict GRIN sequencing into explicit Loom continuations.
module Aihc.Cps.Lower
  ( lowerProgram,
  )
where

import Aihc.Cps.Syntax
import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, put)

lowerProgram :: GrinProgram -> LoomProgram
lowerProgram program =
  LoomProgram
    { loomConstructors = grinConstructors program,
      loomPrimitives = grinPrimitives program,
      loomForeignCalls = grinForeignCalls program,
      loomWhnfGlobals = grinWhnfGlobals program,
      loomCafs = grinCafs program,
      loomFunctions = map lowerFunction (grinFunctions program)
    }

lowerFunction :: GrinFunction -> LoomFunction
lowerFunction function =
  LoomFunction
    { loomFunctionName = grinFunctionName function,
      loomFunctionParameters = grinFunctionParameters function,
      loomFunctionReturn = returnContinuation,
      loomFunctionBody = evalState (lowerExpr returnContinuation (grinFunctionBody function)) 1
    }
  where
    returnContinuation = LoomContVar "return" 0 (runtimeRepComponents (grinFunctionResultRep function))

lowerExpr :: LoomContVar -> GrinExpr -> State Int LoomTerm
lowerExpr successor expression =
  case expression of
    GrinReturn values -> pure (LoomContinue successor values)
    GrinBind parameters valueExpression body -> do
      continuationName <- freshContinuation parameters
      continuationBody <- lowerExpr successor body
      valueTerm <- lowerExpr continuationName valueExpression
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
    GrinStoreRec bindings body -> LoomStoreRec bindings <$> lowerExpr successor body
    GrinFetch runtimeRep pointer -> pure (LoomInvoke (LoomFetch runtimeRep pointer) successor)
    GrinUpdate pointer value -> pure (LoomInvoke (LoomUpdate pointer value) successor)
    GrinEval runtimeRep value -> pure (LoomInvoke (LoomEval runtimeRep value) successor)
    GrinApply runtimeRep function arguments -> pure (LoomInvoke (LoomApply runtimeRep function arguments) successor)
    GrinCase scrutinee binder alternatives ->
      LoomCase scrutinee binder <$> mapM (lowerAlt successor) alternatives
    GrinDictSelect runtimeRep dictionary index -> pure (LoomInvoke (LoomDictSelect runtimeRep dictionary index) successor)
    GrinThrow exception -> pure (LoomInvoke (LoomThrow exception) successor)
    GrinCatch runtimeRep action handler state -> pure (LoomInvoke (LoomCatch runtimeRep action handler state) successor)
    GrinForeignCallExpr foreignCall arguments -> pure (LoomInvoke (LoomForeignCall foreignCall arguments) successor)

lowerAlt :: LoomContVar -> GrinAlt -> State Int LoomAlt
lowerAlt successor alternative =
  LoomAlt
    (grinAltCon alternative)
    (grinAltBinders alternative)
    <$> lowerExpr successor (grinAltRhs alternative)

freshContinuation :: [GrinVar] -> State Int LoomContVar
freshContinuation parameters = do
  unique <- get
  put (unique + 1)
  pure (LoomContVar "k" unique (map grinVarRuntimeRep parameters))
