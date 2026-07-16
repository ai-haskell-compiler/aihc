-- | Explicit continuation form for runtime-explicit GRIN.
--
-- The transformation removes 'GrinBind' from the control spine. Every
-- operation receives its success continuation, including scheduler
-- operations that may either complete immediately or suspend the fiber.
module Aihc.Grin.Cps
  ( CpsExpr (..),
    CpsContinuation (..),
    CpsOperation (..),
    CpsAlt (..),
    cpsExpr,
    schedulerContinuations,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep)

data CpsExpr
  = CpsContinue !CpsContinuation !GrinValue
  | CpsOperation !CpsOperation !CpsContinuation
  | CpsStoreRec ![(GrinVar, GrinNode)] !CpsExpr
  | CpsCase !GrinValue !GrinVar ![CpsAlt]
  deriving (Eq, Show)

data CpsContinuation
  = CpsReturn
  | CpsBind !GrinVar !CpsExpr
  deriving (Eq, Show)

data CpsOperation
  = CpsStore !GrinNode
  | CpsFetch !RuntimeRep !GrinValue
  | CpsUpdate !GrinValue !GrinValue
  | CpsEval !RuntimeRep !GrinValue
  | CpsApply !RuntimeRep !GrinValue !GrinValue
  | CpsDictSelect !RuntimeRep !GrinValue !Int
  | CpsThrow !GrinValue
  | CpsCatch !RuntimeRep !GrinValue !GrinValue !GrinValue
  | CpsScheduler !RuntimeRep !SchedulerPrimOp ![GrinValue]
  | CpsForeignCall !GrinForeignCall ![GrinValue]
  deriving (Eq, Show)

data CpsAlt = CpsAlt
  { cpsAltCon :: !GrinAltCon,
    cpsAltBinders :: ![GrinVar],
    cpsAltRhs :: !CpsExpr
  }
  deriving (Eq, Show)

cpsExpr :: GrinExpr -> CpsExpr
cpsExpr expression = transform expression CpsReturn

transform :: GrinExpr -> CpsContinuation -> CpsExpr
transform expression continuation =
  case expression of
    GrinReturn value -> CpsContinue continuation value
    GrinBind binder valueExpression body ->
      transform valueExpression (CpsBind binder (transform body continuation))
    GrinStore node -> CpsOperation (CpsStore node) continuation
    GrinStoreRec bindings body -> CpsStoreRec bindings (transform body continuation)
    GrinFetch runtimeRep pointer -> CpsOperation (CpsFetch runtimeRep pointer) continuation
    GrinUpdate pointer value -> CpsOperation (CpsUpdate pointer value) continuation
    GrinEval runtimeRep value -> CpsOperation (CpsEval runtimeRep value) continuation
    GrinApply runtimeRep function argument ->
      CpsOperation (CpsApply runtimeRep function argument) continuation
    GrinCase scrutinee binder alternatives ->
      CpsCase scrutinee binder (map (transformAlt continuation) alternatives)
    GrinDictSelect runtimeRep dictionary index ->
      CpsOperation (CpsDictSelect runtimeRep dictionary index) continuation
    GrinThrow exception -> CpsOperation (CpsThrow exception) continuation
    GrinCatch runtimeRep action handler state ->
      CpsOperation (CpsCatch runtimeRep action handler state) continuation
    GrinScheduler runtimeRep schedulerOp arguments ->
      CpsOperation (CpsScheduler runtimeRep schedulerOp arguments) continuation
    GrinForeignCallExpr foreignCall arguments ->
      CpsOperation (CpsForeignCall foreignCall arguments) continuation

transformAlt :: CpsContinuation -> GrinAlt -> CpsAlt
transformAlt continuation alternative =
  CpsAlt
    { cpsAltCon = grinAltCon alternative,
      cpsAltBinders = grinAltBinders alternative,
      cpsAltRhs = transform (grinAltRhs alternative) continuation
    }

-- | Collect scheduler operations together with the continuation saved when
-- the operation suspends. This is useful to validate backend lowering and GC
-- root metadata.
schedulerContinuations :: CpsExpr -> [(SchedulerPrimOp, CpsContinuation)]
schedulerContinuations expression =
  case expression of
    CpsContinue continuation _ -> continuationSchedulers continuation
    CpsOperation operation continuation ->
      operationScheduler operation continuation <> continuationSchedulers continuation
    CpsStoreRec _ body -> schedulerContinuations body
    CpsCase _ _ alternatives -> concatMap (schedulerContinuations . cpsAltRhs) alternatives
  where
    operationScheduler operation continuation =
      case operation of
        CpsScheduler _ schedulerOp _ -> [(schedulerOp, continuation)]
        _ -> []

continuationSchedulers :: CpsContinuation -> [(SchedulerPrimOp, CpsContinuation)]
continuationSchedulers continuation =
  case continuation of
    CpsReturn -> []
    CpsBind _ body -> schedulerContinuations body
