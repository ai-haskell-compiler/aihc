-- | Make GRIN variable numbers easier to read.
--
-- Lowering hands out one globally descending unique per variable, which keeps
-- every binder distinct but leaves the printed program full of noise like
-- @x%-1000000000@. Numbering is only needed to tell apart two binders that
-- share a name, so this pass renumbers each function from zero: the first
-- binder called @x@ becomes @x%0@, the next distinct one @x%1@, and a name
-- that occurs once keeps the number the printer omits entirely.
--
-- Numbers are not reused between sibling scopes even though their binders are
-- never live at the same time. 'GrinVar' equality ignores the runtime
-- representation, and the code generators key one map per function on that
-- equality, so two same-named binders with different representations must not
-- collide. Distinctness per function is therefore the invariant to preserve,
-- not merely distinctness per scope.
module Aihc.Grin.Tidy
  ( tidyGrinProgram,
  )
where

import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, gets, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)

data TidyState = TidyState
  { tidyNextNumbers :: !(Map Text Int),
    tidyRenaming :: !(Map GrinVar GrinVar)
  }

type TidyM = State TidyState

-- | Renumber the local variables of every function.
tidyGrinProgram :: GrinProgram -> GrinProgram
tidyGrinProgram program =
  program
    { grinPrimitives = map tidyPrimitive (grinPrimitives program),
      grinFunctions = map tidyFunction (grinFunctions program)
    }

-- | A primitive declaration only carries a name and a result representation;
-- nothing refers to it as a variable, so it needs no number at all.
tidyPrimitive :: (GrinVar, Int) -> (GrinVar, Int)
tidyPrimitive (var, arity) = (var {grinVarUnique = 0}, arity)

tidyFunction :: GrinFunction -> GrinFunction
tidyFunction function =
  evalState tidy (TidyState Map.empty Map.empty)
  where
    tidy = do
      parameters <- mapM bindVar (grinFunctionParameters function)
      body <- tidyExpr (grinFunctionBody function)
      pure
        function
          { grinFunctionParameters = parameters,
            grinFunctionBody = body
          }

-- | Traverse in printing order so that a binder is always renumbered before
-- the uses that follow it.
tidyExpr :: GrinExpr -> TidyM GrinExpr
tidyExpr expression =
  case expression of
    GrinConstant values -> GrinConstant <$> useValues values
    GrinBind vars valueExpression body -> do
      vars' <- mapM bindVar vars
      GrinBind vars' <$> tidyExpr valueExpression <*> tidyExpr body
    GrinStore node -> GrinStore <$> useNode node
    GrinStoreUnchecked node -> GrinStoreUnchecked <$> useNode node
    GrinEnsureHeap requiredWords roots ->
      GrinEnsureHeap <$> useValue requiredWords <*> useValues roots
    GrinStoreRec bindings body -> tidyStoreRec GrinStoreRec bindings body
    GrinStoreRecUnchecked bindings body -> tidyStoreRec GrinStoreRecUnchecked bindings body
    GrinFetch runtimeRep pointer -> GrinFetch runtimeRep <$> useValue pointer
    GrinUpdate pointer value -> GrinUpdate <$> useValue pointer <*> useValue value
    GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole <$> useValue pointer <*> useValue value
    GrinEval runtimeRep value -> GrinEval runtimeRep <$> useValue value
    GrinCpsEval runtimeRep value continuation updateContinuation ->
      GrinCpsEval runtimeRep <$> useValue value <*> useValue continuation <*> useValue updateContinuation
    GrinCall runtimeRep functionName arguments ->
      GrinCall runtimeRep functionName <$> useValues arguments
    GrinPrimitiveCall runtimeRep name arguments ->
      GrinPrimitiveCall runtimeRep name <$> useValues arguments
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      GrinCpsPrimitiveCall runtimeRep name <$> useValues arguments <*> useValue continuation
    GrinApply runtimeRep function arguments ->
      GrinApply runtimeRep <$> useValue function <*> useValues arguments
    GrinCpsApply runtimeRep function arguments continuation ->
      GrinCpsApply runtimeRep <$> useValue function <*> useValues arguments <*> useValue continuation
    GrinContinue continuation arguments ->
      GrinContinue <$> useValue continuation <*> useValues arguments
    GrinCpsRaise exception continuation ->
      GrinCpsRaise <$> useValue exception <*> useValue continuation
    GrinHalt values -> GrinHalt <$> useValues values
    GrinExit status -> GrinExit <$> useValue status
    GrinCase scrutinee binder alternatives -> do
      scrutinee' <- useValue scrutinee
      binder' <- bindVar binder
      GrinCase scrutinee' binder' <$> mapM tidyAlternative alternatives
    GrinThrow exception -> GrinThrow <$> useValue exception
    GrinCatch runtimeRep action handler state ->
      GrinCatch runtimeRep <$> useValue action <*> useValue handler <*> useValues state
    GrinForeignCallExpr foreignCall arguments ->
      GrinForeignCallExpr foreignCall <$> useValues arguments

-- | Recursive allocations bind their own names before their nodes.
tidyStoreRec :: ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> [(GrinVar, GrinNode)] -> GrinExpr -> TidyM GrinExpr
tidyStoreRec rebuild bindings body = do
  vars <- mapM (bindVar . fst) bindings
  nodes <- mapM (useNode . snd) bindings
  rebuild (zip vars nodes) <$> tidyExpr body

tidyAlternative :: GrinAlt -> TidyM GrinAlt
tidyAlternative alternative = do
  binders <- mapM bindVar (grinAltBinders alternative)
  rhs <- tidyExpr (grinAltRhs alternative)
  pure alternative {grinAltBinders = binders, grinAltRhs = rhs}

useNode :: GrinNode -> TidyM GrinNode
useNode node = do
  fields <- useValues (grinNodeFields node)
  pure node {grinNodeFields = fields}

useValues :: [GrinValue] -> TidyM [GrinValue]
useValues = mapM useValue

useValue :: GrinValue -> TidyM GrinValue
useValue value =
  case value of
    GrinVarValue var -> GrinVarValue <$> useVar var
    GrinGlobalValue {} -> pure value
    GrinLitValue {} -> pure value

-- | A well-formed function binds every variable it mentions. An unbound
-- variable keeps its original number so this pass never invents a name clash.
useVar :: GrinVar -> TidyM GrinVar
useVar var = gets (Map.findWithDefault var var . tidyRenaming)

bindVar :: GrinVar -> TidyM GrinVar
bindVar var = do
  TidyState nextNumbers renaming <- get
  let name = grinVarName var
      number = Map.findWithDefault 0 name nextNumbers
      var' = var {grinVarUnique = number}
  put
    TidyState
      { tidyNextNumbers = Map.insert name (number + 1) nextNumbers,
        tidyRenaming = Map.insert var var' renaming
      }
  pure var'
