{-# LANGUAGE OverloadedStrings #-}

-- | Make post-CPS allocation safepoints and relocated roots explicit.
module Aihc.Grin.Gc
  ( GcGrinProgram,
    entryGcProgram,
    gcContinuationFrames,
    gcContinuationFunctions,
    gcFunctionContinuations,
    gcGrinProgram,
    lowerGc,
    nodeWords,
    dynamicAllocationSize,
  )
where

import Aihc.Grin.Analysis (freeExprVars, maximumProgramVarUnique)
import Aihc.Grin.Cps (ContinuationFrameKind, CpsGrinError, CpsGrinProgram (..), toCpsGrin)
import Aihc.Grin.Heap (normalizeHeapReservations)
import Aihc.Grin.Primitive (primitiveHeapWords, primitiveInitializes)
import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)

-- | A CPS-GRIN program whose managed allocations have explicit safepoints.
-- Keeping this phase distinct prevents native backends from accidentally
-- consuming CPS-GRIN before roots have been made relocatable.
data GcGrinProgram = GcGrinProgram
  { gcGrinProgram :: !GrinProgram,
    gcContinuationFunctions :: !(Set FunctionName),
    gcContinuationFrames :: !(Map FunctionName ContinuationFrameKind),
    gcFunctionContinuations :: !(Map FunctionName GrinVar)
  }
  deriving (Eq, Show, Read)

-- | Make the fixed GC-GRIN unit for the executable entry archive.
entryGcProgram :: Either CpsGrinError GcGrinProgram
entryGcProgram =
  lowerGc
    <$> toCpsGrin
      GrinProgram
        { grinConstructors = [],
          grinPrimitives = [],
          grinForeignCalls = [],
          grinGlobals = [],
          grinFunctions = []
        }

-- | Insert allocations and normalize nursery reservations. Give each safepoint explicit live
-- roots and fresh SSA names for roots that collection can relocate.
lowerGc :: CpsGrinProgram -> GcGrinProgram
lowerGc cps =
  GcGrinProgram
    { gcGrinProgram =
        normalizedProgram
          { grinFunctions = evalState (mapM relocateFunction (grinFunctions normalizedProgram)) nextUnique
          },
      gcContinuationFunctions = cpsContinuationFunctions cps,
      gcContinuationFrames = cpsContinuationFrames cps,
      gcFunctionContinuations = cpsFunctionContinuations cps
    }
  where
    program = cpsGrinProgram cps
    dynamicFunctions = evalState (mapM insertDynamicFunction (grinFunctions program)) (1 + maximumProgramVarUnique program)
    dynamicProgram = program {grinFunctions = dynamicFunctions, grinPrimitives = grinPrimitives program <> dynamicDeclarations}
    dynamicDeclarations =
      [ (GrinVar helper (-2000000001) (if primitiveInitializes helper then BoxedRep Unlifted else WordRep), arity)
      | (names, helper, arity) <- allocationSizeHelpers <> initializationHelpers,
        any (\(var, _) -> grinVarName var `elem` names) (grinPrimitives program)
      ]
    normalizedProgram = normalizeHeapReservations (insertHeapReservations (cpsContinuationFunctions cps) dynamicProgram)
    nextUnique = 1 + maximumProgramVarUnique normalizedProgram

-- | Size helpers validate counts before allocation. They cannot collect.
allocationSizeHelpers :: [([Text], Text, Int)]
allocationSizeHelpers =
  [ (["newByteArray#", "newPinnedByteArray#", "newAlignedPinnedByteArray#"], "aihcByteArrayWords#", 3),
    (["resizeMutableByteArray#"], "aihcResizeByteArrayWords#", 2),
    (arrayCountPrimitives, "aihcArrayWords#", 1)
  ]

-- | The boxed-array primitives that allocate an array of a given element
-- count. The small-array family shares the representation and the helper.
arrayCountPrimitives :: [Text]
arrayCountPrimitives =
  [ "newArray#",
    "cloneArray#",
    "cloneMutableArray#",
    "freezeArray#",
    "thawArray#",
    "newSmallArray#",
    "cloneSmallArray#",
    "cloneSmallMutableArray#",
    "freezeSmallArray#",
    "thawSmallArray#",
    "resizeSmallMutableArray#"
  ]

insertDynamicFunction :: GrinFunction -> State Int GrinFunction
insertDynamicFunction function = do
  body <- insertDynamic (grinFunctionBody function)
  pure function {grinFunctionBody = body}

insertDynamic :: GrinExpr -> State Int GrinExpr
insertDynamic expression = case expression of
  GrinBind results (GrinPrimitiveCall resultRep name arguments) body
    | Just (helper, operands) <- dynamicAllocationSize name arguments -> do
        unique <- get
        put (unique + 3)
        let wordsVar = GrinVar "$gc_words" unique WordRep
            memoryVar = GrinVar "$gc_memory" (unique + 1) AddrRep
            pinnedVar = GrinVar "$gc_pinned" (unique + 2) WordRep
            memory = GrinVarValue memoryVar
            (initializer, initialArguments) = initializePrimitive name arguments
            pinned = case name of
              "newPinnedByteArray#" -> literal 1
              "newAlignedPinnedByteArray#" -> literal 1
              "resizeMutableByteArray#" -> GrinVarValue pinnedVar
              _ -> literal 0
            withPinned rest = case (name, arguments) of
              ("resizeMutableByteArray#", array : _) -> GrinBind [pinnedVar] (GrinPrimitiveCall WordRep "aihcByteArrayPinned#" [array]) rest
              _ -> rest
        rest <- insertDynamic body
        pure
          ( GrinBind
              [wordsVar]
              (GrinPrimitiveCall WordRep helper operands)
              (withPinned (GrinBind [memoryVar] (GrinAllocate (GrinVarValue wordsVar) pinned []) (GrinBind results (GrinPrimitiveCall resultRep initializer (memory : initialArguments)) rest)))
          )
  GrinBind results value body -> GrinBind results <$> insertDynamic value <*> insertDynamic body
  GrinCase value binder alternatives -> GrinCase value binder <$> mapM alter alternatives
  GrinIfWhnf value ready slow -> GrinIfWhnf value <$> insertDynamic ready <*> insertDynamic slow
  GrinStoreRec bindings body -> GrinStoreRec bindings <$> insertDynamic body
  GrinStoreRecUnchecked bindings body -> GrinStoreRecUnchecked bindings <$> insertDynamic body
  _ -> pure expression
  where
    literal = GrinLitValue . GrinLitInt WordRep
    alter alternative = do
      rhs <- insertDynamic (grinAltRhs alternative)
      pure alternative {grinAltRhs = rhs}

-- | Initialization writes into the address returned by allocation.
-- These primitives do not allocate or collect.
initializationHelpers :: [([Text], Text, Int)]
initializationHelpers =
  [ (["newArray#", "newSmallArray#"], "aihcInitArray#", 3),
    (filter (`notElem` ["newArray#", "newSmallArray#", "resizeSmallMutableArray#"]) arrayCountPrimitives, "aihcInitArrayClone#", 4),
    (["resizeSmallMutableArray#"], "aihcInitArrayResize#", 4),
    (["newByteArray#", "newPinnedByteArray#", "newAlignedPinnedByteArray#"], "aihcInitByteArray#", 4),
    (["resizeMutableByteArray#"], "aihcInitByteArrayResize#", 3),
    (["resizeMutableByteArray#"], "aihcByteArrayPinned#", 1)
  ]

initializePrimitive :: Text -> [GrinValue] -> (Text, [GrinValue])
initializePrimitive name arguments = case (name, arguments) of
  ("newByteArray#", [size]) -> ("aihcInitByteArray#", [size, literal 0, literal 8])
  ("newPinnedByteArray#", [size]) -> ("aihcInitByteArray#", [size, literal 1, literal 8])
  ("newAlignedPinnedByteArray#", [size, alignment]) -> ("aihcInitByteArray#", [size, literal 1, alignment])
  ("resizeMutableByteArray#", _) -> ("aihcInitByteArrayResize#", arguments)
  ("resizeSmallMutableArray#", _) -> ("aihcInitArrayResize#", arguments)
  _ | name `elem` ["newArray#", "newSmallArray#"] -> ("aihcInitArray#", arguments)
  _ -> ("aihcInitArrayClone#", arguments)
  where
    literal = GrinLitValue . GrinLitInt IntRep

-- | The size helper and its operands for a primitive whose allocation size
-- depends on an argument. A boxed array of n elements takes n plus two
-- words: the header and the length.
dynamicAllocationSize :: Text -> [GrinValue] -> Maybe (Text, [GrinValue])
dynamicAllocationSize name arguments = case (name, arguments) of
  ("newByteArray#", [size]) -> Just ("aihcByteArrayWords#", [size, literal 0, literal 8])
  ("newPinnedByteArray#", [size]) -> Just ("aihcByteArrayWords#", [size, literal 1, literal 8])
  ("newAlignedPinnedByteArray#", [size, alignment]) -> Just ("aihcByteArrayWords#", [size, literal 1, alignment])
  ("resizeMutableByteArray#", [array, size]) -> Just ("aihcResizeByteArrayWords#", [array, size])
  ("newArray#", [count, _]) -> arrayWords count
  ("newSmallArray#", [count, _]) -> arrayWords count
  ("resizeSmallMutableArray#", [_, count, _]) -> arrayWords count
  (_, [_, _, count])
    | name `elem` arrayCountPrimitives -> arrayWords count
  _ -> Nothing
  where
    literal = GrinLitValue . GrinLitInt IntRep
    arrayWords count = Just ("aihcArrayWords#", [count])

-- | Give each managed store a reservation. A continuation frame is not a
-- managed object: the backend pushes it on the stack of the thread, which
-- never collects. But the runtime charges a new stack chunk to the nursery,
-- because the next minor collection scans it. Thus a frame store gets a
-- reservation of zero words: a recursion that does not allocate then
-- collects after about one nursery of stack growth.
insertHeapReservations :: Set FunctionName -> GrinProgram -> GrinProgram
insertHeapReservations continuations program =
  program {grinFunctions = map (insertFunctionReservations continuations) (grinFunctions program)}

insertFunctionReservations :: Set FunctionName -> GrinFunction -> GrinFunction
insertFunctionReservations continuations function =
  function {grinFunctionBody = insertExprReservations continuations (grinFunctionBody function)}

insertExprReservations :: Set FunctionName -> GrinExpr -> GrinExpr
insertExprReservations continuations expression =
  case expression of
    GrinBind resultVars (GrinStore node) body
      | isFrame node ->
          GrinBind
            []
            (GrinEnsureNursery 0 [])
            (GrinBind resultVars (GrinStoreUnchecked node) (insertExprReservations continuations body))
    GrinBind resultVars (GrinStore node) body ->
      GrinBind
        []
        (GrinEnsureNursery (toInteger (nodeWords node)) [])
        (GrinBind resultVars (GrinStoreUnchecked node) (insertExprReservations continuations body))
    GrinBind resultVars call@(GrinPrimitiveCall _ name _) body
      | Just requiredWords <- primitiveHeapWords name ->
          GrinBind
            []
            (GrinEnsureNursery (toInteger requiredWords) [])
            (GrinBind resultVars call (insertExprReservations continuations body))
    call@(GrinCpsPrimitiveCall _ name _ _)
      | Just requiredWords <- primitiveHeapWords name ->
          GrinBind [] (GrinEnsureNursery (toInteger requiredWords) []) call
    GrinBind resultVars valueExpression body ->
      GrinBind resultVars (insertExprReservations continuations valueExpression) (insertExprReservations continuations body)
    GrinStore node
      | isFrame node ->
          GrinBind
            []
            (GrinEnsureNursery 0 [])
            (GrinStoreUnchecked node)
    GrinStore node ->
      GrinBind
        []
        (GrinEnsureNursery (toInteger (nodeWords node)) [])
        (GrinStoreUnchecked node)
    GrinStoreRec bindings body ->
      GrinBind
        []
        (GrinEnsureNursery (toInteger (sum (map (nodeWords . snd) bindings))) [])
        (GrinStoreRecUnchecked bindings (insertExprReservations continuations body))
    GrinStoreRecUnchecked bindings body ->
      GrinStoreRecUnchecked bindings (insertExprReservations continuations body)
    GrinIfWhnf value ready slow -> GrinIfWhnf value (insertExprReservations continuations ready) (insertExprReservations continuations slow)
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder (map (insertAlternativeReservations continuations) alternatives)
    _ -> expression
  where
    isFrame node =
      case grinNodeTag node of
        GrinClosure name _ -> name `Set.member` continuations
        _ -> False

insertAlternativeReservations :: Set FunctionName -> GrinAlt -> GrinAlt
insertAlternativeReservations continuations alternative =
  alternative {grinAltRhs = insertExprReservations continuations (grinAltRhs alternative)}

relocateFunction :: GrinFunction -> State Int GrinFunction
relocateFunction function = do
  body <- relocateExpr (Set.fromList (grinFunctionParameters function)) (grinFunctionBody function)
  pure function {grinFunctionBody = body}

relocateExpr :: Set GrinVar -> GrinExpr -> State Int GrinExpr
relocateExpr bound expression =
  case expression of
    GrinBind resultVars (GrinAllocate requiredWords pinned []) body ->
      relocateSafepoint bound resultVars (GrinAllocate requiredWords pinned) body
    GrinBind [] (GrinEnsureNursery words' []) body ->
      relocateSafepoint bound [] (GrinEnsureNursery words') body
    GrinBind resultVars valueExpression body -> do
      valueExpression' <- relocateExpr bound valueExpression
      body' <- relocateExpr (bound <> Set.fromList resultVars) body
      pure (GrinBind resultVars valueExpression' body')
    GrinIfWhnf value ready slow -> GrinIfWhnf value <$> relocateExpr bound ready <*> relocateExpr bound slow
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM (relocateAlternative (Set.insert binder bound)) alternatives
    GrinConstant {} -> pure expression
    GrinEnsureNursery {} -> pure expression
    GrinAllocate {} -> pure expression
    GrinStore {} -> pure expression
    GrinStoreUnchecked {} -> pure expression
    GrinStoreRec {} -> pure expression
    GrinStoreRecUnchecked bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      body' <- relocateExpr (bound <> recursiveVars) body
      pure (GrinStoreRecUnchecked bindings body')
    GrinUpdate {} -> pure expression
    GrinUpdateBlackhole {} -> pure expression
    GrinEval {} -> pure expression
    GrinCpsEval {} -> pure expression
    GrinFetch {} -> pure expression
    GrinCall {} -> pure expression
    GrinPrimitiveCall {} -> pure expression
    GrinCpsPrimitiveCall {} -> pure expression
    GrinApply {} -> pure expression
    GrinForward -> pure expression
    GrinCpsApply {} -> pure expression
    GrinContinue {} -> pure expression
    GrinCpsRaise {} -> pure expression
    GrinHalt {} -> pure expression
    GrinExit {} -> pure expression
    GrinThrow {} -> pure expression
    GrinCatch {} -> pure expression
    GrinForeignCallExpr {} -> pure expression

-- Give each safepoint its live pointer roots.
-- Use the relocated SSA names after the safepoint.
relocateSafepoint :: Set GrinVar -> [GrinVar] -> ([GrinValue] -> GrinExpr) -> GrinExpr -> State Int GrinExpr
relocateSafepoint bound resultVars reservation body = do
  let roots = livePointerRoots bound (freeExprVars body)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
      bodyWithRelocatedRoots = substituteExpr substitutions body
  body' <- relocateExpr (bound <> Set.fromList (resultVars <> relocated)) bodyWithRelocatedRoots
  pure
    ( GrinBind
        (resultVars <> relocated)
        (reservation (map GrinVarValue roots))
        body'
    )

relocateAlternative :: Set GrinVar -> GrinAlt -> State Int GrinAlt
relocateAlternative bound alternative = do
  rhs <- relocateExpr (bound <> Set.fromList (grinAltBinders alternative)) (grinAltRhs alternative)
  pure alternative {grinAltRhs = rhs}

livePointerRoots :: Set GrinVar -> Set GrinVar -> [GrinVar]
livePointerRoots bound uses =
  Set.toAscList (Set.filter (isPointerRuntimeRep . grinVarRuntimeRep) (bound `Set.intersection` uses))

freshRelocated :: GrinVar -> State Int GrinVar
freshRelocated var = do
  unique <- get
  put (unique + 1)
  pure var {grinVarName = grinVarName var <> "$gc", grinVarUnique = unique}

-- | One info-table pointer plus the statically known payload. A thunk
-- spends its first payload word on its owner word, which also lets it become
-- an indirection in place. The Lir lowering uses this for the heap-pointer
-- bump of an unchecked store, so the two always agree on an object's width.
nodeWords :: GrinNode -> Int
nodeWords node =
  1
    + case grinNodeTag node of
      GrinThunk {} -> 1 + fieldCount
      -- A constructor that still wants arguments spends one payload word on
      -- the count of the fields it holds, because every stage of one
      -- constructor shares a single info table.
      GrinConstructor _ remaining | remaining > 0 -> 1 + fieldCount
      _ -> fieldCount
  where
    fieldCount = length (grinNodeFields node)

substituteExpr :: Map GrinVar GrinVar -> GrinExpr -> GrinExpr
substituteExpr substitutions expression =
  case expression of
    GrinConstant values -> GrinConstant (map (substituteValue substitutions) values)
    GrinBind vars valueExpression body ->
      GrinBind vars (substituteExpr substitutions valueExpression) (substituteExpr (without vars substitutions) body)
    GrinStore node -> GrinStore (substituteNode substitutions node)
    GrinEnsureNursery words' roots -> GrinEnsureNursery words' (map (substituteValue substitutions) roots)
    GrinAllocate requiredWords pinned roots -> GrinAllocate (substituteValue substitutions requiredWords) (substituteValue substitutions pinned) (map (substituteValue substitutions) roots)
    GrinStoreUnchecked node -> GrinStoreUnchecked (substituteNode substitutions node)
    GrinStoreRec bindings body -> substituteStoreRec GrinStoreRec substitutions bindings body
    GrinStoreRecUnchecked bindings body -> substituteStoreRec GrinStoreRecUnchecked substitutions bindings body
    GrinUpdate pointer value -> GrinUpdate (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinUpdateBlackhole pointer value -> GrinUpdateBlackhole (substituteValue substitutions pointer) (substituteValue substitutions value)
    GrinEval update runtimeRep value -> GrinEval update runtimeRep (substituteValue substitutions value)
    GrinCpsEval update runtimeRep value continuation ->
      GrinCpsEval update runtimeRep (substituteValue substitutions value) (substituteValue substitutions continuation)
    GrinFetch tag value -> GrinFetch tag (substituteValue substitutions value)
    GrinCall runtimeRep name arguments -> GrinCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinPrimitiveCall runtimeRep name arguments -> GrinPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments)
    GrinCpsPrimitiveCall runtimeRep name arguments continuation ->
      GrinCpsPrimitiveCall runtimeRep name (map (substituteValue substitutions) arguments) (substituteValue substitutions continuation)
    GrinApply runtimeRep function arguments -> GrinApply runtimeRep (substituteValue substitutions function) (map (map (substituteValue substitutions)) arguments)
    GrinForward -> GrinForward
    GrinCpsApply runtimeRep function arguments continuation ->
      GrinCpsApply runtimeRep (substituteValue substitutions function) (map (map (substituteValue substitutions)) arguments) (substituteValue substitutions continuation)
    GrinContinue continuation values -> GrinContinue (substituteValue substitutions continuation) (map (substituteValue substitutions) values)
    GrinCpsRaise exception continuation -> GrinCpsRaise (substituteValue substitutions exception) (substituteValue substitutions continuation)
    GrinHalt values -> GrinHalt (map (substituteValue substitutions) values)
    GrinExit status -> GrinExit (substituteValue substitutions status)
    GrinIfWhnf value ready slow -> GrinIfWhnf (substituteValue substitutions value) (substituteExpr substitutions ready) (substituteExpr substitutions slow)
    GrinCase scrutinee binder alternatives ->
      GrinCase
        (substituteValue substitutions scrutinee)
        binder
        [ alternative
            { grinAltRhs = substituteExpr (without (binder : grinAltBinders alternative) substitutions) (grinAltRhs alternative)
            }
        | alternative <- alternatives
        ]
    GrinThrow exception -> GrinThrow (substituteValue substitutions exception)
    GrinCatch runtimeRep action handler state ->
      GrinCatch runtimeRep (substituteValue substitutions action) (substituteValue substitutions handler) (map (substituteValue substitutions) state)
    GrinForeignCallExpr foreignCall arguments -> GrinForeignCallExpr foreignCall (map (substituteValue substitutions) arguments)

substituteStoreRec :: ([(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr) -> Map GrinVar GrinVar -> [(GrinVar, GrinNode)] -> GrinExpr -> GrinExpr
substituteStoreRec constructor substitutions bindings body =
  constructor
    [(var, substituteNode substitutions' node) | (var, node) <- bindings]
    (substituteExpr substitutions' body)
  where
    substitutions' = without (map fst bindings) substitutions

substituteNode :: Map GrinVar GrinVar -> GrinNode -> GrinNode
substituteNode substitutions node =
  node {grinNodeFields = map (substituteValue substitutions) (grinNodeFields node)}

substituteValue :: Map GrinVar GrinVar -> GrinValue -> GrinValue
substituteValue substitutions value =
  case value of
    GrinVarValue var -> GrinVarValue (Map.findWithDefault var var substitutions)
    GrinGlobalValue {} -> value
    GrinLitValue {} -> value

without :: [GrinVar] -> Map GrinVar GrinVar -> Map GrinVar GrinVar
without vars substitutions = foldr Map.delete substitutions vars
