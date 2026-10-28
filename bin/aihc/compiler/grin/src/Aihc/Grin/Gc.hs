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
    dynamicReservation,
  )
where

import Aihc.Grin.Analysis (freeExprVars, maximumProgramVarUnique)
import Aihc.Grin.Cps (ContinuationFrameKind, CpsGrinError, CpsGrinProgram (..), toCpsGrin)
import Aihc.Grin.Heap (normalizeHeapReservations)
import Aihc.Grin.Primitive (primitiveHeapWords, primitiveInitializes, uncheckedByteArrayLimit, uncheckedByteArrayPrimitive, uncheckedByteArrayWords)
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
          grinExternFunctions = [],
          grinExportedFunctions = [],
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
          { grinFunctions = evalState (mapM (relocateFunction frames (cpsFunctionContinuations cps)) (grinFunctions normalizedProgram)) nextUnique
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
      [ (GrinVar helper (-2000000001) (if primitiveInitializes helper || helper == uncheckedByteArrayPrimitive then BoxedRep Unlifted else WordRep), arity)
      | (names, helper, arity) <- reservationHelpers <> initializationHelpers <> [(["newByteArray#"], uncheckedByteArrayPrimitive, 1)],
        any (\(var, _) -> grinVarName var `elem` names) (grinPrimitives program)
      ]
    frames = cpsContinuationFunctions cps
    normalizedProgram = normalizeHeapReservations (insertHeapReservations (cpsContinuationFunctions cps) dynamicProgram)
    nextUnique = 1 + maximumProgramVarUnique normalizedProgram

-- | Size helpers validate counts before allocation. They cannot collect.
reservationHelpers :: [([Text], Text, Int)]
reservationHelpers =
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
  -- A movable byte array of a small fixed size gets a fixed reservation,
  -- which can merge with the reservations around it. Then the backend
  -- writes the array in the heap, and no runtime call occurs.
  GrinBind results (GrinPrimitiveCall resultRep "newByteArray#" [size@(GrinLitValue (GrinLitInt _ bytes))]) body
    | bytes >= 0 && bytes <= uncheckedByteArrayLimit -> do
        rest <- insertDynamic body
        pure
          ( GrinBind
              []
              (GrinEnsureHeap (staticHeapWords (fromInteger (uncheckedByteArrayWords bytes))) [])
              (GrinBind results (GrinPrimitiveCall resultRep uncheckedByteArrayPrimitive [size]) rest)
          )
  GrinBind results (GrinPrimitiveCall resultRep name arguments) body
    | Just (helper, operands) <- dynamicReservation name arguments -> do
        unique <- get
        put (unique + 3)
        let wordsVar = GrinVar (if helper == "aihcArrayWords#" then "$gc_array_words" else "$gc_byte_array_words") unique WordRep
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
dynamicReservation :: Text -> [GrinValue] -> Maybe (Text, [GrinValue])
dynamicReservation name arguments = case (name, arguments) of
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
    -- The evaluation helpers are safepoints: each collects at its entry when
    -- the heap pointer is above the heap limit. Thus a frame that the code
    -- pushes only to evaluate a value needs no reservation of its own. The
    -- frame push stays the only code before the transfer, and the cold
    -- collection with every live root goes out of the function.
    GrinBind resultVars@[frame] (GrinStore node) body@(GrinCpsEval _ _ _ (GrinVarValue continuation))
      | isFrame node,
        continuation == frame ->
          GrinBind resultVars (GrinStoreUnchecked node) body
    GrinBind resultVars (GrinStore node) body
      | isFrame node ->
          GrinBind
            []
            (GrinEnsureHeap (staticHeapWords 0) [])
            (GrinBind resultVars (GrinStoreUnchecked node) (insertExprReservations continuations body))
    GrinBind resultVars (GrinStore node) body ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (nodeWords node)) [])
        (GrinBind resultVars (GrinStoreUnchecked node) (insertExprReservations continuations body))
    -- A store safepoint ends a reservation, so the reservation of an
    -- escaping primitive that allocates comes after its safepoint.
    GrinBind resultVars call@(GrinPrimitiveCall _ name _) body
      | isEscapingStore call,
        Just requiredWords <- primitiveHeapWords name ->
          GrinBind
            []
            (GrinSafepoint Nothing [])
            ( GrinBind
                []
                (GrinEnsureHeap (staticHeapWords requiredWords) [])
                (GrinBind resultVars call (insertExprReservations continuations body))
            )
    GrinBind resultVars call@(GrinPrimitiveCall _ name _) body
      | Just requiredWords <- primitiveHeapWords name ->
          GrinBind
            []
            (GrinEnsureHeap (staticHeapWords requiredWords) [])
            (GrinBind resultVars call (insertExprReservations continuations body))
    call@(GrinCpsPrimitiveCall _ name _ _)
      | Just requiredWords <- primitiveHeapWords name ->
          GrinBind [] (GrinEnsureHeap (staticHeapWords requiredWords) []) call
    GrinBind resultVars store body
      | isEscapingStore store ->
          GrinBind [] (GrinSafepoint Nothing []) (GrinBind resultVars store (insertExprReservations continuations body))
    store
      | isEscapingStore store -> GrinBind [] (GrinSafepoint Nothing []) store
    GrinBind resultVars valueExpression body ->
      GrinBind resultVars (insertExprReservations continuations valueExpression) (insertExprReservations continuations body)
    GrinStore node
      | isFrame node ->
          GrinBind
            []
            (GrinEnsureHeap (staticHeapWords 0) [])
            (GrinStoreUnchecked node)
    GrinStore node ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (nodeWords node)) [])
        (GrinStoreUnchecked node)
    GrinStoreRec bindings body ->
      GrinBind
        []
        (GrinEnsureHeap (staticHeapWords (sum (map (nodeWords . snd) bindings))) [])
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

-- | A store that can be an escape: an update, or a pointer store into an
-- existing object. Its barrier can promote the stored value, and a
-- promotion that reaches a mutable object collects. Thus it gets a
-- safepoint with its live roots. See "Local heaps" in
-- docs/multi-threaded-rts-design.md.
isEscapingStore :: GrinExpr -> Bool
isEscapingStore expression =
  case expression of
    GrinUpdate {} -> True
    GrinUpdateBlackhole {} -> True
    GrinPrimitiveCall _ name _ -> name `elem` escapingStorePrimitives
    _ -> False

-- | The primitives whose pointer store into an existing object takes the
-- write barrier: in compiled code, or in a runtime call that gives a
-- sentinel when its promotion needs a collection. makeStableName# stores
-- into the global list of names, and stmCommit# stores the writes of a
-- transaction into their variables.
escapingStorePrimitives :: [Text]
escapingStorePrimitives =
  [ "writeMutVar#",
    "casMutVar#",
    "writeArray#",
    "writeSmallArray#",
    "casArray#",
    "casSmallArray#",
    "tryPutMVar#",
    "makeStableName#",
    "copyArray#",
    "copyMutableArray#",
    "copySmallArray#",
    "copySmallMutableArray#",
    "stmCommit#"
  ]

insertAlternativeReservations :: Set FunctionName -> GrinAlt -> GrinAlt
insertAlternativeReservations continuations alternative =
  alternative {grinAltRhs = insertExprReservations continuations (grinAltRhs alternative)}

-- | The relocation of a function. It also tracks the top frame of the
-- thread: the continuation of the function, or the last frame that the
-- function pushed. A store safepoint names it, so a promotion can run the
-- pause walk from it.
relocateFunction :: Set FunctionName -> Map FunctionName GrinVar -> GrinFunction -> State Int GrinFunction
relocateFunction frames continuations function = do
  body <- relocateExpr frames (Map.lookup (grinFunctionName function) continuations) (Set.fromList (grinFunctionParameters function)) (grinFunctionBody function)
  pure function {grinFunctionBody = body}

relocateExpr :: Set FunctionName -> Maybe GrinVar -> Set GrinVar -> GrinExpr -> State Int GrinExpr
relocateExpr frames top bound expression =
  case expression of
    GrinBind resultVars (GrinAllocate requiredWords pinned []) body ->
      relocateReservation frames top bound resultVars (GrinAllocate requiredWords pinned) body
    GrinBind [] (GrinEnsureHeap words' []) body ->
      relocateReservation frames top bound [] (GrinEnsureHeap words') body
    GrinBind [] (GrinSafepoint Nothing []) body -> relocateSafepoint frames top bound body
    GrinBind resultVars valueExpression body -> do
      valueExpression' <- relocateExpr frames top bound valueExpression
      let top' = case (resultVars, valueExpression) of
            ([frame], GrinStoreUnchecked node) | isFrameNode frames node -> Just frame
            _ -> top
      body' <- relocateExpr frames top' (bound <> Set.fromList resultVars) body
      pure (GrinBind resultVars valueExpression' body')
    GrinIfWhnf value ready slow -> GrinIfWhnf value <$> relocateExpr frames top bound ready <*> relocateExpr frames top bound slow
    GrinCase scrutinee binder alternatives ->
      GrinCase scrutinee binder <$> mapM (relocateAlternative frames top (Set.insert binder bound)) alternatives
    GrinConstant {} -> pure expression
    GrinEnsureHeap {} -> pure expression
    GrinAllocate {} -> pure expression
    GrinSafepoint {} -> pure expression
    GrinStore {} -> pure expression
    GrinStoreUnchecked {} -> pure expression
    GrinStoreRec {} -> pure expression
    GrinStoreRecUnchecked bindings body -> do
      let recursiveVars = Set.fromList (map fst bindings)
      body' <- relocateExpr frames top (bound <> recursiveVars) body
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

-- | Whether a node is a continuation frame, which goes on the stack.
isFrameNode :: Set FunctionName -> GrinNode -> Bool
isFrameNode frames node =
  case grinNodeTag node of
    GrinClosure name _ -> name `Set.member` frames
    _ -> False

-- Give each safepoint its live pointer roots.
-- Use the relocated SSA names after the safepoint.
relocateReservation :: Set FunctionName -> Maybe GrinVar -> Set GrinVar -> [GrinVar] -> ([GrinValue] -> GrinExpr) -> GrinExpr -> State Int GrinExpr
relocateReservation frames top bound resultVars reservation body = do
  let roots = livePointerRoots bound (freeExprVars body)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
      bodyWithRelocatedRoots = substituteExpr substitutions body
  body' <- relocateExpr frames (substituteVar substitutions <$> top) (bound <> Set.fromList (resultVars <> relocated)) bodyWithRelocatedRoots
  pure
    ( GrinBind
        (resultVars <> relocated)
        (reservation (map GrinVarValue roots))
        body'
    )

-- | Give a store safepoint its live roots, as a reservation gets them, and
-- the top frame. The store that follows uses the relocated names, so the
-- barrier runs it again with them after a collection. A frame does not
-- move, so the top frame needs no relocation of its own.
relocateSafepoint :: Set FunctionName -> Maybe GrinVar -> Set GrinVar -> GrinExpr -> State Int GrinExpr
relocateSafepoint frames top bound body = do
  let roots = livePointerRoots bound (freeExprVars body)
  relocated <- mapM freshRelocated roots
  let substitutions = Map.fromList (zip roots relocated)
  body' <- relocateExpr frames (substituteVar substitutions <$> top) (bound <> Set.fromList relocated) (substituteExpr substitutions body)
  pure (GrinBind relocated (GrinSafepoint (GrinVarValue <$> top) (map GrinVarValue roots)) body')

substituteVar :: Map GrinVar GrinVar -> GrinVar -> GrinVar
substituteVar substitutions var = Map.findWithDefault var var substitutions

relocateAlternative :: Set FunctionName -> Maybe GrinVar -> Set GrinVar -> GrinAlt -> State Int GrinAlt
relocateAlternative frames top bound alternative = do
  rhs <- relocateExpr frames top (bound <> Set.fromList (grinAltBinders alternative)) (grinAltRhs alternative)
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

staticHeapWords :: Int -> GrinValue
staticHeapWords = GrinLitValue . GrinLitInt WordRep . toInteger

substituteExpr :: Map GrinVar GrinVar -> GrinExpr -> GrinExpr
substituteExpr substitutions expression =
  case expression of
    GrinConstant values -> GrinConstant (map (substituteValue substitutions) values)
    GrinBind vars valueExpression body ->
      GrinBind vars (substituteExpr substitutions valueExpression) (substituteExpr (without vars substitutions) body)
    GrinStore node -> GrinStore (substituteNode substitutions node)
    GrinEnsureHeap requiredWords roots -> GrinEnsureHeap (substituteValue substitutions requiredWords) (map (substituteValue substitutions) roots)
    GrinAllocate requiredWords pinned roots -> GrinAllocate (substituteValue substitutions requiredWords) (substituteValue substitutions pinned) (map (substituteValue substitutions) roots)
    GrinSafepoint top roots -> GrinSafepoint (substituteValue substitutions <$> top) (map (substituteValue substitutions) roots)
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
