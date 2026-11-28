-- | Place heap allocations inside the case alternatives that use them.
-- The code of a thunk or closure stays in its separate function.
module Aihc.Grin.Allocation
  ( placeGrinAllocations,
  )
where

import Aihc.Grin.Analysis (freeExprVars, freeNodeVars, freeValueVars, maximumProgramVarUnique)
import Aihc.Grin.Syntax
import Aihc.Util.Occurrences qualified as Occurrences
import Control.Monad.Trans.State.Strict (evalState, state)
import Data.List qualified as List
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import Data.Monoid (Sum)
import Data.MonoidMap (MonoidMap)
import Data.MonoidMap qualified as MonoidMap
import Data.Set (Set)
import Data.Set qualified as Set

-- | Move plain stores before CPS conversion and GC safepoint insertion.
-- Each selected path retains one shared object. Recursive groups stay intact.
placeGrinAllocations :: GrinProgram -> GrinProgram
placeGrinAllocations program0 =
  program {grinFunctions = map placeFunction (grinFunctions program)}
  where
    program = storeArguments program0
    placeFunction function = function {grinFunctionBody = fromPlaced (place (grinFunctionBody function))}
    place expression =
      case expression of
        GrinBind [pointer] (GrinStore node) body ->
          let body' = place body
           in fromMaybe (placedBind [pointer] (placedOther (GrinStore node)) body') (move pointer node body')
        GrinBind vars value body -> placedBind vars (place value) (place body)
        GrinStoreRec nodes body -> placedStoreRec nodes (place body)
        GrinCase value binder alternatives ->
          placedCase value binder [(alternative, place (grinAltRhs alternative)) | alternative <- alternatives]
        _ -> placedOther expression

-- | An expression with the set of its free variables. A move asks if
-- the pointer is free in the code after each bind that it crosses. A new
-- walk of that code for each store made long bind chains quadratic, as in
-- the table of @Text.TeXMath.Unicode.ToTeX@. The set is lazy, and a node
-- makes it from the sets of its parts, so the pass makes each set one
-- time at most.
data Placed = Placed
  { placedFree :: Set GrinVar,
    placedShape :: !PlacedShape
  }

-- | The forms that a move crosses or enters. Each other form is a leaf.
-- The right side of each alternative of a case is in the pair, not in the
-- alternative.
data PlacedShape
  = PlacedBind ![GrinVar] !Placed !Placed
  | PlacedCase !GrinValue !GrinVar ![(GrinAlt, Placed)]
  | PlacedStoreRec ![(GrinVar, GrinNode)] !Placed
  | PlacedOther !GrinExpr

placedBind :: [GrinVar] -> Placed -> Placed -> Placed
placedBind vars value body =
  Placed
    (placedFree value <> (placedFree body `Set.difference` Set.fromList vars))
    (PlacedBind vars value body)

placedCase :: GrinValue -> GrinVar -> [(GrinAlt, Placed)] -> Placed
placedCase value binder alternatives =
  Placed
    (freeValueVars value <> foldMap alternativeFree alternatives)
    (PlacedCase value binder alternatives)
  where
    alternativeFree (alternative, rhs) =
      placedFree rhs `Set.difference` Set.fromList (binder : grinAltBinders alternative)

placedStoreRec :: [(GrinVar, GrinNode)] -> Placed -> Placed
placedStoreRec bindings body =
  Placed
    ((foldMap (freeNodeVars . snd) bindings <> placedFree body) `Set.difference` Set.fromList (map fst bindings))
    (PlacedStoreRec bindings body)

placedOther :: GrinExpr -> Placed
placedOther expression = Placed (freeExprVars expression) (PlacedOther expression)

fromPlaced :: Placed -> GrinExpr
fromPlaced placed =
  case placedShape placed of
    PlacedBind vars value body -> GrinBind vars (fromPlaced value) (fromPlaced body)
    PlacedCase value binder alternatives ->
      GrinCase value binder [alternative {grinAltRhs = fromPlaced rhs} | (alternative, rhs) <- alternatives]
    PlacedStoreRec bindings body -> GrinStoreRec bindings (fromPlaced body)
    PlacedOther expression -> expression

-- | Cross a bind only when it neither uses the pointer nor captures an operand.
-- Stop at every other use, including aliases, calls, updates, and captures.
-- A successful move must omit the allocation from at least one path. A case
-- whose alternatives all use the pointer still takes a copy in each of
-- them when a case inside one of them has a path without a use. The copies
-- are in separate alternatives, so each path still allocates once at most.
--
-- The hot path of a scanning loop often evaluates a thunk only through a
-- direct call of its code, after 'Aihc.Grin.Simplify' replaced a
-- single-entry @eval@ of the known thunk. The stored object is then dead
-- on that path, and only a rarer path, such as the inside of a string
-- literal, passes it on.
move :: GrinVar -> GrinNode -> Placed -> Maybe Placed
move pointer node = go
  where
    protected = Set.insert pointer (freeNodeVars node)
    safe vars = Set.disjoint protected (Set.fromList vars)
    uses = Set.member pointer . placedFree
    store = placedOther (GrinStore node)
    wrap body =
      case placedShape body of
        PlacedOther (GrinConstant [GrinVarValue var]) | var == pointer -> store
        _ -> placedBind [pointer] store body
    placeBranch body
      | not (uses body) = body
      | otherwise = fromMaybe (wrap body) (go body)
    go expression =
      case placedShape expression of
        PlacedBind vars value body
          | safe vars,
            not (uses value) ->
              placedBind vars value <$> go body
          | not (uses body) -> (\value' -> placedBind vars value' body) <$> go value
        PlacedCase value binder alternatives
          | movable expression ->
              Just (placedCase value binder [(alternative, placeBranch rhs) | (alternative, rhs) <- alternatives])
        _ -> Nothing
    -- Whether the store can enter a case, and a path through it does not
    -- use the pointer.
    movable expression =
      case placedShape expression of
        PlacedCase value binder alternatives ->
          Set.notMember pointer (freeValueVars value)
            && safe (binder : concatMap (grinAltBinders . fst) alternatives)
            && any (avoids . snd) alternatives
        _ -> False
    -- Whether a path through the expression does not use the pointer, past
    -- the binds and into the cases that the move can cross and enter.
    avoids expression = not (uses expression) || avoidsUsed expression
    avoidsUsed expression =
      case placedShape expression of
        PlacedBind vars value body
          | safe vars,
            not (uses value) ->
              avoidsUsed body
          | not (uses body) -> avoidsUsed value
        _ -> movable expression

-- * Store arguments

-- | Move the store of an argument into the function that takes it, when
-- each call of the function gives that parameter a node that the caller
-- stores for the call alone. The function then takes the fields of the
-- node and stores the node itself, and 'move' places that store in the
-- branches of the function that use it.
--
-- A join point that the lowering made a function is such a function. A
-- loop that calls it for each byte stores, at each call, a thunk and the
-- boxes of its counters, which only a rare branch of the join point reads:
-- the end of the line, or the second byte of a comment opener. After the
-- move, the common path stores nothing.
--
-- A function that a node or a global names, or that another unit can call,
-- keeps its parameters, because a call through a closure gives them as they
-- are. A node with more than 'storeArgumentFieldLimit' fields stays at the
-- caller.
storeArguments :: GrinProgram -> GrinProgram
storeArguments program
  | MonoidMap.null positions = program
  | otherwise = program {grinFunctions = evalState (mapM rewriteFunction (grinFunctions program)) (1 + maximumProgramVarUnique program)}
  where
    functions = grinFunctions program
    named =
      Set.fromList (grinExportedFunctions program)
        <> foldMap (nodeFunctions . grinGlobalNode) (grinGlobals program)
        <> foldMap (exprNodeFunctions . grinFunctionBody) functions
    -- Each call of each function, with what each argument is: a node that
    -- the caller stores for this argument alone, or nothing.
    calls :: MonoidMap FunctionName [[Maybe GrinNode]]
    calls = MonoidMap.fromListWith (flip (<>)) [(callee, [arguments]) | function <- functions, (callee, arguments) <- functionCalls function]
    positions :: MonoidMap FunctionName (Map Int GrinNodeTag)
    positions =
      MonoidMap.fromList
        [ (grinFunctionName function, qualified)
        | function <- functions,
          Set.notMember (grinFunctionName function) named,
          let sites = MonoidMap.get (grinFunctionName function) calls,
          let arity = length (grinFunctionParameters function)
              qualified =
                Map.fromList
                  [ (index, tag)
                  | index <- [0 .. arity - 1],
                    Just (GrinNode tag fields : others) <- [traverse (\site -> if length site == arity then site !! index else Nothing) sites],
                    movableTag tag,
                    length fields <= storeArgumentFieldLimit,
                    all ((== tag) . grinNodeTag) others,
                    all ((== length fields) . length . grinNodeFields) others
                  ]
        ]
    rewriteFunction function = do
      let body = rewriteCalls (grinFunctionBody function)
      let moved = MonoidMap.get (grinFunctionName function) positions
      if Map.null moved
        then pure function {grinFunctionBody = body}
        else do
          (parameters, stores) <- fmap unzip . traverse (parameter moved) $ zip [0 ..] (grinFunctionParameters function)
          pure
            function
              { grinFunctionParameters = concat parameters,
                grinFunctionBody = foldr (\(var, node) inner -> GrinBind [var] (GrinStore node) inner) body (concat stores)
              }
      where
        stored = functionStores function
        parameter moved (index, var) =
          case Map.lookup index moved of
            Nothing -> pure ([var], [])
            Just tag ->
              case [node | site <- MonoidMap.get (grinFunctionName function) calls, Just node <- [site !! index]] of
                example : _ -> do
                  fields <- traverse (freshField var) (grinNodeFields example)
                  pure (fields, [(var, GrinNode tag (map GrinVarValue fields))])
                [] -> pure ([var], [])
        removed =
          Set.fromList
            [ var
            | (callee, arguments) <- rawCalls (grinFunctionBody function),
              let moved = MonoidMap.get callee positions,
              (index, GrinVarValue var) <- zip [0 ..] arguments,
              Map.member index moved
            ]
        rewriteCalls expression =
          case expression of
            GrinBind [var] (GrinStore _) body
              | Set.member var removed -> rewriteCalls body
            GrinBind vars value body -> GrinBind vars (rewriteCalls value) (rewriteCalls body)
            GrinStoreRec bindings body -> GrinStoreRec bindings (rewriteCalls body)
            GrinCase value binder alternatives ->
              GrinCase value binder (map (\alternative -> alternative {grinAltRhs = rewriteCalls (grinAltRhs alternative)}) alternatives)
            GrinCall rep callee arguments ->
              let moved = MonoidMap.get callee positions
               in GrinCall rep callee (concat [expand moved index argument | (index, argument) <- zip [0 ..] arguments])
            _ -> expression
        expand moved index argument =
          case (Map.lookup index moved, argument) of
            (Just _, GrinVarValue var)
              | Just node <- Map.lookup var stored -> grinNodeFields node
            _ -> [argument]
    freshField var value = state $ \unique -> (GrinVar (grinVarName var <> "_field") unique (grinValueRuntimeRep value), unique + 1)
    movableTag tag =
      case tag of
        GrinConstructor _ 0 -> True
        GrinThunk _ -> True
        _ -> False

-- | The most fields of a node that 'storeArguments' gives as arguments.
storeArgumentFieldLimit :: Int
storeArgumentFieldLimit = 4

-- | The calls of a function, with each argument that is a node the
-- function stores for that argument alone.
functionCalls :: GrinFunction -> [(FunctionName, [Maybe GrinNode])]
functionCalls function =
  [ (callee, map argument arguments)
  | (callee, arguments) <- rawCalls (grinFunctionBody function)
  ]
  where
    stored = functionStores function
    counts = useCounts (grinFunctionBody function)
    argument value =
      case value of
        GrinVarValue var
          | Just node <- Map.lookup var stored,
            MonoidMap.get var counts == 1 ->
              Just node
        _ -> Nothing

-- | The variables that a function binds to a plain store.
functionStores :: GrinFunction -> Map GrinVar GrinNode
functionStores function = go Map.empty (grinFunctionBody function)
  where
    go found expression =
      case expression of
        GrinBind [var] (GrinStore node) body -> go (Map.insert var node found) body
        GrinBind _ value body -> go (go found value) body
        GrinStoreRec _ body -> go found body
        GrinCase _ _ alternatives -> List.foldl' (\current alternative -> go current (grinAltRhs alternative)) found alternatives
        _ -> found

-- | The direct calls in an expression, outside the code of thunks and
-- closures, which are functions of their own.
rawCalls :: GrinExpr -> [(FunctionName, [GrinValue])]
rawCalls expression =
  case expression of
    GrinCall _ callee arguments -> [(callee, arguments)]
    GrinBind _ value body -> rawCalls value <> rawCalls body
    GrinStoreRec _ body -> rawCalls body
    GrinStoreRecUnchecked _ body -> rawCalls body
    GrinIfWhnf _ ready slow -> rawCalls ready <> rawCalls slow
    GrinCase _ _ alternatives -> concatMap (rawCalls . grinAltRhs) alternatives
    _ -> []

-- | How many times each variable occurs in an expression.
useCounts :: GrinExpr -> MonoidMap GrinVar (Sum Int)
useCounts expression = Occurrences.count (occurrences expression)
  where
    occurrences current =
      case current of
        GrinBind _ value body -> occurrences value <> occurrences body
        GrinStoreRec bindings body -> concatMap (nodeOccurrences . snd) bindings <> occurrences body
        GrinStoreRecUnchecked bindings body -> concatMap (nodeOccurrences . snd) bindings <> occurrences body
        GrinIfWhnf value ready slow -> valueOccurrences value <> occurrences ready <> occurrences slow
        GrinCase value _ alternatives -> valueOccurrences value <> concatMap (occurrences . grinAltRhs) alternatives
        _ -> Set.toList (freeExprVars current) >>= \var -> replicate (count var current) var
    count var current = length (filter (== var) (leafValues current))
    nodeOccurrences = concatMap valueOccurrences . grinNodeFields
    valueOccurrences value =
      case value of
        GrinVarValue var -> [var]
        _ -> []
    leafValues current =
      case current of
        GrinConstant values -> concatMap valueOccurrences values
        GrinStore node -> nodeOccurrences node
        GrinStoreUnchecked node -> nodeOccurrences node
        GrinCall _ _ values -> concatMap valueOccurrences values
        GrinPrimitiveCall _ _ values -> concatMap valueOccurrences values
        GrinApply _ function groups -> concatMap valueOccurrences (function : concat groups)
        GrinFetch _ value -> valueOccurrences value
        GrinEval _ _ value -> valueOccurrences value
        GrinUpdate left right -> concatMap valueOccurrences [left, right]
        GrinForeignCallExpr _ values -> concatMap valueOccurrences values
        GrinThrow value -> valueOccurrences value
        GrinCatch _ left right values -> concatMap valueOccurrences (left : right : values)
        -- Any other expression counts each free variable two times, so
        -- that a variable that it uses never counts as used once.
        _ -> concatMap (\var -> [var, var]) (Set.toList (freeExprVars current))

-- | The functions that the nodes of an expression name.
exprNodeFunctions :: GrinExpr -> Set FunctionName
exprNodeFunctions expression =
  case expression of
    GrinStore node -> nodeFunctions node
    GrinStoreUnchecked node -> nodeFunctions node
    GrinBind _ value body -> exprNodeFunctions value <> exprNodeFunctions body
    GrinStoreRec bindings body -> foldMap (nodeFunctions . snd) bindings <> exprNodeFunctions body
    GrinStoreRecUnchecked bindings body -> foldMap (nodeFunctions . snd) bindings <> exprNodeFunctions body
    GrinIfWhnf _ ready slow -> exprNodeFunctions ready <> exprNodeFunctions slow
    GrinCase _ _ alternatives -> foldMap (exprNodeFunctions . grinAltRhs) alternatives
    _ -> Set.empty

nodeFunctions :: GrinNode -> Set FunctionName
nodeFunctions node =
  case grinNodeTag node of
    GrinClosure name _ -> Set.singleton name
    GrinThunk name -> Set.singleton name
    _ -> Set.empty
