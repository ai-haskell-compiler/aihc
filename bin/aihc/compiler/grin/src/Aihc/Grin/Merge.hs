-- | Merge identical functions of a GC-GRIN program.
--
-- Inlining, CPS conversion, and closure conversion give many functions with
-- the same code: a continuation for each call site of an inlined function,
-- or a closure for each copy of a lambda. Two functions are identical when
-- their parameters, results, and bodies are the same after a consistent
-- renaming of their variables, and when the CPS and GC facts of the two
-- functions are the same. One of them then stays, and each reference to the
-- others refers to it.
--
-- The pass runs after GC lowering, because CPS conversion makes most of the
-- identical functions. The backends make the info tables, the entry code,
-- and the static reference tables of a function from its name, so a merged
-- function also has only one of each.
--
-- Another unit can call an exported function by its name, so the pass does
-- not merge exported functions.
module Aihc.Grin.Merge
  ( mergeIdenticalFunctions,
  )
where

import Aihc.Grin.Gc (GcGrinProgram (..))
import Aihc.Grin.Syntax
import Control.Monad.Trans.State.Strict (State, evalState, get, put)
import Data.ByteString (ByteString)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | Merge identical functions until no two functions are identical.
--
-- A merge can make two other functions identical, for example two functions
-- that each call one of the merged functions. Thus, the merge repeats until
-- a round finds no identical functions. Each round removes at least one
-- function, so the repetition ends.
mergeIdenticalFunctions :: GcGrinProgram -> GcGrinProgram
mergeIdenticalFunctions gc
  | Map.null replacements = gc
  | otherwise = mergeIdenticalFunctions (replaceFunctions replacements gc)
  where
    program = gcGrinProgram gc
    exported = Set.fromList (grinExportedFunctions program)
    groups =
      Map.fromListWith
        (flip (<>))
        [ (functionKey gc function, [grinFunctionName function])
        | function <- grinFunctions program,
          not (Set.member (grinFunctionName function) exported)
        ]
    -- The first function of a group in program order stays.
    replacements =
      Map.fromList
        [ (removed, kept)
        | kept : others <- Map.elems groups,
          removed <- others
        ]

-- | Remove the replaced functions and make each reference to one of them
-- refer to its replacement.
replaceFunctions :: Map FunctionName FunctionName -> GcGrinProgram -> GcGrinProgram
replaceFunctions replacements gc =
  gc
    { gcGrinProgram =
        program
          { grinFunctions =
              [ function {grinFunctionBody = replaceExpr rename (grinFunctionBody function)}
              | function <- grinFunctions program,
                not (Map.member (grinFunctionName function) replacements)
              ],
            grinGlobals =
              [ global {grinGlobalNode = replaceNode rename (grinGlobalNode global)}
              | global <- grinGlobals program
              ]
          },
      gcContinuationFunctions = Set.filter (`Map.notMember` replacements) (gcContinuationFunctions gc),
      gcContinuationFrames = Map.withoutKeys (gcContinuationFrames gc) removed,
      gcFunctionContinuations = Map.withoutKeys (gcFunctionContinuations gc) removed
    }
  where
    program = gcGrinProgram gc
    removed = Map.keysSet replacements
    rename name = Map.findWithDefault name name replacements

replaceExpr :: (FunctionName -> FunctionName) -> GrinExpr -> GrinExpr
replaceExpr rename = go
  where
    go expression =
      case expression of
        GrinBind vars value body -> GrinBind vars (go value) (go body)
        GrinStore node -> GrinStore (replaceNode rename node)
        GrinStoreUnchecked node -> GrinStoreUnchecked (replaceNode rename node)
        GrinStoreRec bindings body -> GrinStoreRec (bindings' bindings) (go body)
        GrinStoreRecUnchecked bindings body -> GrinStoreRecUnchecked (bindings' bindings) (go body)
        GrinFetch tag value -> GrinFetch (replaceTag rename tag) value
        GrinIfWhnf value ready slow -> GrinIfWhnf value (go ready) (go slow)
        GrinCall rep name arguments -> GrinCall rep (rename name) arguments
        GrinCase scrutinee binder alternatives ->
          GrinCase scrutinee binder [alternative {grinAltRhs = go (grinAltRhs alternative)} | alternative <- alternatives]
        _ -> expression
    bindings' bindings = [(var, replaceNode rename node) | (var, node) <- bindings]

replaceNode :: (FunctionName -> FunctionName) -> GrinNode -> GrinNode
replaceNode rename node = node {grinNodeTag = replaceTag rename (grinNodeTag node)}

replaceTag :: (FunctionName -> FunctionName) -> GrinNodeTag -> GrinNodeTag
replaceTag rename tag =
  case tag of
    GrinClosure name layouts -> GrinClosure (rename name) layouts
    GrinThunk name -> GrinThunk (rename name)
    GrinConstructor {} -> tag

-- | The comparison key of a function. Two functions have the same key when
-- they are identical.
data Key
  = KeyNode !Int ![Key]
  | KeyInt !Integer
  | KeyChar !Char
  | KeyText !Text
  | KeyBytes !ByteString
  | KeyRep !GrinRep
  | KeyResult !GrinResultRep
  | KeyFunction !FunctionName
  | KeySelf
  | KeyVar !Int !GrinRep
  deriving (Eq, Ord)

-- | The number of each variable, in the order of its first occurrence.
type KeyM = State (Map GrinVar Int)

functionKey :: GcGrinProgram -> GrinFunction -> Key
functionKey gc function =
  evalState
    ( do
        parameters <- mapM varKey (grinFunctionParameters function)
        continuation <- traverse varKey (Map.lookup name (gcFunctionContinuations gc))
        body <- exprKey name (grinFunctionBody function)
        pure
          ( KeyNode
              0
              [ KeyNode 0 parameters,
                KeyResult (grinFunctionResultRep function),
                KeyInt (frameCode (Map.lookup name (gcContinuationFrames gc))),
                KeyInt (if Set.member name (gcContinuationFunctions gc) then 1 else 0),
                maybe (KeyNode 0 []) (KeyNode 1 . pure) continuation,
                body
              ]
          )
    )
    Map.empty
  where
    name = grinFunctionName function
    frameCode = maybe (-1) (toInteger . fromEnum)

varKey :: GrinVar -> KeyM Key
varKey var = do
  numbers <- get
  case Map.lookup var numbers of
    Just number -> pure (KeyVar number (grinVarRuntimeRep var))
    Nothing -> do
      let number = Map.size numbers
      put (Map.insert var number numbers)
      pure (KeyVar number (grinVarRuntimeRep var))

exprKey :: FunctionName -> GrinExpr -> KeyM Key
exprKey self = go
  where
    node index parts = KeyNode index <$> sequence parts
    values = fmap (KeyNode 0) . mapM valueKey
    vars = fmap (KeyNode 0) . mapM varKey
    function name = pure (if name == self then KeySelf else KeyFunction name)
    nodeKey (GrinNode tag fields) = node 0 [tagKey tag, values fields]
    tagKey tag =
      case tag of
        GrinConstructor name remaining -> node 0 [pure (KeyText name), pure (KeyInt (toInteger remaining))]
        GrinClosure name layouts -> node 1 [function name, pure (KeyNode 0 [KeyNode 0 (map KeyRep layout) | layout <- layouts])]
        GrinThunk name -> node 2 [function name]
    bindings entries = KeyNode 0 <$> mapM (\(var, value) -> node 0 [varKey var, nodeKey value]) entries
    update mode = pure (KeyInt (if mode == EvalUpdate then 0 else 1))
    go expression =
      case expression of
        GrinConstant vs -> node 0 [values vs]
        GrinBind bound value body -> node 1 [vars bound, go value, go body]
        GrinStore n -> node 2 [nodeKey n]
        GrinEnsureHeap required roots -> node 3 [valueKey required, values roots]
        GrinAllocate required pinned roots -> node 4 [valueKey required, valueKey pinned, values roots]
        GrinSafepoint top roots -> node 5 [maybe (pure (KeyNode 0 [])) (fmap (KeyNode 1 . pure) . valueKey) top, values roots]
        GrinStoreUnchecked n -> node 6 [nodeKey n]
        GrinStoreRec entries body -> node 7 [bindings entries, go body]
        GrinStoreRecUnchecked entries body -> node 8 [bindings entries, go body]
        GrinUpdate pointer value -> node 9 [valueKey pointer, valueKey value]
        GrinEval mode rep value -> node 10 [update mode, pure (KeyRep rep), valueKey value]
        GrinCpsEval mode rep value continuation -> node 11 [update mode, pure (KeyRep rep), valueKey value, valueKey continuation]
        GrinFetch tag value -> node 12 [tagKey tag, valueKey value]
        GrinIfWhnf value ready slow -> node 13 [valueKey value, go ready, go slow]
        GrinCall rep name arguments -> node 14 [pure (KeyResult rep), function name, values arguments]
        GrinPrimitiveCall rep name arguments -> node 15 [pure (KeyRep rep), pure (KeyText name), values arguments]
        GrinCpsPrimitiveCall rep name arguments continuation -> node 16 [pure (KeyRep rep), pure (KeyText name), values arguments, valueKey continuation]
        GrinApply rep value groups -> node 17 [pure (KeyResult rep), valueKey value, KeyNode 0 <$> mapM values groups]
        GrinForward -> node 18 []
        GrinCpsApply rep value groups continuation -> node 19 [pure (KeyResult rep), valueKey value, KeyNode 0 <$> mapM values groups, valueKey continuation]
        GrinContinue continuation arguments -> node 20 [valueKey continuation, values arguments]
        GrinCpsRaise exception continuation -> node 21 [valueKey exception, valueKey continuation]
        GrinUpdateBlackhole pointer value -> node 22 [valueKey pointer, valueKey value]
        GrinHalt vs -> node 23 [values vs]
        GrinExit status -> node 24 [valueKey status]
        GrinCase scrutinee binder alternatives -> node 25 [valueKey scrutinee, varKey binder, KeyNode 0 <$> mapM alternativeKey alternatives]
        GrinThrow exception -> node 26 [valueKey exception]
        GrinCatch rep action handler state -> node 27 [pure (KeyRep rep), valueKey action, valueKey handler, values state]
        -- A foreign call has no order. Its printed form identifies it, and
        -- foreign calls are rare.
        GrinForeignCallExpr call arguments -> node 28 [pure (KeyText (T.pack (show call))), values arguments]
    alternativeKey (GrinAlt con binders rhs) = node 0 [pure (altConKey con), vars binders, go rhs]
    altConKey con =
      case con of
        GrinDataAlt name index -> KeyNode 0 [KeyText name, KeyInt (toInteger index)]
        GrinLitAlt literal -> KeyNode 1 [literalKey literal]
        GrinDefaultAlt -> KeyNode 2 []

valueKey :: GrinValue -> KeyM Key
valueKey value =
  case value of
    GrinVarValue var -> varKey var
    GrinGlobalValue name -> pure (KeyNode 1 [KeyText name])
    GrinLitValue literal -> pure (KeyNode 2 [literalKey literal])

literalKey :: GrinLiteral -> Key
literalKey literal =
  case literal of
    GrinLitInt rep value -> KeyNode 0 [KeyRep rep, KeyInt value]
    GrinLitChar rep value -> KeyNode 1 [KeyRep rep, KeyChar value]
    GrinLitAddr bytes -> KeyNode 2 [KeyBytes bytes]
