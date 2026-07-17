{-# LANGUAGE OverloadedStrings #-}

-- | Lower runtime-explicit GRIN to textual AArch64 assembly for Darwin.
-- Generated Haskell entries transfer only with branches; calls are reserved
-- for the C runtime and foreign functions.
module Aihc.Arm64.Codegen
  ( Arm64Error (..),
    LinkLayout,
    LinkInterface,
    buildLinkLayout,
    buildLinkLayoutFromInterfaces,
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    extendLinkLayoutWithInterface,
    extractLinkInterface,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Arm64.Emit (EmitError, renderAllocatedBlock)
import Aihc.Arm64.Lir qualified as Lir
import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep (..))
import Control.Monad (forM, replicateM)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, execStateT, get, modify')
import Data.Char (ord)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Numeric (showHex)

data Arm64Error
  = Arm64MissingEntry !Text
  | Arm64MissingGlobal !Text
  | Arm64MissingFunction !FunctionName
  | Arm64MissingConstructor !Text
  | Arm64UnsupportedPrimitive !Text
  | Arm64UnsupportedExpression !Text
  | Arm64UnsupportedValue !Text
  | Arm64UnsupportedRuntimeRep !RuntimeRep
  | Arm64EmitError !EmitError
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileFunctionArities :: !(Map FunctionName Int),
    compileAllowUnsupportedPrimitives :: !Bool
  }

-- | The process-wide constructor tags and global table slots shared by all
-- native compilation units in one executable. Extending a dependency layout
-- appends user-program entries, so cached dependency objects keep the same
-- assignments regardless of the program that links them.
data LinkLayout = LinkLayout
  { linkConstructors :: ![(Text, Int)],
    linkGlobalNames :: ![Text]
  }
  deriving (Eq, Show)

-- | Constructor and global-table metadata exported by a native compilation
-- unit. Code generation for another unit never needs its GRIN bodies.
data LinkInterface = LinkInterface
  { linkInterfaceConstructors :: ![(Text, Int)],
    linkInterfaceGlobalNames :: ![Text]
  }
  deriving (Eq, Show, Read)

data Block = Block
  { blockLabel :: !Text,
    blockLines :: ![Text]
  }

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![Block]
  }

type FunctionM = StateT FunctionState (Either Arm64Error)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocalSlots :: !(Map GrinVar Int)
  }

compileProgram :: Text -> GrinProgram -> Either Arm64Error Text
compileProgram entryName program =
  compileProgramWithDependencies (buildLinkLayout [program]) [] entryName program

-- | Reject primitives that reachable native code would not execute correctly.
-- Relocatable library objects may carry dormant primitive declarations, but
-- the linked program is checked after whole-program dead-code elimination.
validateProgramPrimitives :: GrinProgram -> Either Arm64Error ()
validateProgramPrimitives program =
  validatePrimitiveNames (map (grinVarName . fst) (grinPrimitives program))

validatePrimitiveNames :: [Text] -> Either Arm64Error ()
validatePrimitiveNames = mapM_ (primitiveIdentifier False)

-- | Build the stable layout for a dependency closure in dependency order.
buildLinkLayout :: [GrinProgram] -> LinkLayout
buildLinkLayout = buildLinkLayoutFromInterfaces . map extractLinkInterface

buildLinkLayoutFromInterfaces :: [LinkInterface] -> LinkLayout
buildLinkLayoutFromInterfaces = foldl extendLinkLayoutWithInterface emptyLinkLayout

extractLinkInterface :: GrinProgram -> LinkInterface
extractLinkInterface program =
  LinkInterface
    { linkInterfaceConstructors = programConstructorArities program,
      linkInterfaceGlobalNames = programGlobalNames program
    }

-- | Append a compilation unit to an existing layout without renumbering any
-- existing constructor or global.
extendLinkLayout :: LinkLayout -> GrinProgram -> LinkLayout
extendLinkLayout layout = extendLinkLayoutWithInterface layout . extractLinkInterface

extendLinkLayoutWithInterface :: LinkLayout -> LinkInterface -> LinkLayout
extendLinkLayoutWithInterface layout interface =
  LinkLayout
    { linkConstructors = uniqueByName (linkConstructors layout <> linkInterfaceConstructors interface),
      linkGlobalNames = uniqueTexts (linkGlobalNames layout <> linkInterfaceGlobalNames interface)
    }

-- | Compile a library SCC to relocatable assembly. The exported initializer
-- installs the unit's primitive, static, and CAF globals into the shared
-- machine table. Constructors are installed once by the executable entry unit.
compileModule :: LinkLayout -> Text -> GrinProgram -> Either Arm64Error Text
compileModule layout initializerSymbol program = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  pure . T.unlines $
    [ ".section __TEXT,__text,regular,pure_instructions",
      ".p2align 2",
      ".globl " <> initializerSymbol,
      initializerSymbol <> ":",
      "  stp x29, x30, [sp, #-48]!",
      "  mov x29, sp",
      "  stp x19, x20, [sp, #16]",
      "  stp x21, x22, [sp, #32]",
      "  mov x22, x0"
    ]
      <> initLines
      <> [ "  ldp x21, x22, [sp, #32]",
           "  ldp x19, x20, [sp, #16]",
           "  ldp x29, x30, [sp], #48",
           "  ret"
         ]
      <> concat functions
  where
    compileEnv = (compileEnvironment layout program) {compileAllowUnsupportedPrimitives = True}

-- | Compile the user program entry unit against cached dependency modules.
-- Dependency initializers are called after constructors are installed and
-- before the user module's own globals are initialized.
compileProgramWithDependencies :: LinkLayout -> [Text] -> Text -> GrinProgram -> Either Arm64Error Text
compileProgramWithDependencies layout dependencyInitializers entryName program = do
  mapM_ validateRuntimeRep (programRuntimeReps program)
  rootSlot <- maybe (Left (Arm64MissingEntry entryName)) Right (Map.lookup entryName globalSlots)
  constructorLines <- compileConstructorInitializers compileEnv
  initLines <- compileInitializers compileEnv program
  functions <- mapM (compileFunction compileEnv) (grinFunctions program)
  pure . T.unlines $
    [ ".section __TEXT,__text,regular,pure_instructions",
      ".p2align 2",
      ".globl _main",
      "_main:",
      "  stp x29, x30, [sp, #-48]!",
      "  mov x29, sp",
      "  stp x19, x20, [sp, #16]",
      "  stp x21, x22, [sp, #32]",
      immediate "x0" (length globalNames),
      "  bl _aihc_machine_new",
      "  mov x22, x0"
    ]
      <> constructorLines
      <> concatMap callInitializer dependencyInitializers
      <> initLines
      <> [ "  ldr x9, [x22, #24]",
           loadAt "x1" "x9" rootSlot,
           "  mov x0, x22",
           "  adr x2, .Laihc_exit",
           "  bl _aihc_start"
         ]
      <> dispatchLines
      <> [ ".Laihc_exit:",
           "  mov w0, #0",
           "  ldp x21, x22, [sp, #32]",
           "  ldp x19, x20, [sp, #16]",
           "  ldp x29, x30, [sp], #48",
           "  ret"
         ]
      <> concat functions
  where
    compileEnv = compileEnvironment layout program
    globalSlots = compileGlobalSlots compileEnv
    globalNames = linkGlobalNames layout
    callInitializer symbol =
      [ "  mov x0, x22",
        "  bl " <> symbol
      ]

emptyLinkLayout :: LinkLayout
emptyLinkLayout =
  LinkLayout
    { linkConstructors = builtinConstructors,
      linkGlobalNames = [name | (name, arity) <- builtinConstructors, arity == 0]
    }

programGlobalNames :: GrinProgram -> [Text]
programGlobalNames program =
  [name | (name, arity) <- programConstructorArities program, arity == 0]
    <> map (grinVarName . fst) (grinWhnfGlobals program)
    <> map (grinVarName . fst) (grinCafs program)

compileEnvironment :: LinkLayout -> GrinProgram -> CompileEnv
compileEnvironment layout program =
  CompileEnv
    { compileConstructorIds = Map.fromList (zip (map fst constructors) [1 ..]),
      compileConstructorArities = Map.fromList constructors,
      compileGlobalSlots = Map.fromList (zip (linkGlobalNames layout) [0 ..]),
      compileFunctionLabels =
        Map.fromList
          ( [ (grinCodeFunctionName info, linkedFunctionLabel (grinCodeSourceName info))
            | info <- grinExternalFunctions program
            ]
              <> [ (grinFunctionName function, localFunctionLabel index function)
                 | (index, function) <- zip [0 ..] (grinFunctions program)
                 ]
          ),
      compileFunctionArities =
        Map.fromList
          ( [ (grinCodeFunctionName info, length (concat (grinCodeParameterLayouts info)))
            | info <- grinExternalFunctions program
            ]
              <> [ (grinFunctionName function, length (grinFunctionParameters function))
                 | function <- grinFunctions program
                 ]
          ),
      compileAllowUnsupportedPrimitives = False
    }
  where
    constructors = linkConstructors layout

compileConstructorInitializers :: CompileEnv -> Either Arm64Error [Text]
compileConstructorInitializers env =
  fmap concat . forM nullaryConstructors $ \(name, constructor) -> do
    slot <- globalSlot env name
    pure $ makeNodeLines 1 (InfoImmediate constructor) 0 0 <> storeGlobal slot
  where
    nullaryConstructors =
      [ (name, constructor)
      | (name, constructor) <- Map.toAscList (compileConstructorIds env),
        Map.lookup name (compileConstructorArities env) == Just 0
      ]

compileInitializers :: CompileEnv -> GrinProgram -> Either Arm64Error [Text]
compileInitializers env program = do
  whnfGlobalLines <- fmap concat . forM (grinWhnfGlobals program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    nodeLines <- materializeNode (ValueEnv env Map.empty) node
    pure (nodeLines <> storeGlobal slot)
  cafCellLines <- fmap concat . forM (grinCafs program) $ \(var, _) -> do
    slot <- globalSlot env (grinVarName var)
    pure (["  bl _aihc_make_cell"] <> storeGlobal slot)
  cafValueLines <- fmap concat . forM (grinCafs program) $ \(var, node) -> do
    slot <- globalSlot env (grinVarName var)
    nodeLines <- materializeNode (ValueEnv env Map.empty) node
    pure $
      nodeLines
        <> [ "  mov x20, x0",
             "  ldr x9, [x22, #24]",
             loadAt "x0" "x9" slot,
             "  mov x1, x20",
             "  bl _aihc_set_cell"
           ]
  pure (cafCellLines <> whnfGlobalLines <> cafValueLines)

compileFunction :: CompileEnv -> GrinFunction -> Either Arm64Error [Text]
compileFunction env function = do
  label <- functionCodeLabel env (grinFunctionName function)
  let localSlots = functionLocalSlots function
      firstScratch = Map.size localSlots
      bodyLabel = label <> "_body"
      initialState = FunctionState 0 firstScratch []
      valueEnv = ValueEnv env localSlots
  finalState <- execStateT (compileExpr valueEnv [] bodyLabel (grinFunctionBody function)) initialState
  let spillBase = functionNextSlot finalState
  (parameterCopies, spillCount) <-
    either (Left . Arm64EmitError) Right (renderAllocatedBlock spillBase (parameterCopyLir localSlots (grinFunctionParameters function)))
  let slotCount = max 1 (spillBase + spillCount)
      entry =
        exportLines function label
          <> [ label <> ":",
               immediate "x0" slotCount,
               "  bl _aihc_alloc_locals",
               "  mov x19, x0",
               "  str x19, [x22, #32]",
               "  ldr x8, [x22, #8]"
             ]
          <> parameterCopies
          <> ["  b " <> bodyLabel]
      blocks = concatMap renderBlock (reverse (functionBlocksRev finalState))
  pure (entry <> blocks)

exportLines :: GrinFunction -> Text -> [Text]
exportLines function label =
  case grinFunctionLinkName function of
    Just _ -> [".globl " <> label]
    Nothing -> []

parameterCopyLir :: Map GrinVar Int -> [GrinVar] -> [Lir.Instruction]
parameterCopyLir slots parameters =
  concat
    [ [ Lir.Load (Lir.Virtual register) (Lir.Physical Lir.X8) (argumentIndex * 8),
        Lir.Store (Lir.Virtual register) (Lir.Physical Lir.X19) (slot * 8)
      ]
    | (argumentIndex, var) <- zip [0 :: Int ..] parameters,
      let register = Lir.VirtualReg argumentIndex,
      Just slot <- [Map.lookup var slots]
    ]

compileExpr :: ValueEnv -> [Text] -> Text -> GrinExpr -> FunctionM ()
compileExpr env prefix label expression =
  case expression of
    GrinReturn values -> do
      valueSlots <- freshSlots (length values)
      valueLines <-
        fmap concat . forM (zip values valueSlots) $ \(value, slot) -> do
          lines' <- liftEither (materializeValue env value)
          pure (lines' <> [storeAt "x0" "x19" slot])
      addBlock label (prefix <> valueLines <> returnValuesLines valueSlots)
    GrinBind vars valueExpression body -> do
      valueLabel <- freshLabel label "bind_value"
      bodyLabel <- freshLabel label "bind_body"
      slot <- contiguousLocalSlots env vars
      addBlock
        label
        ( prefix
            <> pushNormalLines bodyLabel slot (length vars)
            <> ["  b " <> valueLabel]
        )
      compileExpr env [] valueLabel valueExpression
      compileExpr env [] bodyLabel body
    GrinStore node -> do
      nodeLines <- liftEither (materializeNode env node)
      addBlock
        label
        ( prefix
            <> nodeLines
            <> [ "  mov x20, x0",
                 "  bl _aihc_make_cell",
                 "  mov x21, x0",
                 "  mov x1, x20",
                 "  bl _aihc_set_cell",
                 "  mov x0, x21"
               ]
            <> returnLines
        )
    GrinStoreRec {} -> unsupportedExpression "recursive heap allocation"
    GrinFetch {} -> unsupportedExpression "fetch"
    GrinUpdate {} -> unsupportedExpression "update"
    GrinEval runtimeRep value -> do
      valueLines <- liftEither (materializeValue env value)
      addBlock label (prefix <> valueLines <> evalLines runtimeRep)
    GrinCall _ functionName arguments -> do
      target <- liftEither (functionCodeLabel (valueCompileEnv env) functionName)
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "x0" "x19" slot])
      addBlock
        label
        ( prefix
            <> argumentLines
            <> [slotPointer "x8" argumentSlots, "  str x8, [x22, #8]", "  b " <> target]
        )
    GrinPrimitiveCall runtimeRep name arguments
      | name == "realWorld#",
        null arguments,
        null (runtimeRepComponents runtimeRep) ->
          addBlock label (prefix <> returnValuesLines [])
      | compileAllowUnsupportedPrimitives (valueCompileEnv env) ->
          addBlock label (prefix <> ["  bl _aihc_unsupported_primitive", "  b " <> label])
      | otherwise -> unsupportedExpression ("primitive call " <> name)
    GrinApply _ function arguments -> do
      scratch <- freshSlot
      functionLines <- liftEither (materializeValue env function)
      argumentSlots <- freshSlots (length arguments)
      argumentLines <-
        fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
          lines' <- liftEither (materializeValue env argument)
          pure (lines' <> [storeAt "x0" "x19" slot])
      addBlock
        label
        ( prefix
            <> functionLines
            <> [storeAt "x0" "x19" scratch]
            <> argumentLines
            <> [ loadAt "x1" "x19" scratch,
                 immediate "x2" (length arguments),
                 slotPointer "x3" argumentSlots,
                 "  mov x0, x22",
                 "  bl _aihc_apply_values"
               ]
            <> dispatchLines
        )
    GrinCase scrutinee binder alternatives ->
      compileCase env prefix label scrutinee binder alternatives
    GrinThrow {} -> unsupportedExpression "throw"
    GrinCatch {} -> unsupportedExpression "catch"
    GrinForeignCallExpr foreignCall arguments ->
      compileForeignCall env prefix label foreignCall arguments
  where
    unsupportedExpression name = lift (Left (Arm64UnsupportedExpression name))

compileForeignCall :: ValueEnv -> [Text] -> Text -> GrinForeignCall -> [GrinValue] -> FunctionM ()
compileForeignCall env prefix label foreignCall arguments = do
  let signature = grinForeignCallSignature foreignCall
      abiArity = length (grinForeignArgumentTypes signature)
      expectedArity = length (grinForeignOperandReps signature)
  if length arguments /= expectedArity
    then lift (Left (Arm64UnsupportedExpression "foreign call arity mismatch"))
    else
      if abiArity > 8
        then lift (Left (Arm64UnsupportedExpression "foreign calls with more than eight arguments"))
        else do
          argumentSlots <- mapM (const freshSlot) arguments
          argumentLines <-
            fmap concat . forM (zip arguments argumentSlots) $ \(argument, slot) -> do
              valueLines <- liftEither (materializeValue env argument)
              pure (valueLines <> [storeAt "x0" "x19" slot])
          let abiSlots = take abiArity argumentSlots
              loadAbiArguments =
                [ loadAt ("x" <> tshow index) "x19" slot
                | (index, slot) <- zip [0 :: Int ..] abiSlots
                ]
              callLines =
                argumentLines
                  <> loadAbiArguments
                  <> ["  bl _" <> grinForeignCallSymbol foreignCall]
                  <> normalizeForeignResult (grinForeignResultType signature)
          addBlock label (prefix <> callLines <> returnLines)

normalizeForeignResult :: GrinForeignType -> [Text]
normalizeForeignResult foreignType =
  case foreignType of
    GrinForeignInt32 -> ["  sxtw x0, w0"]
    GrinForeignWord64 -> []

compileCase :: ValueEnv -> [Text] -> Text -> GrinValue -> GrinVar -> [GrinAlt] -> FunctionM ()
compileCase env prefix label scrutinee binder alternatives = do
  resultSlot <- freshSlot
  dispatchLabel <- freshLabel label "case_dispatch"
  scrutineeLines <- liftEither (materializeValue env scrutinee)
  let scrutineeRep = grinValueRuntimeRep scrutinee
      scrutineeIsLifted = isLiftedRuntimeRep scrutineeRep
      scrutineeIsPointer = isPointerRuntimeRep scrutineeRep
  if scrutineeIsLifted
    then
      addBlock
        label
        ( prefix
            <> scrutineeLines
            <> ["  mov x20, x0"]
            <> pushNormalLines dispatchLabel resultSlot 1
            <> [ "  mov x1, x20",
                 immediate "x2" (1 :: Int),
                 "  mov x0, x22",
                 "  bl _aihc_eval"
               ]
            <> dispatchLines
        )
    else
      addBlock
        label
        ( prefix
            <> scrutineeLines
            <> [storeAt "x0" "x19" resultSlot, "  b " <> dispatchLabel]
        )
  binderSlot <- localSlot env binder
  alternativeTargets <- forM alternatives $ \alternative -> do
    alternativeLabel <- freshLabel label "case_alt"
    prefixLines <- alternativePrefix env resultSlot alternative
    compileExpr env prefixLines alternativeLabel (grinAltRhs alternative)
    pure (alternative, alternativeLabel)
  checks <- caseChecks env resultSlot scrutineeIsPointer alternativeTargets
  addBlock
    dispatchLabel
    ( [ loadAt "x9" "x19" resultSlot,
        storeAt "x9" "x19" binderSlot
      ]
        <> checks
    )

alternativePrefix :: ValueEnv -> Int -> GrinAlt -> FunctionM [Text]
alternativePrefix env resultSlot alternative =
  case grinAltCon alternative of
    GrinDataAlt _ ->
      fmap concat . forM (zip [0 ..] (grinAltBinders alternative)) $ \(index, binder) -> do
        slot <- localSlot env binder
        pure
          [ loadAt "x9" "x19" resultSlot,
            loadByteOffset "x10" "x9" (32 + index * 8),
            storeAt "x10" "x19" slot
          ]
    GrinLitAlt _ -> pure []
    GrinDefaultAlt ->
      fmap concat . forM (grinAltBinders alternative) $ \binder -> do
        slot <- localSlot env binder
        pure
          [ loadAt "x9" "x19" resultSlot,
            storeAt "x9" "x19" slot
          ]

caseChecks :: ValueEnv -> Int -> Bool -> [(GrinAlt, Text)] -> FunctionM [Text]
caseChecks env resultSlot scrutineeIsPointer targets = do
  let nonDefault = [(alternative, label) | (alternative, label) <- targets, grinAltCon alternative /= GrinDefaultAlt]
      defaultTarget = [label | (alternative, label) <- targets, grinAltCon alternative == GrinDefaultAlt]
  checks <- fmap concat . forM nonDefault $ \(alternative, target) ->
    case grinAltCon alternative of
      GrinDataAlt name -> do
        if scrutineeIsPointer
          then do
            identifier <- liftEither (constructorId (valueCompileEnv env) name)
            pure
              [ loadAt "x9" "x19" resultSlot,
                "  ldr x10, [x9, #0]",
                "  cmp x10, #1",
                "  b.ne 1f",
                "  ldr x10, [x9, #8]",
                "  cmp x10, #" <> tshow identifier,
                "  b.eq " <> target,
                "1:"
              ]
          else lift (Left (Arm64UnsupportedExpression "constructor case on an unboxed value"))
      GrinLitAlt literal ->
        case normalizedLiteralInteger literal of
          Just integer ->
            if scrutineeIsPointer
              then lift (Left (Arm64UnsupportedExpression "literal case on a lifted value"))
              else
                pure
                  [ loadAt "x10" "x19" resultSlot,
                    immediate "x11" integer,
                    "  cmp x10, x11",
                    "  b.eq " <> target
                  ]
          Nothing -> lift (Left (Arm64UnsupportedValue "string case alternative"))
      GrinDefaultAlt -> pure []
  pure $
    checks
      <> case defaultTarget of
        target : _ -> ["  b " <> target]
        [] -> ["  bl _aihc_no_match", "  brk #0"]

materializeValue :: ValueEnv -> GrinValue -> Either Arm64Error [Text]
materializeValue env value =
  case value of
    GrinVarValue var -> loadVariable env var
    GrinLitValue literal -> materializeLiteral literal
    GrinNodeValue node -> materializeNode env node

materializeLiteral :: GrinLiteral -> Either Arm64Error [Text]
materializeLiteral literal =
  case normalizedLiteralInteger literal of
    Just integer -> Right [immediate "x0" integer]
    Nothing -> Left (Arm64UnsupportedValue "string literal")

normalizedLiteralInteger :: GrinLiteral -> Maybe Integer
normalizedLiteralInteger literal = do
  integer <- literalInteger literal
  pure $
    case literal of
      GrinLitInt runtimeRep _ -> normalizeScalar runtimeRep integer
      GrinLitChar {} -> normalizeUnsigned 64 integer
      GrinLitString {} -> integer

normalizeScalar :: RuntimeRep -> Integer -> Integer
normalizeScalar runtimeRep integer =
  case runtimeRep of
    IntRep -> normalizeSigned 64 integer
    Int8Rep -> normalizeSigned 8 integer
    Int16Rep -> normalizeSigned 16 integer
    Int32Rep -> normalizeSigned 32 integer
    Int64Rep -> normalizeSigned 64 integer
    WordRep -> normalizeUnsigned 64 integer
    Word8Rep -> normalizeUnsigned 8 integer
    Word16Rep -> normalizeUnsigned 16 integer
    Word32Rep -> normalizeUnsigned 32 integer
    Word64Rep -> normalizeUnsigned 64 integer
    _ -> integer

normalizeSigned :: Int -> Integer -> Integer
normalizeSigned bits integer =
  let modulus = 2 ^ bits
      signBit = 2 ^ (bits - 1)
      unsigned = integer `mod` modulus
   in if unsigned >= signBit then unsigned - modulus else unsigned

normalizeUnsigned :: Int -> Integer -> Integer
normalizeUnsigned bits integer = integer `mod` (2 ^ bits)

literalInteger :: GrinLiteral -> Maybe Integer
literalInteger literal =
  case literal of
    GrinLitInt _ integer -> Just integer
    GrinLitChar _ character -> Just (fromIntegral (ord character))
    GrinLitString _ -> Nothing

materializeNode :: ValueEnv -> GrinNode -> Either Arm64Error [Text]
materializeNode env node = do
  (kind, info, arity) <- nodeHeader env node
  fieldLines <- fmap concat . forM (zip [0 :: Int ..] (grinNodeFields node)) $ \(index, field) -> do
    unlessAtomic field
    valueLines <- materializeValue env field
    pure $
      valueLines
        <> [ "  mov x2, x0",
             "  mov x0, x20",
             immediate "x1" index,
             "  bl _aihc_set_field"
           ]
  pure $
    makeNodeLines kind info arity (length (grinNodeFields node))
      <> ["  mov x20, x0"]
      <> fieldLines
      <> ["  mov x0, x20"]
  where
    unlessAtomic field =
      case field of
        GrinNodeValue {} -> Left (Arm64UnsupportedValue "nested node field")
        _ -> Right ()

nodeHeader :: ValueEnv -> GrinNode -> Either Arm64Error (Int, NodeInfo, Int)
nodeHeader env node =
  case grinNodeTag node of
    GrinConstructor name -> do
      identifier <- constructorId compileEnv name
      arity <- constructorArity compileEnv name
      pure (1, InfoImmediate identifier, arity)
    GrinClosure functionName argumentCount -> do
      label <- functionCodeLabel compileEnv functionName
      pure (2, InfoAddress label, argumentCount)
    GrinThunk functionName -> do
      label <- functionCodeLabel compileEnv functionName
      arity <- functionArity compileEnv functionName
      pure (3, InfoAddress label, arity)
    GrinPrimitive name arity -> do
      identifier <- primitiveId compileEnv name
      pure (4, InfoImmediate identifier, arity)
  where
    compileEnv = valueCompileEnv env

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text

makeNodeLines :: Int -> NodeInfo -> Int -> Int -> [Text]
makeNodeLines kind info arity count =
  [ immediate "x0" kind,
    infoLine info,
    immediate "x2" arity,
    immediate "x3" count,
    "  bl _aihc_make_node"
  ]
  where
    infoLine nodeInfo =
      case nodeInfo of
        InfoImmediate integer -> immediate "x1" integer
        InfoAddress label -> address "x1" label

loadVariable :: ValueEnv -> GrinVar -> Either Arm64Error [Text]
loadVariable env var =
  case Map.lookup var (valueLocalSlots env) of
    Just slot -> Right [loadAt "x0" "x19" slot]
    Nothing -> do
      slot <- globalSlot (valueCompileEnv env) (grinVarName var)
      pure ["  ldr x9, [x22, #24]", loadAt "x0" "x9" slot]

localSlot :: ValueEnv -> GrinVar -> FunctionM Int
localSlot env var =
  case Map.lookup var (valueLocalSlots env) of
    Just slot -> pure slot
    Nothing -> lift (Left (Arm64UnsupportedExpression ("missing local slot for " <> grinVarName var)))

freshSlot :: FunctionM Int
freshSlot = do
  state <- get
  let slot = functionNextSlot state
  modify' $ \current -> current {functionNextSlot = slot + 1}
  pure slot

freshSlots :: Int -> FunctionM [Int]
freshSlots count = replicateM count freshSlot

contiguousLocalSlots :: ValueEnv -> [GrinVar] -> FunctionM Int
contiguousLocalSlots _ [] = pure 0
contiguousLocalSlots env vars = do
  slots <- mapM (localSlot env) vars
  case slots of
    first : rest
      | rest == [first + 1 .. first + length rest] -> pure first
      | otherwise -> lift (Left (Arm64UnsupportedExpression "non-contiguous multi-value bind"))
    [] -> pure 0

freshLabel :: Text -> Text -> FunctionM Text
freshLabel parent kind = do
  state <- get
  let identifier = functionNextLabel state
  modify' $ \current -> current {functionNextLabel = identifier + 1}
  pure (parent <> "_" <> kind <> "_" <> tshow identifier)

addBlock :: Text -> [Text] -> FunctionM ()
addBlock label lines' =
  modify' $ \state -> state {functionBlocksRev = Block label lines' : functionBlocksRev state}

renderBlock :: Block -> [Text]
renderBlock block = blockLabel block <> ":" : blockLines block

pushNormalLines :: Text -> Int -> Int -> [Text]
pushNormalLines code slot count =
  [ "  mov x0, x22",
    "  adr x1, " <> code,
    "  mov x2, x19",
    immediate "x3" slot,
    immediate "x4" count,
    "  bl _aihc_push_normal"
  ]

returnValuesLines :: [Int] -> [Text]
returnValuesLines slots =
  [ "  mov x0, x22",
    immediate "x1" (length slots),
    slotPointer "x2" slots,
    "  bl _aihc_return_values"
  ]
    <> dispatchLines

slotPointer :: Text -> [Int] -> Text
slotPointer register slots =
  case slots of
    first : _ -> "  add " <> register <> ", x19, #" <> tshow (first * 8)
    [] -> "  mov " <> register <> ", xzr"

returnLines :: [Text]
returnLines =
  [ "  mov x1, x0",
    "  mov x0, x22",
    "  bl _aihc_return"
  ]
    <> dispatchLines

evalLines :: RuntimeRep -> [Text]
evalLines runtimeRep =
  [ "  mov x1, x0",
    immediate "x2" (fromEnum (isLiftedRuntimeRep runtimeRep)),
    "  mov x0, x22",
    "  bl _aihc_eval"
  ]
    <> dispatchLines

dispatchLines :: [Text]
dispatchLines =
  [ "  ldr x19, [x22, #32]",
    "  ldr x9, [x22, #0]",
    "  br x9"
  ]

globalSlot :: CompileEnv -> Text -> Either Arm64Error Int
globalSlot env name =
  maybe (Left (Arm64MissingGlobal name)) Right (Map.lookup name (compileGlobalSlots env))

constructorId :: CompileEnv -> Text -> Either Arm64Error Int
constructorId env name =
  maybe (Left (Arm64MissingConstructor name)) Right (Map.lookup name (compileConstructorIds env))

constructorArity :: CompileEnv -> Text -> Either Arm64Error Int
constructorArity env name =
  maybe (Left (Arm64MissingConstructor name)) Right (Map.lookup name (compileConstructorArities env))

functionCodeLabel :: CompileEnv -> FunctionName -> Either Arm64Error Text
functionCodeLabel env name =
  maybe (Left (Arm64MissingFunction name)) Right (Map.lookup name (compileFunctionLabels env))

functionArity :: CompileEnv -> FunctionName -> Either Arm64Error Int
functionArity env name =
  maybe (Left (Arm64MissingFunction name)) Right (Map.lookup name (compileFunctionArities env))

primitiveId :: CompileEnv -> Text -> Either Arm64Error Int
primitiveId env = primitiveIdentifier (compileAllowUnsupportedPrimitives env)

primitiveIdentifier :: Bool -> Text -> Either Arm64Error Int
primitiveIdentifier allowUnsupported name
  | name == "realWorld#" = Right 1
  | allowUnsupported = Right 0
  | otherwise = Left (Arm64UnsupportedPrimitive name)

functionLocalSlots :: GrinFunction -> Map GrinVar Int
functionLocalSlots function = snd (foldl' assignGroup (0, Map.empty) groups)
  where
    groups = grinFunctionParameters function : boundVarGroups (grinFunctionBody function)
    assignGroup = foldl' assignVar
    assignVar (next, slots) var =
      case Map.lookup var slots of
        Just _ -> (next, slots)
        Nothing -> (next + 1, Map.insert var next slots)

boundVarGroups :: GrinExpr -> [[GrinVar]]
boundVarGroups expression =
  case expression of
    GrinReturn _ -> []
    GrinBind vars valueExpression body -> vars : boundVarGroups valueExpression <> boundVarGroups body
    GrinStore _ -> []
    GrinStoreRec bindings body -> map (pure . fst) bindings <> boundVarGroups body
    GrinFetch _ _ -> []
    GrinUpdate _ _ -> []
    GrinEval _ _ -> []
    GrinCall {} -> []
    GrinPrimitiveCall {} -> []
    GrinApply {} -> []
    GrinCase _ binder alternatives -> [binder] : concatMap altBoundVarGroups alternatives
    GrinThrow _ -> []
    GrinCatch {} -> []
    GrinForeignCallExpr {} -> []
  where
    altBoundVarGroups alternative = grinAltBinders alternative : boundVarGroups (grinAltRhs alternative)

uniqueTexts :: [Text] -> [Text]
uniqueTexts = reverse . snd . foldl' step (Set.empty, [])
  where
    step (seen, values) value
      | value `Set.member` seen = (seen, values)
      | otherwise = (Set.insert value seen, value : values)

uniqueByName :: [(Text, Int)] -> [(Text, Int)]
uniqueByName values =
  [ (name, arity)
  | name <- uniqueTexts (map fst values),
    Just arity <- [lookup name values]
  ]

programConstructorArities :: GrinProgram -> [(Text, Int)]
programConstructorArities program =
  [(name, length fieldReps) | (name, fieldReps) <- grinConstructors program]

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
  concatMap snd (grinConstructors program)
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

exprRuntimeReps :: GrinExpr -> [RuntimeRep]
exprRuntimeReps expression =
  case expression of
    GrinReturn values -> concatMap valueRuntimeReps values
    GrinBind vars valueExpression body ->
      map grinVarRuntimeRep vars <> exprRuntimeReps valueExpression <> exprRuntimeReps body
    GrinStore node -> nodeRuntimeReps node
    GrinStoreRec bindings body ->
      concatMap (\(var, node) -> grinVarRuntimeRep var : nodeRuntimeReps node) bindings
        <> exprRuntimeReps body
    GrinFetch runtimeRep pointer -> runtimeRep : valueRuntimeReps pointer
    GrinUpdate pointer value -> valueRuntimeReps pointer <> valueRuntimeReps value
    GrinEval runtimeRep value -> runtimeRep : valueRuntimeReps value
    GrinCall runtimeRep _ arguments ->
      runtimeRep : concatMap valueRuntimeReps arguments
    GrinPrimitiveCall runtimeRep _ arguments ->
      runtimeRep : concatMap valueRuntimeReps arguments
    GrinApply runtimeRep function arguments ->
      runtimeRep : valueRuntimeReps function <> concatMap valueRuntimeReps arguments
    GrinCase scrutinee binder alternatives ->
      valueRuntimeReps scrutinee
        <> (grinVarRuntimeRep binder : concatMap altRuntimeReps alternatives)
    GrinThrow exception -> valueRuntimeReps exception
    GrinCatch runtimeRep action handler state ->
      runtimeRep : concatMap valueRuntimeReps (action : handler : state)
    GrinForeignCallExpr foreignCall arguments ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall)
        <> concatMap valueRuntimeReps arguments
  where
    altRuntimeReps alternative =
      map grinVarRuntimeRep (grinAltBinders alternative)
        <> exprRuntimeReps (grinAltRhs alternative)

valueRuntimeReps :: GrinValue -> [RuntimeRep]
valueRuntimeReps value =
  grinValueRuntimeRep value
    : case value of
      GrinNodeValue node -> nodeRuntimeReps node
      _ -> []

nodeRuntimeReps :: GrinNode -> [RuntimeRep]
nodeRuntimeReps node = concatMap valueRuntimeReps (grinNodeFields node)

functionLabel :: Int -> Text
functionLabel index = ".Laihc_function_" <> tshow index

localFunctionLabel :: Int -> GrinFunction -> Text
localFunctionLabel index function =
  case grinFunctionLinkName function of
    Just name -> linkedFunctionLabel name
    Nothing -> functionLabel index

linkedFunctionLabel :: Text -> Text
linkedFunctionLabel name =
  "_aihc_entry_" <> T.concatMap encode name
  where
    encode character = T.pack (showHex (ord character) "_")

storeGlobal :: Int -> [Text]
storeGlobal slot =
  [ "  ldr x9, [x22, #24]",
    storeAt "x0" "x9" slot
  ]

loadAt :: Text -> Text -> Int -> Text
loadAt destination base slot = loadByteOffset destination base (slot * 8)

storeAt :: Text -> Text -> Int -> Text
storeAt source base slot = storeByteOffset source base (slot * 8)

loadByteOffset :: Text -> Text -> Int -> Text
loadByteOffset destination base offset =
  "  ldr " <> destination <> ", [" <> base <> ", #" <> tshow offset <> "]"

storeByteOffset :: Text -> Text -> Int -> Text
storeByteOffset source base offset =
  "  str " <> source <> ", [" <> base <> ", #" <> tshow offset <> "]"

immediate :: (Show value) => Text -> value -> Text
immediate register value = "  ldr " <> register <> ", =" <> T.pack (show value)

address :: Text -> Text -> Text
address register label =
  "  adrp " <> register <> ", " <> label <> "@PAGE\n  add " <> register <> ", " <> register <> ", " <> label <> "@PAGEOFF"

tshow :: (Show value) => value -> Text
tshow = T.pack . show

liftEither :: Either Arm64Error value -> FunctionM value
liftEither = lift
