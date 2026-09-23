-- | Inline functions. See the "Inline functions" section of @docs/lir.md@.
--
-- A function declared @inline func@ has no symbol of its own. 'inlineModule'
-- splices its body into every call of it and drops the definition, so a
-- backend never sees one and never emits code for one.
--
-- The splice is a control-flow graph splice, not a textual one. A call
-- @%a, %b = call \@f(args)@ in the middle of a block cuts the block in two:
-- the first half jumps into a renamed copy of the body of @f@, whose entry
-- block takes the parameters of @f@ as block parameters, and every @return@
-- of the body jumps to the second half, which takes the results of the call
-- as block parameters. A @tailcall \@f(args)@ needs no cut: it becomes a
-- jump into the copy, whose returns stay returns.
--
-- The linter rejects what this pass cannot do: recursion among inline
-- functions, a tail call inside an inline body, and a use of the name of an
-- inline function as a value. 'inlineModule' is therefore total and assumes a
-- linted module.
module Aihc.Lir.Inline
  ( prepareModule,
    prepareCheckedModule,
    inlineModule,
    hasInlineFunctions,
  )
where

import Aihc.Lir.Lint (LintError, lintModuleFor)
import Aihc.Lir.Resolve (resolveConstants)
import Aihc.Lir.Syntax
import Control.Monad.Trans.State.Strict (State, runState, state)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

-- | The whole of what a module needs between the linter and a backend:
-- constants substituted and inline functions spliced. Every backend and the
-- interpreter run this, so a module reaches them the same way whether it was
-- parsed from text or lowered by the compiler.
-- The two passes commute: the splice copies operands, addresses and switch
-- cases without reading them, so it neither consumes nor creates a constant
-- reference. Constants are resolved first only because it is the cheaper
-- half of the order -- a constant in an inline body is then substituted once
-- in the definition rather than once in each copy of it.
prepareModule :: Integer -> Module -> Module
prepareModule wordBytes = inlineModule . resolveConstants wordBytes

-- | 'prepareModule', with the module linted as it was written and again as
-- the backend will receive it.
--
-- The second pass is what makes the splice answerable to the same rules as
-- hand-written Lir. Without it nothing checks the module a backend is handed:
-- a splice that dropped a value out of scope, or left a block parameter
-- unbound, reached the backend as @unknown value \<x\>@ or as invalid LLVM IR,
-- naming neither the block nor the rule it broke.
--
-- A caller that does not lint calls 'prepareModule' instead. The backends
-- lint a unit they were given and lint one the compiler lowered only under
-- @--lint@, so the cost of the second pass falls where the splice runs.
prepareCheckedModule :: Integer -> Module -> Either [LintError] Module
prepareCheckedModule wordBytes lirModule =
  case lintModuleFor wordBytes lirModule of
    errors@(_ : _) -> Left errors
    [] ->
      let prepared = prepareModule wordBytes lirModule
       in case lintModuleFor wordBytes prepared of
            errors@(_ : _) -> Left errors
            [] -> Right prepared

-- | Whether a module defines any inline function, so a caller can skip the
-- pass on the modules the compiler itself lowers.
hasInlineFunctions :: Module -> Bool
hasInlineFunctions (Module items) =
  or [functionLinkage function == Inline | ItemFunction function <- items]

-- | Splice every call of an inline function and drop the definitions.
inlineModule :: Module -> Module
inlineModule lirModule@(Module items)
  | not (hasInlineFunctions lirModule) = lirModule
  | otherwise = Module (concatMap rewrite items)
  where
    inlines = [function | ItemFunction function <- items, functionLinkage function == Inline]
    -- An inline function may call another one, so each body is spliced
    -- before the bodies that call it. Splicing a body that still held calls
    -- of its own would leave them behind, and the definitions are dropped.
    expanded :: Map Symbol Function
    expanded = foldl' splice Map.empty (calleesFirst inlines)
    splice done function = Map.insert (functionName function) (inlineInto done function) done
    rewrite item =
      case item of
        ItemFunction function
          | functionLinkage function == Inline -> []
          | otherwise -> [ItemFunction (inlineInto expanded function)]
        _ -> [item]

-- | The inline functions with every one before the functions that call it.
-- The linter rejects recursion, so a depth-first walk gives that order; a
-- cycle that reached here would leave the calls on it unspliced rather than
-- diverge.
calleesFirst :: [Function] -> [Function]
calleesFirst functions = reverse (snd (foldl' visit (Set.empty, []) functions))
  where
    byName = Map.fromList [(functionName function, function) | function <- functions]
    -- The name is marked before the callees are walked, so a cycle stops
    -- here rather than diverging; the linter has already reported it.
    visit (done, ordered) function
      | Set.member name done = (done, ordered)
      | otherwise =
          let (walked, callees) = foldl' visit (Set.insert name done, ordered) (calledFunctions function)
           in (walked, function : callees)
      where
        name = functionName function
    calledFunctions function =
      [ callee
      | block <- functionBlocks function,
        symbol <- calledSymbols block,
        Just callee <- [Map.lookup symbol byName]
      ]
    calledSymbols block =
      [symbol | instruction <- blockInstructions block, Call symbol _ <- [instructionOperation instruction]]
        <> [symbol | TailCall symbol _ <- [blockTerminator block]]

-- | Splice every inline call in one function.
inlineInto :: Map Symbol Function -> Function -> Function
inlineInto inlines function
  | not (any callsInline (functionBlocks function)) = function
  | otherwise = function {functionBlocks = blocks}
  where
    callsInline block =
      any (isInlineOperation . instructionOperation) (blockInstructions block)
        || isInlineTerminator (blockTerminator block)
    isInlineOperation operation =
      case operation of
        Call symbol _ -> Map.member symbol inlines
        _ -> False
    isInlineTerminator terminator =
      case terminator of
        TailCall symbol _ -> Map.member symbol inlines
        _ -> False
    prefix = freshPrefix function
    (spliced, _) = runState (concat <$> mapM (spliceBlock inlines prefix) (functionBlocks function)) 0
    blocks = mergeBlocks spliced

-- | A name prefix that no value and no label of the function starts with, so
-- a renamed copy of a body collides with nothing already there.
freshPrefix :: Function -> Text
freshPrefix function = extend "inl"
  where
    extend candidate
      | any (T.isPrefixOf candidate) names = extend (candidate <> "_")
      | otherwise = candidate
    names =
      [unVar name | (name, _) <- functionParameters function]
        <> concat
          [ unLabel (blockLabel block)
              : [unVar name | (name, _) <- blockParameters block]
                <> [unVar name | instruction <- blockInstructions block, name <- instructionResults instruction]
          | block <- functionBlocks function
          ]

-- | Rewrite one block into the blocks that replace it. The state is the
-- number of splices made in this function, which makes each copy's names
-- unique.
spliceBlock :: Map Symbol Function -> Text -> Block -> State Int [Block]
spliceBlock inlines prefix = go []
  where
    go done block =
      case break isInlineCall (blockInstructions block) of
        (before, instruction : after) -> splice done block before instruction after
        (_, []) -> finish done block
    isInlineCall instruction =
      case instructionOperation instruction of
        Call symbol _ -> Map.member symbol inlines
        _ -> False

    -- The call cuts the block: what precedes it jumps into the body, and what
    -- follows it becomes a block that takes the results of the call.
    splice done block before instruction after = do
      copy <- copyOf (calleeOf instruction)
      let continueLabel = Label (copyPrefix copy <> "return")
          arguments = argumentsOf instruction
          entered = block {blockInstructions = before, blockTerminator = Jump (Target (copyEntry copy) arguments)}
          resumed =
            Block
              { blockLabel = continueLabel,
                blockParameters = zip (instructionResults instruction) (copyResults copy),
                blockInstructions = after,
                blockTerminator = blockTerminator block
              }
      go (done <> [entered] <> copyBlocks copy (Jump . Target continueLabel)) resumed

    -- A tail call needs no cut. The results of the body are the results of
    -- the caller, which the linter has already checked, so its returns stay.
    finish done block =
      case blockTerminator block of
        TailCall symbol arguments
          | Just callee <- Map.lookup symbol inlines -> do
              copy <- copyOf callee
              let entered = block {blockTerminator = Jump (Target (copyEntry copy) arguments)}
              pure (done <> [entered] <> copyBlocks copy Return)
        _ -> pure (done <> [block])

    calleeOf instruction =
      case instructionOperation instruction of
        Call symbol _ -> inlines Map.! symbol
        _ -> error "Lir inline splice reached a call it did not select"
    argumentsOf instruction =
      case instructionOperation instruction of
        Call _ arguments -> arguments
        _ -> error "Lir inline splice reached a call it did not select"
    copyOf callee = state (\index -> (Copy prefix index callee, index + 1))

-- | One renamed copy of the body of an inline function.
data Copy = Copy !Text !Int !Function

copyPrefix :: Copy -> Text
copyPrefix (Copy prefix index _) = prefix <> T.pack (show index) <> "$"

copyResults :: Copy -> [Type]
copyResults (Copy _ _ callee) = functionResults callee

-- | The label the call jumps to. It is the entry block of the body, which
-- takes the parameters of the function as its block parameters: the entry
-- block dominates every other block of the body, so the parameters stay
-- visible throughout it, as they were in the function.
copyEntry :: Copy -> Label
copyEntry copy@(Copy _ _ callee) =
  case functionBlocks callee of
    block : _ -> renameLabel copy (blockLabel block)
    [] -> error ("Lir inline function " <> T.unpack (unSymbol (functionName callee)) <> " has no blocks")

-- | The blocks of the copy, with every @return@ replaced by the terminator
-- the call site needs.
copyBlocks :: Copy -> ([Operand] -> Terminator) -> [Block]
copyBlocks copy@(Copy _ _ callee) onReturn =
  [ renameBlock copy onReturn (isEntry index) block
  | (index, block) <- zip [0 :: Int ..] (functionBlocks callee)
  ]
  where
    parameters = functionParameters callee
    isEntry index = if index == 0 then parameters else []

renameBlock :: Copy -> ([Operand] -> Terminator) -> [(Var, Type)] -> Block -> Block
renameBlock copy onReturn entryParameters block =
  Block
    { blockLabel = renameLabel copy (blockLabel block),
      blockParameters = [(renameVar copy name, ty) | (name, ty) <- entryParameters <> blockParameters block],
      blockInstructions = map renameInstruction (blockInstructions block),
      blockTerminator = mapTerminator operand (renameLabel copy) onReturn (blockTerminator block)
    }
  where
    renameInstruction instruction =
      Instruction
        { instructionResults = map (renameVar copy) (instructionResults instruction),
          instructionOperation = mapOperation operand (instructionOperation instruction)
        }
    operand value =
      case value of
        OperandVar name -> OperandVar (renameVar copy name)
        OperandLiteral _ -> value

renameVar :: Copy -> Var -> Var
renameVar copy (Var name) = Var (copyPrefix copy <> name)

renameLabel :: Copy -> Label -> Label
renameLabel copy (Label name) = Label (copyPrefix copy <> name)

-- | Absorb into its predecessor every block that has exactly one.
--
-- The splice cuts a block in two and adds the blocks of the body between the
-- halves, which leaves straight-line code spread over blocks that jump to
-- one another once. The register allocator would then emit a move for each
-- of their parameters. Putting them back together makes an inline function
-- cost exactly what the body written out at the call site costs, which is
-- what lets a hand-written module use one wherever it reads better.
--
-- This runs only over a function the splice changed.
mergeBlocks :: [Block] -> [Block]
mergeBlocks blocks =
  case [(block, target) | block <- blocks, Just target <- [absorbable block]] of
    [] -> blocks
    (block, target) : _ -> mergeBlocks (absorb block target)
  where
    entry = map blockLabel (take 1 blocks)
    byLabel = Map.fromList [(blockLabel block, block) | block <- blocks]
    -- Every block a terminator can continue at, with repeats: a block that a
    -- switch names twice has two predecessors and stays where it is.
    predecessors =
      Map.fromListWith (+) [(targetLabel target, 1 :: Int) | block <- blocks, target <- terminatorTargets (blockTerminator block)]
    absorbable block =
      case blockTerminator block of
        Jump target
          | label <- targetLabel target,
            label `notElem` entry,
            label /= blockLabel block,
            Map.lookup label predecessors == Just 1,
            Just absorbed <- Map.lookup label byLabel ->
              Just (target, absorbed)
        _ -> Nothing
    absorb block (target, absorbed) =
      [ substitute (if blockLabel other == blockLabel block then joined else other)
      | other <- blocks,
        blockLabel other /= blockLabel absorbed
      ]
      where
        -- The parameters of the absorbed block stand for the arguments of
        -- the jump, which is the whole of what the jump did.
        bindings = Map.fromList (zip (map fst (blockParameters absorbed)) (targetArguments target))
        operand value =
          case value of
            OperandVar name -> Map.findWithDefault value name bindings
            OperandLiteral _ -> value
        -- The absorbed block dominated every block below it, so its
        -- parameters are in scope there and the substitution has to reach
        -- them too, not only the block being joined.
        substitute other =
          other
            { blockInstructions =
                [ instruction {instructionOperation = mapOperation operand (instructionOperation instruction)}
                | instruction <- blockInstructions other
                ],
              blockTerminator = mapTerminator operand id Return (blockTerminator other)
            }
        joined =
          block
            { blockInstructions = blockInstructions block <> blockInstructions absorbed,
              blockTerminator = blockTerminator absorbed
            }

-- | Rewrite the operands an operation reads.
mapOperation :: (Operand -> Operand) -> Operation -> Operation
mapOperation operand operation =
  case operation of
    Binary op ty left right -> Binary op ty (operand left) (operand right)
    Unary op ty value -> Unary op ty (operand value)
    Wide op ty left right -> Wide op ty (operand left) (operand right)
    Compare op ty left right -> Compare op ty (operand left) (operand right)
    FloatBinary op ty left right -> FloatBinary op ty (operand left) (operand right)
    FloatUnary op ty value -> FloatUnary op ty (operand value)
    Convert op from value to -> Convert op from (operand value) to
    PtrToInt value -> PtrToInt (operand value)
    PtrFromInt value -> PtrFromInt (operand value)
    Select ty condition whenTrue whenFalse -> Select ty (operand condition) (operand whenTrue) (operand whenFalse)
    Load ty address align -> Load ty (mapAddress operand address) align
    Store ty value address align -> Store ty (operand value) (mapAddress operand address) align
    PtrAdd base offset -> PtrAdd (operand base) (operand offset)
    StackAlloc _ _ -> operation
    GlobalGet _ -> operation
    GlobalSet symbol value -> GlobalSet symbol (operand value)
    Call symbol arguments -> Call symbol (map operand arguments)
    CallIndirect callee arguments signature -> CallIndirect (operand callee) (map operand arguments) signature

mapAddress :: (Operand -> Operand) -> Address -> Address
mapAddress operand address = address {addressBase = operand (addressBase address)}

-- | Rewrite the operands and the labels of a terminator. @onReturn@ gives
-- the terminator that a @return@ becomes, which is @Return@ everywhere
-- except at a call site that is waiting for the results.
mapTerminator :: (Operand -> Operand) -> (Label -> Label) -> ([Operand] -> Terminator) -> Terminator -> Terminator
mapTerminator operand label onReturn terminator =
  case terminator of
    Jump target -> Jump (mapTarget target)
    Branch condition whenTrue whenFalse -> Branch (operand condition) (mapTarget whenTrue) (mapTarget whenFalse)
    Switch ty scrutinee cases fallback -> Switch ty (operand scrutinee) (map mapCase cases) (fmap mapTarget fallback)
    Return values -> onReturn (map operand values)
    TailCall symbol arguments -> TailCall symbol (map operand arguments)
    TailCallIndirect callee arguments signature -> TailCallIndirect (operand callee) (map operand arguments) signature
    Trap message -> Trap message
  where
    mapTarget target = Target (label (targetLabel target)) (map operand (targetArguments target))
    mapCase switchCase =
      let target = mapTarget (switchCaseTarget switchCase)
       in case switchCase of
            SwitchCase value _ -> SwitchCase value target
            SwitchCaseConstant symbol _ -> SwitchCaseConstant symbol target
