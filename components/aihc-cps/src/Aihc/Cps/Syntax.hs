-- | The formal syntax of AIHC's continuation-passing IR, Loom.
--
-- Loom sits immediately after GRIN. Its terms obey this grammar:
--
-- @
-- term ::= letcont continuation in term
--        | invoke operation -> continuation
--        | continue continuation values
--        | case value as variable of alternatives
--        | store-rec bindings in term
-- @
--
-- There is deliberately no implicit return or fall-through term. A function
-- receives its caller's continuation as an explicit parameter, while local
-- continuations are introduced by 'LoomLetCont'. Every path therefore ends in
-- either 'LoomContinue' or an operation with an explicit successor.
module Aihc.Cps.Syntax
  ( LoomProgram (..),
    LoomFunction (..),
    LoomContVar (..),
    LoomContinuation (..),
    LoomTerm (..),
    LoomOperation (..),
    LoomAlt (..),
    loomOperationResultReps,
  )
where

import Aihc.Grin.Syntax
import Aihc.Tc.Types (RuntimeRep, liftedRuntimeRep)
import Data.Text (Text)

-- | A whole Loom program. Heap-layout declarations remain identical to GRIN;
-- only executable function bodies change representation.
data LoomProgram = LoomProgram
  { loomConstructors :: ![(Text, [RuntimeRep])],
    loomPrimitives :: ![(GrinVar, Int)],
    loomForeignCalls :: ![GrinForeignCall],
    loomWhnfGlobals :: ![(GrinVar, GrinNode)],
    loomCafs :: ![(GrinVar, GrinNode)],
    loomFunctions :: ![LoomFunction]
  }
  deriving (Eq, Show)

-- | First-order code with an explicit return-continuation parameter.
data LoomFunction = LoomFunction
  { loomFunctionName :: !FunctionName,
    loomFunctionParameters :: ![GrinVar],
    loomFunctionReturn :: !LoomContVar,
    loomFunctionBody :: !LoomTerm
  }
  deriving (Eq, Show)

-- | A continuation variable and the flattened runtime layout of its arguments.
-- The text and unique form its identity; the layout is its checked signature.
data LoomContVar = LoomContVar
  { loomContName :: !Text,
    loomContUnique :: !Int,
    loomContRuntimeReps :: ![RuntimeRep]
  }
  deriving (Show)

instance Eq LoomContVar where
  left == right =
    loomContName left == loomContName right
      && loomContUnique left == loomContUnique right

instance Ord LoomContVar where
  compare left right =
    compare
      (loomContName left, loomContUnique left)
      (loomContName right, loomContUnique right)

-- | A lexically scoped continuation. Free data and continuation variables in
-- its body are captured lexically; a later closure-conversion pass may make
-- those captures explicit.
data LoomContinuation = LoomContinuation
  { loomContinuationName :: !LoomContVar,
    loomContinuationParameters :: ![GrinVar],
    loomContinuationBody :: !LoomTerm
  }
  deriving (Eq, Show)

-- | CPS control terms. Constructors that contain another term bind lexical
-- structure; the other constructors transfer control and cannot fall through.
data LoomTerm
  = LoomLetCont !LoomContinuation !LoomTerm
  | LoomInvoke !LoomOperation !LoomContVar
  | LoomContinue !LoomContVar ![GrinValue]
  | LoomCase !GrinValue !GrinVar ![LoomAlt]
  | LoomStoreRec ![(GrinVar, GrinNode)] !LoomTerm
  deriving (Eq, Show)

-- | A primitive operation with no implicit successor. Its result is delivered
-- to the continuation named by the surrounding 'LoomInvoke'.
data LoomOperation
  = LoomStore !GrinNode
  | LoomFetch !RuntimeRep !GrinValue
  | LoomUpdate !GrinValue !GrinValue
  | LoomEval !RuntimeRep !GrinValue
  | LoomApply !RuntimeRep !GrinValue ![GrinValue]
  | LoomForeignCall !GrinForeignCall ![GrinValue]
  deriving (Eq, Show)

data LoomAlt = LoomAlt
  { loomAltCon :: !GrinAltCon,
    loomAltBinders :: ![GrinVar],
    loomAltBody :: !LoomTerm
  }
  deriving (Eq, Show)

-- | The register layout delivered by an operation.
loomOperationResultReps :: LoomOperation -> [RuntimeRep]
loomOperationResultReps operation =
  case operation of
    LoomStore {} -> [liftedRuntimeRep]
    LoomFetch runtimeRep _ -> runtimeRepComponents runtimeRep
    LoomUpdate _ value -> [grinValueRuntimeRep value]
    LoomEval runtimeRep _ -> runtimeRepComponents runtimeRep
    LoomApply runtimeRep _ _ -> runtimeRepComponents runtimeRep
    LoomForeignCall foreignCall _ ->
      grinForeignCallResultReps (grinForeignCallSignature foreignCall)
