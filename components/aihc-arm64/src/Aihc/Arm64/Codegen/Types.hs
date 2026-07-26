-- | Shared state and descriptions used by the AArch64 codegen pipeline.
module Aihc.Arm64.Codegen.Types
  ( Arm64Error (..),
    CompileEnv (..),
    CompiledFunction (..),
    FunctionM,
    FunctionState (..),
    NodeInfo (..),
    ObservedProgram (..),
    RuntimeEnter (..),
    RuntimeInfo (..),
    RuntimeInfoKey (..),
    ValueEnv (..),
  )
where

import Aihc.Grin.Syntax (FunctionName, GrinVar)
import Aihc.Native.BlockLayout qualified as BlockLayout
import Aihc.Native.RegisterAllocate (Location)
import Aihc.Tc.Types (RuntimeRep)
import Control.Monad.Trans.State.Strict (StateT)
import Data.ByteString qualified as BS
import Data.Map.Strict (Map)
import Data.Set qualified as Set
import Data.Text (Text)

data Arm64Error
  = Arm64MissingEntry !Text
  | Arm64MissingGlobal !Text
  | Arm64MissingFunction !FunctionName
  | Arm64MissingConstructor !Text
  | Arm64UnsupportedPrimitive !Text
  | Arm64UnsupportedExpression !Text
  | Arm64UnsupportedValue !Text
  | Arm64UnsupportedRuntimeRep !RuntimeRep
  deriving (Eq, Show)

data CompileEnv = CompileEnv
  { compileConstructorIds :: !(Map Text Int),
    compileConstructorArities :: !(Map Text Int),
    compileGlobalSlots :: !(Map Text Int),
    compileFunctionLabels :: !(Map FunctionName Text),
    compileAddrLiteralLabels :: !(Map BS.ByteString Text),
    compileNodeInfoLabels :: !(Map RuntimeInfoKey Text),
    compileRuntimeInfos :: ![RuntimeInfo],
    compileContinuationFunctions :: !(Set.Set FunctionName),
    compileExposeAllFunctions :: !Bool,
    compileAllowUnsupportedPrimitives :: !Bool
  }

data ObservedProgram = ObservedProgram
  { observedAssembly :: !Text,
    observedMetadataSource :: !Text
  }
  deriving (Eq, Show)

data FunctionState = FunctionState
  { functionNextLabel :: !Int,
    functionNextSlot :: !Int,
    functionBlocksRev :: ![BlockLayout.Block Text Text]
  }

data CompiledFunction = CompiledFunction
  { compiledFunctionSlots :: !Int,
    compiledFunctionLines :: ![Text]
  }

type FunctionM = StateT FunctionState (Either Arm64Error)

data ValueEnv = ValueEnv
  { valueCompileEnv :: !CompileEnv,
    valueLocations :: !(Map GrinVar (Location Text)),
    valueLabelPrefix :: !Text,
    valueFunctionName :: !FunctionName,
    valueFunctionParameters :: ![GrinVar],
    valueBodyLabel :: !Text
  }

data RuntimeInfo = RuntimeInfo
  { runtimeInfoLabel :: !Text,
    runtimeInfoIdentity :: !NodeInfo,
    runtimeInfoFields :: ![RuntimeRep],
    runtimeInfoRemainingArity :: !Int,
    runtimeInfoNext :: !(Maybe Text),
    runtimeInfoEnter :: !(Maybe RuntimeEnter)
  }

data RuntimeEnter = RuntimeEnter
  { runtimeEnterTarget :: !Text,
    runtimeEnterStoredCount :: !Int,
    runtimeEnterSuppliedCount :: !Int
  }

data RuntimeInfoKey
  = ConstructorRuntimeInfo !Text !Int
  | ClosureRuntimeInfo !FunctionName ![RuntimeRep] ![[RuntimeRep]]
  | ThunkRuntimeInfo !FunctionName ![RuntimeRep]
  deriving (Eq, Ord, Show)

data NodeInfo
  = InfoImmediate !Int
  | InfoAddress !Text
