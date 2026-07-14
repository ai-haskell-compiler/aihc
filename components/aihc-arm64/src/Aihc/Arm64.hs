-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    compileProgram,
    runtimeSourcePath,
  )
where

import Aihc.Arm64.Codegen (Arm64Error (..), compileProgram)
import Paths_aihc_arm64 (getDataFileName)

-- | Locate the C runtime used by generated assembly.
runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "runtime/aihc_runtime.c"
