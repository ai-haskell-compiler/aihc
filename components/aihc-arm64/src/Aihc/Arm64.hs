-- | Native AArch64 code generation for runtime-explicit GRIN.
module Aihc.Arm64
  ( Arm64Error (..),
    LinkLayout,
    buildLinkLayout,
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    runtimeSourcePath,
    validateProgramPrimitives,
  )
where

import Aihc.Arm64.Codegen
  ( Arm64Error (..),
    LinkLayout,
    buildLinkLayout,
    compileModule,
    compileProgram,
    compileProgramWithDependencies,
    extendLinkLayout,
    validateProgramPrimitives,
  )
import Paths_aihc_arm64 (getDataFileName)

-- | Locate the C runtime used by generated assembly.
runtimeSourcePath :: IO FilePath
runtimeSourcePath = getDataFileName "runtime/aihc_runtime.c"
