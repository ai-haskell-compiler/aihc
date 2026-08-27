-- | Native AMD64 code generation for runtime-explicit GRIN.
module Aihc.Amd64
  ( Amd64Error (..),
    compileEntry,
    compileModule,
    ObservedProgram (..),
    compileObservedFunction,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Amd64.Codegen
  ( Amd64Error (..),
    ObservedProgram (..),
    compileEntry,
    compileModule,
    compileObservedFunction,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( NativeTarget (LinuxAmd64),
    nativeTargetTriple,
    snapshotSourcePath,
  )

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple LinuxAmd64
