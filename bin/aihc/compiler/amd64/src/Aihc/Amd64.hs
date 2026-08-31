-- | Native AMD64 code generation for runtime-explicit GRIN.
module Aihc.Amd64
  ( Amd64Error (..),
    compileEntryObject,
    compileModuleObject,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Amd64.Codegen
  ( Amd64Error (..),
    compileEntryObject,
    compileModuleObject,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Native
  ( NativeTarget (LinuxAmd64),
    nativeTargetTriple,
  )

-- | LLVM target triple for this backend.
targetTriple :: String
targetTriple = nativeTargetTriple LinuxAmd64
