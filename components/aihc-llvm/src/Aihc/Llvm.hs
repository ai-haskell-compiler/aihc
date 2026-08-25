-- | LLVM code generation with guaranteed tail calls.
module Aihc.Llvm
  ( LlvmError (..),
    compileModule,
    compileProgram,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Llvm.Codegen
