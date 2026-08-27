-- | LLVM code generation with guaranteed tail calls.
module Aihc.Llvm
  ( LlvmError (..),
    compileEntry,
    compileModule,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
where

import Aihc.Llvm.Codegen
