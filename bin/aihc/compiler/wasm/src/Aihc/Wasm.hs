-- | Direct WebAssembly code generation for runtime-explicit GRIN.
module Aihc.Wasm
  ( WasmError (..),
    compileEntry,
    compileModule,
    validatePrimitiveNames,
    validateProgramPrimitives,
    wasip3RuntimeSourcePath,
    wasip3RuntimeSourcePaths,
    wasip3WorldPath,
  )
where

import Aihc.Wasm.Codegen
import Paths_aihc (getDataFileName)

wasip3RuntimeSourcePath :: IO FilePath
wasip3RuntimeSourcePath = getDataFileName "compiler/wasm/runtime/aihc_wasip3.c"

wasip3RuntimeSourcePaths :: IO [FilePath]
wasip3RuntimeSourcePaths =
  mapM
    getDataFileName
    [ "compiler/wasm/runtime/aihc_wasm_libc.c",
      "compiler/wasm/runtime/aihc_wasm_adapter.c",
      "compiler/wasm/runtime/aihc_wasip3.c"
    ]

wasip3WorldPath :: IO FilePath
wasip3WorldPath = getDataFileName "compiler/wasm/runtime/wit"
