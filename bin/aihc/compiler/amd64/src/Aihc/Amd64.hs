-- | Native AMD64 code generation for runtime-explicit GRIN.
module Aihc.Amd64
  ( Amd64Error (..),
    compileEntry,
    compileEntryObject,
    compileModule,
    compileModuleObject,
    ObservedProgram (..),
    compileObservedFunction,
    snapshotSourcePath,
    targetTriple,
    validateProgramPrimitives,
    validatePrimitiveNames,
  )
where

import Aihc.Amd64.Assemble (assembleElf)
import Aihc.Amd64.Codegen
  ( Amd64Error (..),
    ObservedProgram (..),
    compileEntry,
    compileModule,
    compileObservedFunction,
    validatePrimitiveNames,
    validateProgramPrimitives,
  )
import Aihc.Grin.Gc (GcGrinProgram)
import Aihc.Native
  ( NativeTarget (LinuxAmd64),
    nativeTargetTriple,
    snapshotSourcePath,
  )
import Data.Bifunctor (first)
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T

compileEntryObject :: Either Amd64Error BL.ByteString
compileEntryObject = compileEntry >>= first (Amd64ObjectError . T.pack . show) . assembleElf

compileModuleObject :: GcGrinProgram -> Either Amd64Error BL.ByteString
compileModuleObject program = compileModule program >>= first (Amd64ObjectError . T.pack . show) . assembleElf

-- | LLVM target triple for the assembly emitted by this backend.
targetTriple :: String
targetTriple = nativeTargetTriple LinuxAmd64
