-- | The Lir backend of each target. Every target lowers GC-GRIN to Lir and
-- compiles the Lir module with one backend: a direct object writer for
-- Apple ARM64 and Linux AMD64, or a text form that Clang assembles for LLVM
-- and WebAssembly.
module Aihc.Cli.Backend
  ( BackendOutput (..),
    compileLir,
    lowerTargetFor,
    nativeSourceExtension,
  )
where

import Aihc.Amd64.Lir qualified as Amd64
import Aihc.Arm64.Lir qualified as Arm64
import Aihc.Lir.Lower (LowerTarget, posixTarget64, wasip3Target)
import Aihc.Lir.Syntax (Module)
import Aihc.Llvm.Lir qualified as Llvm
import Aihc.Native (NativeTarget (..))
import Aihc.Wasm.Lir qualified as Wasm
import Data.ByteString.Lazy qualified as BL
import Data.Text (Text)

data BackendOutput
  = -- | A finished object file.
    BackendObject !BL.ByteString
  | -- | A source file for the compiler driver of the target.
    BackendSource !Text

-- | The lowering target of a native target.
lowerTargetFor :: NativeTarget -> LowerTarget
lowerTargetFor target =
  case target of
    Wasm32Wasip3 -> wasip3Target
    _ -> posixTarget64

-- | Compile one Lir module for the target.
compileLir :: NativeTarget -> Module -> Either String BackendOutput
compileLir target lirModule =
  case target of
    AppleArm64 -> either (Left . show) (Right . BackendObject) (Arm64.compileLirObject lirModule)
    LinuxAmd64 -> either (Left . show) (Right . BackendObject) (Amd64.compileLirObject lirModule)
    Llvm -> either (Left . show) (Right . BackendSource) (Llvm.compileLirModule lirModule)
    Wasm32Wasip3 -> either (Left . show) (Right . BackendSource) (Wasm.compileLirModule lirModule)

-- | The extension of the source kept next to an object. An object target
-- keeps the Lir text.
nativeSourceExtension :: NativeTarget -> String
nativeSourceExtension target =
  case target of
    AppleArm64 -> ".lir"
    LinuxAmd64 -> ".lir"
    Llvm -> ".ll"
    Wasm32Wasip3 -> ".s"
