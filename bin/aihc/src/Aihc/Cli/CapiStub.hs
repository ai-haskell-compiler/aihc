-- | How the C wrappers of the @capi@ foreign imports of a module are
-- compiled.
--
-- What goes into a wrapper is 'Aihc.Capi'; this is the command line that
-- turns it into an object.  It is the one the handwritten C sources of a
-- package take, so a wrapper and the package's own C code see the same
-- target, sysroot and options.
module Aihc.Cli.CapiStub
  ( CapiStubOptions (..),
    noCapiStubOptions,
    capiStubArguments,
  )
where

import Aihc.DataFiles (getDataFileName)
import Aihc.Native (NativeTarget (..), OptimizationLevel, WasmSysroot (..), backendCompiler, handwrittenCArguments, wasmSysroot)
import System.FilePath (takeDirectory)

-- | Where the C compiler looks for the headers a capi wrapper includes.
--
-- These are the include directories and options of the package, because a
-- capi import names a header of the package it is declared in as readily as a
-- system one.
data CapiStubOptions = CapiStubOptions
  { capiStubIncludeDirs :: ![FilePath],
    capiStubCcOptions :: ![String]
  }
  deriving (Eq, Show)

noCapiStubOptions :: CapiStubOptions
noCapiStubOptions = CapiStubOptions [] []

-- | The command line a capi wrapper compile takes, apart from its files.
capiStubArguments :: NativeTarget -> OptimizationLevel -> CapiStubOptions -> IO [String]
capiStubArguments target level options = do
  (_, targetArguments) <- backendCompiler target
  ffiHeader <- getDataFileName "compiler/native/runtime/include/HsFFI.h"
  sysrootIncludes <-
    case target of
      Wasm32Wasip3 -> do
        sysroot <- wasmSysroot
        pure ["-isystem" <> wasmSysrootInclude sysroot]
      _ -> pure []
  pure
    ( targetArguments
        <> handwrittenCArguments level
        <> capiStubCcOptions options
        <> sysrootIncludes
        <> ["-I" <> directory | directory <- capiStubIncludeDirs options]
        <> ["-I" <> takeDirectory ffiHeader]
    )
