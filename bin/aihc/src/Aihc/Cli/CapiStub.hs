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

import Aihc.Native (NativeTarget (..), OptimizationLevel, WasmSysroot (..), backendCompiler, handwrittenCArguments, wasmSysroot)

-- | Where the C compiler looks for the headers a capi wrapper includes.
--
-- These are the include directories and options of the package, because a
-- capi import names a header of the package it is declared in as readily as a
-- system one.  The headers of the compiler come after them.
data CapiStubOptions = CapiStubOptions
  { capiStubIncludeDirs :: ![FilePath],
    capiStubCcOptions :: ![String]
  }
  deriving (Eq, Show)

noCapiStubOptions :: CapiStubOptions
noCapiStubOptions = CapiStubOptions [] []

-- | The command line a capi wrapper compile takes, apart from its files.
--
-- The header directory is the one 'Aihc.Cli.CompilerHeaders.ensureCompilerHeaders'
-- wrote for this target, because a wrapper includes @HsFFI.h@ and the header
-- of the package can include any other header of the compiler.
capiStubArguments :: NativeTarget -> OptimizationLevel -> CapiStubOptions -> FilePath -> IO [String]
capiStubArguments target level options headerDirectory = do
  (_, targetArguments) <- backendCompiler target
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
        <> ["-I" <> headerDirectory]
    )
