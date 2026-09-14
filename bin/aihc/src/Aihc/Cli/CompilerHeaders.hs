-- | The headers of the emulated GHC installation, for one native target.
--
-- 'Aihc.Hackage.Headers' holds the text of every header.  This module says
-- what the text says for a target of the compiler, puts it where a C compile
-- can read it, and gives the identity that a store entry records.
module Aihc.Cli.CompilerHeaders
  ( cabalPlatformForTarget,
    hostPlatformMacros,
    headerTargetFor,
    compilerHeaderIdentity,
    ensureCompilerHeaders,
  )
where

import Aihc.Cli.ArtifactCache (hashChunks)
import Aihc.Hackage.Headers (HeaderTarget (..), compilerHeaderTexts, writeCompilerHeaders)
import Aihc.Native (NativeTarget (..))
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Distribution.Pretty (prettyShow)
import Distribution.System (Arch (..), OS (..), buildArch, buildOS)

-- | The platform a target's code runs on. The @llvm@ target has no platform
-- of its own: its C is compiled for the host that runs the compiler.
cabalPlatformForTarget :: NativeTarget -> (OS, Arch)
cabalPlatformForTarget target =
  case target of
    AppleArm64 -> (OSX, AArch64)
    LinuxAmd64 -> (Linux, X86_64)
    Llvm -> (buildOS, buildArch)
    Wasm32Wasip3 -> (Wasi, Wasm32)

-- | The @-D@ flags that name the platform a target's code runs on, in the
-- spelling GHC and Cabal use: @darwin_HOST_OS@, not @osx@.
hostPlatformMacros :: NativeTarget -> [String]
hostPlatformMacros target =
  let (os, arch) = platformNames target
   in ["-D" <> os <> "_HOST_OS=1", "-D" <> arch <> "_HOST_ARCH=1"]

platformNames :: NativeTarget -> (String, String)
platformNames target =
  let (os, arch) = cabalPlatformForTarget target
      osName = case os of
        OSX -> "darwin"
        other -> prettyShow other
   in (osName, prettyShow arch)

-- | What the headers of a target say.
--
-- @wasm32@ is ILP32: four-byte pointers and a four-byte @long@.  Every other
-- target is LP64.  The two widths agree on both, which is why each one is
-- written out rather than derived from the other.
headerTargetFor :: NativeTarget -> HeaderTarget
headerTargetFor target =
  HeaderTarget
    { headerPointerBytes = case target of
        Wasm32Wasip3 -> 4
        _ -> 8,
      headerLongBytes = case target of
        Wasm32Wasip3 -> 4
        _ -> 8,
      headerBigEndian = False,
      headerOs = T.pack os,
      headerArch = T.pack arch
    }
  where
    (os, arch) = platformNames target

-- | A digest of every header of the target.
--
-- A store entry records it, so a change to a header rebuilds the packages
-- that its text could have changed.
compilerHeaderIdentity :: NativeTarget -> String
compilerHeaderIdentity target =
  hashChunks
    (concat [[TE.encodeUtf8 (T.pack path), TE.encodeUtf8 text] | (path, text) <- compilerHeaderTexts (headerTargetFor target)])

-- | Write the headers of the target under a root directory and give the
-- directory that the C compiles take as an include directory and the CPP
-- pass over the Haskell sources searches.
ensureCompilerHeaders :: NativeTarget -> FilePath -> IO FilePath
ensureCompilerHeaders target = writeCompilerHeaders (headerTargetFor target)
