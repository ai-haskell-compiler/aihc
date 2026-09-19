-- | Reading a core library's own sources the way the compiler reads them.
--
-- The compiler reaches a core library module through the package plan, which
-- runs the CPP pass over it and resolves an @#include@ against the headers it
-- wrote for the target.  A tool that reads @core-libs@ straight off disk --
-- the evaluation fixtures, the interface comparison -- has none of that, and
-- a module that uses CPP is a parse error to it.  This is the small piece
-- those tools need: the headers of this host, and the pass that reads them.
module Aihc.Cli.CoreLibrarySource
  ( coreLibraryHeaderDirectory,
    preprocessCoreLibraryModule,
  )
where

import Aihc.Cli.CompilerHeaders (ensureCompilerHeaders)
import Aihc.Cpp qualified as Cpp
import Aihc.Hackage.Cpp (builtinCppMacros)
import Aihc.Native (hostNativeTarget)
import Aihc.Parser.Syntax (Extension (CPP), LanguageEdition (Haskell2010Edition), effectiveExtensions, headerExtensionSettings)
import Aihc.Parser.Token (readModuleHeaderPragmas)
import Data.ByteString qualified as BS
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import System.Directory (doesFileExist)
import System.FilePath (normalise, (</>))
import System.IO.Temp (createTempDirectory, getCanonicalTemporaryDirectory)

-- | Write the compiler's own headers for this host and give back the
-- directory that holds them.
--
-- A core library module reads them through CPP: @Foreign.C.Error@ takes its
-- error numbers from the @CONST_E@/xxx/@ macros of @HsBaseConfig.h@, which
-- the compiler writes for the target it compiles for.
coreLibraryHeaderDirectory :: IO FilePath
coreLibraryHeaderDirectory =
  case hostNativeTarget of
    Nothing -> fail "reading the core library sources needs a native target for this host"
    Just target -> do
      temporaryRoot <- getCanonicalTemporaryDirectory
      directory <- createTempDirectory temporaryRoot "aihc-core-headers"
      ensureCompilerHeaders target directory

-- | Run the CPP pass over a core library module that asks for it.
--
-- The compiler runs CPP on a module whose extensions include @CPP@ and on no
-- other, and this stands in for the reader that decides that: nothing here
-- reads the cabal file, so only the module's own pragmas can say so.  A core
-- library that turned CPP on for a whole component in its cabal file would
-- have to be read here the way the package plan reads it.
preprocessCoreLibraryModule :: FilePath -> FilePath -> Text -> IO Text
preprocessCoreLibraryModule headerDirectory path source
  | not (cppRequested source) = pure source
  | otherwise = drive (Cpp.preprocess config (TE.encodeUtf8 source))
  where
    config =
      Cpp.defaultConfig
        { Cpp.configInputFile = path,
          Cpp.configMacros = Map.mapKeys TE.encodeUtf8 (Map.map TE.encodeUtf8 builtinCppMacros)
        }
    -- Only the compiler's own headers answer an include: a core library
    -- ships none of its own, and one that started to would have to be
    -- resolved here against its @include-dirs@ as well.
    drive step =
      case step of
        Cpp.Done result -> pure (TE.decodeUtf8With lenientDecode (Cpp.resultOutput result))
        Cpp.NeedInclude request k -> do
          let candidate = headerDirectory </> normalise (Cpp.includePath request)
          exists <- doesFileExist candidate
          if exists
            then BS.readFile candidate >>= \content -> drive (k (Just content))
            else fail (path <> ": no header " <> Cpp.includePath request)

-- | Whether a module turns CPP on.
--
-- No language edition carries CPP, so a pragma of the module is the only
-- thing that can, and the edition a reader would fold the pragmas into
-- cannot change the answer.
cppRequested :: Text -> Bool
cppRequested source =
  CPP `elem` effectiveExtensions Haskell2010Edition (headerExtensionSettings (readModuleHeaderPragmas source))
